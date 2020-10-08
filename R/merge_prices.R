#' Merge REMIND-derived fuel prices with non-fuel costs.
#'
#' @param gdx path to REMIND binary output, gdx format
#' @param REMINDmapping mapping of REMIND regions to ISO3 country codes
#' @param REMINDyears range of REMIND timesteps
#' @param intensity_data logit level intensity data
#' @param nonfuel_costs logti level non-fuel costs
#' @param module GDX input is based on old "complex" module or new "edge_esm" module
#' @import data.table
#' @importFrom rmndt disaggregate_dt magpie2dt
#' @importFrom gdx readGDX
#' @importFrom magclass time_interpolate lowpass dimSums mbind
#' @export

merge_prices <- function(gdx, REMINDmapping, REMINDyears,
                         intensity_data,
                         nonfuel_costs,
                         module="edge_esm") {

    sector_fuel <- pih <- value <- fuel_price <- fuel_price_pkm <- EJ_Mpkm_final <- NULL
    non_fuel_price <- technology <- GDP_cap <- iso <- `.` <- weight <- POP_val <- NULL
    GDP <- yearconv <- time <- year_at_yearconv <- non_fuel_price_new <- non_fuel_price_conv <- NULL
    vehicle_type <- subsector_L3 <- subsector_L2 <- subsector_L1 <- sector <- tot_price <- NULL

    ## report prices from REMIND gdx in 2005$/MJ

    tdptwyr2dpgj <- 31.71  #TerraDollar per TWyear to Dollar per GJ
    CONV_2005USD_1990USD <- 0.67
    startyear <- 2020

    ## load entries from the gdx
    fety <- readGDX(gdx, c("entyFe", "fety"), format = "first_found")
    pebal_subset <- c("pegas")

    budget.m <- readGDX(gdx, name = "qm_budget", types = "equations", field = "m",
                        format = "first_found")[, REMINDyears[REMINDyears >= startyear],]  # Alternative: calcPrice

    interpolate_first_timesteps <- function(obj){
        ## interpolate values for 1990, 2005 and 2010
        obj = time_interpolate(obj, c(1990, seq(2005, startyear, 5)),
                             integrate_interpolated_years=T,
                             extrapolation_type = "constant")
        return(obj)
    }

    budget.m <- interpolate_first_timesteps(budget.m)
    budget.m <- lowpass(budget.m)

    if(module == "edge_esm"){

        bal_eq <- "qm_balFeForCesAndEs"
        febal.m <- readGDX(gdx, name = bal_eq, types = "equations",
                           field = "m", format = "first_found")

        if (is.null(febal.m)){ # temporary fix for compatibility with REMIND-EU
          febal.m <- readGDX(gdx, name = "q35_demFeTrans", types = "equations",
                             field = "m", format = "first_found")
          febal.m <- dimSums(febal.m,dim=c(3.2))
        }

        febal.m <- febal.m[, REMINDyears[REMINDyears>=startyear], fety]

        if(any(febal.m > 0)){
            sprintf("Found positive marginals on %s. We correct this, but the issue should be adressed.", bal_eq)
            febal.m[febal.m > 0] <- -1e-10
        }

        febal.m <- interpolate_first_timesteps(febal.m)

        ## in some regions and time steps, 0 final energy demand for an entry could give problems
        tmp <- magclass::setNames(lowpass(lowpass(febal.m[, , "fegat"]))/(budget.m + 1e-10) * tdptwyr2dpgj, "delivered gas")
    }else{
        bal_eq <- "q_balFe"
        pebal.m <- readGDX(gdx, name = c("q_balPe", "qm_pebal"), types = "equations",
                           field = "m", format = "first_found")[, REMINDyears[REMINDyears>=startyear],
                                                                pebal_subset]
        febal.m <- readGDX(gdx, name = bal_eq, types = "equations",
                           field = "m", format = "first_found")[, REMINDyears[REMINDyears>=startyear],
                                                                fety]
        if(any(febal.m > 0)){
            sprintf("Found positive marginals on %s. We correct this, but the issue should be adressed.", bal_eq)
            febal.m[febal.m > 0] <- -1e-10
        }

        febal.m <- interpolate_first_timesteps(febal.m)
        pebal.m <- interpolate_first_timesteps(pebal.m)

        ## Nat. Gas too cheap on PE level (4x)
        tmp <- magclass::setNames(4*lowpass(lowpass(pebal.m[, , "pegas"]))/(budget.m + 1e-10) * tdptwyr2dpgj, "delivered gas")
    }
    sprintf("Loading prices for module `%s` from eq. `%s`.", module, bal_eq)

    tmp <- mbind(tmp, magclass::setNames(lowpass(lowpass(febal.m[, , "feelt"]))/(budget.m + 1e-10) * tdptwyr2dpgj, "elect_td_trn"))

    tmp <- mbind(tmp, magclass::setNames(lowpass(lowpass(febal.m[, , "feh2t"]))/(budget.m + 1e-10) * tdptwyr2dpgj, "H2 enduse"))

    tmp <- mbind(tmp, magclass::setNames((dimSums(lowpass(febal.m[, , "fedie"] + febal.m[, , "fepet"]), dim=3))/(2*budget.m + 1e-10) * tdptwyr2dpgj, "refined liquids enduse"))

    tmp <- magpie2dt(tmp, regioncol = "region", yearcol = "year", datacols = "sector_fuel")

    ## define plug in hybrid as consuming 60% liquids and 40% electricity
    tmp_PIH <- tmp[sector_fuel %in% c("elect_td_trn", "refined liquids enduse")]
    tmp_PIH[, pih := 0.6*value[sector_fuel == "refined liquids enduse"] + 0.4*value[sector_fuel == "elect_td_trn"], by = c("region", "year")]
    tmp_PIH <- tmp_PIH[sector_fuel == "elect_td_trn"][, c("sector_fuel", "value") := list("Liquids-Electricity", NULL)]
    setnames(tmp_PIH, old = "pih", new = "value")

    tmp <- rbind(tmp, tmp_PIH)

    test <- tmp[year > 2005 & value <= 0]
    if(nrow(test)){
        print(paste("Zero Fuel Prices found. Regions:", unique(test$region)))
    }

    fuel_price_REMIND <- disaggregate_dt(tmp, mapping = REMINDmapping)

    ## rename the fuel price
    setnames(fuel_price_REMIND, old = c("value"), new = c("fuel_price"))

    ## fuel price in 2005USD/GJ -> 1990USD/EJ

    fuel_price_REMIND[, fuel_price := fuel_price * CONV_2005USD_1990USD * 1e9]

    ## join with vehicle intensity and load factor to get the 1990USD/pkm

    km_intensity <- intensity_data[year %in% REMINDyears]
    fuel_price_REMIND <- merge(fuel_price_REMIND, km_intensity, by = c("iso",
        "year", "sector_fuel"), all.y = TRUE)

    ## fuel_price [$/EJ * EJ/Mpkm * Mpkm/pkm],
    tech_cost <- fuel_price_REMIND[, fuel_price_pkm := fuel_price * EJ_Mpkm_final * 1e-6]

    ## merge the non energy prices, they are $/pkm
    tech_cost <- merge(tech_cost, nonfuel_costs,
                        by = c("iso", "year", "technology",
                               "vehicle_type", "subsector_L1",
                               "subsector_L2", "subsector_L3","sector"),
                        all.x = TRUE)

    ## missing non energy price for coal, Adv-Electric and Adv-Liquids freight rail
    ## (is not in the original database). Attribute the same non energy price as in
    ## El. Freight Rail
    tech_cost[, non_fuel_price := ifelse(technology=="Adv-Electric", .SD[technology == "Electric"]$non_fuel_price, non_fuel_price), by=c("year", "iso")]
    tech_cost[, non_fuel_price := ifelse(technology=="Adv-Liquid", .SD[technology == "Liquids"]$non_fuel_price, non_fuel_price), by=c("year", "iso")]

    ## convergence of non_fuel_price according to GDPcap
    ## working principle: non_fuel_price follows linear convergence between 2010 and the year it reaches GDPcap@(2010,richcountry). Values from richcountry for the following time steps (i.e. when GDPcap@(t,developing)>GDPcap@(2010,richcountry))
    ## load gdp per capita
    GDP_POP = getRMNDGDPcap(usecache = TRUE)

    tmp = merge(tech_cost, GDP_POP, by = c("iso", "year"))

    ## define rich countries
    richcountries = unique(unique(tmp[year == 2010 & GDP_cap > 25000, iso]))
    ## calculate average non fuel price (averaged on GDP) across rich countries and find total GDP and population
    richave = tmp[iso %in% richcountries & non_fuel_price > 0,]
    richave = richave[, .(non_fuel_price = sum(non_fuel_price*weight)/sum(weight)), by = c("subsector_L1", "vehicle_type", "technology", "year")]
    GDP_POP = GDP_POP[iso %in% richcountries,]
    GDP_POP = GDP_POP[, .(GDP = sum(weight), POP_val = sum(POP_val)), by = c("year")]
    richave = merge(richave, GDP_POP, by = "year")
    ## average gdp per capita of the rich countries
    richave[, GDP_cap := GDP/POP_val]

    ## dt on which the GDPcap is checked
    tmp1 = tmp[!iso %in% richcountries, c("iso", "year", "non_fuel_price", "GDP_cap", "technology", "vehicle_type", "fuel_price", "subsector_L1", "subsector_L2", "subsector_L3", "sector", "sector_fuel", "EJ_Mpkm_final" , "fuel_price_pkm")]
    ## dt contaning the gdp towards which to converge
    tmp2 = richave[, c("year", "GDP_cap")]
    ## dt containing the non fuel price for rich countries
    tmp3 = richave[, c("year", "non_fuel_price", "technology", "vehicle_type")]
    ## names has to be different across dts for roll join
    setnames(tmp2, old = c("year"), new = c("time"))
    setnames(tmp3, old = c("year", "non_fuel_price"), new = c("time", "non_fuel_price_new"))

    setkey(tmp1,GDP_cap)
    setkey(tmp2,GDP_cap)

    ## find the time step at which the GDPcap matches the GDPcap of the rich countries
    tmp2 <- tmp2[tmp1, roll = "nearest", on = .(GDP_cap)]

    ## merge with non fuel price of corresponding values
    tmp2 = merge(tmp2, tmp3, by = c("time", "technology", "vehicle_type"))

    ## find year closest to 2010 for each ISO, this is the year at which is going to converge
    tmp2[, yearconv := time[which.min(abs(time - 2010))], by = c("iso")]


    ## in case one time step has multiple matches in more than one time step, the value is attributed only in the last time step
    tmp2[time == yearconv & yearconv > 1990, time := ifelse(year == min(year), time, 1980), by = c("iso", "time")]
    tmp2[time == yearconv & yearconv == 1990, time := ifelse(year == max(year), time, 1980), by = c("iso", "time")]
    ## year at which the convergence happens
    tmp2[, year_at_yearconv := year[time == yearconv], by = c("iso","technology", "vehicle_type")]
    ## values of GDPcap equal to GDPcap_rich have the same values as non_fuel_prices of rich countries
    tmp2[year >= year_at_yearconv & year > 2010, non_fuel_price := non_fuel_price_new, by = c("iso","technology", "vehicle_type")]

    ## value of yearconv represents the convergence value
    tmp2[, non_fuel_price_conv := non_fuel_price_new[time==yearconv], by = c("iso","technology", "vehicle_type")]
    ## convergence is linear until the value corresponding to 2010 is reached
    tmp2[year <= year_at_yearconv & year >= 2010, non_fuel_price := non_fuel_price[year == 2010]+(year-2010)/(year_at_yearconv-2010)*(non_fuel_price_conv-non_fuel_price[year == 2010]), by =c("technology", "vehicle_type", "iso")]
    tmp2[year >= year_at_yearconv, non_fuel_price := non_fuel_price[year == year_at_yearconv], by =c("technology", "vehicle_type", "iso")]
    ## select only useful columns
    tmp2 = tmp2[,.(iso, year, non_fuel_price, technology, vehicle_type, fuel_price, subsector_L1, subsector_L2, subsector_L3, sector, sector_fuel, EJ_Mpkm_final , fuel_price_pkm)]

    ## rich countries need to be reintegrated
    tech_cost = rbind(tmp2, tech_cost[iso %in% richcountries])
    ## calculate the total price
    tech_cost[, tot_price := fuel_price_pkm + non_fuel_price]

    return(tech_cost)

}
