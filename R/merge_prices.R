#' Merge REMIND-derived fuel prices with non-fuel costs.
#'
#' @param gdx
#' @param REMINDmapping
#' @param intensity_data
#' @param nonfuel_costs
#' @import remind
#' @import data.table
#' @importFrom rmndt disaggregate_dt magpie2dt
#' @export

merge_prices <- function(gdx, REMINDmapping, REMINDyears,
                         intensity_data,
                         nonfuel_costs,
                         module="edge_esm") {
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
                           field = "m", format = "first_found")[
        , REMINDyears[REMINDyears>=startyear], fety]

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
    tech_cost2 <- fuel_price_REMIND[, fuel_price_pkm := fuel_price * EJ_Mpkm_final * 1e-6]

    ## merge the non energy prices, they are $/pkm
    tech_cost2 <- merge(tech_cost2, nonfuel_costs,
                        by = c("iso", "year", "technology",
                               "vehicle_type", "subsector_L1",
                               "subsector_L2", "subsector_L3","sector"),
                        all.x = TRUE)

    ## missing non energy price for coal, Adv-Electric and Adv-Liquids freight rail
    ## (is not in the GCAM database). Attribute the same non energy price as in
    ## El. Freight Rail
    tech_cost2[, non_fuel_price := ifelse(technology=="Adv-Electric", .SD[technology == "Electric"]$non_fuel_price, non_fuel_price), by=c("year", "iso")]
    tech_cost2[, non_fuel_price := ifelse(technology=="Adv-Liquid", .SD[technology == "Liquids"]$non_fuel_price, non_fuel_price), by=c("year", "iso")]

    ## calculate the total price
    tech_cost2[, tot_price := fuel_price_pkm + non_fuel_price]

    return(tech_cost2)

}
