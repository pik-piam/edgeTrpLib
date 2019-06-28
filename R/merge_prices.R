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

    lowpass_no_warnings <- function(...){
        suppressWarnings(lowpass(...))
    }

    tdptwyr2dpgj <- 31.71  #TerraDollar per TWyear to Dollar per GJ
    CONV_2005USD_1990USD <- 0.67

    ## load entries from the gdx
    fety <- readGDX(gdx, c("entyFe", "fety"), format = "first_found")
    pebal_subset <- c("pegas")

    budget.m <- readGDX(gdx, name = "qm_budget", types = "equations", field = "m",
                        format = "first_found")[, REMINDyears,]  # Alternative: calcPrice
    if(module == "edge_esm"){
        bal_eq <- "qm_balFeForCesAndEs"
        febal.m <- readGDX(gdx, name = bal_eq, types = "equations",
                           field = "m", format = "first_found")[, REMINDyears, fety]
        tmp <- setNames(febal.m[, , "fegat"]/(budget.m + 1e-10) * tdptwyr2dpgj, "delivered gas")
    }else{
        bal_eq <- "q_balFe"
        pebal.m <- readGDX(gdx, name = c("q_balPe", "qm_pebal"), types = "equations",
                           field = "m", format = "first_found")[, REMINDyears, pebal_subset]
        febal.m <- readGDX(gdx, name = bal_eq, types = "equations",
                           field = "m", format = "first_found")[, REMINDyears, fety]
        ## Nat. Gas too cheap on PE level (4x)
        tmp <- setNames(4*pebal.m[, , "pegas"]/(budget.m + 1e-10) * tdptwyr2dpgj, "delivered gas")
    }
    sprintf("Loading prices for module `%s` from eq. `%s`.", module, bal_eq)

    tmp <- mbind(tmp, setNames(abs(lowpass_no_warnings(febal.m[, , "feelt"]/(budget.m + 1e-10), fix = "both",
                                altFilter = match(2010, REMINDyears))) * tdptwyr2dpgj, "elect_td_trn"))

    tmp <- mbind(tmp, setNames(abs(lowpass_no_warnings(febal.m[, , "feh2t"]/(budget.m + 1e-10),
                                           fix = "both", altFilter = match(2010, REMINDyears))) * tdptwyr2dpgj, "H2 enduse"))
    tmp <- mbind(tmp, setNames(abs(lowpass_no_warnings(febal.m[, , "fedie"]/(budget.m + 1e-10),
                                           fix = "both", altFilter = match(2010, REMINDyears))) * tdptwyr2dpgj, "refined liquids enduse"))
    tmp <- magpie2dt(tmp, regioncol = "region", yearcol = "year", datacols = "sector_fuel")

    if(all(tmp[year == 1990]$value == 0)){
        ## if no 1990 prices are found, lets use 2005 prices and issue warning
        print("No 1990 fuel prices found in REMIND, using 2005 prices.")
        tmp[year == 1990, value := tmp[year==2005]$value]
    }

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
    tech_cost2=tech_cost2[,-c("EJ_Mpkm","EJ_Mpkm_adjusted","lambda","EJ_Mpkm")]

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

    ## coal compatibility -- TODO: remove coal
    ## tech_cost2[is.na(non_fuel_price), non_fuel_price:=max(non_fuel_price)]
    ## tech_cost2[is.na(fuel_price_pkm), non_fuel_price:=max(fuel_price)]

    ## calculate the total price
    tech_cost2[, tot_price := fuel_price_pkm + non_fuel_price]

    return(tech_cost2)

}
