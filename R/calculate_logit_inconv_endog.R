#' Evaluate logit tree shares from real prices and inconvenience costs.
#' Inconvenience costs are updated based on shares from previous timesteps (endogenous formulation).
#'
#' @param prices logit prices
#' @param vot_data value-of-time data
#' @param pref_data inconvenience cost data and SWs
#' @param logit_params contains logit exponents
#' @param intensity_data logit level intensity data
#' @param price_nonmot price of non-motorized modes in the logit tree
#' @param nfprices_advanced price of advanced and mid-advanced ICEs
#' @param techswitch technology that the policymaker wants to promote
#' @param stations share of stations offering the different fuels
#' @import data.table
#' @export

calculate_logit_inconv_endog = function(prices,
                                        vot_data,
                                        pref_data,
                                        logit_params,
                                        intensity_data,
                                        price_nonmot,
                                        nfprices_advanced,
                                        techswitch,
                                        stations = NULL) {

  tot_price <- non_fuel_price <- subsector_L3 <- logit.exponent <- share <- sw <- time_price <- NULL
  tot_VOT_price <- `.` <- fuel_price_pkm <- subsector_L1 <- D <- index_yearly <- pinco <- NULL
  shareVS1 <- sw <- iso <- vehicle_type <- shareFS1 <- weighted_shares <- weighted_sharessum <- NULL
  technology <- cluster <- combined_shareEL <- combined_shareLiq <- tail <- NULL
  sector <- subsector_L2 <- MJ_km <- EJ_Mpkm_final <- type <- dpp_nfp <- fuel_price <- value_time <- NULL
  logit_type <- pchar <- pinco_tot <- pmod_av <- prange <- pref <- prisk <- NULL

  ## X2Xcalc is used to traverse the logit tree, calculating shares and intensities
  X2Xcalc <- function(prices, pref_data, logit_params, value_time, mj_km_data, level_base, level_next, group_value) {
    final_pref <- pref_data[[paste0(level_next, "_final_pref")]]
    logit_exponent <- logit_params[[paste0("logit_exponent_", level_next)]]

    ## data contains all the prices in the beginning
    all_subsectors <- c("technology", "vehicle_type", "subsector_L1", "subsector_L2",
                        "subsector_L3", "sector")
    ## takes the right files using the names as inputs
    value_time <- vot_data[[paste0("value_time_", level_next)]]
    ## joins the df containing the prices with the df containing the logit exponents
    df <- merge(prices, logit_exponent,
                by=intersect(names(prices), names(logit_exponent)), all.x = TRUE)

    ## joins the previous df with gathe df containing the inconvenience costs
    df <- merge(df, final_pref, by=intersect( names(df),names(final_pref)), all.y=TRUE)
    ## delete entries have tot_price NA (e.g. 1900 BEV)
    df <- df[ !(is.na(tot_price))]
    ## entries that are not present in the mix have non_fuel_price == 0, but also Walk and Cycle: delete all the not-present in the mix options
    df <- df[(non_fuel_price>0)|(non_fuel_price==0 & subsector_L3 %in% c("Walk", "Cycle"))]
    ## needs random lambdas for the sectors that are not explicitly calculated
    df <- df[ is.na(logit.exponent), logit.exponent := -10]

    ## calculate the shares given prices, lambda and inco
    df <- df[, share := sw*tot_price^logit.exponent/(sum(sw*tot_price^logit.exponent)),
             by = c(group_value, "iso", "year")]

    ## merge value of time for the selected level and assign 0 to the entries that don't have it
    df <- merge(df, value_time, by=intersect(names(df),names(value_time)), all.x=TRUE)

    df <- df[is.na(time_price), time_price := 0]
    df <- df[, tot_VOT_price := time_price + tot_VOT_price]
    df <- df[, tot_price := tot_price + time_price]

    MJ_km <- merge(df, mj_km_data, by=intersect(names(df),names(mj_km_data)),all = FALSE)

    MJ_km <- MJ_km[, .(MJ_km = sum(share * MJ_km)),
                   by = c("iso", "year", "technology", group_value)]

    ## get rid of the ( misleading afterwards) columns
    df_shares <- copy(df)

    df_shares <- df_shares[
      , c("share", "iso", "year",
          all_subsectors[
            seq(match(group_value, all_subsectors) - 1,
                length(all_subsectors), 1)],
          "tot_VOT_price",
          "fuel_price_pkm",
          "non_fuel_price",
          "tot_price"), with = FALSE]

    ## calculate 'one level up' database with the useful columns only
    df <- df[
      , c("share","tot_price","tot_VOT_price",
          "fuel_price_pkm","non_fuel_price","iso","year",
          all_subsectors[
            seq(match(group_value, all_subsectors) - 1,
                length(all_subsectors), 1)]), with = FALSE]

    ## calculate prices of one level up
    df=df[,.(tot_price=sum(share*tot_price),
             tot_VOT_price=sum(share*tot_VOT_price),
             fuel_price_pkm=sum(share*fuel_price_pkm),
             non_fuel_price=sum(share*non_fuel_price)),
          by = c("iso","year",
                 all_subsectors[
                   seq(match(group_value,all_subsectors),
                       length(all_subsectors),1)])]

    return(list(df = df, MJ_km = MJ_km, df_shares = df_shares))

  }



  F2Vcalc <- function(prices, pref_data, logit_params, value_time, mj_km_data, group_value, stations, techswitch) {
    final_prefFV <- pref_data[["FV_final_pref"]]
    final_prefVS1 <- pref_data[["VS1_final_pref"]]
    logit_exponentFV <- logit_params[["logit_exponent_FV"]]
    logit_exponentVS1 <- logit_params[["logit_exponent_VS1"]]
    ## create single datatable for F->V with columns separated for inconvenience and sw
    final_prefFV = dcast(final_prefFV, iso + year + technology + vehicle_type + subsector_L1 + subsector_L2 + subsector_L3 + sector ~ logit_type, value.var = "value")
    ## liquids don't have pref, prange, pchar, pmod_av, prisk. Other techs don't have the generic pinco_tot
    final_prefFV[, pinco_tot := ifelse(is.na(pinco_tot), 0, pinco_tot)]
    final_prefFV[, prange := ifelse(is.na(prange), 0, prange)]
    final_prefFV[, pchar := ifelse(is.na(pchar), 0, pchar)]
    final_prefFV[, pref := ifelse(is.na(pref), 0, pref)]
    final_prefFV[, pmod_av := ifelse(is.na(pmod_av), 0, pmod_av)]
    final_prefFV[, prisk := ifelse(is.na(prisk), 0, prisk)]

    ## data contains all the prices in the beginning
    all_subsectors <- c("technology", "vehicle_type", "subsector_L1", "subsector_L2",
                        "subsector_L3", "sector")
    ## takes the right files using the names as inputs
    value_time <- vot_data[[paste0("value_time_FV")]]
    ## joins the df containing the prices with the df containing the logit exponents
    df <- merge(prices, logit_exponentFV,
                by = intersect(names(prices), names(logit_exponentFV)), all.x = TRUE)

    ## joins the previous df with gathe df containing the inconvenience costs
    df <- merge(df, final_prefFV, by=intersect(names(df),names(final_prefFV)), all.y = TRUE)
    ## delete entries have tot_price NA (e.g. 1900 BEV)
    df <- df[ !(is.na(tot_price))]
    ## entries that are not present in the mix have non_fuel_price == 0, but also Walk and Cycle: delete all the not-present in the mix options
    df <- df[(non_fuel_price>0)|(non_fuel_price==0 & subsector_L3 %in% c("Walk", "Cycle"))]
    ## needs random lambdas for the sectors that are not explicitly calculated
    df <- df[ is.na(logit.exponent), logit.exponent := -10]

    ## define the years on which the inconvenience price will be calculated on the basis of the previous time steps sales
    futyears_all = seq(2020, 2101, 1)
    ## all modes other then 4W calculated with exogenous sws
    dfother = df[(subsector_L1 != "trn_pass_road_LDV_4W"), c("iso", "year", "subsector_L2", "subsector_L3", "sector", "subsector_L1", "vehicle_type", "technology", "tot_price", "logit.exponent", "sw", "tot_VOT_price", "fuel_price_pkm", "non_fuel_price")]
    dfhistorical4W = df[(subsector_L1 == "trn_pass_road_LDV_4W" & year < 2020), c("iso", "year", "subsector_L2", "subsector_L3", "sector", "subsector_L1", "vehicle_type", "technology", "tot_price", "logit.exponent", "pinco_tot", "prange", "pref", "pmod_av", "prisk", "pchar", "tot_VOT_price", "fuel_price_pkm", "non_fuel_price")]

    ## 4W are calculated separately
    df4W = df[subsector_L1 == "trn_pass_road_LDV_4W", c("iso", "year", "subsector_L1", "vehicle_type", "technology", "tot_price", "logit.exponent")]

    ## extrapolate for all years
    df4W = approx_dt(dt = df4W, xdata = futyears_all,
                     xcol = "year", ycol = "tot_price",
                     idxcols = c("iso",  "subsector_L1", "vehicle_type", "technology"),
                     extrapolate=T)

    ## other price components for 4W are useful later but will not be carried on in the yearly calculation
    dfprices4W = prices[subsector_L1 == "trn_pass_road_LDV_4W",  c("iso", "year", "subsector_L1", "vehicle_type", "technology", "fuel_price_pkm", "non_fuel_price")]

    ## starting value for inconvenience cost of 2020 for 4W is needed as a starting point for the iterative calculations
    dfpinco2020 = df[subsector_L1 == "trn_pass_road_LDV_4W" & year == 2020, c("iso", "subsector_L1", "vehicle_type", "technology", "pinco_tot", "prange", "pref", "pmod_av", "prisk", "pchar")]

    ## merge the yearly values and the starting inconvenience cost
    df = merge(df4W, dfpinco2020, all = TRUE, by = c("iso", "subsector_L1", "vehicle_type", "technology"))
    ## apply the same logit exponent to all the years
    df[, logit.exponent := as.double(logit.exponent)]
    df[, logit.exponent := ifelse(is.na(logit.exponent), mean(logit.exponent, na.rm = TRUE), logit.exponent), by = c("vehicle_type")]

    ## for 4W the value of V->S1 market shares is needed on a yearly basis
    final_prefVS1cp = final_prefVS1[subsector_L1 == "trn_pass_road_LDV_4W"]

    final_prefVS1cp = approx_dt(dt = final_prefVS1cp, xdata = futyears_all,
                                xcol = "year", ycol = "sw",
                                idxcols = c("iso", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector"),
                                extrapolate=T)

    ## initialize values needed for the for loop
    tmp2past = NULL
    tmp1 = df[year < 2020,]
    tmp1[, share := NA]

    ## define the weight that has to be attributed to each year
    paux = 15   ## approximate lifetime of a car
    Ddt = data.table(index_yearly = seq(1,paux,1))
    Ddt[, D := 1-((index_yearly-0.5)/paux)^4]
    Ddt[, D := ifelse(D<0,0,D)]

    if (is.null(stations)) {
      stations = CJ(iso =unique(df[, iso]), year = unique(df[, year]), technology = c("BEV", "NG", "FCEV"))
      stations[, fracst := 1]
      ## attribute a very low value to current available refuelling stations with alternative fuels
      stations[year == 2020, fracst := 0.01]

      if (techswitch == "FCEV") {
       ## policymaker plans many hydrogen refuelling points: all stations have H2 available by 2050
       yearconv = 2050
       stations[year <= yearconv & technology == "FCEV", fracst := (fracst[year == 2020]-fracst[year == yearconv])/(2020-yearconv)*(year-2020)+fracst[year==2020], by = c("iso", "technology")]
       ## policymaker plans to install few electricity rechargers: in 2100 only 10% of stations have them
       stations[year <= 2100 & technology == "BEV", fracst := (fracst[year == 2020]-0.1)/(2020-2100)*(year-2020)+fracst[year==2020], by = c("iso", "technology")]
       stations[year >= 2100 & technology == "BEV", fracst := 0.1]
       
       ## NG stations are converging to 1 in 2100
       stations[year <= 2100 & technology == "NG", fracst := (fracst[year == 2020]-fracst[year == 2100])/(2020-2100)*(year-2020)+fracst[year==2020], by = c("iso", "technology")]

       } else {
        ## stations converge to 1 in 2100
        stations[year <= 2100, fracst := (fracst[year == 2020]-fracst[year == 2100])/(2020-2100)*(year-2020)+fracst[year==2020], by = c("iso", "technology")]
      }
      }

    start <- Sys.time()
    for (t in futyears_all[futyears_all>2020]) {

      if (t > 2021) {
        tmp <- df[year == t,][, c("share") := NA]
        tmp <- merge(tmp, tmp1, all = TRUE, by = intersect(names(tmp), names(tmp1)))
      } else {
        tmp <- df[year %in% c(2020, 2021),]
      }
      tmp <- tmp[year == (t-1), share := (tot_price + pinco_tot + prange + pref + pmod_av + prisk  + pchar)^logit.exponent/(sum((tot_price + pinco_tot + prange + pref + pmod_av + prisk + pchar)^logit.exponent)),
                 by = c(group_value, "year", "iso")]
      tmp2 <- tmp[year == (t-1)]
      tmp2 <- tmp2[,.(tot_price=sum(share*tot_price)),
                   by = c("iso","year","vehicle_type","subsector_L1")]

      ## calculate the average share FS1 (the vehicle type is not important)
      tmp2 <- merge(tmp2, final_prefVS1cp, by=intersect( names(tmp2),names(final_prefVS1cp)), all.y=TRUE)
      tmp2 <- merge(tmp2, logit_exponentVS1,
                    by=intersect(names(tmp2), names(logit_exponentVS1)), all.x = TRUE)
      tmp2 <- tmp2[, shareVS1 := sw*tot_price^logit.exponent/sum(sw*tot_price^logit.exponent),
                   by = c("subsector_L1", "year", "iso")]
      tmp2 <- tmp2[,.(subsector_L1, year, iso, vehicle_type, shareVS1)]

      ## market share is calculated on the basis of the values in t-1
      tmp2 <- merge(tmp2, tmp[year %in% (t-1),], by = c("iso", "year", "vehicle_type", "subsector_L1"))

      ## calculate the share from fuel to S1 (FS1)
      tmp2[, shareFS1 := share*shareVS1]

      ## calculate the share of all fuels on the subsector_L1 (I don't care anymore about the vehicle_type)
      tmp2 <- tmp2[,.(shareFS1=sum(shareFS1)),by=c("iso", "technology", "subsector_L1","year")]

      ## merge with previous years' values
      if (!is.null(tmp2past)) {
        tmp3 <- rbind(tmp2, tmp2past)
      } else {
        tmp3 <- copy(tmp2)
      }

      ## save the values of the past for the next iteration
      tmp2past <- copy(tmp3)

      ## merge the FS1 shares to the FV shares
      tmp <- merge(tmp,tmp3, by= c("technology", "year", "iso", "subsector_L1"), all=TRUE)

      ## find depreciation to attribute to each time step
      Ddt1 = copy(Ddt)
      Ddt1 = Ddt1[, year := seq(t-1,t-paux,-1)]
      Ddt1 = Ddt1[year>=2020]

      tmp = merge(tmp, Ddt1, all.x = TRUE, by = "year")
      tmp[is.na(D), D := 0]
      tmp[is.na(shareFS1), shareFS1 := 0]

      ## weighted shares are the weighted average of each time step's FS1 share and how much it depreciated in time
      tmp[, weighted_shares := mean(shareFS1*D), by = c("iso", "technology", "vehicle_type", "subsector_L1", "year")]

      ## weighted share depends on the composition of the fleet, which is a weighted average of the previous years' composition
      tmp[, weighted_sharessum := ifelse(year == (t-1), sum(weighted_shares[year<t])/sum(D[year<t]), 0), by = c("iso", "technology", "vehicle_type", "subsector_L1")]

      ## for 2020, we assume the value was constant for all the previous years (hence the value for 2020 coincides with the share)
      if (t == 2021) {
        tmp[, weighted_sharessum := ifelse(year == 2020, weighted_shares, NA)]
        tmp_2020 = tmp[year == 2020,]
      }

      tmp = rbind(tmp[year!=2020], tmp_2020)

      ## coefficients of the intangible costs trend
      bfuelav = -20    ## value based on Greene 2001
      bmodelav = -12   ## value based on Greene 2001
      coeffrisk = 3800 ## value based on Pettifor 2017

      ## merge with fraction of stations offering fuel
      tmp = merge(tmp, stations, by = c("iso", "year", "technology"), all.x = TRUE)
      tmp[is.na(fracst), fracst := 0]

      ## Calculate trend of inconvenience costs components
      tmp[, pref := ifelse(year == t,
                            pref[year == 2020]*exp(1)^(fracst[year == (t-1)]*bfuelav),
                            pref), by = c("iso", "technology", "vehicle_type", "subsector_L1")]

      tmp[, prange := ifelse(year == t,
                           prange[year == 2020]*exp(1)^(fracst[year == (t-1)]*bfuelav),
                           prange), by = c("iso", "technology", "vehicle_type", "subsector_L1")]

      if (techswitch == "FCEV") {
        ## the policymaker pushes carmakers and car retailers to provide FCEVs models, resulting in a decrease in model availability cost for H2 vehicles
        tmp[technology == "FCEV", pmod_av := ifelse(year == t,
                                0.5*pmod_av[year == 2020]*exp(1)^(weighted_sharessum[year == (t-1)]*bmodelav),
                                pmod_av), by = c("iso", "technology", "vehicle_type", "subsector_L1")]
        ## for all other vehicles, the model availability cost is the same as in the input
        tmp[technology != "FCEV", pmod_av := ifelse(year == t,
                                pmod_av[year == 2020]*exp(1)^(weighted_sharessum[year == (t-1)]*bmodelav),
                                pmod_av), by = c("iso", "technology", "vehicle_type", "subsector_L1")]
       
     } else {
        tmp[, pmod_av := ifelse(year == t,
                                pmod_av[year == 2020]*exp(1)^(weighted_sharessum[year == (t-1)]*bmodelav),
                                pmod_av), by = c("iso", "technology", "vehicle_type", "subsector_L1")]
     }

      tmp[, prisk := ifelse(year == t,
                            pmax(prisk[year == 2020]-coeffrisk*weighted_sharessum[year == (t-1)], 0),
                            prisk), by = c("iso", "technology", "vehicle_type", "subsector_L1")]

      tmp[, pinco_tot := ifelse(year == t,
                                pinco_tot[year == 2020]*exp(1)^(weighted_sharessum[year == (t-1)]*bmodelav),
                                pinco_tot), by = c("iso", "technology", "vehicle_type", "subsector_L1")]

      ## annual sales, needed for reporting purposes
      if (t == 2101) {
        annual_sales = tmp[year<=2100, c("iso", "year", "technology", "shareFS1", "vehicle_type", "subsector_L1", "share")]
      }

      ## remove "temporary" columns
      tmp[, c("shareFS1","D", "weighted_shares", "weighted_sharessum", "index_yearly", "fracst") := NULL]

      if (t<tail(futyears_all,1)) {
        tmp1 = copy(tmp)
      }

    }
    print(paste("Iterative logit calculation finished in",
                difftime(Sys.time(), start, units="mins"),
                "Minutes"))

    ## tmp1 needs the same structure as dfhistorical4W to produce the complete trend in time of 4wheelers
    tmp1[, c("subsector_L2", "subsector_L3", "sector") := list("trn_pass_road_LDV", "trn_pass_road", "trn_pass")]
    tmp1[, share := NULL]
    ## merge tmp1 and historical 4W
    tmp1 = rbind(tmp1, dfhistorical4W[, c("year", "technology", "iso", "subsector_L1", "vehicle_type", "tot_price", "logit.exponent", "pinco_tot", "prange", "pref", "pmod_av", "prisk", "pchar", "subsector_L2", "subsector_L3", "sector")])
    tmp1 <- tmp1[, share := (tot_price+pinco_tot+prange+pref+pmod_av+prisk+pchar)^logit.exponent/(sum((tot_price+pinco_tot+prange+pref+pmod_av+prisk+pchar)^logit.exponent)),
                 by = c("vehicle_type", "year", "iso")]

    ## merge with prices
    tmp1 = merge(tmp1[year %in% unique(dfother$year)], dfprices4W, all.x = TRUE, by = intersect(names(tmp1), names(dfprices4W)))

    ## copy of tmp1 is needed to create the updated version of preferences trend
    inconv = copy(tmp1[,.(year, iso, sector, subsector_L3, subsector_L2, subsector_L1, vehicle_type, technology, pinco_tot, prange, pref, pmod_av, prisk, pchar)])
    ## values after 2100 are set to be equal to 2100
    inconv = rbind(inconv, inconv[year==2100][,year:=2110], inconv[year==2100][,year:=2130],inconv[year==2100][,year:=2150])
    ## inconv and final_prefFV need the same structure as pref_data[["FV_final_pref"]]
    inconv = melt(inconv, id.vars = c("year", "iso", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology"))
    setnames(inconv, old = "variable", new = "logit_type")
    final_prefFV = melt(final_prefFV, id.vars = c("year", "iso", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology"))
    setnames(final_prefFV, old = "variable", new = "logit_type")
    final_prefFV = rbind(final_prefFV[subsector_L1 != "trn_pass_road_LDV_4W" & logit_type == "sw"],
                         inconv[(technology == "Liquids" & logit_type == "pinco_tot")|
                                  (technology == "BEV" & logit_type %in% c("prange", "prisk", "pchar", "pmod_av"))|
                                  (technology == "FCEV" & logit_type %in% c("pref", "prisk", "pmod_av"))|
                                  (technology == "NG" & logit_type %in% c("pref", "prisk", "pmod_av"))|
                                  (technology == "Hybrid Electric" & logit_type %in% c("prisk", "pchar", "pmod_av"))|
                                  (technology == "Hybrid Liquids" & logit_type %in% c("prisk", "pmod_av"))])
    intensity_data = intensity_data[EJ_Mpkm_final>0]
    final_prefNonMot = final_prefFV[vehicle_type %in% c("Cycle_tmp_vehicletype", "Walk_tmp_vehicletype"),]
    final_prefFV = merge(final_prefFV, unique(intensity_data[, c("iso", "vehicle_type")]), by = c("iso", "vehicle_type"), all.y = TRUE)
    final_prefFV = rbind(final_prefNonMot, final_prefFV)
    ## overwrite the preferences with the market based ones
    pref_data[["FV_final_pref"]] = final_prefFV

    ## for all entries other than 4wheelers, shares based on SW are calculated
    dfother[, share := sw*tot_price^logit.exponent/(sum(sw*tot_price^logit.exponent)),
            by = c(group_value, "year", "iso")]
    ## 4W and all other entries are merged
    df <- rbind(dfother[, .(iso, year, share, technology, vehicle_type, subsector_L1, subsector_L2, subsector_L3, sector, fuel_price_pkm, non_fuel_price, tot_price)],
                tmp1[, .(iso, year, share, technology, vehicle_type, subsector_L1, subsector_L2, subsector_L3, sector, fuel_price_pkm, non_fuel_price, tot_price)])

    ## merge value of time for the selected level and assign 0 to the entries that don't have it
    df <- merge(df, value_time, by=intersect(names(df),names(value_time)), all.x=TRUE)
    df <- df[is.na(time_price), time_price := 0]
    df <- df[, tot_VOT_price := time_price]
    df <- df[, tot_price := tot_price + time_price]

    ## merge energy intensity
    MJ_km <- merge(df, mj_km_data, by=intersect(names(df),names(mj_km_data)),all = FALSE)
    MJ_km <- MJ_km[, .(MJ_km = sum(share * MJ_km)),
                   by = c("iso", "year", "technology", group_value)]

    ## save complete dt at this level
    df_shares <- copy(df)
    ## get rid of the ( misleading afterwards) columns
    df_shares <- df_shares[
      , c("share", "iso", "year",
          all_subsectors[
            seq(match(group_value, all_subsectors) - 1,
                length(all_subsectors), 1)],
          "tot_VOT_price",
          "fuel_price_pkm",
          "non_fuel_price",
          "tot_price"), with = FALSE]

    ## calculate 'one level up' database with the useful columns only
    df <- df[
      , c("share","tot_price","tot_VOT_price",
          "fuel_price_pkm","non_fuel_price","iso","year",
          all_subsectors[
            seq(match(group_value, all_subsectors) - 1,
                length(all_subsectors), 1)]), with = FALSE]

    ## calculate prices of one level up
    df=df[,.(tot_price=sum(share*tot_price),
             tot_VOT_price=sum(share*tot_VOT_price),
             fuel_price_pkm=sum(share*fuel_price_pkm),
             non_fuel_price=sum(share*non_fuel_price)),
          by = c("iso","year",
                 all_subsectors[
                   seq(match(group_value,all_subsectors),
                       length(all_subsectors),1)])]

    return(list(df = df, MJ_km = MJ_km, df_shares = df_shares, pref_data = pref_data, annual_sales = annual_sales))

  }

  E2Fcalc <- function(pricesLDV, mj_km_data){
    logit_exponent <- -4
    ## joins the df containing the prices with the df containing the logit exponents
    df <- pricesLDV[, logit.exponent := logit_exponent]
    ## delete entries have tot_price NA (e.g. 1900 BEV)
    df <- df[ !(is.na(tot_price))]
    ## entries that are not present in the mix have non_fuel_price == 0, but also Walk and Cycle: delete all the not-present in the mix options
    df <- df[non_fuel_price>0]
    ## calculate the shares given prices, lambda and inco
    df <- df[, share := (tot_price)^logit.exponent/(sum((tot_price)^logit.exponent)),
             by = c("technology", "vehicle_type", "iso", "year")]
    df[, tot_VOT_price := 0]

    MJ_km <- merge(df, mj_km_data, by=intersect(names(df), names(mj_km_data)), all = FALSE)

    MJ_km <- MJ_km[, .(MJ_km = sum(share * MJ_km)),
                   by = c("iso", "year", "technology", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector", "sector_fuel")]

    ## get rid of the ( misleading afterwards) columns
    df_shares <- copy(df)

    df_shares <- df_shares[
      , c("share", "iso", "year",
          "technology", "vehicle_type",
          "subsector_L1", "subsector_L2", "subsector_L3", "sector",
          "tot_VOT_price",
          "fuel_price_pkm",
          "non_fuel_price",
          "tot_price",
          "type"), with = FALSE]

    ## calculate 'one level up' database with the useful columns only
    df <- df[
      , c("share", "tot_price",
          "fuel_price_pkm", "non_fuel_price", "tot_VOT_price", "iso", "year",
          "subsector_L1", "subsector_L2", "subsector_L3", "sector",
          "technology", "vehicle_type", "sector_fuel"), with = FALSE]

    ## calculate prices of one level up
    df=df[,.(tot_price=sum(share*tot_price),
             fuel_price_pkm=sum(share*fuel_price_pkm),
             tot_VOT_price=sum(share*tot_VOT_price),
             non_fuel_price=sum(share*non_fuel_price)),
          by = c("iso", "year", "technology", "vehicle_type",
                 "subsector_L1", "subsector_L2", "subsector_L3", "sector", "sector_fuel")]


    return(list(df = df, MJ_km = MJ_km, df_shares = df_shares))

  }

  ## FV load technology prices and merge with value of time (~technology price for
  ## non-motorized)
  ## non-fuel prices
  base <- merge(prices, price_nonmot, all = TRUE,
                by = c("tot_price","iso","year",
                       "technology","vehicle_type",
                       "subsector_L1","subsector_L2","subsector_L3","sector"))

  base[,tot_VOT_price := 0]
  #Cycling and Walking have no fuel and non fuel prices, 0 instead of NA is given
  base[is.na(fuel_price_pkm), fuel_price_pkm := 0]
  base[is.na(non_fuel_price), non_fuel_price := 0]

  ## energy intensity
  mj_km_data <- intensity_data[, MJ_km := EJ_Mpkm_final
                               * 1e12 # to MJ
                               * 1e-6 # MJ/km
                               ]
  mj_km_data <- mj_km_data[,-"EJ_Mpkm_final"]

  ## Conventional Liquids energy intensity

  ## create choice of  more expensive, more efficient alternatives (for now only Liquids)
  mj_km_dataEF <- rbind(mj_km_data[subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020 & technology =="Liquids"][, c("type", "MJ_km") := list("advanced", MJ_km*0.6)],
                        mj_km_data[subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020 & technology =="Liquids"][, c("type", "MJ_km") := list("middle", MJ_km*0.8)],
                        mj_km_data[subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020 & technology =="Liquids"][, type := "normal"])

  ## non fuel prices of Liquids LDVs advanced and mid-advanced categories is merged to the other prices
  nfprices_advanced = merge(nfprices_advanced[year>2020],
                            base[subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020 & technology =="Liquids", ][, non_fuel_price := NULL],
                            by = names(nfprices_advanced)[!(names(nfprices_advanced) %in% c("non_fuel_price", "type"))])

  baseEF = rbind(base[subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020 & technology =="Liquids"][, type := "normal"], nfprices_advanced)
  baseEF = merge(baseEF,
                 mj_km_dataEF[,c("iso", "year", "technology", "vehicle_type", "type", "MJ_km"), ],
                 by = c("iso", "year", "technology", "vehicle_type", "type"))

  ## fuel price needs to be recalculated as the energy intensity changed
  baseEF[, c("fuel_price_pkm", "MJ_km") := list(fuel_price    ## in $/EJ
                                                *MJ_km        ## in MJ/km
                                                *1e-12,       ## in $/km
                                                NULL)]

  baseEF[, tot_price := fuel_price_pkm + non_fuel_price]


  E2F_all <- E2Fcalc(baseEF,
                     mj_km_dataEF)
  EF <- E2F_all[["df"]]
  mj_km_dataEF <- E2F_all[["MJ_km"]]
  EF_shares <- E2F_all[["df_shares"]]

  base <- rbind(base[!(subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020 & technology =="Liquids"),  c("iso","year","technology","vehicle_type","subsector_L1","subsector_L2","subsector_L3","sector", "tot_price","fuel_price_pkm", "non_fuel_price", "tot_VOT_price", "sector_fuel")],
                EF)

  mj_km_data <- rbind(mj_km_data[!(subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020 & technology =="Liquids"), c("iso", "year", "technology", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector", "MJ_km", "sector_fuel")],
                      mj_km_dataEF)

  ## FV
  FV_all <- F2Vcalc(prices = base,
                    pref_data = pref_data,
                    logit_params = logit_params,
                    value_time = value_time,
                    mj_km_data = mj_km_data,
                    group_value = "vehicle_type",
                    techswitch = techswitch,
                    stations = stations)

  FV <- FV_all[["df"]]
  MJ_km_FV <- FV_all[["MJ_km"]]
  FV_shares <- FV_all[["df_shares"]]
  pref_data <- FV_all[["pref_data"]]
  annual_sales <- FV_all[["annual_sales"]]

  # VS1
  VS1_all <- X2Xcalc(prices = FV,
                     pref_data = pref_data,
                     logit_params = logit_params,
                     value_time = value_time,
                     mj_km_data = MJ_km_FV,
                     level_base = "FV",
                     level_next = "VS1",
                     group_value = "subsector_L1")

  VS1 <- VS1_all[["df"]]
  MJ_km_VS1 <- VS1_all[["MJ_km"]]
  VS1_shares <- VS1_all[["df_shares"]]
  VS1_shares=VS1_shares[,-c("sector","subsector_L2","subsector_L3")]

  # S1S2
  S1S2_all <- X2Xcalc(prices = VS1,
                      pref_data = pref_data,
                      logit_params = logit_params,
                      value_time = value_time,
                      mj_km_data = MJ_km_VS1,
                      level_base = "VS1",
                      level_next = "S1S2",
                      group_value = "subsector_L2")
  S1S2 <- S1S2_all[["df"]]
  MJ_km_S1S2 <- S1S2_all[["MJ_km"]]
  S1S2_shares <- S1S2_all[["df_shares"]]


  # S2S3
  S2S3_all <- X2Xcalc(prices = S1S2,
                      pref_data = pref_data,
                      logit_params = logit_params,
                      value_time = value_time,
                      mj_km_data = MJ_km_S1S2,
                      level_base = "S1S2",
                      level_next = "S2S3",
                      group_value = "subsector_L3")

  S2S3 <- S2S3_all[["df"]]
  MJ_km_S2S3 <- S2S3_all[["MJ_km"]]
  S2S3_shares <- S2S3_all[["df_shares"]]

  # S3S
  S3S_all <- X2Xcalc(prices = S2S3,
                     pref_data = pref_data,
                     logit_params = logit_params,
                     value_time = value_time,
                     mj_km_data = MJ_km_S2S3,
                     level_base = "S2S3",
                     level_next = "S3S",
                     group_value = "sector")

  S3S <- S3S_all[["df"]]
  MJ_km_S3S <- S3S_all[["MJ_km"]]
  S3S_shares <- S3S_all[["df_shares"]]

  share_list=list(S3S_shares=S3S_shares,
                  S2S3_shares=S2S3_shares,
                  S1S2_shares=S1S2_shares,
                  VS1_shares=VS1_shares,
                  FV_shares=FV_shares)

  prices_list=list(S3S=S3S,
                   S2S3=S2S3,
                   S1S2=S1S2,
                   VS1=VS1,
                   FV=FV,
                   base=base)

  result=list(mj_km_data = mj_km_data,
              prices_list = prices_list,
              share_list = share_list,
              pref_data = pref_data,
              EF_shares = EF_shares,
              annual_sales = annual_sales)

  return(result)
}
