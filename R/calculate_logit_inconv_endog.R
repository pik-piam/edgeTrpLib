calculate_logit_inconv_endog = function(prices,
                                    vot_data,
                                    inco_data,
                                    logit_params,
                                    intensity_data,
                                    price_nonmot,
                                    full_data = F) {
  ## X2Xcalc is used to traverse the logit tree, calculating shares and intensities
  X2Xcalc <- function(prices, mj_km_data, level_base, level_next, group_value) {
    final_inco <- inco_data[[paste0(level_next, "_final_inconv")]]
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
    df <- merge(df, final_inco, by=intersect( names(df),names(final_inco)), all.y=TRUE)
    ## delete entries have tot_price NA (e.g. 1900 BEV)
    df <- df[ !(is.na(tot_price))]
    ## entries that are not present in the mix have non_fuel_price == 0, but also Walk and Cycle: delete all the not-present in the mix options
    df <- df[(non_fuel_price>0)|(non_fuel_price==0 & subsector_L3 %in% c("Walk", "Cycle"))]
    ## needs random lambdas for the sectors that are not explicitly calculated
    df <- df[ is.na(logit.exponent), logit.exponent := -10]

    ## calculate the shares given prices, lambda and inco
    df <- df[, share := (tot_price+pinco)^logit.exponent/(sum((tot_price+pinco)^logit.exponent)),
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

    return(list(df, MJ_km, df_shares))

  }



  F2Vcalc <- function(prices, mj_km_data, group_value) {
    final_incoFV <- inco_data[["FV_final_inconv"]]
    final_incoVS1 <- inco_data[["VS1_final_inconv"]]
    logit_exponentFV <- logit_params[["logit_exponent_FV"]]
    logit_exponentVS1 <- logit_params[["logit_exponent_VS1"]]

    ## data contains all the prices in the beginning
    all_subsectors <- c("technology", "vehicle_type", "subsector_L1", "subsector_L2",
                        "subsector_L3", "sector")
    ## takes the right files using the names as inputs
    value_time <- vot_data[[paste0("value_time_FV")]]
    ## joins the df containing the prices with the df containing the logit exponents
    df <- merge(prices, logit_exponentFV,
                by=intersect(names(prices), names(logit_exponentFV)), all.x = TRUE)

    ## joins the previous df with gathe df containing the inconvenience costs
    df <- merge(df, final_incoFV, by=intersect( names(df),names(final_incoFV)), all.y=TRUE)
    ## delete entries have tot_price NA (e.g. 1900 BEV)
    df <- df[ !(is.na(tot_price))]
    ## entries that are not present in the mix have non_fuel_price == 0, but also Walk and Cycle: delete all the not-present in the mix options
    df <- df[(non_fuel_price>0)|(non_fuel_price==0 & subsector_L3 %in% c("Walk", "Cycle"))]
    ## needs random lambdas for the sectors that are not explicitly calculated
    df <- df[ is.na(logit.exponent), logit.exponent := -10]

    ## define the years on which the inconvenience price will be calculated on the basis of the previous time steps sales
    futyears_all = seq(2010, 2100, 1)
    ## all modes other then 4W calculated with exogenous inconvenience costs
    dfother = df[(subsector_L1 != "trn_pass_road_LDV_4W")|(subsector_L1 == "trn_pass_road_LDV_4W" & year < 2010), c("iso", "year", "subsector_L2", "subsector_L3", "sector", "subsector_L1", "vehicle_type", "technology", "tot_price", "logit.exponent", "pinco", "tot_VOT_price", "fuel_price_pkm", "non_fuel_price")]

    ## 4W are calculated separately
    df4W = df[subsector_L1 == "trn_pass_road_LDV_4W", c("iso", "year", "subsector_L1", "vehicle_type", "technology", "tot_price", "logit.exponent")]

    ## extrapolate for all years
    setnames(df4W, old = "tot_price", new = "value") ## rename otherwise approx_dt complains
    df4W = approx_dt(dt = df4W, xdata = futyears_all,
                            idxcols = c("iso",  "subsector_L1", "vehicle_type", "technology"),
                            extrapolate=T)
    setnames(df4W, old = "value", new = "tot_price")  ## rename back

    ## other price components for 4W are useful later but will not be carried on in the yearly calculation
    dfprices4W = df[subsector_L1 == "trn_pass_road_LDV_4W" & year >= 2010,  c("iso", "year", "subsector_L1", "vehicle_type", "technology", "fuel_price_pkm", "non_fuel_price")]

    ## starting value for inconvenience cost of 2010 for 4W is needed as a starting point for the iterative calculations
    dfpinco2010 = df[subsector_L1 == "trn_pass_road_LDV_4W" & year == 2010, c("iso", "subsector_L1", "vehicle_type", "technology", "pinco")]

    ## merge the yearly values and the starting inconvenience cost
    df = merge(df4W, dfpinco2010, all = TRUE, by = c("iso", "subsector_L1", "vehicle_type", "technology"))
    ## apply the same logit exponent to all the years
    df[, logit.exponent := as.double(logit.exponent)]
    df[, logit.exponent := ifelse(is.na(logit.exponent), mean(logit.exponent, na.rm = TRUE), logit.exponent), by = c("vehicle_type")]

    ## for 4W the value of V->S1 market shares is needed on a yearly basis
    final_incoVS1cp = final_incoVS1[subsector_L1 == "trn_pass_road_LDV_4W"]
    setnames(final_incoVS1cp, old = "pinco", new = "value") ## rename otherwise approx_dt complains
    final_incoVS1cp = approx_dt(dt = final_incoVS1cp, xdata = futyears_all,
                   idxcols = c("iso", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector"),
                   extrapolate=T)
    setnames(final_incoVS1cp, old = "value", new = "pinco")  ## rename back

    ## initialize values needed for the for loop
    tmp2past = NULL
    tmp1 = df[year <2010,]
    tmp1[, share := NA]

    ## define the weight that has to be attributed to each year
    paux = 15   ## approximate lifetime of a car
    Ddt = data.table(index_yearly = seq(1,paux,1))
    Ddt[, D := 1-((index_yearly-0.5)/paux)^4]
    Ddt[, D := ifelse(D<0,0,D)]


    start <- Sys.time()
    for (t in futyears_all[futyears_all>2010]) {
      if (t > 2011) {
        tmp <- df[year == t,][, c("share") := NA]
        tmp <- merge(tmp, tmp1, all = TRUE, by = intersect(names(tmp), names(tmp1)))
      } else {
        tmp <- df[year %in% c(2010, 2011),]
      }
      tmp <- tmp[year == (t-1), share := (tot_price+pinco)^logit.exponent/(sum((tot_price+pinco)^logit.exponent)),
                 by = c(group_value, "year", "iso")]
      tmp2 <- tmp[year == (t-1)]
      tmp2 <- tmp2[,.(tot_price=sum(share*tot_price)),
                   by = c("iso","year","vehicle_type","subsector_L1")]

      ## calculate the average share FS1 (the vehicle type is not important)
      tmp2 <- merge(tmp2, final_incoVS1cp, by=intersect( names(tmp2),names(final_incoVS1)), all.x=TRUE)
      tmp2 <- merge(tmp2, logit_exponentVS1,
                    by=intersect(names(tmp2), names(logit_exponentVS1)), all.x = TRUE)
      tmp2 <- tmp2[, shareVS1 := (tot_price+pinco)^logit.exponent/(sum((tot_price+pinco)^logit.exponent)),
                   by = c("subsector_L1", "year", "iso")]
      tmp2 <- tmp2[,.(subsector_L1, year, iso, vehicle_type, shareVS1)]

      ## ??
      tmp2 <- merge(tmp2, tmp[year %in% (t-1),], by = c("iso", "year", "vehicle_type", "subsector_L1"))

      ## calculate the share from the cluster to S1 (CS1)
      tmp2[, shareFS1 := share*shareVS1]

      ## calculate the share of all Clusters on the subsector_L1 (I don't care anymore about the vehicle_type)
      tmp2 <- tmp2[,.(shareFS1=sum(shareFS1)),by=c("iso", "technology", "subsector_L1","year")]

      ## merge with previous years' values
      if (!is.null(tmp2past)) {
        tmp3 <- rbind(tmp2, tmp2past)
      } else {
        tmp3 <- copy(tmp2)
      }

      ## save the values of the past for the next iteration
      tmp2past <- copy(tmp3)

      ## merge the CS1 shares to the FV shares
      tmp <- merge(tmp,tmp3, by= c("technology", "year", "iso", "subsector_L1"), all=TRUE)

      ## find depreciation to attribute to each time step
      Ddt1 = copy(Ddt)
      Ddt1 = Ddt1[, year := seq(t-1,t-paux,-1)]
      Ddt1 = Ddt1[year>=2010]

      tmp = merge(tmp, Ddt1, all.x = TRUE, by = "year")
      tmp[is.na(D), D := 0]
      tmp[is.na(shareFS1), shareFS1 := 0]

      ## weighted shares are the weighted average of each time step's CS1 share and how much it depreciated in time
      tmp[, weighted_shares := mean(shareFS1*D), by = c("iso", "technology", "vehicle_type", "subsector_L1", "year")]

      ## print(paste0("time is ", t))
      tmp[, weighted_sharessum := ifelse(year == (t-1), sum(weighted_shares[year<t])/sum(D[year<t]), 0), by = c("iso", "technology", "vehicle_type", "subsector_L1")]

      ## for 2010, we assume the value was constant for all the previous years (hence the value for 2010 coincides with the share)
      if (t == 2011) {
        tmp[, weighted_sharessum := ifelse(year == 2010, weighted_shares, NA)]
        tmp_2010 = tmp[year == 2010,]
      }

      tmp = rbind(tmp[year!=2010], tmp_2010)


      if (selfmarket_acceptancy & selfmarket_policypush) {

        acceptancy = 5 ## meaning: reaches 0 inconvenience cost at 1/5=0.2 market share
        marketsharepush = 0.05
        additional_inconv_liq = 0.1 ## inconvenience for ICE increases

      } else {

        acceptancy = 3 ## meaning: reaches 0 inconvenience cost at 1/3=0.33 market share
        marketsharepush = 0
        additional_inconv_liq = 0 ## inconvenience for ICE increases

      }

      ## hybrid liquids and liquids belong to the same cluster (because they need to be independent on the cluster level)
      tmp=tmp[technology=="BEV", cluster := "El"]
      tmp=tmp[technology=="Hybrid Electric", cluster := "El"]
      tmp[is.na(cluster), cluster:= technology]

      ## hybrid liquids become a "conventional technology" if there is scarcity of refuelling stations (when there are little conventional+hybrid liquids):
      ## I need to define a column that represents the combined share of Hybrid Liquids and Liquids
      tmp[, combined_shareEL:= sum(weighted_sharessum), by = c("iso", "vehicle_type", "subsector_L1", "cluster", "year")]

      ## attribute inconvenience update to the "policy focused" technology
      tmp[, pinco:= ifelse(year == t & technology %in% techswitch,
                           pmax(-acceptancy*pinco[year==2010]*(combined_shareEL[year == (t-1)] + ifelse(year > 2020, marketsharepush, 0))+pinco[year==2010],0),
                           pinco), by = c("iso", "technology", "vehicle_type", "subsector_L1")]

      othertechs = setdiff(c("BEV", "FCEV"), techswitch)

      ## other alternative technologies don't have benefits from the market push
      tmp[, pinco:= ifelse(year == t & technology %in% othertechs,
                           pmax(-5*pinco[year==2010]*(combined_shareEL[year == (t-1)])+pinco[year==2010],0),
                           pinco), by = c("iso", "technology", "vehicle_type", "subsector_L1")]

      ## Hybrid Electric has a floor cost set to 0.1$/km
      tmp[, pinco:= ifelse(year == t & technology == "Hybrid Electric",
                           pmax(-5*pinco[year==2010]*(combined_shareEL[year == (t-1)])+pinco[year==2010], ifelse(t>=2020,additional_inconv_liq,0)),
                           pinco), by = c("iso", "technology", "vehicle_type", "subsector_L1")]

      ## hybrid liquids and liquids belong to the same cluster (because they need to be independent on the cluster level)
      tmp=tmp[technology=="Hybrid Liquids", cluster := "Liq"]
      tmp=tmp[technology=="Liquids", cluster := "Liq"]
      tmp=tmp[technology=="Hybrid Electric", cluster := "Liq"]
      tmp=tmp[technology=="BEV", cluster := "BEV"]



      ## hybrid liquids are "an alternative tehnology" until there are a few of them
      tmp[, pinco:= ifelse(year == t & technology %in% c("Hybrid Liquids"),
                           ifelse(shareFS1[year == (t-1)]<0.2,
                                  pmax(-5*pinco[year==2010]*
                                         (weighted_sharessum[year == (t-1)])+
                                         pinco[year==2010],ifelse(t>=2020,additional_inconv_liq,0)),
                                  NA
                           ),pinco),
          by = c("iso", "technology", "vehicle_type", "subsector_L1")]

      ## hybrid liquids become a "conventional technology" if there is scarcity of refuelling stations (when there are little conventional+hybrid liquids):
      ## I need to define a column that represents the combined share of Hybrid Liquids and Liquids
      tmp[, combined_shareLiq:= sum(weighted_sharessum), by = c("iso", "vehicle_type", "subsector_L1", "cluster", "year")]


      ## until Liquids are well established they don't have inconvenience, unless exogenously driven by policies
      tmp[, pinco:= ifelse(year == t & technology %in% c("Hybrid Liquids") &
                             shareFS1[year == (t-1)] >0.2 &
                             combined_shareLiq[year == (t-1)]>0.2,
                                  pinco[year == (t-1)] + ifelse(t>=2020,additional_inconv_liq,0),
                                  pinco
                           ),
          by = c("iso", "technology", "vehicle_type", "subsector_L1")]


      ## hybrid liquids become a "conventional technology" if there is scarcity of refuelling stations (when there are little conventional+hybrid liquids)
      tmp[, pinco:= ifelse(year == t & technology %in% c("Hybrid Liquids"),
                           ifelse(is.na(pinco),
                                  pmax(-5*(pinco[year==2010]+ifelse(t>=2020,additional_inconv_liq,0))*
                                         (combined_shareLiq[year == (t-1) & technology == "Hybrid Liquids"])+
                                         pinco[year==2010]+ifelse(t>=2020,additional_inconv_liq,0),
                                    0.7),
                                  pinco),
                           pinco),
          by = c("iso", "vehicle_type", "subsector_L1", "cluster")]

      ## until Liquids are well established they don't have inconvenience, unless exogenously driven by policies
      tmp[, pinco:= ifelse(year == t & technology %in% c("Liquids"),
                           ifelse(combined_shareLiq[year == (t-1) & technology == "Liquids"]>0.2,
                                  pinco[year==2010] + ifelse(t>=2020,additional_inconv_liq,0),
                                  NA
                           ),pinco),
          by = c("iso", "technology", "vehicle_type", "subsector_L1")]

      ## when they become less established, they have increasing inconvenience
      tmp[, pinco:= ifelse(year == t & technology %in% c("Liquids"),
                           ifelse(is.na(pinco),
                                  pmax(-5*0.7*
                                         (combined_shareLiq[year == (t-1) & technology == "Liquids"])+
                                         0.7,
                                       ifelse(t>=2020,additional_inconv_liq,0)),
                                  pinco
                           ),pinco),
          by = c("iso","vehicle_type", "subsector_L1", "cluster")]

      ## Hybrid Electric partially suffer from lack of infrastructure of Liquids
      tmp[, pinco:= ifelse(year == t & technology == "Hybrid Electric" & combined_shareLiq[year == (t-1) & technology == "Liquids"]<0.2,
                                  pmax(0.5*pinco[year == (t) & technology == "Liquids"], ifelse(t>=2020,additional_inconv_liq,0)),
                           pinco),
          by = c("iso", "vehicle_type", "subsector_L1")]

      ## NG is not incentivized but its inconvenience cost is allowed to decrease (slowly)
      tmp[, pinco:= ifelse(year == t & technology %in% c("NG"),
                           pmax(-2*pinco[year==2010]*
                                  (weighted_sharessum[year == (t-1)])+
                                  pinco[year==2010],
                                ifelse(t>=2020,additional_inconv_liq,0)),
                           pinco), by = c("iso", "technology", "vehicle_type", "subsector_L1")]

      ## if I want to make a reporting, I need the temporary input to be saved
      if (t == 2100 & savetmpinput) {
        saveRDS(tmp, levelVWpath(scenarioVW = techswitch, "yearly_values_sales.RDS"))
      }

      ## remove "temporary" columns
      tmp[, c("cluster", "combined_shareEL", "combined_shareLiq", "shareFS1","D", "weighted_shares", "weighted_sharessum", "index_yearly") := NULL]

      if (t<tail(futyears_all,1)) {
        tmp1 = copy(tmp)
      }

    }
    print(paste("Iterative logit calculation finished in",
                difftime(Sys.time(), start, units="mins"),
                "Minutes"))

    tmp1 <- tmp[, share := (tot_price+pinco)^logit.exponent/(sum((tot_price+pinco)^logit.exponent)),
                 by = c("vehicle_type", "year", "iso")]

    tmp1[, c("subsector_L2", "subsector_L3", "sector", "tot_VOT_price") := list("trn_pass_road_LDV", "trn_pass_road", "trn_pass", 0)]

    tmp1 = merge(tmp1[year %in% unique(dfother$year)], dfprices4W, all = TRUE, by = intersect(names(tmp1), names(dfprices4W)))
    df <- rbind(dfother[, share := NA], tmp1)

    ## merge value of time for the selected level and assign 0 to the entries that don't have it
    df <- merge(df, value_time, by=intersect(names(df),names(value_time)), all.x=TRUE)
    df[, share := ifelse(is.na(share),(tot_price+pinco)^logit.exponent/(sum((tot_price+pinco)^logit.exponent)), share),
       by = c(group_value, "year", "iso")]

    inconv=copy(df[,.(year,iso,sector,subsector_L3,subsector_L2,subsector_L1,vehicle_type,technology,pinco)])


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

    return(list(df, MJ_km, df_shares, inconv))

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


    return(list(df, MJ_km, df_shares))

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

  if (endogeff) {
    ## create choice of  more expensive, more efficient alternatives (for now only Liquids)
    mj_km_dataEF <- rbind(mj_km_data[subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020 & technology =="Liquids"][, c("type", "MJ_km") := list("advanced", MJ_km*0.6)],
                          mj_km_data[subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020 & technology =="Liquids"][, c("type", "MJ_km") := list("middle", MJ_km*0.35)],
                          mj_km_data[subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020 & technology =="Liquids"][, type := "normal"])

    baseEF = merge(base[subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020 & technology =="Liquids"],
                   mj_km_dataEF[,c("iso", "year", "technology", "vehicle_type", "type", "MJ_km"), ],
                   by = c("iso", "year", "technology", "vehicle_type"))

    ## temporary shortcut to roughly represent the increase in non fuel price due to additional 3500 dollars purchase cost
    tmp = data.table(vehicle_type = c("Mini Car", "Subcompact Car", "Compact Car", "Midsize Car", "Large Car", "Large Car and SUV", "Van", "Light Truck and SUV", "Multipurpose Vehicle"),
                     dpp_nfp = c(0.27, 0.15, 0.1, 0.07, 0.05, 0.05, 0.05, 0.05, 0.05)) ## PP'=PP+3500$ -> PP~50%NFP, for small cars 3500$=55%PP, for big cars 3500$=10%PP ->small cars: 55%*50%~27%, big cars 10%*50%~5%

    baseEF = merge(baseEF, tmp, by = c("vehicle_type"))
    baseEF[type == "advanced", non_fuel_price := (1+dpp_nfp)*non_fuel_price]
    baseEF[type == "middle", non_fuel_price := (1+0.5*dpp_nfp)*non_fuel_price]
    baseEF[, dpp_nfp := NULL]

    ## fuel price needs to be recalculated as the energy intensity changed
    baseEF[, c("fuel_price_pkm", "MJ_km") := list(fuel_price    ## in $/EJ
                                                  *MJ_km        ## in MJ/km
                                                  *1e-12,       ## in $/km
                                                  NULL)]

    baseEF[, tot_price := fuel_price_pkm + non_fuel_price]


    E2F_all <- E2Fcalc(baseEF,
                       mj_km_dataEF)
    EF <- E2F_all[[1]]
    mj_km_dataEF <- E2F_all[[2]]
    EF_shares <- E2F_all[[3]]

    base <- rbind(base[!(subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020 & technology =="Liquids"),  c("iso","year","technology","vehicle_type","subsector_L1","subsector_L2","subsector_L3","sector", "tot_price","fuel_price_pkm", "non_fuel_price", "tot_VOT_price", "sector_fuel")],
                  EF)

    mj_km_data <- rbind(mj_km_data[!(subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020 & technology =="Liquids"), c("iso", "year", "technology", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector", "MJ_km", "sector_fuel")],
                        mj_km_dataEF)

  }

  FV_all <- F2Vcalc(prices = base,
                    mj_km_data,
                    group_value = "vehicle_type")
  FV <- FV_all[[1]]
  MJ_km_FV <- FV_all[[2]]
  FV_shares <- FV_all[[3]]
  inconv <- FV_all[[4]]

  # VS1
  VS1_all <- X2Xcalc(FV, MJ_km_FV,
                     level_base = "FV",
                     level_next = "VS1",
                     group_value = "subsector_L1")

  VS1 <- VS1_all[[1]]
  MJ_km_VS1 <- VS1_all[[2]]
  VS1_shares <- VS1_all[[3]]
  VS1_shares=VS1_shares[,-c("sector","subsector_L2","subsector_L3")]

  # S1S2
  S1S2_all <- X2Xcalc(VS1, MJ_km_VS1,
                      level_base = "VS1",
                      level_next = "S1S2",
                      group_value = "subsector_L2")
  S1S2 <- S1S2_all[[1]]
  MJ_km_S1S2 <- S1S2_all[[2]]
  S1S2_shares <- S1S2_all[[3]]


  # S2S3
  S2S3_all <- X2Xcalc(S1S2, MJ_km_S1S2,
                      level_base = "S1S2",
                      level_next = "S2S3",
                      group_value = "subsector_L3")

  S2S3 <- S2S3_all[[1]]
  MJ_km_S2S3 <- S2S3_all[[2]]
  S2S3_shares <- S2S3_all[[3]]

  # S3S
  S3S_all <- X2Xcalc(S2S3, MJ_km_S2S3,
                     level_base = "S2S3",
                     level_next = "S3S",
                     group_value = "sector")
  S3S <- S3S_all[[1]]
  MJ_km_S3S <- S3S_all[[2]]
  S3S_shares <- S3S_all[[3]]

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

  result=list(mj_km_data=mj_km_data,
              prices_list=prices_list,
              share_list=share_list,
              inconv_cost=inconv,
              EF_shares = EF_shares)

  return(result)
}

