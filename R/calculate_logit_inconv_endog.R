calculate_logittimeprova = function(prices,
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
    ## clusters of technologies: BEV and PlugInH are together
    mapcluster = data.table(cluster = c("Electric", "Electric", "Liquids", "Hybrid Liquids", "NG", "Hydrogen"), technology = c("BEV", "Hybrid Electric", "Liquids", "Hybrid Liquids", "NG", "FCEV"))
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
    dfother = df[subsector_L1 != "trn_pass_road_LDV_4W", c("iso", "year", "subsector_L2", "subsector_L3", "sector", "subsector_L1", "vehicle_type", "technology", "tot_price", "logit.exponent", "pinco", "tot_VOT_price", "fuel_price_pkm", "non_fuel_price")]

    ## 4W are calculated separately
    df4W = df[subsector_L1 == "trn_pass_road_LDV_4W", c("iso", "year", "subsector_L1", "vehicle_type", "technology", "tot_price", "logit.exponent")]

    ## extrapolate for all years
    setnames(df4W, old = "tot_price", new = "value") ## rename otherwise approx_dt complains
    df4W = approx_dt(dt = df4W, xdata = futyears_all,
                            idxcols = c("iso",  "subsector_L1", "vehicle_type", "technology"),
                            extrapolate=T)
    setnames(df4W, old = "value", new = "tot_price")  ## rename back

    ## other price components for 4W are useful later but will not be carried on in the yearly calculation
    dfprices4W = df[subsector_L1 == "trn_pass_road_LDV_4W",  c("iso", "year", "subsector_L1", "vehicle_type", "technology", "fuel_price_pkm", "non_fuel_price")]

    ## starting value for inconvenience cost of 2010 for 4W is needed as a starting point for the iterative calculations
    dfpinco2010 = df[subsector_L1 == "trn_pass_road_LDV_4W" & year == 2010, c("iso", "subsector_L1", "vehicle_type", "technology", "pinco")]

    ## merge the yearly values and the starting inconvenience cost
    df = merge(df4W, dfpinco2010, all = TRUE, by = c("iso", "subsector_L1", "vehicle_type", "technology"))
    ## apply the same logit exponent to all the years
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

      ## attribute the cluster to each "group" (hybrid liquids and liquids are for the first part of the loop independent)
      tmp2 <- merge(tmp2, tmp[year %in% (t-1),], by = c("iso", "year", "vehicle_type", "subsector_L1"))
      tmp2[technology %in% c("BEV", "Hybrid Electric"), cluster := "Electric"]
      tmp2[technology %in% c("Liquids"), cluster := "Liquids"]
      tmp2[technology %in% c("Hybrid Liquids"), cluster := "Hybrid Liquids"]
      tmp2[technology %in% c("NG"), cluster := "NG"]
      tmp2[technology %in% c("FCEV"), cluster := "Hydrogen"]
      ## calculate the share from the cluster to S1 (CS1)
      tmp2[, shareCS1 := share*shareVS1]

      ## calculate the share of all Clusters on the subsector_L1 (I don't care anymore about the vehicle_type)
      tmp2 <- tmp2[,.(shareCS1=sum(shareCS1)),by=c("iso", "cluster", "subsector_L1","year")]
      ## attribute the same (cluster) value to every (cluster) member
      tmp2 <- merge(tmp2, mapcluster, all = TRUE, by = "cluster", allow.cartesian = TRUE)


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
      tmp[is.na(shareCS1), shareCS1 := 0]

      ## weighted shares are the weighted average of each time step's CS1 share and how much it depreciated in time
      tmp[, weighted_shares := mean(shareCS1*D), by = c("iso", "cluster", "vehicle_type", "subsector_L1", "year")]
      ## for 2010, we assume the value was constant for all the previous years (hence the value for 2010 coincides with the share)
      tmp[, weighted_sharessum := ifelse(year == 2010, weighted_shares, NA)]
      print(paste0("time is ", t))
      tmp[, weighted_sharessum := ifelse(year == (t-1), sum(weighted_shares[year<t])/sum(D[year<t]), weighted_sharessum), by = c("iso", "technology", "vehicle_type", "subsector_L1")]

      if (selfmarket_acceptancy & selfmarket_policypush) {

        acceptancy = 5 ## meaning: reaches 0 inconvenience cost at 1/5=0.2 market share
        marketsharepush = 0.05
        additional_inconv_liq = 0.1 ## inconvenience for ICE increases

      } else {

        acceptancy = 10 ## meaning: reaches 0 inconvenience cost at 1/10=0.1 market share
        marketsharepush = 0
        additional_inconv_liq = 0 ## inconvenience for ICE increases

      }

      ## attribute inconvenience update to the "policy focused" technology
      tmp[, pinco:= ifelse(year == t & technology %in% techswitch,
                           pmax(-acceptancy*pinco[year==2010]*(weighted_sharessum[year == (t-1)] - weighted_sharessum[year == 2010] + ifelse(year > 2020, marketsharepush, 0))+pinco[year==2010],0),
                           pinco), by = c("iso", "technology", "vehicle_type", "subsector_L1")]

      othertechs = setdiff(c("BEV", "FCEV", "Hybrid Electric"), techswitch)

      ## all other alternative technologies have benefits
      tmp[, pinco:= ifelse(year == t & technology %in% othertechs,
                           pmax(-5*pinco[year==2010]*(weighted_sharessum[year == (t-1)] - weighted_sharessum[year == 2010] + ifelse(year > 2020, 2/3*marketsharepush, 0))+pinco[year==2010],0),
                           pinco), by = c("iso", "technology", "vehicle_type", "subsector_L1")]

      ## hybrid liquids and liquids belong to the same "uppercluster" (because they need to be independent on the cluster level)
      tmp=tmp[technology=="Hybrid Liquids", uppercluster := "Liq"]
      tmp=tmp[technology=="Liquids", uppercluster := "Liq"]
      tmp[is.na(uppercluster), uppercluster:= technology]

      ## hybrid liquids are "an alternative tehnology" until there are a few of them
      tmp[, pinco:= ifelse(year == t & technology %in% c("Hybrid Liquids"),
                           ifelse(shareCS1[year == (t-1)]<0.2,
                                  pmax(-5*pinco[year==2010]*
                                         (weighted_sharessum[year == (t-1)]-weighted_sharessum[year == 2010])+
                                         pinco[year==2010],ifelse(t>=2020,additional_inconv_liq,0)),
                                  NA
                           ),pinco),
          by = c("iso", "technology", "vehicle_type", "subsector_L1")]

      ## hybrid liquids become a "conventional technology" if there is scarcity of refuelling stations (when there are little conventional+hybrid liquids):
      ## I need to define a column that represents the combined share of Hybrid Liquids and Liquids
      tmp[, combined_share:= sum(weighted_sharessum), by = c("iso", "vehicle_type", "subsector_L1", "uppercluster", "year")]


      ## until Liquids are well established they don't have inconvenience, unless exogenously driven by policies
      tmp[, pinco:= ifelse(year == t & technology %in% c("Hybrid Liquids") &
                             shareCS1[year == (t-1)] >0.2 &
                             combined_share[year == (t-1)]>0.2,
                                  pinco[year == (t-1)] + ifelse(t>=2020,additional_inconv_liq,0),
                                  pinco
                           ),
          by = c("iso", "technology", "vehicle_type", "subsector_L1")]


      ## hybrid liquids become a "conventional technology" if there is scarcity of refuelling stations (when there are little conventional+hybrid liquids)
      tmp[, pinco:= ifelse(year == t & technology %in% c("Hybrid Liquids"),
                           ifelse(is.na(pinco),
                                  pmax(-5*(pinco[year==2010]+ifelse(t>=2020,additional_inconv_liq,0))*
                                         (combined_share[year == (t-1) & technology == "Hybrid Liquids"])+
                                         pinco[year==2010]+ifelse(t>=2020,additional_inconv_liq,0),
                                    0.3+ifelse(t>=2020,additional_inconv_liq,0)),
                                  pinco),
                           pinco),
          by = c("iso", "vehicle_type", "subsector_L1", "uppercluster")]

      ## until Liquids are well established they don't have inconvenience, unless exogenously driven by policies
      tmp[, pinco:= ifelse(year == t & technology %in% c("Liquids"),
                           ifelse(combined_share[year == (t-1) & technology == "Liquids"]>0.2,
                                  pinco[year==2010] + ifelse(t>=2020,additional_inconv_liq,0),
                                  NA
                           ),pinco),
          by = c("iso", "technology", "vehicle_type", "subsector_L1")]

      ## when they become less established, they have increasing inconvenience
      tmp[, pinco:= ifelse(year == t & technology %in% c("Liquids"),
                           ifelse(is.na(pinco),
                                  pmax(-5*(0.3+ifelse(t>=2020,additional_inconv_liq,0))*
                                         (combined_share[year == (t-1) & technology == "Liquids"])+
                                         0.3+ifelse(t>=2020,additional_inconv_liq,0),
                                       0),
                                  pinco
                           ),pinco),
          by = c("iso","vehicle_type", "subsector_L1", "uppercluster" )]

      ## Hybrid Electric partially suffer from lack of infrastructure of Liquids
      tmp[, pinco:= ifelse(year == t & technology == "Hybrid Electric" & combined_share[year == (t-1) & technology == "Liquids"]<0.2,
                                  pinco[year == (t) & technology == "Liquids"],
                           pinco),
          by = c("iso", "vehicle_type", "subsector_L1")]

      ## NG is not incentivized but its inconvenience cost is allowed to decrease
      tmp[, pinco:= ifelse(year == t & technology %in% c("NG"),
                           pmax(-5*
                                  pinco[year==2010]*
                                  (weighted_sharessum[year == (t-1)]-weighted_sharessum[year == 2010])+
                                  pinco[year==2010],
                                0),
                           pinco), by = c("iso", "technology", "vehicle_type", "subsector_L1")]

      ## remove "temporary" columns
      tmp[, c("uppercluster", "combined_share","shareCS1","D", "weighted_shares", "weighted_sharessum", "cluster", "index_yearly") := NULL]

      if (t<tail(futyears_all,1)) {
        tmp1 = copy(tmp)
      }

    }

    tmp1 <- tmp1[year == 2100, share := (tot_price+pinco)^logit.exponent/(sum((tot_price+pinco)^logit.exponent)),
                 by = c(group_value, "year", "iso")]

    tmp1[, c("subsector_L2", "subsector_L3", "sector", "tot_VOT_price") := list("trn_pass_road_LDV", "trn_pass_road", "trn_pass", 0)]

    tmp1 = merge(tmp1[year %in% unique(dfother$year)], dfprices4W, all = TRUE, by = intersect(names(tmp1), names(dfprices4W)))
    df <- rbind(dfother[, share := NA], tmp1)

    ## merge value of time for the selected level and assign 0 to the entries that don't have it
    df <- merge(df, value_time, by=intersect(names(df),names(value_time)), all.x=TRUE)
    df[is.na(share), share := (tot_price+pinco)^logit.exponent/(sum((tot_price+pinco)^logit.exponent)),
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
    ## create choice of  more expensive, more efficient alternatives (for now only LDVs)
    mj_km_dataEF <- rbind(mj_km_data[subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020][, c("type", "MJ_km") := list("advanced", MJ_km*0.7)],
                          mj_km_data[subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020][, type := "normal"])

    baseEF <- rbind(base[subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020][, c("type", "non_fuel_price") := list("advanced", non_fuel_price*1.3)],
                    base[subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020][, "type" := "normal"])

    baseEF[, tot_price := fuel_price_pkm + non_fuel_price]

    E2F_all <- E2Fcalc(baseEF,
                       mj_km_dataEF)
    EF <- E2F_all[[1]]
    mj_km_dataEF <- E2F_all[[2]]
    EF_shares <- E2F_all[[3]]

    base <- rbind(base[!(subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020),  c("iso","year","technology","vehicle_type","subsector_L1","subsector_L2","subsector_L3","sector", "tot_price","fuel_price_pkm", "non_fuel_price", "tot_VOT_price", "sector_fuel")],
                  EF)

    mj_km_data <- rbind(mj_km_data[!(subsector_L1 == "trn_pass_road_LDV_4W" & year > 2020), c("iso", "year", "technology", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector", "MJ_km", "sector_fuel")],
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

