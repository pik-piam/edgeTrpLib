calculate_logittime = function(prices,
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

    cols <- all_subsectors[seq(match(group_value, all_subsectors) - 1,length(all_subsectors), 1)]

    futyears = years[years>=2010 & years <= 2110]
    tmp1 = df[year <2010,]
    tmp1[, share := NA]
    for (t in futyears[futyears>2010]) {
      if (t > 2015) {
        tmp2 <- df[year %in% c(futyears[which(futyears==t)]),]
        tmp2[, share := NA]
        tmp <- rbind(tmp2, tmp1[year %in% c(futyears[which(futyears==t)-1]),])
        # print(tmp[iso=="ARM" & vehicle_type =="3W Rural"])
        } else {
          tmp <- df[year %in% c(futyears[which(futyears==t)-1], futyears[which(futyears==t)]),]
        }
      tmp <- tmp[year == futyears[which(futyears==t)-1], share := (tot_price+pinco)^logit.exponent/(sum((tot_price+pinco)^logit.exponent)),
                   by = c(group_value, "year", "iso")]
      tmp2 <- tmp[year==futyears[which(futyears==t)-1] & subsector_L1=="trn_pass_road_LDV_4W"]
      tmp2 <- tmp2[,.(tot_price=sum(share*tot_price)),
                   by = c("iso","year",
                          all_subsectors[seq(match(group_value,all_subsectors),length(all_subsectors),1)]
                          )]
      ## joins the previous df with gathe df containing the inconvenience costs
      tmp2 <- merge(tmp2, final_incoVS1, by=intersect( names(tmp2),names(final_incoVS1)), all.x=TRUE)

      tmp2 <- merge(tmp2, logit_exponentVS1,
                    by=intersect(names(tmp2), names(logit_exponentVS1)), all.x = TRUE)

      tmp2 <- tmp2[year == futyears[which(futyears==t)-1], shareVS1 := (tot_price+pinco)^logit.exponent/(sum((tot_price+pinco)^logit.exponent)),
                   by = c("subsector_L1", "year", "iso")]
      tmp2 <- tmp2[,.(subsector_L1, year, iso, vehicle_type, shareVS1)]

      tmp2 <- merge(tmp2, tmp, by = c("iso", "year", "vehicle_type", "subsector_L1"))
      tmp2[, share := share*shareVS1]
      ## calculate the share of all technologies on the subsector_L1 (I don't care anymore about the vehicle_type)
      tmp2 <- tmp2[,.(shareFS1=sum(share)),by=c("iso","technology","subsector_L1","year")]

      tmp <- merge(tmp,tmp2, by=c("technology", "year", "iso", "subsector_L1"), all=TRUE)
      if (t >2015) {
        tmp = rbind(tmp, tmp2010)
      }

      if (selfmarket_policypush & !selfmarket_acceptancy ) {
        marketsharepush = 0.1
        tmp[, pinco:= ifelse(year == futyears[which(futyears==t)] & technology %in% techswitch & subsector_L1 == "trn_pass_road_LDV_4W",
                               ifelse(shareFS1[year == futyears[which(futyears==t)-1]]<marketsharepush & year>=2020,
                                      (pmax(-4*pinco[year==2010]*(shareFS1[year == futyears[which(futyears==t)-1]]-shareFS1[year == 2010] + marketsharepush)+pinco[year==2010],0)),
                                      (pmax(-4*pinco[year==2010]*(shareFS1[year == futyears[which(futyears==t)-1]]-shareFS1[year == 2010])+ pinco[year==2010]))),
                               pinco), by = c("iso", cols)]

        othertechs = setdiff(c("BEV", "FCEV", "Hybrid Liquids"), techswitch)

        tmp[, pinco:= ifelse(year == futyears[which(futyears==t)] & technology %in% othertechs & subsector_L1 == "trn_pass_road_LDV_4W",
                             ifelse(shareFS1[year == futyears[which(futyears==t)-1]]<marketsharepush & year>=2020,
                                    (pmax(-4*pinco[year==2010]*(shareFS1[year == futyears[which(futyears==t)-1]]-shareFS1[year == 2010] + marketsharepush)+pinco[year==2010],0)),
                                    (pmax(-4*pinco[year==2010]*(shareFS1[year == futyears[which(futyears==t)-1]]-shareFS1[year == 2010])+ pinco[year==2010]))),
                             pinco), by = c("iso", cols)]

      } else if (selfmarket_acceptancy & !selfmarket_policypush) {
        acceptancy = 5 ## meaning: reaches 0 inconvenience cost at 1/4=0.25 market share
        tmp[, pinco:= ifelse(year == futyears[which(futyears==t)] & technology %in% techswitch & subsector_L1 == "trn_pass_road_LDV_4W",
                             pmax(-acceptancy*pinco[year==2010]*(shareFS1[year == futyears[which(futyears==t)-1]]-shareFS1[year == 2010])+pinco[year==2010],0),
                             pinco), by = c("iso", cols)]

        othertechs = setdiff(c("BEV", "FCEV", "Hybrid Liquids"), techswitch)

        tmp[, pinco:= ifelse(year == futyears[which(futyears==t)] & technology %in% othertechs & subsector_L1 == "trn_pass_road_LDV_4W",
                             pmax(-5*pinco[year==2010]*(shareFS1[year == futyears[which(futyears==t)-1]]-shareFS1[year == 2010])+pinco[year==2010],0),
                             pinco), by = c("iso", cols)]

      } else if (selfmarket_acceptancy & selfmarket_policypush) {
        acceptancy = 5 ## meaning: reaches 0 inconvenience cost at 1/4=0.25 market share
        marketsharepush = 0.1
        tmp[, pinco:= ifelse(year == futyears[which(futyears==t)] & technology %in% techswitch & subsector_L1 == "trn_pass_road_LDV_4W",
                             ifelse(shareFS1[year == futyears[which(futyears==t)-1]]<marketsharepush & year>=2020,
                                    (pmax(-acceptancy*pinco[year==2010]*(shareFS1[year == futyears[which(futyears==t)-1]]-shareFS1[year == 2010] + marketsharepush)+pinco[year==2010],0)),
                                    (pmax(-acceptancy*pinco[year==2010]*(shareFS1[year == futyears[which(futyears==t)-1]]-shareFS1[year == 2010])+ pinco[year==2010],0))),
                             pinco), by = c("iso", cols)]

        othertechs = setdiff(c("BEV", "FCEV", "Hybrid Liquids"), techswitch)

        tmp[, pinco:= ifelse(year == futyears[which(futyears==t)] & technology %in% othertechs & subsector_L1 == "trn_pass_road_LDV_4W",
                             ifelse(shareFS1[year == futyears[which(futyears==t)-1]]<marketsharepush & year>=2020,
                                    (pmax(-5*pinco[year==2010]*(shareFS1[year == futyears[which(futyears==t)-1]]-shareFS1[year == 2010] + marketsharepush)+pinco[year==2010],0)),
                                    (pmax(-5*pinco[year==2010]*(shareFS1[year == futyears[which(futyears==t)-1]]-shareFS1[year == 2010])+ pinco[year==2010],0))),
                             pinco), by = c("iso", cols)]

      } else {
        tmp[, pinco:= ifelse(year == futyears[which(futyears==t)]& technology %in% techswitch  & subsector_L1 == "trn_pass_road_LDV_4W",
                             pmax(-4*
                                    pinco[year==2010]*
                                    (shareFS1[year == futyears[which(futyears==t)-1]]-shareFS1[year == 2010])+
                                    pinco[year==2010],
                                  0),
                             pinco), by = c("iso", cols)]

        othertechs = setdiff(c("BEV", "FCEV", "Hybrid Liquids"), techswitch)

        tmp[, pinco:= ifelse(year == futyears[which(futyears==t)] & technology %in% othertechs & subsector_L1 == "trn_pass_road_LDV_4W",
                             pmax(-4*
                                    pinco[year==2010]*
                                    (shareFS1[year == futyears[which(futyears==t)-1]]-shareFS1[year == 2010])+
                                    pinco[year==2010],
                                  0),
                             pinco), by = c("iso", cols)]

      }

      tmp[, pinco:= ifelse(year == futyears[which(futyears==t)]& technology %in% c("Liquids")  & subsector_L1 == "trn_pass_road_LDV_4W",
                           ifelse(shareFS1[year == futyears[which(futyears==t)-1]]>0.2 & year>=2020,pinco[year==2010], pmin(1.25*
                                  0.4*
                                  (-shareFS1[year == futyears[which(futyears==t)-1]]+shareFS1[year == 2010]),
                                0.4)),
                           pinco), by = c("iso", cols)]

      tmp[, pinco:= ifelse(year == futyears[which(futyears==t)]& technology=="NG"  & subsector_L1 == "trn_pass_road_LDV_4W",
                           pmax(-4*
                                  pinco[year==2010]*
                                  (shareFS1[year == futyears[which(futyears==t)-1]]-shareFS1[year == 2010])+
                                  pinco[year==2010],
                                0),
                           pinco), by = c("iso", cols)]


      if(t==2015){
        tmp2010 = tmp[year == 2010]
      }

      if(t>2015){
        tmp = tmp[year>2010]
      }




      tmp[, shareFS1 := NULL]

      if (t > 2015) {
        tmp1 <-rbind(tmp1[year != futyears[which(futyears==t)-1]], tmp)
        } else {
          tmp1 <- rbind(tmp1,tmp)
        }
      }

    tmp1 <- tmp1[, share := (tot_price+pinco)^logit.exponent/(sum((tot_price+pinco)^logit.exponent)),
                   by = c(group_value, "year", "iso")]

    df <- tmp1


    ## merge value of time for the selected level and assign 0 to the entries that don't have it
    df <- merge(df, value_time, by=intersect(names(df),names(value_time)), all.x=TRUE)


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

