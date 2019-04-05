#' Merge REMIND-derived fuel prices with non-fuel costs.
#'
#' @param prices
#' @param path2VOT
#' @param path2calibration_outpu
#' @param path2logitparam
#' @param path2intensities
#' @param path2UCD
#' @param full_data
#' @import remind
#' @import data.table
#' @importFrom rmndt toISO_dt toRegions_dt
#' @export

calculate_logit <- function(prices,
                            path2VOT,
                            path2calibration_output,
                            path2logitparam,
                            path2intensities,
                            path2UCD,
                            full_data = F) {
    logit_data <- readRDS(path2logitparam)
    sw_data <- readRDS(path2calibration_output)
    vot_data <- readRDS(path2VOT)

    ## X2Xcalc is used to traverse the logit tree, calculating shares and intensities
    X2Xcalc <- function(prices, conv_pkm_mj, level_base, level_next, group_value) {
        final_SW <- sw_data[[paste0(level_next, "_final_SW")]]
        logit_exponent <- logit_data[[paste0("logit_exponent_", level_next)]]
        
        ## data contains all the prices in the beginning
        all_subsectors <- c("technology", "vehicle_type", "subsector_L1", "subsector_L2",
                            "subsector_L3", "sector")
        ## takes the right files using the names as inputs
        value_time <- vot_data[[paste0("value_time_", level_next)]]
        ## joins the df containing the prices with the df containing the logit exponents
        df <- merge(prices, logit_exponent,
                    by=intersect(names(prices),names(logit_exponent)), all.x = TRUE)

        ## joins the previous df with gathe df containing the sw
        df <- merge(df, final_SW,by=intersect(names(df),names(final_SW)), all.y=TRUE)

        ## needs random lambdas for the sectors that are not explicitly calculated
        df <- df[, logit.exponent := ifelse(is.na(logit.exponent), -10, logit.exponent)]

        ## calculate the shares given prices, lambda and sw
        df <- df[, share := sw * tot_price^logit.exponent/sum(sw * tot_price^logit.exponent),
                 by = c(group_value, "region", "year")]

        ## merge value of time for the selected level and assign 0 to the entries that don't have it
        df <- merge(df, value_time,by=intersect(names(df),names(value_time)),all.x=TRUE)

        df <- df[, `:=`(time_price, ifelse(is.na(time_price), 0, time_price))]
        df <- df[, `:=`(tot_VOT_price, time_price + tot_VOT_price)]
        df <- df[, `:=`(tot_price, tot_price + time_price)]

        MJ_vkm <- merge(df, conv_pkm_mj,by=intersect(names(df),names(conv_pkm_mj)),all = FALSE)

        MJ_vkm <- MJ_vkm[, .(conv_pkm_MJ = sum(share * conv_pkm_MJ)), by = c("region",
                                                                             "year", group_value)]

        ## get rid of the ( misleading afterwards) columns
        df_shares <- copy(df)

        df_shares <- df_shares[
          , c("share", "region", "year",
              all_subsectors[
                  seq(match(group_value, all_subsectors) - 1,
                      length(all_subsectors), 1)],
              "tot_VOT_price",
              "fuel_price",
              "non_fuel_price"), with = FALSE]
        
        ## calculate 'one level up' database with the useful columns only
        df <- df[
          , c("share","tot_price","tot_VOT_price",
              "fuel_price","non_fuel_price","region","year",
              all_subsectors[
                  seq(match(group_value, all_subsectors) - 1,
                      length(all_subsectors), 1)]), with = FALSE]

        ## calculate prices of one level up
        df=df[,.(tot_price=sum(share*tot_price),
                 tot_VOT_price=sum(share*tot_VOT_price),
                 fuel_price=sum(share*fuel_price),
                 non_fuel_price=sum(share*non_fuel_price)),
              by = c("region","year",
                     all_subsectors[
                         seq(match(group_value,all_subsectors),
                             length(all_subsectors),1)])]

        return(list(df, MJ_vkm, df_shares))

    }


    ## FV load technology prices and merge with value of time (~technology price for
    ## non-motorized)

    base <- prices[year %in% years,]

    ## non-fuel prices
    price_nonmot <- vot_data[["price_nonmot"]]
    base= merge(base, price_nonmot, all.x = TRUE,
                by = c("tot_price","region","year",
                       "technology","vehicle_type",
                       "subsector_L1","subsector_L2","subsector_L3","sector"))

    base[,tot_VOT_price := 0]
    #Cycling and Walking have no fuel and non fuel prices, 0 instead of NA is given
    base[,fuel_price:=ifelse(is.na(fuel_price),0,fuel_price)]
    base[,non_fuel_price:=ifelse(is.na(non_fuel_price),0,non_fuel_price)]

    ## energy intensity
    conv_pkm_mj <- readRDS(path2intensities)
    conv_pkm_mj <- conv_pkm_mj[, `:=`(conv_pkm_MJ, EJ_Mpkm_final/CONV_MJ_EJ*CONV_unit_million)]  #MJ/pkm
    conv_pkm_mj=conv_pkm_mj[,-c("EJ_Mpkm","EJ_Mpkm_adjusted","lambda","EJ_Mpkm","EJ_Mpkm_final")]

    FV_all <- X2Xcalc(base, conv_pkm_mj,
                      level_base = "base",
                      level_next = "FV",
                      group_value = "vehicle_type")
    FV <- FV_all[[1]]
    MJ_vkm_FV <- FV_all[[2]]
    FV_shares <- FV_all[[3]]

    # VS1
    VS1_all <- X2Xcalc(FV, MJ_vkm_FV,
                       level_base = "FV",
                       level_next = "VS1",
                       group_value = "subsector_L1")

    VS1 <- VS1_all[[1]]
    MJ_vkm_VS1 <- VS1_all[[2]]
    VS1_shares <- VS1_all[[3]]

    # S1S2
    S1S2_all <- X2Xcalc(VS1, MJ_vkm_VS1,
                        level_base = "VS1",
                        level_next = "S1S2",
                        group_value = "subsector_L2")
    S1S2 <- S1S2_all[[1]]
    MJ_vkm_S1S2 <- S1S2_all[[2]]
    S1S2_shares <- S1S2_all[[3]]


    # S2S3
    S2S3_all <- X2Xcalc(S1S2, MJ_vkm_S1S2,
                        level_base = "S1S2",
                        level_next = "S2S3",
                        group_value = "subsector_L3")

    S2S3 <- S2S3_all[[1]]
    MJ_vkm_S2S3 <- S2S3_all[[2]]
    S2S3_shares <- S2S3_all[[3]]

    # S3S
    S3S_all <- X2Xcalc(S2S3, MJ_vkm_S2S3,
                       level_base = "S2S3",
                       level_next = "S3S",
                       group_value = "sector")
    S3S <- S3S_all[[1]]
    MJ_vkm_S3S <- S3S_all[[2]]
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

    result=list(conv_pkm_mj=conv_pkm_mj,
                MJ_vkm_S3S=MJ_vkm_S3S,
                prices_list=prices_list,
                share_list=share_list)

    return(result)
}
