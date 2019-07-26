#' Prepare outputs for REMIND: Set correct tech names, CES node names and years.
#'
#' @param share
#' @param intensity
#' @param capCosts
#' @param EDGE2teESmap
#' @param REMINDtall
#' @import remind
#' @import data.table
#' @importFrom rmndt approx_dt
#' @export

prepare4REMIND <- function(demByTech, intensity, capCost,
                           EDGE2teESmap,
                           REMINDtall){
    ## load conversion factor
    EJ_2_Twa <- 31.71e-03 ## TWa is the unit expected in REMIND for the final energy values
    conv_2005USD_1990USD=0.67 ## 2005USD=0.67*1990USD
    ## energy intensity
    intensity=merge(intensity, EDGE2teESmap[,c("CES_node","teEs")],
                    by="CES_node",all.x=TRUE)
    intensity=intensity[, .(tall = year, iso, all_teEs=teEs, value)]
    intensity=approx_dt(dt=intensity, xdata=REMINDtall,
                        xcol="tall",
                        idxcols=c("iso", "all_teEs"),
                        extrapolate=T)
    intensity[,value:=value ## in [milliokm/EJ]
              /EJ_2_Twa     ## in [millionkm/Twa]
              *1e-6         ## in [trillionkm/Twa]
              ]
    setcolorder(intensity, c("tall", "iso", "all_teEs", "value"))

    ## non-fuel price
    budget=merge(capCost, unique(EDGE2teESmap[,c("teEs","EDGE_top")]),
                 by="teEs",all.x=TRUE)
    budget=budget[, .(tall = year, iso, all_teEs=teEs, value)]
    budget=approx_dt(dt=budget, xdata=REMINDtall,
                     xcol="tall",
                     idxcols=c("iso", "all_teEs"),
                     extrapolate=T)

    budget[,value:=value ## in 1990USD/pkm
                   /conv_2005USD_1990USD] ## in [2005USD/pkm]
    setcolorder(budget, c("tall", "iso", "all_teEs", "value"))

    ## demand by technology
    demByTech=merge(demByTech, EDGE2teESmap[,c("CES_node","all_in","all_enty","teEs")],
                by="CES_node", all.x=TRUE)
    demByTech=demByTech[, .(tall = year, iso, all_enty, all_in, all_teEs=teEs, value, CES_parent)]
    demByTech=approx_dt(dt=demByTech, xdata=REMINDtall,
                    xcol="tall",
                    idxcols=c("iso","all_in","all_enty","all_teEs", "CES_parent"),
                    extrapolate=T)
    setcolorder(demByTech, c("tall", "iso","all_enty","all_in","all_teEs", "CES_parent", "value"))

    result=list(demByTech=demByTech,
                intensity=intensity,
                capCost=budget)
    return(result)

}
