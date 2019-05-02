#' Prepare outputs for REMIND: Set correct tech names, CES node names and years.
#'
#' @param share
#' @param intensity
#' @param capCosts
#' @param EDGE2teESmap
#' @param REMINDtall
#' @import remind
#' @import data.table
#' @importFrom rmndt toISO_dt toRegions_dt approx_dt
#' @export

prepare4REMIND <- function(share, intensity, capCost,
                           EDGE2teESmap,
                           REMINDtall){
    ## load conversion factor
    EJ_2_Twa <- 31.71e-03 ## TWa is the unit expected in REMIND for the final energy values
    conv_2005USD_1990USD=0.67 ## 2005USD=0.67*1990USD
    ## energy intensity
    intensity=merge(intensity, EDGE2teESmap[,c("CES_node","teEs")],
                    by="CES_node",all.x=TRUE)
    intensity=intensity[, .(tall = year, all_regi=region, all_teEs=teEs, value)]
    intensity=approx_dt(dt=intensity, xdata=REMINDtall,
                        xcol="tall",
                        idxcols=c("all_regi", "all_teEs"),
                        extrapolate=T)
    intensity=intensity[order(all_regi,tall,all_teEs)]
    intensity[,value:=value ## in [milliokm/EJ]
              /EJ_2_Twa     ## in [millionkm/Twa]
              *1e-6         ## in [trillionkm/Twa]
              ]
    setcolorder(intensity, c("tall", "all_regi", "all_teEs", "value"))

    ## non-fuel price
    budget=merge(capCost, unique(EDGE2teESmap[,c("teEs","EDGE_top")]),
                 by="teEs",all.x=TRUE)
    budget=budget[, .(tall = year, all_regi=region, all_teEs=teEs, value)]
    budget=approx_dt(dt=budget, xdata=REMINDtall,
                     xcol="tall",
                     idxcols=c("all_regi", "all_teEs"),
                     extrapolate=T)

    budget[,value:=value ## in 1990USD/pkm
                   /conv_2005USD_1990USD ## in [2005USD/pkm]
                   *1e+12                ## in [2005USD/trillionpkm]
                     ]

    ## set columns order
    setcolorder(budget, c("tall", "all_regi", "all_teEs", "value"))

    ## shares
    share[, c("CES_parent","value"):=list(NULL,NULL)]
    share=merge(share, EDGE2teESmap[,c("CES_node","all_in","all_enty","teEs")],
                by="CES_node", all.x=TRUE)
    share=share[, .(tall = year, all_regi=region, all_enty, all_in,all_teEs=teEs, value=shares)]
    share=approx_dt(dt=share, xdata=REMINDtall,
                    xcol="tall",
                    idxcols=c("all_regi","all_in","all_enty","all_teEs"),
                    extrapolate=T)

    ## set columns order
    setcolorder(share, c("tall", "all_regi","all_enty","all_in","all_teEs", "value"))

    result=list(share=share,
                intensity=intensity,
                capCost=budget)
    return(result)

}
