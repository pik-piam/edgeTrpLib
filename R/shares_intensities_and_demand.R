#' Derive shares, demand and intensities from the logit tree.
#'
#' @param logit_shares
#' @param path2intensities
#' @param demand_input
#' @import data.table
#' @importFrom rmndt aggregate_dt approx_dt
#' @export

shares_intensity_and_demand <- function(logit_shares,
                                        MJ_km_base,
                                        EDGE2CESmap,
                                        REMINDyears,
                                        scenario,
                                        demand_input=NULL,
                                        REMIND2ISO_MAPPING=NULL){

    ## load the shares at each level
    S3S_shares <- logit_shares[["S3S_shares"]]
    S2S3_shares <- logit_shares[["S2S3_shares"]]
    S1S2_shares <- logit_shares[["S1S2_shares"]]
    VS1_shares <- logit_shares[["VS1_shares"]]
    FV_shares <- logit_shares[["FV_shares"]]
    ## create a normalized total demand OR loads absolute demand if given
    if (is.null(demand_input)) {
        demand=CJ(iso=unique(S3S_shares$iso),
                  sector=unique(S3S_shares$sector),
                  year=unique(S3S_shares$year))
        demand[,demand:=1]
    }else {
        demand=demand_input
    }

    ## define regional aggregation
    if (!is.null(REMIND2ISO_MAPPING)) {
      regcol = "region"
    } else { regcol = "iso"}

    ## calculate demand in million pkm for each level
    #S->S3
    demand = merge(demand, S3S_shares, all.y = TRUE, by = c("iso", "year", "sector"))
    demand = demand[,.(demand_L3 = demand*share, iso, year, sector, subsector_L3)]
    #S3->S2
    demand = merge(demand, S2S3_shares, all=TRUE, by = c("iso", "year", "sector", "subsector_L3"))
    demand = demand[,.(demand_L2 = demand_L3*share, iso, sector, year, subsector_L3, subsector_L2)]
    #S2->S1
    demand = merge(demand, S1S2_shares, all=TRUE, by = c("iso", "year", "sector", "subsector_L3", "subsector_L2"))
    demand = demand[,.(demand_L1 = demand_L2*share, iso, sector, year, subsector_L3, subsector_L2, subsector_L1)]
    #S1->V
    demand = merge(demand, VS1_shares, all=TRUE, by = c("iso", "year", "subsector_L1"))
    demand = demand[,.(demand_V = demand_L1*share, iso, sector, year, subsector_L3, subsector_L2, subsector_L1, vehicle_type)]
    #V->F
    demand = merge(demand, FV_shares, all=TRUE, by = c("iso", "year", "subsector_L1", "vehicle_type"))
    demand = demand[,.(demand_F = demand_V*share, iso, sector, year, subsector_L3, subsector_L2, subsector_L1, vehicle_type, technology)]

    demandF_plot_pkm = copy(demand)

    ## Calculate demand in EJ
    ## merge the demand in pkm with the energy intensity
    demandF = merge(demand, MJ_km_base, all=FALSE, by = c("iso", "sector", "year", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology"))

    demandF[, demand_EJ:=demand_F # in Mpkm or Mtkm
            * 1e6 # in pkm or tkm
            * MJ_km # in MJ
            * 1e-12 # in EJ
            ]

    demandF_plot_EJ = copy(demandF)


    ## downscale to ISO level the pkm demand
    ## first I need to merge with a mapping that represents how the entries match to the CES
    demandF = merge(demandF, EDGE2CESmap, all=TRUE,
                  by = c("sector", "subsector_L3", "subsector_L2",
                         "subsector_L1", "vehicle_type", "technology"))

    ## calculate both shares and average energy intensity
    demandF = demandF[,.(iso, year, Value_demand = demand_EJ, demand_F, CES_node, sector)]

    demandF = demandF[,.(Value_demand = sum(Value_demand),
                       Value_intensity = sum(demand_F)/sum(Value_demand)), #in million pkm/EJ
                    by=c("iso","year","CES_node","sector")]

    ## from wide to long format
    demandF = melt(demandF, id.vars = c("iso","year","CES_node","sector"),
                   measure.vars = c("Value_demand", "Value_intensity"))

    ## get rid on NaNs energy intensity (they appear wherever demand is 0, so they are not useful)
    demandF = demandF[!is.nan(value),]

    ## calculate demand
    demand = demandF[variable == "Value_demand", .(iso, year, CES_node, value)]
    demand = approx_dt(demand, REMINDyears,
                     idxcols = c("iso", "CES_node"),
                     extrapolate=T)


    if (!is.null(REMIND2ISO_MAPPING)) {
      demand = aggregate_dt(demand,REMIND2ISO_MAPPING,
                          datacols = "CES_node",
                          valuecol = "value")
    }


    ## create parent node
    demand[, CES_parent:= sub("^[^_]*_", "",CES_node)]
    setcolorder(demand, neworder = c(regcol, "year", "CES_parent", "CES_node", "value"))

    ## calculate intensity
    demandI = demandF[variable == "Value_intensity", .(iso, year, CES_node, value)]
    demandI = approx_dt(demandI, REMINDyears,
                      idxcols = c("iso", "CES_node"),
                      extrapolate=T)

    if (!is.null(REMIND2ISO_MAPPING)) {
      gdp <- getRMNDGDP(scenario = scenario, usecache = T)

      demandI = aggregate_dt(demandI, REMIND2ISO_MAPPING,
                           datacols = "CES_node",
                           valuecol = "value",
                           weights = gdp)
    }



    demand_list = list(demand = demand,
                     demandI = demandI,
                     demandF_plot_pkm = demandF_plot_pkm,
                     demandF_plot_EJ = demandF_plot_EJ)

    return(demand_list)
}
