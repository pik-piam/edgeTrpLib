#' Derive shares, demand and intensities from the logit tree.
#'
#' @param logit_shares logit tree level shares
#' @param MJ_km_base logit tree level intensities
#' @param EDGE2CESmap map top level EDGE-T categories to CES tree nodes
#' @param REMINDyears range of REMIND time steps
#' @param scenario REMIND GDP scenario
#' @param demand_input full REMIND CES level ES demand, optional. If not given, logit level demand output is normalized.
#' @import data.table
#' @importFrom rmndt aggregate_dt approx_dt
#' @export

shares_intensity_and_demand <- function(logit_shares,
                                        MJ_km_base,
                                        EDGE2CESmap,
                                        REMINDyears,
                                        scenario,
                                        demand_input=NULL){

    ## variable masks for code checking facility
    `.` <- share <- region <- sector <- subsector_L3 <- subsector_L2 <- subsector_L1 <- NULL
    demand_L2 <- demand_L1 <- demand_L3 <- vehicle_type <- demand_V <- demand_EJ <- demand_F <- NULL
    technology <- MJ_km <- demand_EJel <- demand_EJliq <- MJ_kmel <- MJ_kmliq <- NULL
    variable <- sector_fuel <- CES_node <- Value_demand <- value <- CES_parent <- NULL

    ## load the shares at each level
    S3S_shares <- logit_shares[["S3S_shares"]]
    S2S3_shares <- logit_shares[["S2S3_shares"]]
    S1S2_shares <- logit_shares[["S1S2_shares"]]
    VS1_shares <- logit_shares[["VS1_shares"]]
    FV_shares <- logit_shares[["FV_shares"]]
    ## create a normalized total demand OR loads absolute demand if given
    if (is.null(demand_input)) {
        demand=CJ(region=unique(S3S_shares$region),
                  sector=unique(S3S_shares$sector),
                  year=unique(S3S_shares$year))
        demand[,demand:=1]
    }else {
        demand=demand_input
    }

    ## calculate demand in million pkm for each level
    #S->S3
    demand = merge(demand, S3S_shares, all.y = TRUE, by = c("region", "year", "sector"))
    demand = demand[,.(demand_L3 = demand*share, region, year, sector, subsector_L3)]
    #S3->S2
    demand = merge(demand, S2S3_shares, all=TRUE, by = c("region", "year", "sector", "subsector_L3"))
    demand = demand[,.(demand_L2 = demand_L3*share, region, sector, year, subsector_L3, subsector_L2)]
    #S2->S1
    demand = merge(demand, S1S2_shares, all=TRUE, by = c("region", "year", "sector", "subsector_L3", "subsector_L2"))
    demand = demand[,.(demand_L1 = demand_L2*share, region, sector, year, subsector_L3, subsector_L2, subsector_L1)]
    #S1->V
    demand = merge(demand, VS1_shares, all=TRUE, by = c("region", "year", "subsector_L1"))
    demand = demand[,.(demand_V = demand_L1*share, region, sector, year, subsector_L3, subsector_L2, subsector_L1, vehicle_type)]
    #V->F
    demand = merge(demand, FV_shares, all=TRUE, by = c("region", "year", "subsector_L1", "vehicle_type"))
    demand = demand[,.(demand_F = demand_V*share, region, sector, year, subsector_L3, subsector_L2, subsector_L1, vehicle_type, technology)]

    ## we save the ES demand and throw away years > 2100 for they are NA
    demandF_plot_pkm = demand[
      year <= 2100,
      c("demand_F", "year","region", "sector", "subsector_L3", "subsector_L2","subsector_L1", "vehicle_type", "technology")]

    ## put aside the non motorized modes
    demandNM = demand[subsector_L3 %in% c("Cycle", "Walk")]

    ## Calculate demand in EJ
    ## merge the demand in pkm with the energy intensity
    demandF = merge(demand, MJ_km_base, all=FALSE, by = c("region", "sector", "year", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology"))

    demandF[, demand_EJ:=demand_F # in Mpkm or Mtkm
            * 1e6 # in pkm or tkm
            * MJ_km # in MJ
            * 1e-12 # in EJ
            ]

    ## plug in hybrids need to be redistributed on liquids and BEVs (on fuel consumtpion)
    demandFPIH = demandF[technology == "Hybrid Electric"]
    demandFPIH[, c("demand_EJel", "demand_EJliq") := list(0.4*demand_EJ, 0.6*demand_EJ)]
    demandFPIH[, c("MJ_kmel", "MJ_kmliq") := list((demand_EJel+demand_EJliq)*MJ_km/demand_EJel, (demand_EJel+demand_EJliq)*MJ_km/demand_EJliq)]
    demandFPIH[, c("demand_Fel", "demand_Fliq") := list(demand_EJel/(1e6*MJ_kmel*1e-12), demand_EJel/(1e6*MJ_kmliq*1e-12))]
    demandFPIH[, c("demand_EJ", "demand_F", "technology", "sector_fuel", "MJ_kmel", "MJ_kmliq") := NULL]
    demandFPIH = melt(demandFPIH, id.vars = c("region", "sector", "year", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "MJ_km"),
                      measure.vars = c("demand_Fel", "demand_Fliq", "demand_EJel", "demand_EJliq"))
    demandFPIH[, technology := ifelse(variable %in%  c("demand_Fel", "demand_EJel"), "BEV", "Liquids")]
    demandFPIHEJ = demandFPIH[variable %in%  c("demand_EJliq", "demand_EJel")]
    setnames(demandFPIHEJ, old = "value", new = "demand_EJ")
    demandFPIHEJ[, variable := NULL]
    demandFPIHF = demandFPIH[variable %in%  c("demand_Fliq", "demand_Fel")]
    setnames(demandFPIHF, old = "value", new = "demand_F")
    demandFPIHF[, variable := NULL]

    demandFPIH = merge(demandFPIHF, demandFPIHEJ, all = TRUE)
    demandFPIH[, sector_fuel := ifelse(technology =="BEV", "elect_td_trn", "refined liquids enduse")]
    demandF = rbind(demandFPIH[,MJ_km := NULL], demandF[technology != "Hybrid Electric"][,MJ_km := NULL])
    demandF = demandF[,.(demand_EJ = sum(demand_EJ), demand_F = sum(demand_F)),
            by = c("year","region", "sector", "subsector_L3", "subsector_L2","subsector_L1", "vehicle_type", "technology")]
    ## demandF_plot_pkm = rbind(demandNM, demandF[,c("demand_F", "year","region", "sector", "subsector_L3", "subsector_L2","subsector_L1", "vehicle_type", "technology")])
    demandF_plot_EJ = copy(demandF)

    ## first I need to merge with a mapping that represents how the entries match to the CES
    demandF = merge(demandF, EDGE2CESmap, all=TRUE,
                  by = c("sector", "subsector_L3", "subsector_L2",
                         "subsector_L1", "vehicle_type", "technology"))

    ## calculate both shares and average energy intensity
    demandF = demandF[,.(region, year, Value_demand = demand_EJ, demand_F, CES_node, sector)]

    demandF = demandF[,.(Value_demand = sum(Value_demand),
                       Value_intensity = sum(demand_F)/sum(Value_demand)), #in million pkm/EJ
                    by=c("region","year","CES_node","sector")]

    ## from wide to long format
    demandF = melt(demandF, id.vars = c("region","year","CES_node","sector"),
                   measure.vars = c("Value_demand", "Value_intensity"))

    ## get rid on NaNs energy intensity (they appear wherever demand is 0, so they are not useful)
    demandF = demandF[!is.nan(value),]

    ## calculate demand
    demand = demandF[variable == "Value_demand", .(region, year, CES_node, value)]
    demand = approx_dt(demand, REMINDyears,
                     xcol = "year", ycol = "value",
                     idxcols = c("region", "CES_node"),
                     extrapolate=T)


    ## create parent node
    demand[, CES_parent:= sub("^[^_]*_", "",CES_node)]
    setcolorder(demand, neworder = c("region", "year", "CES_parent", "CES_node", "value"))

    ## calculate intensity
    demandI = demandF[variable == "Value_intensity", .(region, year, CES_node, value)]
    demandI = approx_dt(demandI, REMINDyears,
                      xcol = "year", ycol = "value",
                      idxcols = c("region", "CES_node"),
                      extrapolate=T)

    demand_list = list(demand = demand,
                     demandI = demandI,
                     demandF_plot_pkm = demandF_plot_pkm,
                     demandF_plot_EJ = demandF_plot_EJ)

    return(demand_list)
}
