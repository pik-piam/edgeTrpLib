#' Evaluate the costs of REMIND technology placeholders (e.g. te_eselt_pass_sm).
#'
#' @param base_price base prices from logit calculations
#' @param Fdemand_ES normalized ES demand
#' @param EDGE2CESmap map from top level EDGE-T/GCAM categories to REMIND CES nodes
#' @param EDGE2teESmap mapping of EDGE-T/GCAM technologies to REMIND ES technologies
#' @param REMINDyears range of REMIND timesteps
#' @param scenario EDGE-T scenario name
#' @param REMIND2ISO_MAPPING mapping of REMIND regions to ISO3 country codes
#' @import data.table
#' @importFrom rmndt aggregate_dt approx_dt
#' @export

calculate_capCosts <-function(base_price, Fdemand_ES,
                              EDGE2CESmap,
                              EDGE2teESmap,
                              REMINDyears,
                              scenario,
                              REMIND2ISO_MAPPING=NULL){
  ## the non fuel price is to be calculated only for motorized entries
  Fdemand_ES=Fdemand_ES[!subsector_L3 %in% c("Walk","Cycle"),]
  base_price=base_price[!subsector_L3 %in% c("Walk","Cycle"),]
  ## merge prices and demand
  data=merge(base_price,Fdemand_ES,all.y=TRUE,by=intersect(names(base_price),names(Fdemand_ES)))
  ## load the mapping to CES nodes ->needed to attribute the CES node to data

  ## merge with mappings
  data=merge(data,EDGE2CESmap,all.x=TRUE,by=intersect(names(data),names(EDGE2CESmap)))
  data=merge(data,EDGE2teESmap,all=TRUE,by=intersect(names(data),names(EDGE2teESmap)))
  ## summarise and find the average prices
  data=data[,.(non_fuel_price=sum(non_fuel_price*demand_F/sum(demand_F))),by=c("iso","year","teEs")]

  non_fuel_price = melt(data, id.vars = c("iso", "year", "teEs"),
                            measure.vars = c("non_fuel_price"))

  setcolorder(non_fuel_price, c("iso","year","teEs","variable","value"))

  #rows with NaNs are deleted (meaning: no price, and no demand, in the specific region/year)
  non_fuel_price=non_fuel_price[!is.nan(value),]

  non_fuel_price = approx_dt(non_fuel_price, REMINDyears,
                                 idxcols = c("iso", "teEs", "variable"),
                                 extrapolate=T)

  non_fuel_price=non_fuel_price[variable=="non_fuel_price",]
  non_fuel_price[,variable:=NULL]
  non_fuel_price=non_fuel_price[order(iso,year,teEs)]

  if (!is.null(REMIND2ISO_MAPPING)) {
    ## aggregate to regions if needed
    gdp <- getRMNDGDP(scenario = scenario, usecache = T)
    non_fuel_price=aggregate_dt(non_fuel_price, REMIND2ISO_MAPPING,
                                datacols = "teEs",
                                valuecol = "value",
                                weights=gdp)
  }

  return(non_fuel_price)
}

