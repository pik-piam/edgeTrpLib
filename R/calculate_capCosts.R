#' Evaluate the costs of REMIND technology placeholders (e.g. te_eselt_pass_sm).
#'
#' @param base_price
#' @param Fdemand_ES
#' @param EDGE2CESmap
#' @param EDGE2teESmap
#' @param REMIND2ISO_MAPPING
#' @param REMINDyears
#' @import data.table
#' @importFrom rmndt toRegions_dt approx_dt
#' @export

calculate_capCosts <-function(base_price, Fdemand_ES,
                              EDGE2CESmap,
                              EDGE2teESmap,
                              REMIND2ISO_MAPPING,
                              REMINDyears){
  ## the non fuel price is to be calculated only for motorized entries
  Fdemand_ES=Fdemand_ES[!subsector_L3 %in% c("Walk","Cycle"),]
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
  
  non_fuel_price=toRegions_dt(non_fuel_price,REMIND2ISO_MAPPING,
                              datacol = "teEs",
                              valuecol = "value",
                              strategy = "gdp",
                              usecache = T)
  
  return(non_fuel_price)
}
