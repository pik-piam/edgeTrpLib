#' Applies learning to BEV purchase cost
#'
#' @param gdx input gdx file
#' @param REMINDmapping mapping of REMIND regions to ISO3 country codes
#' @param EDGE2teESmap mapping of EDGE-T/GCAM technologies to REMIND ES technologies
#' @param demand_BEVtmp the demand for BEVs from the previous iteration
#' @param ES_demandpr the ES demand
#'
#' @import data.table
#' @export

applylearning <- function(gdx,REMINDmapping,EDGE2teESmap, demand_BEVtmp, ES_demandpr){
  ## find the estimated number of cars
  demand = merge(ES_demand, ES_demandpr)
  demand = demand[, ratio := demand/demandpr][,-c("demand", "demandpr")] ## ratio from previous iteration of total demand
  demand = merge(demand, demand_BEVtmp, all.x =TRUE, by = c("iso", "year"))
  demand[, vehicles_number := vehicles_number*ratio]

  ## find the cumulative capacity and the reduction factor of costs
  demand = demand[,.(vehicles_number = sum(vehicles_number)), by = "year"]
  demand = demand[!is.nan(vehicles_number) & !is.na(vehicles_number)]
  demand[, cumul := cumsum(vehicles_number)]
  initialcap = min(demand$cumul[demand$cumul != 0 & !is.na(demand$cumul)])
  initialyear = demand[demand$cumul == initialcap,year]
  demand[, cumul := cumul/initialcap]
  demand[, cumul := ifelse(year>2100, cumul[year == 2100], cumul)]
  demand[, cumul := ifelse(year<initialyear, cumul[year == initialyear], cumul)]
  demand[, factor := cumul^-2.5]  ## according to https://about.bnef.com/blog/behind-scenes-take-lithium-ion-battery-prices/  LR ~ 18% -> b=ln(LR)/ln(2)~-2.5
                                  ## same for https://www.sciencedirect.com/science/article/pii/S0959652618337211 (LR = 23%)
  demand[, factor := ifelse(is.infinite(factor), 1, factor)] ## factor is 1 if there is no increase in cumulated demand (->no decrease in costs)

  ## only BEV car costs are affected
  nonfuel_costsBEV = nonfuel_costs[technology =="BEV" & subsector_L1 == "trn_pass_road_LDV_4W",]
  nonfuel_costsBEV[year >= 2020, non_fuel_price := non_fuel_price[year == 2020], by = c("iso", "vehicle_type")]
  nonfuel_costsBEV = merge(demand, nonfuel_costsBEV, all.x = TRUE, by = "year")
  ## powertrain represents ~20% of the total purchase price, which represents the ~80% of the non-fuel price
  batterycomponent = 0.2*0.8 ## average number 80% for purchase cost
  nonfuel_costsBEV[year >= 2020, non_fuel_price := ifelse(!is.na(factor),factor*batterycomponent*non_fuel_price+(1-batterycomponent)*non_fuel_price, non_fuel_price)]
  nonfuel_costsBEV[,c("factor", "cumul", "vehicles_number"):= NULL]
  nonfuel_costs = nonfuel_costs[!(technology=="BEV" & subsector_L1 =="trn_pass_road_LDV_4W"),]
  nonfuel_costs = rbind(nonfuel_costs, nonfuel_costsBEV)

  return(nonfuel_costs)
}


#' Calculate number of vehicles scaling up the normalized demand with the aggregate total demand
#'
#' @param norm_dem_BEV normalized demand shares for BEVs
#' @param ES_demand total demand for ESs
#'
#' @import data.table
#' @export


calc_num_vehicles_stations <- function(norm_dem, ES_demand_all){
  LDVdem = merge(norm_dem, ES_demand_all, by = c("iso", "year", "sector"))
  LDVdem[, demand_F := demand_F*demand] ## scale up the normalized demand

  LDVdem[, load_factor := 2]

  LDVdem[, annual_mileage := 15000]

  LDVdem[,vehicles_number:=demand_F   ## in trillionpkm
         /load_factor                  ## in trillionvkm
         /annual_mileage]             ## in trillion veh

  LDVdem = LDVdem[, .(iso, year, vehicles_number, technology, vehicle_type)]
  BEVdem = LDVdem[technology == "BEV",][, .(iso, year, vehicles_number, vehicle_type)]

  stations = LDVdem[, .(vehicles_number = sum(vehicles_number)), by = c("iso", "year", "technology")]
  stations = stations[year >= 2020 & technology %in% c("BEV", "NG", "FCEV")]
  stations[, statnum := vehicles_number*  ## in trillion veh
                        1e6/              ## in kveh
                        1000]             ## in stations

  stations[, fracst := statnum/sum(statnum), by = c("iso", "year")]

  stations = approx_dt(stations, seq(2020, 2101, 1),
                       xcol = "year", ycol = "fracst",
                       idxcols = c("iso", "technology"),
                       extrapolate=T)

  stations = stations[,.(iso, technology, year, fracst)]

  return(list(BEVdem = BEVdem, stations = stations))
}
