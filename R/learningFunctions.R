#' Applies learning to BEV purchase cost
#'
#' @param gdx input gdx file
#' @param REMINDmapping mapping of REMIND regions to ISO3 country codes
#' @param EDGE2teESmap mapping of EDGE-T/GCAM technologies to REMIND ES technologies
#' @param ES_demandpr the ES demand of the previous iteration
#' @param non_fuel_costs non fuel costs on which learning is applied
#' @param demand_learntmp the demand for vehicles from the previous iteration
#' @param rebates_febatesBEV option rebates for BEVs
#' @param rebates_febatesFCEV option rebates for FCEVs
#' @param ES_demand the ES demand of the current iteration
#'
#' @import data.table
#' @export

applylearning <- function(non_fuel_costs, gdx,REMINDmapping,EDGE2teESmap, demand_learntmp, ES_demandpr, ES_demand, rebates_febatesBEV, rebates_febatesFCEV){

  `.` <- ratio <- demandpr <- vehicles_number <- cumul <- technology <- subsector_L1 <- non_fuel_price <- b <- initialyear <- NULL

  ## find the estimated number of cars
  demand = merge(ES_demand, ES_demandpr)
  demand = demand[, ratio := demand/demandpr][,-c("demand", "demandpr")] ## ratio from previous iteration of total demand
  demand = merge(demand, demand_learntmp, all.x =TRUE, by = c("iso", "year"))
  demand[, vehicles_number := vehicles_number*ratio]

  ## find the cumulative capacity and the reduction factor of costs
  demand = demand[,.(vehicles_number = sum(vehicles_number)), by = c("technology", "year")]
  demand = demand[!is.nan(vehicles_number) & !is.na(vehicles_number)]
  demand[, cumul := cumsum(vehicles_number), by = "technology"]
  initialcap = demand[cumul!=0 & !is.na(demand$cumul),]
  initialcap = initialcap[ , .SD[which.min(cumul)], by = "technology"]
  initialcap = initialcap[,.(technology, initialcap = cumul, initialyear = year)]
  demand = merge(demand, initialcap, by = c("technology"))
  demand[, cumul := cumul/initialcap, by = "technology"]
  demand[, cumul := ifelse(year>2100, cumul[year == 2100], cumul), by = "technology"]
  demand[, cumul := ifelse(year<=initialyear, cumul[year == initialyear], cumul), by = "technology"]

  exponent = data.table(b = c(-2.5, -2.5), technology =  c("BEV", "FCEV")) ## according to https://about.bnef.com/blog/behind-scenes-take-lithium-ion-battery-prices/  LR ~ 18% -> b=ln(LR)/ln(2)~-2.5
                  ## same for https://www.sciencedirect.com/science/article/pii/S0959652618337211 (LR = 23%)
  ## for FCEVs: https://www.sciencedirect.com/science/article/abs/pii/S0360544218302998

  demand = merge(exponent, demand, by = "technology")
  demand[, factor := cumul^b]
  demand[, b := NULL]
  demand[, factor := ifelse(is.infinite(factor), 1, factor)] ## factor is 1 if there is no increase in cumulated demand (->no decrease in costs)

  ## only BEV car costs are affected
  nonfuel_costslearn = nonfuel_costs[technology %in% c("BEV", "FCEV") & subsector_L1 == "trn_pass_road_LDV_4W",]


  if (rebates_febatesBEV) {
    ## after 2035, the "original" price of 2020 (before the rebates) is used
    nonfuel_costslearn[technology == "BEV", non_fuel_price := ifelse(year > 2035, non_fuel_price[year == 2020], non_fuel_price), by = c("iso", "vehicle_type", "technology")]
    nonfuel_costslearn[technology == "FCEV" & year >= 2020, non_fuel_price := non_fuel_price[year == 2020], by = c("iso", "vehicle_type", "technology")]
  } else if (rebates_febatesFCEV){
    ## after 2035, the "original" price of 2020 (before the rebates) is used
    nonfuel_costslearn[technology == "FCEV", non_fuel_price := ifelse(year > 2035, non_fuel_price[year == 2020], non_fuel_price), by = c("iso", "vehicle_type", "technology")]
    nonfuel_costslearn[technology == "BEV" & year >= 2020, non_fuel_price := non_fuel_price[year == 2020], by = c("iso", "vehicle_type", "technology")]
  } else {
    ## in case of no rebates, the price of 2020 applies to all time steps
    nonfuel_costslearn[year >= 2020, non_fuel_price := non_fuel_price[year == 2020], by = c("iso", "vehicle_type", "technology")]
  }

  nonfuel_costslearn = merge(demand, nonfuel_costslearn, all.x = TRUE, by = c("year", "technology"))
  ## powertrain represents ~20% of the total purchase price, which represents the ~80% of the non-fuel price
  batterycomponent = 0.2*0.8 ## average number 80% for purchase cost
  ## powertrain represents ??% of the average purchase price
  fuelcellcomponent = 0.4 ## average https://www.energy.gov/sites/prod/files/2014/03/f9/fcev_status_prospects_july2013.pdf
  nonfuel_costslearn[year >= 2020 & technology == "BEV", non_fuel_price := ifelse(!is.na(factor),factor*batterycomponent*non_fuel_price+(1-batterycomponent)*non_fuel_price, non_fuel_price)]
  nonfuel_costslearn[year >= 2020 & technology == "FCEV", non_fuel_price := ifelse(!is.na(factor),factor*fuelcellcomponent*non_fuel_price+(1-fuelcellcomponent)*non_fuel_price, non_fuel_price)]
  nonfuel_costslearn = nonfuel_costslearn[year >= 2020]
  nonfuel_costslearn[,c("factor", "cumul", "vehicles_number", "initialcap", "initialyear"):= NULL]

  ## technologies subjected to learning
  techlearn = c("BEV", "FCEV")
  ## integrate non fuel costs from the input data  with the updated values from learning
  nonfuel_costs = nonfuel_costs[!(technology %in% techlearn & subsector_L1 =="trn_pass_road_LDV_4W" & year >=2020),]
  nonfuel_costs = rbind(nonfuel_costs, nonfuel_costslearn[technology %in% techlearn])

  return(nonfuel_costs)
}

#' Calculate number of vehicles scaling up the normalized demand with the aggregate total demand
#'
#' @param norm_dem normalized demand shares
#' @param ES_demand_all total demand for ESs
#' @param techswitch technology that the policymaker wants to promote
#' @import data.table
#' @export



calc_num_vehicles_stations <- function(norm_dem, ES_demand_all, techswitch){
  demand_F <- demand <- load_factor <- annual_mileage <- iso <- `.` <- vehicles_number <- vehicle_type <- demand_F <- technology <- statnum <- fracst <- NULL

  LDVdem = merge(norm_dem, ES_demand_all, by = c("iso", "year", "sector"))
  LDVdem[, demand_F := demand_F*demand] ## scale up the normalized demand

  LDVdem[, load_factor := 2]

  LDVdem[, annual_mileage := 15000]

  LDVdem[,vehicles_number:=demand_F   ## in trillionpkm
         /load_factor                  ## in trillionvkm
         /annual_mileage]             ## in trillion veh

  LDVdem = LDVdem[, .(iso, year, vehicles_number, technology, vehicle_type)]
  learntechdem = LDVdem[technology %in% c("BEV", "FCEV"),][, .(iso, year, vehicles_number, vehicle_type, technology)]

  stations = LDVdem[, .(vehicles_number = sum(vehicles_number)), by = c("iso", "year", "technology")]
  stations = stations[year >= 2020]

 if (techswitch =="FCEV"){
   stations[technology == "FCEV",  statnum := 10*               ## policy over-reacts to FCEVs number and incentivize the construction of stations
                                              vehicles_number*  ## in trillion veh
                                              1e6/              ## in kveh
                                              1000]             ## in stations

   stations[technology != "FCEV",  statnum := vehicles_number*  ## in trillion veh
                                              1e6/              ## in kveh
                                              1000]             ## in stations
 } else {
    stations[,  statnum := vehicles_number*  ## in trillion veh
                        1e6/              ## in kveh
                        1000]             ## in stations
 }

  stations[, fracst := statnum/sum(statnum), by = c("iso", "year")]
  stations = stations[technology %in% c("BEV", "NG", "FCEV"),]

  stations = approx_dt(stations, seq(2020, 2101, 1),
                       xcol = "year", ycol = "fracst",
                       idxcols = c("iso", "technology"),
                       extrapolate=T)

  stations = stations[,.(iso, technology, year, fracst)]

  return(list(learntechdem = learntechdem, stations = stations))
}
