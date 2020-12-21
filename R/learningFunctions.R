#' Applies learning to BEV purchase cost
#'
#' @param gdx input gdx file
#' @param REMINDmapping mapping of REMIND regions to ISO3 country codes
#' @param EDGE2teESmap mapping of EDGE-T/GCAM technologies to REMIND ES technologies
#' @param ES_demandpr the ES demand of the previous iteration
#' @param non_fuel_costs total non fuel costs
#' @param capcost4W purchase prices of 4W on which learning is applied
#' @param demand_learntmp the demand for vehicles from the previous iteration
#' @param rebates_febatesBEV option rebates for BEVs
#' @param rebates_febatesFCEV option rebates for FCEVs
#' @param ES_demand the ES demand of the current iteration
#'
#' @import data.table
#' @export

applylearning <- function(non_fuel_costs, capcost4W, gdx, REMINDmapping, EDGE2teESmap, demand_learntmp, ES_demandpr, ES_demand, rebates_febatesBEV, rebates_febatesFCEV){
  `.` <- ratio <- demandpr <- vehicles_number <- cumul <- technology <- subsector_L1 <- non_fuel_price <- b <- initialyear <- NULL
  Capital_costs_purchase <- totalNE_cost <- price_component <- NULL
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
  capcost4W = capcost4W[technology %in% c("BEV", "FCEV") & subsector_L1 == "trn_pass_road_LDV_4W",]
  capcost4W = merge(capcost4W, unique(non_fuel_costs[subsector_L1 == "trn_pass_road_LDV_4W", c("iso", "vehicle_type")]), by = c("iso", "vehicle_type"), all.y = TRUE)
  ## substract purchase price to the non fuel price to obtain the non fuel price not affected by learning
  nonfuel_costslearn = rbind(non_fuel_costs[subsector_L1 == "trn_pass_road_LDV_4W" & technology %in% c("BEV", "FCEV") & year >= 2020][, price_component := "totalNE_cost"], capcost4W[year >= 2020])
  nonfuel_costslearn = dcast(nonfuel_costslearn, iso + year + technology + vehicle_type + subsector_L1 + subsector_L2 + subsector_L3 + sector + type ~ price_component, value.var = "non_fuel_price")
  nonfuel_costslearn[, non_fuel_price := totalNE_cost - Capital_costs_purchase]
  nonfuel_costslearn[, c("Capital_costs_purchase", "price_component", "totalNE_cost") := list(NULL, "remainingprice", NULL)]

  if (rebates_febatesBEV) {
    ## after 2035, the "original" price of 2020 (before the rebates) is used
    capcost4W[technology == "BEV", non_fuel_price := ifelse(year > 2035, non_fuel_price[year == 2020], non_fuel_price), by = c("iso", "vehicle_type", "technology")]
    capcost4W[technology == "FCEV" & year >= 2020, non_fuel_price := non_fuel_price[year == 2020], by = c("iso", "vehicle_type", "technology")]
  } else if (rebates_febatesFCEV){
    ## after 2035, the "original" price of 2020 (before the rebates) is used
    capcost4W[technology == "FCEV", non_fuel_price := ifelse(year > 2035, non_fuel_price[year == 2020], non_fuel_price), by = c("iso", "vehicle_type", "technology")]
    capcost4W[technology == "BEV" & year >= 2020, non_fuel_price := non_fuel_price[year == 2020], by = c("iso", "vehicle_type", "technology")]
  } else {
    ## in case of no rebates, the price of 2020 applies to all time steps
    capcost4W[year >= 2020, non_fuel_price := non_fuel_price[year == 2020], by = c("iso", "vehicle_type", "technology")]
  }
  capcost4W = merge(demand, capcost4W, all.x = TRUE, by = c("year", "technology"))
  ## powertrain represents ~20% of the total purchase price
  batterycomponent = 0.2 ##
  ## powertrain represents ??% of the average purchase price
  fuelcellcomponent = 0.4 ## average https://www.energy.gov/sites/prod/files/2014/03/f9/fcev_status_prospects_july2013.pdf
  capcost4W[year >= 2020 & technology == "BEV", non_fuel_price := ifelse(!is.na(factor), factor*batterycomponent*non_fuel_price + (1-batterycomponent)*non_fuel_price, non_fuel_price)]
  capcost4W[year >= 2020 & technology == "FCEV", non_fuel_price := ifelse(!is.na(factor), factor*fuelcellcomponent*non_fuel_price + (1-fuelcellcomponent)*non_fuel_price, non_fuel_price)]
  capcost4W = capcost4W[year >= 2020]
  capcost4W[,c("factor", "cumul", "vehicles_number", "initialcap", "initialyear"):= NULL]
  ## merge with other components of non fuel price and calculate total non fuel price
  nonfuel_costslearn = rbind(nonfuel_costslearn, capcost4W)
  nonfuel_costslearn = nonfuel_costslearn[,.(non_fuel_price = sum(non_fuel_price)), by = c("iso", "year", "type", "technology", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector")]

  ## technologies subjected to learning
  techlearn = c("BEV", "FCEV")
  ## integrate non fuel costs from the input data  with the updated values from learning
  nonfuel_costs = nonfuel_costs[!(technology %in% techlearn & subsector_L1 =="trn_pass_road_LDV_4W" & year >=2020),]
  nonfuel_costs = rbind(nonfuel_costs, nonfuel_costslearn[technology %in% techlearn])

  nonfuel_costs = list(capcost4W = capcost4W, nonfuel_costs = nonfuel_costs)
  return(nonfuel_costs)
}

#' Calculate number of vehicles scaling up the normalized demand with the aggregate total demand
#'
#' @param norm_dem normalized demand shares
#' @param ES_demand_all total demand for ESs
#' @param techswitch technology that the policymaker wants to promote
#' @param loadFactor load factor of vehicles
#' @import data.table
#' @export



calc_num_vehicles_stations <- function(norm_dem, ES_demand_all, techswitch, loadFactor){
  demand_F <- demand <- annual_mileage <- iso <- `.` <- vehicles_number <- vehicle_type <- demand_F <- technology <- statnum <- fracst <- NULL

  LDVdem = merge(norm_dem, ES_demand_all, by = c("iso", "year", "sector"))
  LDVdem[, demand_F := demand_F*demand] ## scale up the normalized demand
  LDVdem = merge(LDVdem, loadFactor, all.x = TRUE, by = c("iso", "year", "vehicle_type"))

  LDVdem[, annual_mileage := 15000]

  LDVdem[,vehicles_number:=demand_F   ## in trillionpkm
         /loadFactor                  ## in trillionvkm
         /annual_mileage]             ## in trillion veh

  LDVdem = LDVdem[, .(iso, year, vehicles_number, technology, vehicle_type)]

  alltechdem = LDVdem[technology %in% c("BEV", "FCEV", "Liquids", "Hybrid Liquids", "Hybrid Electric"),]
  alltechdem = alltechdem[,.(vehicles_number = sum(vehicles_number)), by = c("iso", "year")]

  learntechdem = LDVdem[technology %in% c("BEV", "FCEV"),][, .(iso, year, vehicles_number, vehicle_type, technology)]

  stations = LDVdem[, .(vehicles_number = sum(vehicles_number)), by = c("iso", "year", "technology")]
  stations = stations[year >= 2020]

 if (techswitch =="FCEV"){
   stations[technology == "FCEV" & year > 2025 &  year <= 2027,  statnum := 1.1*               ## policy over-reacts to FCEVs number and incentivize the construction of stations
                                              vehicles_number*  ## in trillion veh
                                              1e6/              ## in kveh
                                              1000]             ## in stations
   stations[technology == "FCEV" & year > 2027 &  year <= 2028,  statnum := 1.4*               ## policy over-reacts to FCEVs number and incentivize the construction of stations
                                              vehicles_number*  ## in trillion veh
                                              1e6/              ## in kveh
                                              1000]             ## in stations

   stations[technology == "FCEV" & year > 2028 &  year <= 2030,  statnum := 1.5*               ## policy over-reacts to FCEVs number and incentivize the construction of stations
                                              vehicles_number*  ## in trillion veh
                                              1e6/              ## in kveh
                                              1000]             ## in stations

   
   stations[technology == "FCEV" & year > 2030 &  year <= 2035,  statnum := 1.6*               ## policy over-reacts to FCEVs number and incentivize the construction of stations
                                              vehicles_number*  ## in trillion veh
                                              1e6/              ## in kveh
                                              1000]             ## in stations

   stations[technology == "FCEV" & year > 2035 &  year <= 2040,  statnum := 1.4*               ## policy over-reacts to FCEVs number and incentivize the construction of stations
                                              vehicles_number*  ## in trillion veh
                                              1e6/              ## in kveh
                                              1000]             ## in stations
   stations[technology == "FCEV" & year <= 2025, statnum := 1*vehicles_number*1e6/1000]
  
   stations[technology == "FCEV" & year > 2040,  statnum := 1*               ## policy over-reacts to FCEVs number and incentivize the construction of stations
                                              vehicles_number*  ## in trillion veh
                                              1e6/              ## in kveh
                                              1000]             ## in stations
   stations[technology != "FCEV",  statnum := vehicles_number*  ## in trillion veh
                                              1e6/              ## in kveh
                                              1000]             ## in stations


   stations[technology == "BEV", statnum := vehicles_number*0.01*1e6/1000]
 } 
 
 else if (techswitch ==  "Liquids") {
    ## industry and policymakers don't push BEVs in case the scenario is ConvCase
    stations[technology == "BEV",  statnum := 0.7*               ## BEV do not take over due to the dispreference
                                              vehicles_number*  ## in trillion veh
                                              1e6/              ## in kveh
                                              1000]             ## in stations

   stations[technology != "BEV",  statnum := vehicles_number*  ## in trillion veh
                                              1e6/              ## in kveh
                                              1000]             ## in stations
 }
 
 else {
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

  return(list(learntechdem = learntechdem, stations = stations, alltechdem = alltechdem))
}
