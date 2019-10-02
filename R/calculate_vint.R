#' calculates vintages composition, costs and intensity
#'
#'
#' @param shares
#' @param totdem_regr
#' @param costs


calcVint <- function(shares, totdem_regr, prices, mj_km_data, years){

  ## vintages prefer all equal time steps
  tall = seq(2010, 2150,5)
  ## last historical year
  baseyear = 2010

  ## rename column in each share data table
  S3S = shares$S3S_shares[,.(shareS3S = share, iso, year, subsector_L3, sector)]
  S2S3 = shares$S2S3_shares[,.(shareS2S3 = share, iso, year, subsector_L2, subsector_L3)]
  S1S2 = shares$S1S2_shares[,.(shareS1S2 = share, iso, year, subsector_L1, subsector_L2)]
  VS1 = shares$VS1_shares[,.(shareVS1 = share, iso, year, vehicle_type, subsector_L1)]
  FV = shares$FV_shares[,.(shareFV = share, iso, year, technology, vehicle_type, subsector_L1)]

  ## calculate the share of 4W on the total passenger demand
  shares_4W = merge(S3S, S2S3, by = c("iso", "year", "subsector_L3"))
  shares_4W[, share := shareS3S*shareS2S3]
  shares_4W = merge(shares_4W, S1S2, by = c("iso", "year", "subsector_L2"))
  shares_4W[, share := share*shareS1S2]
  shares_4W = shares_4W[subsector_L1 == "trn_pass_road_LDV_4W", c("iso", "year", "subsector_L1", "share")]

  ## find total demand LDV
  passdem = totdem_regr[sector == "trn_pass"]
  passdem = merge(passdem, shares_4W, by = c("iso", "year"))
  passdem[, totdem := demand*share][, c("demand", "share") :=NULL]
  setnames(passdem, old = "totdem", new = "value")                      ## rename col value otherwise approx_dt complains
  passdem = approx_dt(dt = passdem, xdata = tall,                       ## extrapolate to the whole time frame
                      idxcols = c("iso", "sector", "subsector_L1"),
                      extrapolate=T)
  setnames(passdem, old = "value", new = "totdem")                      ## rename back column

  ## depreciation
  paux = 15   ## approximate lifetime of a car
  Ddt = data.table(index_yearly = seq(1,140,1))
  Ddt[, D := 1-((index_yearly-0.5)/paux)^4]
  Ddt[, D := ifelse(D<0,0,D)]

  ## first time step has no depreciation
  Ddt = rbind(data.table(index_yearly = 0, D = 1), Ddt)

  ## five years time steps: only what happens every 5 years matters
  Ddt = Ddt[index_yearly %in% index_yearly[index_yearly %% 5 == 0]]   ## select index_yearly such that it is multiple of 5 years
  Ddt[, index := seq(0,length(index_yearly)-1, 1)]   ## index is on a 5 years basis
  Ddt[, index_yearly:= NULL]

  ## find historical capacity vintaging in time
  Cap_2010 = copy(passdem)
  setnames(Cap_2010, old = "totdem", new = "C_2010")
  ## apply the starting value to all time steps
  Cap_2010[, C_2010 := 1/3*C_2010[year == baseyear], by = c("iso", "subsector_L1", "sector")] ## approximate: 1/3 of the cars in 2010 are new enough to be vintages later. Others are phased out

  ## apply depreciation and clean up data
  Cap_2010 = merge(Cap_2010, Ddt[, year:= tall], by = "year")
  Cap_2010 = Cap_2010[, C_2010 := C_2010*D]
  Cap_2010 = Cap_2010[,c("iso", "sector", "subsector_L1", "year", "C_2010")]
  ## create vintages of base year capacity
  Vint = Cap_2010[year > baseyear]
  Vint[year == tall[tall>baseyear][1], vint := C_2010]
  ## create tmp structure that is used in the for loop
  ##NB this is NOT goingto work when there are trucks! they belong to a different sector!
  tmp = CJ(year = tall, iso = unique(Cap_2010$iso), subsector_L1 = unique(Cap_2010$subsector_L1), sector = unique(Cap_2010$sector))

  for (i in seq(1,length(tall)-2,1)) {
    ## time step that is considered in the current iteration
    y = tall[tall>baseyear][i]
    ## starting value of capacity built up in the current year
    Cval_t = merge(passdem[year == y, c("totdem", "iso", "subsector_L1", "sector")], Vint[year == y, c("vint", "iso", "subsector_L1", "sector")], by = c("iso", "subsector_L1", "sector"))
    Cval_t = Cval_t[,.(C_t = totdem-vint, iso, subsector_L1, sector)]
    Cap_t = merge(Cval_t, tmp, by = c("iso", "subsector_L1", "sector"))
    ## merge with depreciation (the 1st year is the current one, so there is no depreciation)
    Cap_t = merge(Cap_t, Ddt[index %in% c(seq(0,length(index)-i-1,1))][, year:= tall[tall>=y]], by = "year") ## smaller than length(index)-i-1
    ## depreciated capacity
    Cap_t = Cap_t[, C_t := C_t*D]
    ## only relevant columns
    Cap_t = Cap_t[,c("year", "C_t", "iso", "subsector_L1", "sector")]
    ## applied the year name to the value column
    setnames(Cap_t, old = "C_t", new = paste0("C_", y))
    ## add the vintaging capacity to the vintage dt
    Vint = merge(Vint, Cap_t[year > y], by = c("iso","subsector_L1", "year", "sector"), all = TRUE)
    ## find all column names of the capacities
    listCol <- colnames(Vint)[grep("C_", colnames(Vint))]
    ## sum up all depreciating capacity for the current time step
    Vint = Vint[year == tall[tall>baseyear][i+1], vint := Reduce(`+`, .SD), .SDcols=c(listCol), by = c("iso", "subsector_L1", "sector")][]
  }

  ## melt according to the columns of the "starting" year
  listCol <- colnames(Vint)[grep("C_", colnames(Vint))]
  Vint = melt(Vint, id.vars = c("year", "vint", "iso", "subsector_L1", "sector"), measure.vars = listCol)
  ## remove all columns with NAs (e.g. what is build in 2030, has NA vintage in 2020)
  Vint = Vint[!is.na(value)]
  Vint$variable <- factor(Vint$variable, levels = sort(listCol, decreasing =TRUE))

  ## composition of the vintages is inherited from the logit (depending on the base year): find the share of each tech-veh with respect to the starting total demand of passenger transport
  shares_tech = merge(shares_4W, VS1[subsector_L1 =="trn_pass_road_LDV_4W"], by = c("iso", "year", "subsector_L1"))
  shares_tech[, share := share*shareVS1]
  shares_tech = merge(shares_tech, FV[subsector_L1 =="trn_pass_road_LDV_4W"], by = c("iso", "year", "vehicle_type", "subsector_L1"), all.y =TRUE)
  shares_tech[, share := share*shareFV]
  setnames(shares_tech, old = "share", new = "value") ## rename otherwise approx_dt complains
  shares_tech = approx_dt(dt = shares_tech, xdata = tall,
                          idxcols = c("iso",  "subsector_L1", "vehicle_type", "technology"),
                          extrapolate=T)
  setnames(shares_tech, old = "value", new = "share")  ## rename back
  shares_tech = shares_tech[, .(iso, year, subsector_L1, vehicle_type, technology, share)]
  shares_tech[, variable := paste0("C_", year)] ## attribute to the column variable the year in wich the logit based value starts
  shares_tech = shares_tech[, year := NULL] ## the composition is interesting only concerning the starting year

  ## find vintages composition
  vintcomp_startyear = merge(shares_tech, Vint, by = c("iso", "variable", "subsector_L1"), allow.cartesian =TRUE)
  vintcomp_startyear[, value :=share*value]

  ## vintages represent a certain share of the 4W total demand
  sharevint = vintcomp_startyear[,.(vintdem = sum(value)), by = c("iso", "year", "subsector_L1")][, sector := "trn_pass"]
  sharevint= merge(passdem, sharevint, by = c("iso", "year", "subsector_L1", "sector"))
  sharevint[, sharevint := vintdem/totdem]


  ## I don't care anymore when something was built, so I aggregate by vintaging starting year, and then find the share of every technology within every vehicle type
  vintcomp = vintcomp_startyear[,.(value = sum(value)), by = c("iso", "year", "sector", "subsector_L1", "vehicle_type", "technology")]
  vintcomp[, sharetech:= value/sum(value),c("iso", "year", "subsector_L1", "sector", "vehicle_type") ]
  vintcomp = merge(vintcomp, sharevint, by = c("iso", "sector", "subsector_L1", "year"))
  vintcomp[, sharetech_vint := sharevint*sharetech] ## relative weight of the vintage structure is given by the shares multiplied times the share of vintages on total demand of 4W
  vintcomp = vintcomp[,.(sharetech_vint, totdem, iso, sector, subsector_L1, vehicle_type, technology, year)]

  ## composition of the new additions is given by the same shares used for the vintages composition, but not delayed in time
  newcomp = merge(FV, sharevint, by = c("iso", "subsector_L1", "year"))
  newcomp[, sharetech_new := (1-sharevint)*shareFV]

  ## find the average composition of the new additions+vintages
  FV_4W = merge(vintcomp, newcomp, by = c("iso", "subsector_L1", "year", "totdem", "vehicle_type", "technology", "sector"))
  FV_4W = FV_4W[,.(shareFV = sum(sharetech_vint, sharetech_new)), by = c("iso", "year", "subsector_L1", "vehicle_type", "technology")]

  ## updated values of FV_shares
  shares$FV_shares = merge(FV[(subsector_L1 != "trn_pass_road_LDV_4W")| (subsector_L1 == "trn_pass_road_LDV_4W" & year == 2010)],
                           FV_4W[year %in% years[years>baseyear]], by = names(FV_4W), all = TRUE)
  setnames(shares$FV_shares, old = "shareFV", new = "share")

  ## calculate the non fuel price
  price4W = prices$base[ subsector_L1 == "trn_pass_road_LDV_4W",]
  price4W[, variable := paste0("C_", year)] ## attribute to the column variable the year in wich the logit based value starts
  price4W_techtmp = copy(price4W) ## create a temporary copy, it is used not to delete the year column in the original dt
  price4W_techtmp = price4W_techtmp[, c("year", "EJ_Mpkm_final", "tot_VOT_price", "tot_price", "fuel_price") := NULL]

  ## calculate costs of the average vintage category
  vintcost = merge(price4W_techtmp,      ## costs of the technology depending on the original year
                   vintcomp_startyear,   ## vintages composition by starting year
                   by = c("iso", "variable", "sector", "subsector_L1", "technology", "vehicle_type"), allow.cartesian =TRUE)

  ## calculate relative share of each "starting year" with respect of the current year
  vintcost[, relative_share := value/sum(value), by = c("iso", "year","technology", "vehicle_type", "subsector_L1")]
  ## only entries that are "really" in the mix have to be averaged
  vintcost = vintcost[!is.nan(relative_share)]
  ## aggregate non fuel price of the vintages fleet
  vintcost = vintcost[,.(non_fuel_price_vint = sum(non_fuel_price*relative_share)), by = c("iso", "year","technology", "vehicle_type",
                                                                                           "subsector_L1", "subsector_L2", "subsector_L3", "sector", "sector_fuel")]
  ## tot cost of LDVs vehicle types and technologies is given by the average between the vintages and the new addtions
  totcost = merge(vintcost, price4W, by = c("iso", "year","technology", "vehicle_type",
                                            "subsector_L1", "subsector_L2", "subsector_L3", "sector", "sector_fuel"), all = TRUE)
  totcost = merge(totcost, sharevint, all = TRUE, by = c("iso", "year", "subsector_L1", "sector"))
  ## entries that have no vintages are for sake of simiplicity as expensive as new additions (to get the weighted average right)
  totcost[, non_fuel_price_vint := ifelse(is.na(non_fuel_price_vint), non_fuel_price, non_fuel_price_vint)]
  ## weighted average to find the total cost of the LDV fleet
  totcost = totcost[, totcost := sum(sharevint*non_fuel_price_vint +(1-sharevint)*non_fuel_price), by = c("iso", "year", "subsector_L1", "technology", "vehicle_type", "sector")]
  ## calculate new tot cost as sum of fuel cost and non fuel cost
  totcost[, tot_price := fuel_price_pkm + non_fuel_price]
  ## updated values of FV_shares
  prices$base = merge(prices$base[(subsector_L1 != "trn_pass_road_LDV_4W")| (subsector_L1 == "trn_pass_road_LDV_4W" & year %in% c(1990, 2005,2010))],
                totcost[year %in% years[years>baseyear], c("iso", "technology", "year", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector", "non_fuel_price", "tot_price", "fuel_price", "fuel_price_pkm", "EJ_Mpkm_final", "tot_VOT_price", "sector_fuel")], by = names(prices$base), all = TRUE)

  ## calculate the average intensity of the fleet
  mj_km_data4W = mj_km_data[ subsector_L1 == "trn_pass_road_LDV_4W",]
  mj_km_data4W[, variable := paste0("C_", year)] ## attribute to the column variable the year in wich the logit based value starts
  mj_km_data4W_techtmp = copy(mj_km_data4W) ## create a temporary copy, it is used not to delete the year column in the original dt
  mj_km_data4W_techtmp = mj_km_data4W_techtmp[, c("year") := NULL]

  ## calculate intensities of the average vintage category
  vintint = merge(mj_km_data4W_techtmp,      ## intensity of the technology depending on the original year
                   vintcomp_startyear,        ## vintages composition by starting year
                   by = c("iso", "variable", "sector", "subsector_L1", "technology", "vehicle_type"), allow.cartesian =TRUE)

  ## calculate relative share of each "starting year" with respect of the current year
  vintint[, relative_share := value/sum(value), by = c("iso", "year","technology", "vehicle_type", "subsector_L1")]
  ## only entries that are "really" in the mix have to be averaged
  vintint = vintint[!is.nan(relative_share)]
  ## aggregate non fuel price of the vintages fleet
  vintint = vintint[,.(MJ_km_vint = sum(MJ_km*relative_share)), by = c("iso", "year","technology", "vehicle_type",
                                                                       "subsector_L1", "subsector_L2", "subsector_L3", "sector", "sector_fuel")]
  ## tot intensity of LDVs vehicle types and technologies is given by the average between the vintages and the new addtions
  totint = merge(vintint, mj_km_data4W, by = c("iso", "year","technology", "vehicle_type",
                                            "subsector_L1", "subsector_L2", "subsector_L3", "sector", "sector_fuel"), all = TRUE)
  totint = merge(totint, sharevint, all = TRUE, by = c("iso", "year", "subsector_L1", "sector"))
  ## entries that have no vintages are for sake of simiplicity as intensive as new additions (to get the weighted average right)
  totint[, MJ_km_vint := ifelse(is.na(MJ_km_vint), MJ_km, MJ_km_vint)]
  ## entries that have no vintages are for sake of simiplicity with 0 vintage share (to get the weighted average right)
  totint[, sharevint := ifelse(year <=2005 & is.na(sharevint), 0, sharevint)]
  ## weighted average to find the total cost of the LDV fleet
  totint = totint[, totint := sum(sharevint*MJ_km_vint +(1-sharevint)*MJ_km), by = c("iso", "year", "subsector_L1", "technology", "vehicle_type", "sector")]
  ## updated values of FV_shares
  mj_km_data = merge(mj_km_data[(subsector_L1 != "trn_pass_road_LDV_4W")| (subsector_L1 == "trn_pass_road_LDV_4W" & year %in% c(1990, 2005,2010))],
                      totint[year %in% years[years>baseyear], c("iso", "technology", "year", "vehicle_type", "subsector_L1", "subsector_L2", "subsector_L3", "sector", "sector_fuel", "MJ_km")], by = names(mj_km_data), all = TRUE)

  return(list(prices = prices,
              shares = shares,
              mj_km_data = mj_km_data,
              vint_composition = vintcomp_startyear))

}
