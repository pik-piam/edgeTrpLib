#' Reporting for the coupled EDGE-T Transport Sector Model (REMIND Module edge_esm)
#'
#' Data is loaded from the EDGE-T subfolder in the output folder.
#' The input files can be (re-) generated calling
#' `Rscript EDGETransport.R --reporting`
#' from the output folder.
#'
#' *Warning* The function modifies the "REMIND_generic_<scenario>.mif" file by appending the
#' additional reporting variables and replaces the "_withoutPlus" version.
#'
#' Region subsets are obtained from fulldata.gdx
#'
#' @param output_folder path to the output folder, default is current folder.
#' @param sub_folder subfolder with EDGE-T output files (level_2 for standalone, EDGE-T for coupled runs)
#' @param loadmif shall we try to load a REMIND MIF file from the output folder to append the variables?
#' @param extendedReporting report a larger set of variables
#' @param scenario_title a scenario title string
#' @param model_name a model name string
#' @param name_mif the name of the MIF file to store the variables to. Not compatible with loadmif.
#' @param gdx path to the GDX file used for the run.
#' @author Alois Dirnaichner Marianna Rottoli
#'
#' @importFrom rmndt approx_dt readMIF writeMIF
#' @importFrom gdxdt readgdx
#' @importFrom data.table fread fwrite rbindlist copy CJ
#' @importFrom remind toolRegionSubsets
#' @export

reportEDGETransport <- function(output_folder=".", sub_folder = "EDGE-T/",
                                loadmif = TRUE , extendedReporting=FALSE,
                                scenario_title=NULL, model_name="EDGE-Transport",
                                name_mif=NULL, gdx=NULL) {
  

  ## NULL Definitons for codeCheck compliance
  RegionCode <- CountryCode <- `.` <- sector <- subsector_L3 <- region <- year <- NULL
  subsector_L2 <- subsector_L1 <- aggr_mode <- vehicle_type <- det_veh <- aggr_nonmot <- NULL
  demand_F <- demand_EJ <- remind_rep <- V25 <- aggr_veh <- technology <- NULL
  ttot <- se_share <- fe_demand <- variable <- value <- demand_VKM <- loadFactor <- NULL
  all_enty <- ef <- variable_agg <- model <- scenario <- period <- NULL
  Region <- Variable <- co2 <- co2val <- elh2 <- fe  <- NULL
  int <- se <- sec  <- sharesec <- te  <- tech <-  val <- share <- NULL
  eff <- sharebio <- sharesyn <- totseliq <- type <- ven <- NULL
  unit <- NULL 

  
  yrs <- c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)
  
  datapath <- function(fname){
    file.path(output_folder, sub_folder, fname)}
  
  reporting <- function(datatable, mode){
    
    report <- list()
    

    datatable[, sector := ifelse(sector %in% c("trn_pass", "trn_aviation_intl"), "Pass", "Freight")]
    datatable <- merge(datatable,Aggrdata,by=c("sector","subsector_L1","subsector_L2","subsector_L3","vehicle_type","technology"), all.x=TRUE, allow.cartesian = TRUE)
    
    #How to account for Hybrid Electric in Final Energy?
    if (mode == "FE") {
      techmap <- data.table(
        technology = c("BEV","Electric","Hybrid Electric","FCEV", "Hydrogen","Liquids","NG"),
        remind_rep = c("Electricity","Electricity","Liquids","Hydrogen","Hydrogen","Liquids","Gases"))
    } else {
      techmap <- data.table(
        technology = c("BEV","Electric","Hybrid Electric","FCEV", "Hydrogen","Liquids","NG"),
        remind_rep = c("BEV","Electric","Hybrid Electric","FCEV","Hydrogen","Liquids","Gases"))
    }
    
    datatable <- merge(datatable,techmap,by=c("technology"), all.x=TRUE)
    
    datatable[!is.na(aggr_mode) & !is.na(remind_rep), aggr_mode_tech := paste0(aggr_mode, "|", remind_rep)]
    datatable[!is.na(aggr_veh) & !is.na(remind_rep), aggr_veh_tech := paste0(aggr_veh, "|", remind_rep)]
    datatable[!is.na(det_veh) & !is.na(remind_rep), det_veh_tech := paste0(det_veh, "|", remind_rep)]
    
    
    unit <- switch(mode,
                   "FE" = "EJ/yr",
                   "ES" = "bn pkm/year",
                   "VKM" = "bn vkm/year")
    
    prefix <- switch(mode,
                     "FE" = "FE|Transport|",
                     "ES" = "ES|Transport|",
                     "VKM" = "ES|Transport|VKM|")
    
    var <- c("Pass","Freight")
    
    Aggr <- c("aggr_mode","aggr_veh","det_veh","nonmot","aggr_nonmot","aggr_mode_tech","aggr_veh_tech", "det_veh_tech")
    
    
    for (var0 in var) {
      
      for (Aggr0 in Aggr) {
        
        
        #Aggregate data
        datatable0 <- copy(datatable)
        datatable0 <- datatable0[!is.na(get(Aggr0))]
        
        
        datatable0 <- datatable0[sector == var0, .(value=sum(value, na.rm=T)),
                                 by = c("region", "year", Aggr0)]
        if (nrow(datatable0>0)) {
          
          setnames(datatable0, "year", "period")
          
          datatable0 <- datatable0[,model:=model_name][,scenario:=scenario_title][,variable:=paste0(prefix, get(Aggr0))][,unit:=unit][,eval(Aggr0):=NULL]
          
          datatable0 <- approx_dt(datatable0, yrs, xcol="period", ycol = "value",
                                  idxcols=c("scenario","variable","unit","model","region"),
                                  extrapolate=T)
          
          report <- rbind(report, datatable0)}
        
      }
    }
    if (mode == "ES") {
      report[sector=="Freight", unit:="bn tkm/yr"]
    }
    
    
    return(report)
  }
  
  
  
  ## Demand emissions
  reportingEmi <- function(repFE, gdx){

    ## load emission factors for fossil fuels
    p_ef_dem <- readgdx(gdx, "p_ef_dem")[all_enty %in% c("fepet", "fedie", "fegas")]  ## MtCO2/EJ
    p_ef_dem[all_enty == "fegas", all_enty := "fegat"]
    setnames(p_ef_dem, old = c("value", "all_regi"), new = c("ef", "region"))
    ## attribute explicitly fuel used to the FE values
    emidem = repFE[grepl("Liquids|Gases|Hydrogen|Electricity", variable) & region != "World"]   ## EJ
    emidem[, all_enty := ifelse(grepl("Liquids", variable), "fedie", NA)]
    emidem[, all_enty := ifelse(grepl("LDV.+Liquids", variable), "fepet", all_enty)]
    emidem[, all_enty := ifelse(grepl("Gases", variable), "fegat", all_enty)]
    emidem[, all_enty := ifelse(grepl("Electricity", variable), "feelt", all_enty)]
    emidem[, all_enty := ifelse(grepl("Hydrogen", variable), "feh2t", all_enty)]
    ## merge with emission factors
    emidem = emidem[p_ef_dem, on = c("all_enty","region")]
    ## calculate emissions and attribute variable and unit names
    emidem[, value := value*ef][, c("variable", "unit") := list(gsub("FE", "Emi\\|CO2", variable), "Mt CO2/yr")]
    
    emi = rbind(copy(emidem)[, c("type", "variable") := list("tailpipe", paste0(variable, "|Tailpipe"))],
                copy(emidem)[, c("type", "variable") := list("demand", paste0(variable, "|Demand"))])
    
    prodFe <- readgdx(gdx, "vm_prodFE")[, ttot := as.numeric(ttot)]
    setnames(prodFe,
             c("period", "region", "se", "all_enty", "te", "fe_demand"))
    prodFe[, se_share := fe_demand/sum(fe_demand), by=c("period", "region", "all_enty")]
    prodFe <- prodFe[all_enty %in% c("fedie", "fepet", "fegat") & se %in% c("segafos", "seliqfos")][, c("se", "te", "fe_demand") := NULL]
    
    emi <- prodFe[emi, on=c("period", "region", "all_enty")]
    ## in case no fossil fuels are used (e.g. 100% biodiesel), the value in se_share results NA. set the NA value to 0
    emi[is.na(se_share), se_share := 0]
    emi <- emi[all_enty %in% c("fedie", "fepet", "fegat") & type == "demand", value := value*se_share]
    
    emi[, c("se_share", "type", "ef", "all_enty") := NULL]
    
    ## aggregate removing the fuel dependency
    emi[, variable_agg := gsub("\\|Liquids|\\|Electricity|\\|Hydrogen|\\|Gases", "", variable)]
    emi = emi[, .(value = sum(value)), by = c("model", "scenario", "region", "unit", "period", "variable_agg")]
    setnames(emi, old = "variable_agg", new = "variable")
    emi = emi[, .(model, scenario, region, variable, unit, period, value)]
    
    return(emi)
  }
  
  reportingVehNum <- function(demand_vkm, annual_mileage){

    venum <- copy(demand_vkm)
    ## merge annual mileage
    anmil <- copy(annual_mileage)
    anmil[grepl("Subcompact", vehicle_type),
          variable := "Pass|Road|LDV|Small"]
    anmil[grepl("Mini", vehicle_type),
          variable := "Pass|Road|LDV|Mini"]
    anmil[vehicle_type == "Compact Car", variable := "Pass|Road|LDV|Medium"]
    anmil[grepl("Large Car|Midsize Car", vehicle_type), variable := "Pass|Road|LDV|Large"]
    anmil[grepl("SUV", vehicle_type),
          variable := "Pass|Road|LDV|SUV"]
    anmil[grepl("Van|Multipurpose", vehicle_type),
          variable := "Pass|Road|LDV|Van"]
    anmil[grepl("Motorcycle|Scooter|Moped", vehicle_type),
          variable := "Pass|Road|LDV|Two-Wheelers"]
    anmil[grepl("^Truck", vehicle_type),
          variable := sprintf("Freight|Road|%s", vehicle_type)]
    anmil[grepl("Bus", vehicle_type),
          variable := "Pass|Road|Bus"]
    
    anmil <- anmil[,.(region, period = year, variable, annual_mileage)]
    
    anmil <- approx_dt(anmil, unique(demand_vkm$period), xcol = "period", ycol = "annual_mileage", idxcols = c("region", "variable"), extrapolate = T)
    anmil<- unique(anmil[,c("period", "region", "variable", "annual_mileage")])
    anmil <- anmil[, variable := paste0("ES|Transport|VKM|", variable)]
    venum <- merge(demand_vkm, anmil, by = c("variable", "region", "period"))
    venum[, ven := value/annual_mileage] # billion vehicle-km -> thousand vehicles
    
    venum <- venum[!is.na(ven)]
    venum[, variable := gsub("|VKM", "|VNUM", variable, fixed=TRUE)][, c("value", "annual_mileage") := NULL]
    venum[, unit := "tsd veh"]
    setnames(venum, "ven", "value")
    venum = venum[,.(model, scenario, region, variable, unit, period, value)]
    return(venum)
  }
  
  reportStockAndSales <- function(annual_mileage){
    if(file.exists(file.path(output_folder, "vintcomp.csv"))){
      vintages_file <- file.path(output_folder, "vintcomp.csv")
      vintgs <- fread(vintages_file)
    } else if (file.exists(datapath(fname = "vintcomp.RDS"))){
      #vintages_file <- datapath(fname = "vintcomp.RDS")
      #vintgs <- readRDS(vintages_file)
      return(NULL)
    } else {
      print("EDGE-T Reporting: No vintages file found.")
      return(NULL)
    }

    year_c <- construction_year <- Stock <- Sales <- vintage_demand_vkm <- fct <- category <- NULL
    
    
    ## backward compat. fix
    fct <- 1.
    if("variable" %in% colnames(vintgs)){
      fct <- 1e-6
      setnames(vintgs, "variable", "construction_year")
    }
    
    vintgs[, year_c := as.numeric(gsub("C_", "", construction_year))]
    
    ## stock is the full stock up to the end of the current year
    ## sales are the sales of the current year
    
    setnames(vintgs, "full_demand_vkm", "Stock")
    vintgs[, Stock := Stock * fct]
    vintgs[, Sales := Stock - sum(vintage_demand_vkm), by=.(year, region, vehicle_type, technology)]
    vintgs[, c("construction_year", "vintage_demand_vkm", "year_c") := NULL]
    vintgs <- unique(vintgs)
    
    vintgs <- data.table::melt(vintgs, measure.vars = c("Stock", "Sales"), variable.name = "category")
    ## vkm -> v-num
    vintgs = merge(vintgs, annual_mileage, by = c("year", "region", "vehicle_type"))
    vintgs[, value := value / annual_mileage]
    vintgs[, variable := ifelse(
      vehicle_type == "Bus_tmp_vehicletype",
      sprintf("%s|Transport|Bus|%s", category, technology),
      sprintf("%s|Transport|LDV|%s|%s", category, vehicle_type, technology))]
    
    ## totals
    vintgs <- rbindlist(list(
      vintgs,
      vintgs[, .(value=sum(value), variable=gsub("(.+)\\|.+$", "\\1", variable)),
             by=c("category", "year", "region", "vehicle_type")],
      vintgs[grepl("|LDV|", variable, fixed=TRUE),
             .(value=sum(value), variable=sprintf("%s|Transport|LDV", category)),
             by=c("category", "year", "region")]), fill=TRUE)
    
    vintgs[, c("vehicle_type", "technology", "annual_mileage", "category") := NULL]
    vintgs <- unique(vintgs[!is.na(value)])
    
    setnames(vintgs, "year", "period")
    
    vintgs = approx_dt(vintgs, c(2005, 2010, unique(vintgs$period), 2110, 2130, 2150),
                       xcol = "period", ycol = "value", idxcols = c("region", "variable"), extrapolate = T)
    vintgs[period <= 2010|period > 2100, value := 0]
    
    ## remove the variable (e.g. vehicle_types) that are not present for this specific region
    vintgs[, `:=`(model=model_name, scenario=scenario_title, unit="Million vehicles")]
    
    return(vintgs)
    
  }
  
  reportTotals <- function(datatable, Aggrvar,vars){
    
    if (length(unique(datatable[variable %in% vars]$variable)) < length(vars)){
     print(paste0("Missing variables to aggregate data to ",Aggrvar))}
    
    datatable <- datatable[variable %in% vars,
                           .(variable=Aggrvar,
                             value=sum(value)),
                           by=c("model", "scenario", "region", "period","unit")]
    
    return(datatable)
  } 
  
  ## check the regional aggregation
  regionSubsetList <- toolRegionSubsets(gdx) 
  
  # ADD EU-27 region aggregation if possible
  if("EUR" %in% names(regionSubsetList)){
    regionSubsetList <- c(regionSubsetList,list(
      "EU27"=c("ENC","EWN","ECS","ESC","ECE","FRA","DEU","ESW")
    ))
  }


  #load Mapping for Aggregation
  ## Aggrdata <- read.csv(system.file("extdata", "EDGETdataAggregation.csv", package = "remind2"),header = TRUE, sep = ",") 
  Aggrdata <- read.csv("~/git/edgeTrpLib/inst/extdata/EDGETdataAggregation.csv", header = TRUE, sep = ",") 

  ## load input data from last EDGE run
  ## Data manipulation shouldnt be necessary
  demand_km <- readRDS(datapath(fname = "demandF_plot_pkm.RDS"))
  demand_km[, demand_F := demand_F * 1e-3] ## million -> billion pkm
  setnames(demand_km,"demand_F","value")
  demand_ej <- readRDS(datapath(fname = "demandF_plot_EJ.RDS")) ## detailed final energy demand, EJ
  setnames(demand_ej,"demand_EJ","value")
  demand_ej[,demand_F:=NULL]
  load_factor <- readRDS(datapath(fname = "loadFactor.RDS"))
  annual_mileage <- readRDS(datapath(fname = "annual_mileage.RDS"))
  if (length(annual_mileage)> 4){
    #Same is done in lvl2_createoutput
    annual_mileage <- unique(annual_mileage[,c("region", "year", "vkm.veh", "vehicle_type")])
    setnames(annual_mileage, old = "vkm.veh", new = "annual_mileage")
  }
  if (length(load_factor)> 4){
   load_factor <- load_factor[,c("year","region","vehicle_type","loadFactor","technology")]
  demand_vkm <- merge(demand_km, load_factor, by=c("year", "region", "vehicle_type","technology"))
  demand_vkm[, value := value/loadFactor] ## billion vkm
  } else {
  demand_vkm <- merge(demand_km, load_factor, by=c("year", "region", "vehicle_type"))
  demand_vkm[, value := value/loadFactor]} ## billion vkm


  if (loadmif == TRUE){
  name_mif = list.files(output_folder, pattern = "REMIND_generic", full.names = F)
  name_mif = file.path(output_folder, name_mif[!grepl("withoutPlu", name_mif)])

  stopifnot(typeof(name_mif) == "character")
  miffile <- readMIF(name_mif)}
 
  repFE <- reporting(
    demand_ej,
    mode ="FE")
  repVKM <- reporting(
    datatable=demand_vkm,
    mode="VKM")
  repES <- reporting(
    datatable=demand_km,
    mode="ES")
  toMIF <- rbind(
    repFE,
    repVKM,
    repES,
    reportingVehNum(repVKM,
                    annual_mileage),
    reportingEmi(repFE = repFE,
                 gdx = gdx)
  )
   

  aggrmode <- c("ES|Transport|Pass|Road",
                "ES|Transport|Pass|Rail",
              "ES|Transport|VKM|Pass|Road",
              "ES|Transport|VKM||Road",
              "ES|Transport|VKM|Rail",
              "FE|Transport|Pass|Road",
              "FE|Transport|Road",
              "FE|Transport|Pass|Rail",
              "FE|Transport|Rail",            
              "Emi|CO2|Transport|Pass|Road|Tailpipe",
              "Emi|CO2|Transport|Pass|Road|Demand",
              "Emi|CO2|Transport|Road|Tailpipe",
              "Emi|CO2|Transport|Rail|Tailpipe",
              "Emi|CO2|Transport|Road|Demand",
              "Emi|CO2|Transport|Rail|Demand"
              )
  
  vars <-list(c("ES|Transport|Pass|Road|LDV","ES|Transport|Pass|Road|Bus","ES|Transport|Pass|Road|Non-Motorized"),
              c("ES|Transport|Pass|Rail|HSR","ES|Transport|Pass|Rail|non-HSR"),
              c("ES|Transport|VKM|Pass|Road|LDV","ES|Transport|VKM|Pass|Road|Bus"),
              c("ES|Transport|VKM|Freight|Road","ES|Transport|VKM|Pass|Road"),
              c("ES|Transport|VKM|Pass|Rail|HSR","ES|Transport|VKM|Pass|Rail|non-HSR","ES|Transport|VKM|Freight|Rail" ),
              c("FE|Transport|Pass|Road|LDV","FE|Transport|Pass|Road|Bus"),
              c("FE|Transport|Freight|Road","FE|Transport|Pass|Road"),
              c("FE|Transport|Pass|Rail|HSR","FE|Transport|Pass|Rail|non-HSR"),
              c("FE|Transport|Pass|Rail|HSR","FE|Transport|Pass|Rail|non-HSR","FE|Transport|Freight|Rail"),
              c("Emi|CO2|Transport|Pass|Road|LDV|Tailpipe","Emi|CO2|Transport|Pass|Road|Bus|Tailpipe"),
              c("Emi|CO2|Transport|Pass|Road|LDV|Demand","Emi|CO2|Transport|Pass|Road|Bus|Demand"),
              c("Emi|CO2|Transport|Freight|Road|Tailpipe","Emi|CO2|Transport|Pass|Road|Tailpipe"),
              c("Emi|CO2|Transport|Pass|Rail|non-HSR|Tailpipe","Emi|CO2|Transport|Freight|Rail|Tailpipe"),
              c("Emi|CO2|Transport|Freight|Road|Demand","Emi|CO2|Transport|Pass|Road|Demand"),
              c("Emi|CO2|Transport|Pass|Rail|non-HSR|Demand","Emi|CO2|Transport|Freight|Rail|Demand" )
              )


  for (i in 1:length(aggrmode) ) {
    toMIF <- rbind(toMIF,reportTotals(toMIF,aggrmode[i],vars[[i]]))
  }
 
  toMIF <- rbindlist(list(toMIF, reportStockAndSales(annual_mileage)), use.names=TRUE)

  if (!is.null(regionSubsetList)){
    toMIF <- rbindlist(list(
      toMIF,
      toMIF[region %in% regionSubsetList[["EUR"]],.(value = sum(value), region = "EUR"), by = .(model, scenario, variable, unit, period)],
      toMIF[region %in% regionSubsetList[["NEU"]],.(value = sum(value), region = "NEU"), by = .(model, scenario, variable, unit, period)],
      toMIF[region %in% regionSubsetList[["EU27"]],.(value = sum(value), region = "EU27"), by = .(model, scenario, variable, unit, period)],
      toMIF[,.(value = sum(value), region = "GLO"), by = .(model, scenario, variable, unit, period)]
    ), use.names=TRUE)
  }

  
  if (extendedReporting) {

    LogitCostplotdata <- function(priceData,prefData,logitExp,groupValue){

      yrs_costs <-c(seq(2005, 2060, 5), seq(2070, 2100, 10)) 
      priceData[,scenario:=scenario_title]
      prefData[,scenario:=scenario_title]
      logitExp[,scenario:=scenario_title]
      
      all_subsectors <- c("technology", "vehicle_type", "subsector_L1", "subsector_L2",
                          "subsector_L3", "sector")
      priceData <- priceData[!grepl("tmp",get(groupValue))]
      prefData <- prefData[!grepl("tmp",get(groupValue))]

      # change variable names for mip
      setnames(priceData, c("year"), c("period"))
      setnames(prefData, c("year"), c("period"))
      
      prefData <- prefData[period %in% yrs_costs]
      priceData<-  priceData[period %in% yrs_costs][,-c("share")]
      
      
      # Calculate Inconvenience Cost from share Weight
      prefData <- merge(prefData, logitExp, all.y = TRUE)
      
      
      price_tot <- priceData[, c("period", "region", "scenario", "tot_price", all_subsectors[
        seq(match(groupValue, all_subsectors) ,
            length(all_subsectors), 1)]), with = FALSE]
      prefData <- merge(prefData, price_tot, by = c("period", "region", "scenario", all_subsectors[
        seq(match(groupValue, all_subsectors) ,
            length(all_subsectors), 1)]))
      
      prefData[, value := tot_price * (sw^(1 / logit.exponent) - 1)]
      
      #Set Inconveniencecost to zero for shareweights where ES demand is anyway zero 
      prefData <- prefData[is.infinite(prefData$value),value:=0]
      prefData <- prefData[, c("region", "period", all_subsectors[
        seq(match(groupValue, all_subsectors) ,
            length(all_subsectors), 1)], "scenario", "value"), with = FALSE][, variable := "Eq inconvenience cost"]
      
      priceData <- melt(priceData[, -c("tot_price")], id.vars = c("scenario", "region", "period", all_subsectors[
        seq(match(groupValue, all_subsectors) ,
            length(all_subsectors), 1)]))
      
      data <- rbind(prefData,priceData)
      data[, unit := "$2005/pkm"][,model:=model_name]
      
      return(data)
    }
    
    LogitCostplotdata_FV <- function(priceData,prefData,logitExp){

      yrs_costs <-c(seq(2005, 2060, 5), seq(2070, 2100, 10)) 
      
      priceData[,scenario:=scenario_title]
      prefData[,scenario:=scenario_title]
      logitExp[,scenario:=scenario_title]
      
      # change variable names for mip
      setnames(priceData, c("year"), c("period"))
      setnames(prefData, c("year"), c("period"))
      
      prefData <- prefData[period %in% yrs_costs]
      priceData<-  priceData[period %in% yrs_costs]
      
      # Calculate Inconvenience Cost from share Weight
      prefData_sw <- copy(prefData)
      prefData_sw <- prefData_sw[logit_type=="sw"][,logit_type:=NULL]
      setnames(prefData_sw,"value","sw")
      prefData_sw <- merge(prefData_sw, logitExp, all.x = TRUE)
      #This should be removed in refactoring process
      prefData_sw <- prefData_sw[is.na(logit.exponent),logit.exponent:=-10]
      
      price_tot <- priceData[, c("period", "region", "scenario", "tot_price", "technology","vehicle_type")]
      prefData_sw <- merge(prefData_sw, price_tot, by = c("period", "region", "scenario", "technology","vehicle_type"),
           all.x=TRUE)
      
      prefData_sw[, value := tot_price * (sw^(1 / logit.exponent) - 1)]
      #Set Inconveniencecost to zero for shareweights where ES demand is anyway zero 
      prefData_sw <- prefData_sw[is.infinite(prefData_sw$value),value:=0]
      prefData_sw <- prefData_sw[, variable := paste0("Logit cost|FV|",gsub("_tmp_vehicletype","",vehicle_type),"|",technology,"|Eq inconvenience cost")][, c("region", "period","scenario","variable","value")]
      prefData <- prefData[!logit_type=="sw"]
      prefData_LDV <- prefData[, variable := paste0("Logit cost|FV|",gsub("_tmp_vehicletype","",vehicle_type),"|",technology,"|",logit_type)][, c("region", "period","scenario","variable","value")]
      
      priceData <- melt(priceData[, -c("tot_price","share","subsector_L1","subsector_L2","subsector_L3","sector")], id.vars = c("scenario", "region", "period","technology","vehicle_type"))
      #Why is this necessary?
      priceData <- as.data.table(priceData)
      priceData <- priceData[, variable := paste0("Logit cost|FV|",gsub("_tmp_vehicletype","",vehicle_type),"|",technology,"|",variable)][, c("region", "period","scenario","variable","value")]
              
      data <- rbind(prefData_sw,prefData_LDV,priceData)
      data[, unit := "$2005/pkm"][,model:=model_name]
      
      return(data)
    }
    
    # Mapping efficiencies for useful energy
    Mapp_UE <- data.table(
      technology = c("FCEV", "BEV", "Electric", "Liquids", "Hydrogen"),
      UE_efficiency = c(0.36, 0.64, 0.8, 0.23, 0.25))
    
    #To calculate!
    vars_toadd <- c(
    "FE|Transport|Pass",
    "FE|Transport",
    "FE|Transport|Freight",
    "ES|Transport|Pass",
    "ES|Transport",
    "ES|Transport|Freight",  
    "EInt|ALL",
    "UE",
    "Logit costs"
    )
    
    
    #Calculate logit Costs
    #Read in additional data if exist
    if (file.exists(datapath(fname = "logit_data.RDS"))){
      logit_data <- readRDS(datapath(fname = "logit_data.RDS"))
      prices <- logit_data$share_list
      Pref <- logit_data$pref_data}
    if (file.exists(datapath(fname = "logit_exp.RDS"))){
      logit_exp <- readRDS(datapath(fname = "logit_exp.RDS"))
      logit_exp <- logit_exp$logit_output}
    
    #Prices S3S
    Prices_S3S <- prices$S3S_shares
    setkey(Prices_S3S, NULL)
    Pref_S3S <- Pref$S3S_final_pref
    setkey(Pref_S3S, NULL)
    logit_exp_S3S <- logit_exp$logit_exponent_S3S
    setkey(logit_exp_S3S, NULL)

    #Adjust in model itself in refactoring process
    Prices_S3S[subsector_L3 %in% c("Cycle","Walk"), tot_VOT_price:=tot_price]
    Prices_S3S <- LogitCostplotdata(priceData=Prices_S3S,prefData=Pref_S3S,logitExp=logit_exp_S3S,groupValue="subsector_L3")
    Prices_S3S[, variable:=paste0("Logit cost|S3S|",subsector_L3,"|",variable)]
    Prices_S3S <- Prices_S3S[,.(region,period,scenario,variable,value,unit,model)]
    Pref_S3S[, variable:=paste0("Shareweight|S3|",subsector_L3)][,unit:="-"][,scenario:=scenario_title][,model:=model_name]
    setnames(Pref_S3S,"year","period")
    Pref_S3S <- Pref_S3S[,.(region,period,scenario,variable,value,unit,model)]

    #Prices S2S3
    Prices_S2S3 <- prices$S2S3_shares
    setkey(Prices_S2S3, NULL)
    Pref_S2S3 <- Pref$S2S3_final_pref
    setkey(Pref_S2S3, NULL)
    logit_exp_S2S3 <- logit_exp$logit_exponent_S2S3
    setkey(logit_exp_S2S3, NULL)
    
    Prices_S2S3 <- LogitCostplotdata(priceData=Prices_S2S3,prefData=Pref_S2S3,logitExp=logit_exp_S2S3,groupValue="subsector_L2")
    Prices_S2S3[, variable:=paste0("Logit cost|S2S3|",subsector_L2,"| ",variable)][,subsector_L2:=NULL][,sector:=NULL]
    Prices_S2S3 <- Prices_S2S3[,.(region,period,scenario,variable,value,unit,model)]
    Pref_S2S3[, variable:=paste0("Shareweight|S2|",subsector_L2)][,unit:="-"][,scenario:=scenario_title][,model:=model_name]
    setnames(Pref_S2S3,c("year","sw"),c("period","value"))
    Pref_S2S3 <- Pref_S2S3[,.(region,period,scenario,variable,value,unit,model)]
    
    #Prices S1S2
    Prices_S1S2 <- prices$S1S2_shares
    setkey(Prices_S1S2, NULL)
    Pref_S1S2 <- Pref$S1S2_final_pref
    setkey(Pref_S1S2, NULL)
    logit_exp_S1S2 <- logit_exp$logit_exponent_S1S2
    setkey(logit_exp_S1S2, NULL)
    
    Prices_S1S2 <- LogitCostplotdata(priceData=Prices_S1S2,prefData=Pref_S1S2,logitExp=logit_exp_S1S2,groupValue="subsector_L1")
    Prices_S1S2[, variable:=paste0("Logit cost|S1S2|",subsector_L1,"|",variable)]
    Prices_S1S2 <- Prices_S1S2[,.(region,period,scenario,variable,value,unit,model)]
    Pref_S1S2[, variable:=paste0("Shareweight|S1|",subsector_L1)][,unit:="-"][,scenario:=scenario_title][,model:=model_name]
    setnames(Pref_S1S2,c("year","sw"),c("period","value"))
    Pref_S1S2 <- Pref_S1S2[,.(region,period,scenario,variable,value,unit,model)]
    
    #Prices VS1
    Prices_VS1 <- prices$VS1_shares
    setkey(Prices_VS1, NULL)
    Pref_VS1 <- Pref$VS1_final_pref
    setkey(Pref_VS1, NULL)
    logit_exp_VS1 <- logit_exp$logit_exponent_VS1
    setkey(logit_exp_VS1, NULL)

    #Add subsector_L2, subsector L3 and sector to Prices_VS1 (for structural conformity)
    Prices_VS1 <- merge(Prices_VS1, unique(Pref_VS1[,c("subsector_L2","subsector_L3","sector","vehicle_type")]),by="vehicle_type",all.x=TRUE)
    Prices_VS1 <- LogitCostplotdata(priceData=Prices_VS1,prefData=Pref_VS1,logitExp=logit_exp_VS1,groupValue="vehicle_type")
    
    #Before prices are finally structured, vehicles are aggregated 
    Aggrdata_veh <- as.data.table(Aggrdata[,c("vehicle_type","det_veh")])
    Aggrdata_veh <- unique(Aggrdata_veh[!is.na(det_veh)])[,det_veh:=gsub("Freight\\|Road\\||Pass\\|Road\\|","",det_veh)]
    Prices_VS1_aggr <- copy(Prices_VS1)
    
    Prices_VS1_aggr <- merge(Prices_VS1_aggr,Aggrdata_veh)
    weight_pkm <- copy(demand_km)
    setnames(weight_pkm,c("value","year"),c("weight","period"))
    weight_pkm <- weight_pkm[,c("region","vehicle_type","technology","weight","period")]
    weight_pkm_VS1 <- weight_pkm[,.(weight=sum(weight)),by=c("region","vehicle_type","period")]
    Prices_VS1_aggr <- merge(Prices_VS1_aggr,weight_pkm_VS1,by=c("region","vehicle_type","period"),all.x = TRUE)
    
    Prices_VS1_aggr <- Prices_VS1_aggr[,c("region","det_veh","period","variable","unit","model","scenario","vehicle_type","weight","value")]
    Prices_VS1_aggr <- Prices_VS1_aggr[,weight:=weight/sum(weight),by=c("region","det_veh","period","variable","unit","model","scenario")]
    Prices_VS1_aggr <- Prices_VS1_aggr[,value:=value*weight]
    Prices_VS1_aggr <- Prices_VS1_aggr[,.(value=sum(value)),by=c("region","det_veh","period","variable","unit","model","scenario")]
    Prices_VS1_aggr <- Prices_VS1_aggr[!duplicated(Prices_VS1_aggr)]
    setnames(Prices_VS1_aggr,"det_veh","vehicle_type")
    Prices_VS1_aggr[, variable:=paste0("Logit cost|VS1|",vehicle_type,"|",variable)]
    Prices_VS1_aggr <- Prices_VS1_aggr[,.(region,period,scenario,variable,value,unit)][,model:=model_name]
    
    Prices_VS1[, variable:=paste0("Logit cost|VS1|",vehicle_type,"|",variable)][,model:=model_name]
    Prices_VS1 <- Prices_VS1[,.(region,period,scenario,variable,value,unit,model)]
    Pref_VS1[, variable:=paste0("Shareweight|V|",vehicle_type)][,unit:="-"][,scenario:=scenario_title][,model:=model_name]
    setnames(Pref_VS1,c("year","sw"),c("period","value"))
    Pref_VS1 <- Pref_VS1[,.(region,period,scenario,variable,value,unit,model)]  
    
    Prices_FV <- prices$FV_shares
    setkey(Prices_FV, NULL)
    Pref_FV <- Pref$FV_final_pref
    setkey(Pref_FV, NULL)
    logit_exp_VS1 <- logit_exp$logit_exponent_FV
    setkey(logit_exp_VS1, NULL)
    
    Prices_FV <- LogitCostplotdata_FV(priceData=Prices_FV,prefData=Pref_FV,logitExp=logit_exp_VS1)
    Pref_FV <- Pref_FV[logit_type=="sw"]
    Pref_FV[, variable:=paste0("Shareweight|F|",gsub("_tmp_vehicletype","",vehicle_type),"|",technology)][,unit:="-"][,scenario:=scenario_title][,model:=model_name]
    setnames(Pref_FV,c("year"),c("period"))
    Pref_FV <- Pref_FV[,.(region,period,scenario,variable,value,unit,model)] 
   
    
    Price_data <- rbind(Prices_FV,Prices_VS1,Prices_VS1_aggr,Prices_S1S2)
     
   
    #Calculate Vehicle Size Shares LDV
     vars <- c(
       "ES|Transport|Pass|Road|LDV|Large",
       "ES|Transport|Pass|Road|LDV|Medium",
       "ES|Transport|Pass|Road|LDV|SUV",
       "ES|Transport|Pass|Road|LDV|Mini",
       "ES|Transport|Pass|Road|LDV|Small",
       "ES|Transport|Pass|Road|LDV|Van"
     )
     ES_shares_LDVsize <- toMIF[variable %in% vars][,unit:=NULL]
     ES_shares_LDVsize[, tot:= sum(value), by= c("period","region","scenario","model")]
     ES_shares_LDVsize[,value:=value/tot*100][,unit:="%"][,tot:=NULL]
     ES_shares_LDVsize <- ES_shares_LDVsize[,variable:=paste0(variable,"|Share")]
     
     #Truck size
     vars <- c(
       "ES|Transport|Freight|Road|Truck (0-3.5t)",
       "ES|Transport|Freight|Road|Truck (18t)",
       "ES|Transport|Freight|Road|Truck (26t)",
       "ES|Transport|Freight|Road|Truck (40t)",
       "ES|Transport|Freight|Road|Truck (7.5t)"
     )
     ES_shares_Trucksize <- toMIF[variable %in% vars][,unit:=NULL]
     ES_shares_Trucksize[, tot:= sum(value), by= c("period","region","scenario","model")]
     ES_shares_Trucksize[,value:=value/tot*100][,unit:="%"][,tot:=NULL]
     ES_shares_Trucksize <- ES_shares_Trucksize[,variable:=paste0(variable,"|Share")]
     
    #Calculate Energy Service Shares 
    #with bunkers
    vars <- c(
      "ES|Transport|Pass|Aviation|Domestic",
      "ES|Transport|Pass|Aviation|International",
      "ES|Transport|Pass|Rail|HSR",
      "ES|Transport|Pass|Rail|non-HSR",
      "ES|Transport|Pass|Road|Bus",
      "ES|Transport|Pass|Road|LDV",
      "ES|Transport|Pass|Road|Non-Motorized|Cycling",
      "ES|Transport|Pass|Road|Non-Motorized|Walking"
    )

    ES_shares_Pass <- toMIF[variable %in% vars][,unit:=NULL]
    ES_shares_Pass[, tot:= sum(value), by= c("period","region","scenario","model")]
    ES_shares_Pass[,value:=value/tot*100][,unit:="%"][,tot:=NULL]
    ES_shares_Pass <- ES_shares_Pass[,variable:=paste0(variable,"|Share")]
    
    vars <- c(
      "ES|Transport|Freight|Road",
      "ES|Transport|Freight|Rail",
      "ES|Transport|Freight|International Shipping",
      "ES|Transport|Freight|Navigation"
    )
    
    ES_shares_Freight <- toMIF[variable %in% vars][,unit:=NULL]
    ES_shares_Freight[, tot:= sum(value), by= c("period","region","scenario","model")]
    ES_shares_Freight[,value:=value/tot*100][,unit:="%"][,tot:=NULL]
    ES_shares_Freight <- ES_shares_Freight[,variable:=paste0(variable,"|Share")]
    
    #without bunkers
    vars <- c(
      "ES|Transport|Pass|Aviation|Domestic",
      "ES|Transport|Pass|Rail|HSR",
      "ES|Transport|Pass|Rail|non-HSR",
      "ES|Transport|Pass|Road|Bus",
      "ES|Transport|Pass|Road|LDV",
      "ES|Transport|Pass|Road|Non-Motorized|Cycling",
      "ES|Transport|Pass|Road|Non-Motorized|Walking"
    )
    
    ES_shares_Pass_wobunk <- toMIF[variable %in% vars][,unit:=NULL]
    ES_shares_Pass_wobunk[, tot:= sum(value), by= c("period","region","scenario","model")]
    ES_shares_Pass_wobunk[,value:=value/tot*100][,unit:="%"][,tot:=NULL]
    ES_shares_Pass_wobunk <- ES_shares_Pass_wobunk[,variable:=paste0(variable,"|Share w/o bunkers")]
    
    vars <- c(
      "ES|Transport|Freight|Road",
      "ES|Transport|Freight|Rail",
      "ES|Transport|Freight|Navigation"
    )
    
    ES_shares_Freight_wobunk <- toMIF[variable %in% vars][,unit:=NULL]
    ES_shares_Freight_wobunk[, tot:= sum(value), by= c("period","region","scenario","model")]
    ES_shares_Freight_wobunk[,value:=value/tot*100][,unit:="%"][,tot:=NULL]
    ES_shares_Freight_wobunk <- ES_shares_Freight_wobunk[,variable:=paste0(variable,"|Share w/o bunkers")]
    
    
    #Aggregate data
    
    
    toMIF <- rbind(toMIF,ES_shares_Pass,ES_shares_Freight,ES_shares_Pass_wobunk,ES_shares_Freight_wobunk,ES_shares_LDVsize,ES_shares_Trucksize,Price_data)
  }
  
  
  ## Make sure there are no duplicates!
  idx <- anyDuplicated(toMIF, by = c("region", "variable", "period"))
  if(idx){
    warning(paste0("Duplicates found in EDGE-T reporting output:",
                   capture.output(toMIF[idx]), collapse="\n"))
  }

  toMIF <- toMIF[!duplicated(toMIF)]
  toMIF <- toMIF[, c("model","scenario","region","variable","unit","period","value")]
 
  toMIF <- data.table::dcast(toMIF, ... ~ period, value.var="value")
  setnames(toMIF, colnames(toMIF)[1:5], c("Model", "Scenario", "Region", "Variable", "Unit"))
  toMIF <- as.data.frame(toMIF)

  writeMIF(toMIF, file.path(output_folder,"/",name_mif), append=T)
  deletePlus(file.path(output_folder,"/",name_mif), writemif=T)
  
}
