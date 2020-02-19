#' Creates RDS files for iterative REMIND-EDGE runs from csv input files.
#'
#' @param input_path
#' @import data.table
#' @export

createRDS <- function(input_path, data_path, SSP_scenario, EDGE_scenario){

  if (length(list.files(path = data_path, pattern = "RDS")) > 0) {
    ## RDS files have been created and previously saved in the expected folder
    print("Loading local RDS input files...")


  } else {
    print("Loading csv data from input folder and creating RDS files...")
    dir.create(file.path(data_path), showWarnings = FALSE)
    ## function that loads the csv input files and converts them into RDS local files
    csv2RDS = function(pattern, filename, input_path, names_dt){
      tmp=fread(paste0(input_path, pattern, ".cs4r"), stringsAsFactors = FALSE, col.names = names_dt, skip = 4)[SSPscen == SSP_scenario & EDGEscen == EDGE_scenario][, -c("SSPscen", "EDGEscen")]
      tmp[,vehicle_type := gsub("DOT", ".", vehicle_type)]
      tmp_list <- split(tmp,tmp$entry)

      for (i in names(tmp_list)) {
        removecol = names_dt[names_dt %in% c("entry", "varname")]
        tmp_list[[i]][, grep(removecol, colnames(tmp_list[[i]])):=NULL]
        tmp_list[[i]] = tmp_list[[i]][,which(unlist(lapply(tmp_list[[i]], function(x)!any(x == "tmp")))),with=F]
      }

      if (length(tmp_list) == 1) {
        tmp_list = tmp_list[[1]]
      }

      saveRDS(tmp_list, file.path(data_path, paste0(filename,".RDS")))

    }

    ## create RDS files for lists
    csv2RDS(pattern = "SW",
            filename = "SW",
            input_path = input_path,
            names_dt = c("year", "iso", "SSPscen", "EDGEscen", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology", "entry", "sw"))

    csv2RDS(pattern = "inconv",
            filename = "inconv",
            input_path = input_path,
            names_dt = c("year", "iso", "SSPscen", "EDGEscen", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology", "entry", "pinco"))

    csv2RDS(pattern = "logit_exponent",
            filename = "logit_exp",
            input_path = input_path,
            names_dt = c("SSPscen", "EDGEscen", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "entry", "logit.exponent"))

    csv2RDS(pattern = "value_time",
            filename = "VOT_iso",
            input_path = input_path,
            names_dt = c("year", "iso", "SSPscen", "EDGEscen", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "entry", "time_price"))

    csv2RDS(pattern = "price_nonmot",
            filename = "price_nonmot",
            input_path = input_path,
            names_dt = c("year", "iso", "SSPscen", "EDGEscen", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology", "entry", "tot_price"))

    ## create RDS files for single dataframes
    csv2RDS(pattern = "harmonized_intensities",
            filename = "harmonized_intensities",
            input_path = input_path,
            names_dt = c("year", "iso", "SSPscen", "EDGEscen", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology", "entry", "sector_fuel", "EJ_Mpkm_final"))

    csv2RDS(pattern = "UCD_NEC_iso",
            filename = "UCD_NEC_iso",
            input_path = input_path,
            names_dt = c("year", "iso", "SSPscen", "EDGEscen", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology", "entry", "non_fuel_price"))

    ## save the values of the run settings for reporting purposes
    settingsEDGE <- data.table(settings = c("selfmarket_taxes", "selfmarket_policypush", "selfmarket_acceptancy", "techswitch", "enhancedtech", "rebates_febates"),
                               value = c(selfmarket_taxes, selfmarket_policypush, selfmarket_acceptancy, techswitch, enhancedtech, rebates_febates))

    saveRDS(settingsEDGE, file.path(data_path, "settingsEDGE.RDS"))


  }


  ## load input data
  vot_data <- readRDS(datapath("VOT_iso.RDS"))
  sw_data <- readRDS(datapath("SW.RDS"))
  inco_data <- readRDS(datapath("inconv.RDS"))
  logit_params <- readRDS(datapath("logit_exp.RDS"))
  int_dat <- readRDS(datapath("harmonized_intensities.RDS"))
  nonfuel_costs <- readRDS(datapath("UCD_NEC_iso.RDS"))
  price_nonmot <- readRDS(datapath("price_nonmot.RDS"))

  ## FIXME: hotfix to make the (empty) vot_data$value_time_VS1 with the right column types. Probably there is another way to do that, did not look for it.
  vot_data$value_time_VS1$iso = as.character(vot_data$value_time_VS1$iso)
  vot_data$value_time_VS1$subsector_L1 = as.character(vot_data$value_time_VS1$subsector_L1)
  vot_data$value_time_VS1$vehicle_type = as.character(vot_data$value_time_VS1$vehicle_type)
  vot_data$value_time_VS1$year = as.numeric(vot_data$value_time_VS1$year)
  vot_data$value_time_VS1$time_price = as.numeric(vot_data$value_time_VS1$time_price)

  output = list(vot_data = vot_data,
                sw_data = sw_data,
                inco_data = inco_data,
                logit_params = logit_params,
                int_dat = int_dat,
                nonfuel_costs = nonfuel_costs,
                price_nonmot = price_nonmot)

  return(output)
}
