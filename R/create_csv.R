#' Creates csv files that contains all EDGE-transport entries for Moinput.
#'
#' @param logit_params
#' @param sw_data
#' @param vot_data
#' @param NEC_data
#' @param demByTech
#' @param int_dat
#' @param intensity
#' @param capCost
#' @param demand_traj
#' @param gdp_ssp_scenario
#' @param EDGE_scenario

create_csv <- function(logit_params, sw_data, vot_data, NEC_data, demByTech, int_dat, intensity, capCost, demand_traj, gdp_ssp_scenario, EDGE_scenario){

  ## function for appending columns of EDGE_scenario and SSP scenario to lists
  appendlist_SSP_EDGEscen <- function(x){
    for (i in names(x)) {
      if (nrow(x[[i]]) > 0) {
        x[[i]] = cbind(x[[i]], SSP = sub('.*\\_', '', gdp_ssp_scenario))
        x[[i]] = cbind(x[[i]], EDGE_scenario = sub('.*\\_', '', EDGE_scenario))
      } else { ## if the data table is empty, we need to add the columns as follows
        x[[i]]$SSP = 1
        x[[i]]$EDGE_scenario = 1}
    }
    return(x)
  }

  ## function for appending columns of EDGE_scenario and SSP scenario to data tables
  appendfile_SSP_EDGEscen <- function(x){
    x = cbind(x, SSP = sub('.*\\_', '', gdp_ssp_scenario))
    x = cbind(x, EDGE_scenario = sub('.*\\_', '', EDGE_scenario))
  }

  all_lists = list(logit_params = logit_params,
                   sw_data = sw_data,
                   vot_data = vot_data)

  all_files = list(NEC_data = NEC_data,
                   demByTech = demByTech,
                   intensity = intensity,
                   capCost = capCost,
                   demand_traj = demand_traj,
                   int_dat =int_dat)

  if (add_scenarios == TRUE) {
    all_lists = lapply(all_lists,appendlist_SSP_EDGEscen)
    all_files = lapply(all_files, appendfile_SSP_EDGEscen)
  }

  dir.create(file.path(level2path("")), showWarnings = FALSE)

  ## writes csv files for intensity, shares and budget, and demand (for calibration and starting point of REMIND)
  print("Creating cs4r files...")
  fwrite(all_files$demByTech, file = level2path("demByTech.cs4r"), col.names=TRUE)
  fwrite(all_files$intensity, file = level2path("fe2es.cs4r"), col.names=TRUE)
  fwrite(all_files$capCost, file = level2path("esCapCost.cs4r"), col.names=TRUE)
  fwrite(all_files$demand_traj, file = level2path("pm_trp_demand.cs4r"), col.names=TRUE)

  ## writes csv files for lambda parameters, SW, VOT and non-motorized costs, energy intensity, non-energy costs (for the EDGE run in between REMIND iterations)
  print("Creating csv files for lambdas...")
  mapply(
    write.table,
    x=all_lists$logit_params, file=level2path(paste0(names(all_lists$logit_params), ".csv")),
    MoreArgs=list(row.names=FALSE, sep=",", col.names=TRUE, quote=F)
  )
  print("Creating csv files for SWs...")
  mapply(
    write.table,
    x=all_lists$sw_data, file=level2path(paste0(names(all_lists$sw_data), ".csv")),
    MoreArgs=list(row.names=FALSE, sep=",", col.names=TRUE, quote=F)
  )
  print("Creating csv files for VOT and non-motorized costs...")
  mapply(
    write.table,
    x=all_lists$vot_data, file=level2path(paste0(names(all_lists$vot_data), ".csv")),
    MoreArgs=list(row.names=FALSE, sep=",", col.names=TRUE, quote=F)
  )
  print("Creating csv files for energy intensity...")
  fwrite(all_files$int_dat, file = level2path("harmonized_intensities.csv"), col.names=TRUE)
  print("Creating csv files for non-energy costs...")
  fwrite(all_files$NEC_data, file = level2path("UCD_NEC_iso.csv"), col.names=TRUE)

}




