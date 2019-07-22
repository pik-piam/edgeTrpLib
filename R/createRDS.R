#' Creates RDS files for iterative REMIND-EDGE runs from csv input files.
#'
#' @param input_path
#' @import data.table
#' @importFrom stats setNames
#' @export


createRDS <- function(input_path){
  if (length(list.files(path = "input_EDGE", pattern = "RDS")) > 0) {
    ## RDS files have been created and previously saved in the expected folder
    print("Loading local RDS input files...")

  } else {
    print("Loading csv data from input folder and creating RDS files...")
    dir.create(file.path("input_EDGE/"), showWarnings = FALSE)
    ## function that loads the csv input files and converts them into RDS local files
    csv2RDS = function(pattern, filename, input_path, ismultiple = TRUE){
      if (ismultiple) {
        ## for multiple csv files that have to be grouped in one single RDS file
        tmp = list.files(path=input_path, pattern = pattern)
        tmp <- paste0(input_path, tmp)
        tmp_dfs <- stats::setNames(object = lapply(tmp, function(f)
          data.table(read.csv(f, stringsAsFactors = FALSE))),nm = sub(".*/([^.]*).*", "\\1", tmp))
        saveRDS(tmp_dfs, paste0("input_EDGE/", filename,".RDS"))
      } else{
        ## for one object only RDS files
        tmp=data.table(read.csv(paste0(input_path, pattern, ".csv"), stringsAsFactors = FALSE))
        saveRDS(tmp, paste0("input_EDGE/", filename,".RDS"))
      }

    }

    ## create RDS files for lists
    csv2RDS(pattern = "SW",
            filename = "SW",
            input_path = input_path)

    csv2RDS(pattern = "logit_exponent",
            filename = "logit_exp",
            input_path = input_path)

    csv2RDS(pattern = "value_time|price_non",
            filename = "VOT_iso",
            input_path = input_path)

    ## create RDS files for single dataframes
    csv2RDS(pattern = "harmonized_intensities",
            filename = "harmonized_intensities",
            input_path = input_path,
            ismultiple = FALSE)

    csv2RDS(pattern = "UCD_NEC_iso",
            filename = "UCD_NEC_iso",
            input_path = input_path,
            ismultiple = FALSE)
  }
}
