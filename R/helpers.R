#' Load GDP per capita data using `moinput` on ISO country resolution
#' for a scenario as data.table object with given colnames.
#'
#' If GDP values are required frequently, a cache file can be used to retrieve the
#' data fast.
#'
#' @param scenario, GDP scenario, default is gdp_SSP2.
#' @param yearcol, name of the year column, default "year".
#' @param isocol, name of the column containing ISO3 codes, default is "iso".
#' @param valuecol, name of the column containing the GDP values, default is "weight".
#' @param usecache, store the result in a RDS file in the working directory, default is FALSE.
#' @param gdpCapfile, if caching is required, specify the filename here. The user has to create 2 distint names for a ISO and a regional cache, in line with to_aggregate.
#' @param to_aggregate, if the data provided has to be on ISO or regional level. Has to be coherent with the selected name of gdpCapfile.
#' @keywords gdpcap
#' @import data.table
#' @importFrom madrat calcOutput
#' @export

getRMNDGDPcap <- function(gdpCapfile,
                          isocol,
                          scenario="gdp_SSP2",
                          yearcol="year",
                          valuecol="weight",
                          to_aggregate = F,
                          usecache = F){

  `.` <- iso <- value <- GDP_cap <- weight <- POP_val <- region <- variable <- Year <- NULL

  if(usecache && file.exists(gdpCapfile)){
    cat("getGDP_dt: Using cached GDP data in", gdpCapfile, "\n")
    GDP_POP = readRDS(gdpCapfile)
  } else {
    GDPppp_country <- calcOutput("GDPppp", aggregate = to_aggregate)[,, scenario]

    gdp <- as.data.table(GDPppp_country)[variable == scenario]
    gdp[, (yearcol) := as.numeric(gsub("y", "", Year))][, Year := NULL]
    setnames(gdp, c("ISO3", "value"), c(isocol, valuecol), skip_absent=TRUE)
    scenario_POP = gsub("gdp_", "", scenario)
    POP_country=calcOutput("Population", aggregate = to_aggregate)[,, paste0("pop_", scenario_POP)]
    POP <- magpie2dt(POP_country, regioncol = isocol,
                     yearcol = "year", datacols = "POP")
    POP=POP[,.(region,year,POP,POP_val=value)]
    GDP_POP=merge(gdp,POP,all = TRUE,by=c(isocol,"year"))
    GDP_POP[,GDP_cap:=weight/POP_val]
  }


  if(usecache){
    saveRDS(GDP_POP, gdpCapfile)
  }

  return(GDP_POP)
}

#' Load GDP data using `moinput` on ISO country resolution for a scenario as
#' data.table object with given colnames.
#'
#' If GDP values are required frequently, a cache file can be used to retrieve the
#' data fast.
#'
#' @param scenario, GDP scenario, default is gdp_SSP2.
#' @param yearcol, name of the year column, default "year".
#' @param isocol, name of the column containing ISO3 codes, default is "iso".
#' @param valuecol, name of the column containing the GDP values, default is "weight".
#' @param usecache, store the result in a RDS file in the working directory, default is FALSE.
#' @param to_aggregate, if the data provided has to be on ISO or regional level. Has to be coherent with the selected name of gdpCapfile.
#' @param gdpfile, if caching is required, specify the filename here. The user has to create 2 distint names for a ISO and a regional cache, in line with to_aggregate.
#' @keywords gdp
#' @import data.table
#' @importFrom madrat calcOutput
#' @export

getRMNDGDP <- function(gdpfile,
                       isocol,
                       scenario="gdp_SSP2",
                       yearcol="year",
                       valuecol="weight",
                       to_aggregate = F,
                       usecache = F){
  variable <- Year <- NULL


  if(usecache && file.exists(gdpfile)){
    cat("getGDP_dt: Using cached GDP data in", gdpfile, "\n")
    return(readRDS(gdpfile))
  }

  GDPppp_country <- calcOutput("GDPppp", aggregate = to_aggregate)[,, scenario]

  gdp <- as.data.table(GDPppp_country)[variable == scenario]
  gdp[, (yearcol) := as.numeric(gsub("y", "", Year))][, Year := NULL]
  setnames(gdp, c("ISO3", "value"), c(isocol, valuecol))

  if(usecache){
    saveRDS(gdp, gdpfile)
  }

  return(gdp)
}
