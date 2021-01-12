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
#' @param gdpfile, if caching is required, specify the filename here, default is "GDPcache.rds"
#' @param isolev, if the data provided has to be on ISO or regional level
#' @keywords gdp
#' @import data.table
#' @importFrom madrat calcOutput
#' @export

getRMNDGDPcap <- function(scenario="gdp_SSP2",
                          yearcol="year",
                          isocol,
                          valuecol="weight",
                          isolev = F,
                          usecache = F,
                          gdpfile ="GDPcache.rds"){

  `.` <- iso <- value <- GDP_cap <- weight <- POP_val <- region <- variable <- Year <- NULL

  if (isolev) {
    gdpfileCap = "GDPCAPcacheISO.rds"
    toagg = FALSE
  } else {
    gdpfileCap = "GDPCAPcache.rds"
    toagg = TRUE
  }

  if(usecache && file.exists(gdpfileCap)){
    cat("getGDP_dt: Using cached GDP data in", gdpfileCap, "\n")
    GDP_POP = readRDS(gdpfileCap)
  } else {
    GDPppp_country <- calcOutput("GDPppp", aggregate = toagg)[,, scenario]

    gdp <- as.data.table(GDPppp_country)[variable == scenario]
    gdp[, (yearcol) := as.numeric(gsub("y", "", Year))][, Year := NULL]
    setnames(gdp, c("ISO3", "value"), c(isocol, valuecol), skip_absent=TRUE)
    scenario_POP = gsub("gdp_", "", scenario)
    POP_country=calcOutput("Population", aggregate = toagg)[,, paste0("pop_", scenario_POP)]
    POP <- magpie2dt(POP_country, regioncol = isocol,
                     yearcol = "year", datacols = "POP")
    POP=POP[,.(region,year,POP,POP_val=value)]
    GDP_POP=merge(gdp,POP,all = TRUE,by=c(isocol,"year"))
    GDP_POP[,GDP_cap:=weight/POP_val]
  }


  if(usecache){
    saveRDS(GDP_POP, gdpfileCap)
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
#' @param isolev, if the data provided has to be on ISO or regional level
#' @keywords gdp
#' @import data.table
#' @importFrom madrat calcOutput
#' @export

getRMNDGDP <- function(scenario="gdp_SSP2",
                       yearcol="year",
                       isocol,
                       valuecol="weight",
                       isolev = F,
                       usecache = F){
  variable <- Year <- NULL

  if (isolev) {
    gdpfile = "GDPcacheISO.rds"
    toagg = FALSE
  } else {
    gdpfile = "GDPcache.rds"
    toagg = TRUE
  }

  if(usecache && file.exists(gdpfile)){
    cat("getGDP_dt: Using cached GDP data in", gdpfile, "\n")
    return(readRDS(gdpfile))
  }

  GDPppp_country <- calcOutput("GDPppp", aggregate = toagg)[,, scenario]

  gdp <- as.data.table(GDPppp_country)[variable == scenario]
  gdp[, (yearcol) := as.numeric(gsub("y", "", Year))][, Year := NULL]
  setnames(gdp, c("ISO3", "value"), c(isocol, valuecol))

  if(usecache){
    saveRDS(gdp, gdpfile)
  }

  return(gdp)
}
