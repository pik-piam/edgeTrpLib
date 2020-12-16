#' Load GDP data using `moinput` on regional resolution for a scenario as
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
#' @param gdpfile, if caching is required, specify the filename here, default is "GDPcache.rds"
#' @keywords gdp
#' @import data.table
#' @importFrom madrat calcOutput
#' @export

getRMNDGDP <- function(scenario="gdp_SSP2",
                      yearcol="year",
                      isocol="iso",
                      valuecol="weight",
                      usecache=F,
                      gdpfile="GDPcache.rds"){

    variable <- Year <- NULL

    if(usecache && file.exists(gdpfile)){
        cat("getGDP_dt: Using cached GDP data in", gdpfile, "\n")
        return(readRDS(gdpfile))
    }

    GDPppp_country <- calcOutput("GDPppp", aggregate = T)[,, scenario]

    gdp <- as.data.table(GDPppp_country)[variable == scenario]
    gdp[, (yearcol) := as.numeric(gsub("y", "", Year))][, Year := NULL]
    setnames(gdp, c("value"), c(valuecol))

    if(usecache){
        saveRDS(gdp, gdpfile)
    }

    return(gdp)
}


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
#' @keywords gdp
#' @import data.table
#' @importFrom madrat calcOutput
#' @export

getRMNDGDPcap <- function(scenario="gdp_SSP2",
                          yearcol="year",
                          isocol="iso",
                          valuecol="weight",
                          usecache=F,
                          gdpfile="GDPcache.rds"){

  `.` <- iso <- value <- GDP_cap <- weight <- POP_val <- region <- NULL
  scenario <- gsub("gdp_", "", scenario)
  gdp <- getRMNDGDP(paste0("gdp_", scenario), usecache=F)
  setnames(gdp, old="ISO3", new = "region", skip_absent = TRUE)
  POP_country=calcOutput("Population", aggregate = T)[,, paste0("pop_", scenario)]
  POP <- magpie2dt(POP_country, regioncol = "region",
                   yearcol = "year", datacols = "POP")
  POP=POP[,.(region,year,POP,POP_val=value)]
  GDP_POP=merge(gdp,POP,all = TRUE,by=c("region","year"))
  GDP_POP[,GDP_cap:=weight/POP_val]

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
#' @param gdpfile, if caching is required, specify the filename here, default is "GDPcache.rds"
#' @keywords gdpiso
#' @import data.table
#' @importFrom madrat calcOutput
#' @export

getRMNDGDPISO <- function(scenario="gdp_SSP2",
                       yearcol="year",
                       isocol="iso",
                       valuecol="weight",
                       usecache=F,
                       gdpfile="GDPcache.rds"){

  variable <- Year <- NULL

  if(usecache && file.exists(gdpfile)){
    cat("getGDP_dt: Using cached GDP data in", gdpfile, "\n")
    return(readRDS(gdpfile))
  }

  GDPppp_country <- calcOutput("GDPppp", aggregate = F)[,, scenario]

  gdp <- as.data.table(GDPppp_country)[variable == scenario]
  gdp[, (yearcol) := as.numeric(gsub("y", "", Year))][, Year := NULL]
  setnames(gdp, c("ISO3", "value"), c(isocol, valuecol))

  if(usecache){
    saveRDS(gdp, gdpfile)
  }

  return(gdp)
}
