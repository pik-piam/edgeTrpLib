#' Load GDP data using `moinput` on ISO country resolution for a scenario as data.table object with given colnames.
#' By default, the 
#'
#' @param scenario, GDP scenario, default is gdp_SSP2.
#' @param yearcol, name of the year column, default "year".
#' @param isocol, name of the column containing ISO3 codes, default is "iso".
#' @param valuecol, name of the column containing the GDP values, default is "weight".
#' @param usecache, store the result in a RDS file in the working directory, default is FALSE.
#' @param gdpfile, if caching is required, specify the filename here, default is "GDPcache.rds"
#' @keywords gdp
#' @import data.table
#' @export
#' @examples
#' gdp <- getRMNDGDP()

getRMNDGDP <- function(scenario="gdp_SSP2",
                      yearcol="year",
                      isocol="iso",
                      valuecol="weight",
                      usecache=F,
                      gdpfile="GDPcache.rds"){

    if(usecache && file.exists(gdpfile)){
        cat("getGDP_dt: Using cached GDP data in", gdpfile, "\n")
        return(readRDS(gdpfile))
    }

    GDPppp_country <- madrat::calcOutput("GDPppp", aggregate = F)[,, scenario]

    gdp <- as.data.table(GDPppp_country)[variable == scenario]
    gdp[, (yearcol) := as.numeric(gsub("y", "", Year))][, Year := NULL]
    setnames(gdp, c("ISO3", "value"), c(isocol, valuecol))

    if(usecache){
        saveRDS(gdp, gdpfile)
    }

    return(gdp)
}

