#' Loads the total demand of transport CES nodes from last iteration
#'
#' @param gdx
#' @param REMINDmapping
#' @param EDGE2teESmap
#' @param years
#'
#' @import data.table
#' @importfrom edgeTrpLib getRMNDGDP


readREMINDdemand= function(gdx,REMINDmapping,EDGE2teESmap, years){
  dem <- readGDX(gdx, c("vm_cesIO"),field = "l")
  dem <- dem[, , c("entrp_pass_sm","entrp_pass_lo","entrp_frgt_sm","entrp_frgt_lo")]

  dem <- magpie2dt(dem, regioncol = "region",
                   yearcol = "year", datacols = "sector_remind")

  ## downscale to iso level
  gdp <- getRMNDGDP(scenario = scenario, usecache = T)
  dem <- dem[year %in% years]
  dem <- disaggregate_dt(dem, REMINDmapping,
                         valuecol="value",
                         datacols=c("sector_remind"),
                         weights=gdp)

  ## attribute EDGE sector names to CES values
  dem <- merge(x=dem, y=unique(EDGE2teESmap[,c("all_in","EDGE_top")]), all = TRUE, by.x = "sector_remind", by.y = "all_in")
  dem <- dem[,.(iso, year, sector = EDGE_top, demand = value)]

  ## find right units
  dem[, demand := demand  ## in trillion pkm or tkm
                  *1e6]   ## in million pkm or tkm

  return(dem)
}
