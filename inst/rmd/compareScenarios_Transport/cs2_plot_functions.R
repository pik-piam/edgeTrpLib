lineplot = function(varname, data){
  p=ggplot(data, aes(x = as.character(period), y = value,group = scenario, color = scenario))+
    geom_line(size = 1)+
    facet_wrap(~data$variable, nrow = 4, scales = "free")+
    scale_x_discrete(breaks = c(1990,2010,2030,2050,2100))+
    labs(x = "", y = paste0(varname," [", unique(data$unit),"]"), title = paste0(varname))
  return(p)
}

mipBarYearDataMod <- function(x, colour = NULL, ylab = NULL, xlab = NULL, title = NULL,
                              scenario_markers = TRUE) { #nolint
  scenarioMarkers <- scenario_markers
  x <- as.quitte(x)
  

  if (length(unique(x$model)) > 1) {
    stop("this plot can only deal with data that have only one model")
  }
  
  if (!is.integer(x$period)) {
    stop("this plot can only deal with data that have integer periods")
  }
  
  # calculate y-axis label
  x$variable <- shorten_legend(x$variable, identical_only = TRUE)
  
  if (is.null(ylab)) {
    ylab <- paste0(sub(".$", "", attr(x$variable, "front")), attr(x$variable, "back"))
    # add unit
    unit <- unique(as.character(x$unit))
    ylab <- paste0(ylab, " (", paste0(unit, collapse = " | "), ")")
  }
  
  # add dummy-dimension for space between the time-steps
  xpos <- crossing(period   = unique(x$period),
                   region = factor(c(levels(x$region), "\x13"))) %>%
    order.levels(region = c(levels(x$region), "\x13")) %>%
    arrange(!!sym("period"), !!sym("region")) %>%
    mutate(xpos = 1:n()) %>%
    filter("\x13" != !!sym("region")) %>%
    droplevels()
  
  x <- x %>%
    inner_join(
      xpos,
      
      c("region", "period")
    )
  
  if (scenarioMarkers) {
    yMarker <- crossing(
      x %>%
        group_by(!!sym("scenario"), !!sym("xpos")) %>%
        summarise(top    = sum(pmax(0, !!sym("value"))),
                  bottom = sum(pmin(0, !!sym("value")))) %>%
        summarise(top    = max(!!sym("top")),
                  bottom = min(!!sym("bottom"))) %>%
        mutate(
          y = !!sym("bottom") - 0.05 * (!!sym("top") + !!sym("bottom"))) %>%
        select(-"top", -"bottom"),
      
      xpos
    )
  }
  
  if (scenarioMarkers) {
    scenarioMarkers <- setNames((1:20)[seq_along(unique(x$region))],
                                levels(x$region))
  }
  
  # calculate positions of period labels
  if (any(scenarioMarkers)) {
    xpos <- xpos %>%
      group_by(!!sym("period")) %>%
      summarise(xpos = mean(!!sym("xpos")))
  }
  
  if (is.null(colour)) {
    colour <- plotstyle(levels(x$variable))
  }
  
  # make plot
  p <- ggplot() +
    geom_col(data = x,
             mapping = aes(x = !!sym("xpos"), y = !!sym("value"),
                           fill = !!sym("variable"))) +
    scale_fill_manual(values = colour, name = NULL,
                      guide = guide_legend(reverse = TRUE)) +
    facet_wrap(~scenario, scales = "free_y") +
    labs(x = xlab, y = ylab, title = title) +
    theme(legend.position = "bottom")
  
  # add markers
  if (any(scenarioMarkers)) {
    p <- p +
      scale_x_continuous(breaks = xpos$xpos,
                         labels = xpos$period) +
      geom_point(data = yMarker,
                 mapping = aes(x = !!sym("xpos"), y = !!sym("y"),
                               shape = !!sym("region")),
                 size = 1.5) +
      scale_shape_manual(values = scenarioMarkers, name = NULL) +
      theme(legend.box = "vertical")
  } else {
    p <- p +
      scale_x_continuous(breaks = xpos$xpos,
                         labels = xpos %>%
                           unite(!!sym("label"), !!sym("region"),
                                 !!sym("period"), sep = " ") %>%
                           getElement("label")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  }
  
  return(p)
}

#' Show Multi-Line Plots by Variable
#'
#' Show plots with different regions in the same plot; x-axis variable chosen by
#' user.
#'
#' Same as \code{\link{showMultiLinePlots}} but with the variable specified by
#' \code{xVar} on x-axis. For every y-axis-value, we need a unique x-axis-value.
#' For historical data, there may be several sources / models of the same
#' variable. For the x-axis-variable a unique historical source / model is
#' chosen via \code{histRefModel}.
#'
#' @param xVar A single string. The variable for the x-axis.
#' @param showHistorical A single logical value. Should historical data be
#'   shown? It is not recommended to set this to \code{TRUE} as the resulting
#'   plot we probably be quite confusing.
#' @param histRefModel A named character vector identifying the unique model to
#'   be chosen for historical data. Use \code{options(mip.histRefModel=<value>)}
#'   to set globally.
#' @param yearsByVariable A numeric vector. The years to be marked in the plots.
#'   As default it uses the value globally set by \code{options(mip.yearsBarPlot=<value>)}.
#' @inheritParams showMultiLinePlots
#' @return \code{NULL} is returned invisible.
#' @section Example Plots:
#' \if{html}{page 1: \figure{showMultiLinePlotsByVariable1.png}{options: width="100\%"}}
#' \if{html}{page 2: \figure{showMultiLinePlotsByVariable2.png}{options: width="100\%"}}
#' @examples
#' \dontrun{
#' options(mip.mainReg = "World")
#' options(mip.yearsBarPlot = c(2010, 2030, 2050, 2100))
#' options(mip.histRefModel = c("GDP|PPP pCap" = "James_IMF"))
#' data <- as.quitte(data)
#' vars <- c(
#'   "FE|Transport pCap",
#'   "FE|Buildings pCap",
#'   "FE|Industry pCap")
#' showMultiLinePlotsByVariable(data, vars, "GDP|PPP pCap")
#' }
#' @export
#' @importFrom rlang .data .env
#' @importFrom tidyr drop_na
#' @importFrom ggplot2 ylim

showMultiLinePlotsByVariable_orig_ETP <- function(
  data, vars, xVar, scales = "free_y",
  showHistorical = FALSE,
  showETPorig = FALSE,
  mainReg = getOption("mip.mainReg"),
  histRefModel = getOption("mip.histRefModel"),
  yearsByVariable = getOption("mip.yearsBarPlot")
) {
  
  data <- as.quitte(data)
  
  # Validate function arguments.
  stopifnot(is.character(vars))
  stopifnot(is.character(xVar) && length(xVar) == 1)
  stopifnot(is.character(scales) && length(scales) == 1)
  stopifnot(identical(showHistorical, TRUE) || identical(showHistorical, FALSE))
  stopifnot(is.null(yearsByVariable) || is.numeric(yearsByVariable))
  checkGlobalOptionsProvided(c("mainReg", "histRefModel"))
  stopifnot(is.character(mainReg) && length(mainReg) == 1)
  stopifnot(is.character(histRefModel) && !is.null(names(histRefModel)))
  stopifnot(xVar %in% names(histRefModel))
  
  dy <- data %>%
    filter(.data$variable %in% .env$vars)
  dx <- data %>%
    filter(.data$variable %in% .env$xVar) %>%
    filter(.data$scenario != "historical" | .data$model == .env$histRefModel[.env$xVar])
  d <- dy %>%
    left_join(dx, by = c("scenario", "region", "period"), suffix = c("", ".x")) %>%
    drop_na(.data$value, .data$value.x) %>%
    arrange(.data$period) %>%
    droplevels()
  dMainScen <- d %>%
    filter(.data$region == .env$mainReg, .data$scenario != "historical") %>%
    droplevels()
  dMainHist <- d %>%
    filter(.data$region == .env$mainReg, .data$scenario == "historical") %>%
    droplevels()
  dRegiScen <- d %>%
    filter(.data$region != .env$mainReg, .data$scenario != "historical") %>%
    droplevels()
  dRegiHist <- d %>%
    filter(.data$region != .env$mainReg, .data$scenario == "historical") %>%
    droplevels()
  
  dRegiETPorig <- readSource("IEA_ETP", subtype = "transport", convert = F)
  dRegiETPorig <- as.quitte(dRegiETPorig)
  Mapping_IEA_ETP <- fread(system.file("extdata", "Mapping_IEA_ETP.csv", package = "edgeTrpLib"), header = TRUE) 
  setnames(Mapping_IEA_ETP,"IEA_ETP","variable")
  dRegiETPorig <- merge(dRegiETPorig, Mapping_IEA_ETP[, -c("Comment")], all.x = TRUE)
  dRegiETPorig <- as.data.table(dRegiETPorig)
  dRegiETPorig[, value := value*Conversion][, Conversion := NULL][, unit := NULL]
  dRegiETPorig <- dRegiETPorig[,.(value = sum(value)), by = .(REMIND, region, period, Unit_REMIND, scenario)]
  setnames(dRegiETPorig, c("REMIND","Unit_REMIND"), c("variable","unit"))
  dRegiETPorig[, model := paste0("IEA ETP ", scenario)][, scenario := "historical"]
  
  GDP_country = {
    x <- calcOutput("GDP", aggregate = F)
    getSets(x)[1] <- "ISO3"
    getSets(x)[2] <- "Year"
    x
  }
  POP_country = {
    x <- calcOutput("Population", aggregate = F)
    getSets(x)[1] <- "iso2c"
    x
  }
  
  
  GDP_country <- as.data.table(GDP_country)
  GDP_country <- GDP_country[, scenario := gsub("gdp_","", variable)][, variable := NULL]
  GDP_country[, period := as.numeric(gsub("y", "", Year))][, Year := NULL]
  POP_country <- as.data.table(POP_country)
  POP_country[, scenario := gsub("pop_","", variable)][, variable := NULL]
  POP_country[, period := as.numeric(gsub("y", "", year))][, conversion := 1e6][, Year := NULL]
  Map_ETP <- data.table(
    ETPreg = c("Brazil","China","India", "Mexico", "Russia", "South Africa", "United States"),
    ISO = c("BRA","CHN","IND", "MEX", "RUS", "ZAF", "USA")
  )
  POP_country[, value := value*conversion][, conversion := NULL]
  setnames(GDP_country, c("value", "ISO3"), c("gdp","ISO"))
  setnames(POP_country, c("value", "iso2c"), c("pop", "ISO"))
  GDP_country <- merge(GDP_country, Map_ETP, all.y = TRUE)
  POP_country <- merge(POP_country, Map_ETP, all.y = TRUE)

  dRegiETPorig <- merge(dRegiETPorig[, scenario := NULL], GDP_country, by.x = c("region","period"), by.y = c("ETPreg","period"), allow.cartesian = TRUE)
  dRegiETPorig <- dRegiETPorig[!is.na(value)]
  dRegiETPorig <- merge(dRegiETPorig, POP_country, by.x =c("region","period","scenario", "ISO"), by.y = c("ETPreg","period", "scenario", "ISO"))
  #Calculate pCap values
  dRegiETPorig[, value := value/pop]
  #Calculate GDP|PPP in kUSD2005 pCap 
  dRegiETPorig[, gdp := gdp/pop][, pop := NULL]
  dRegiETPorig[, model:= paste0(model, " ", scenario)][, variable := paste0(variable, " ", "p Cap")]
  
  
  #if (("OAS"|"NES"|"MEA"|"NEU"|"NEN"|"CAZ") %in% .data$region != .env$mainReg) regiETP <- rbind(regiETP,"NonOECD")
  #if (("CAZ"|"LAM"|"NEN"|"NEU"|"MEA"|"JPN"|"OAS"|"NES") %in% .data$region != .env$mainReg) regiETP <- rbind(regiETP,"OECD")
  if (("OAS") %in% .data$region != .env$mainReg) regiETP <- rbind(regiETP,"ASEAN")
  if (("LAM") %in% .data$region != .env$mainReg) regiETP <- rbind(regiETP,"Brazil")
  if (("CHA") %in% .data$region != .env$mainReg) regiETP <- rbind(regiETP,"China")
  if (("ENC"|"EWN"|"ECS"|"ESC"|"ECE"|"FRA"|"DEU"|"UKI"|"ESW"|"EUR") %in% .data$region != .env$mainReg) regiETP <- rbind(regiETP,"European Union")
  if (("IND") %in% .data$region != .env$mainReg) regiETP <- rbind(regiETP,"India")
  if (("LAM") %in% .data$region != .env$mainReg) regiETP <- rbind(regiETP,"Mexico")
  if (("REF") %in% .data$region != .env$mainReg) regiETP <- rbind(regiETP,"Russia")
  if (("SSA") %in% .data$region != .env$mainReg) regiETP <- rbind(regiETP,"South Africa")
  if (("USA") %in% .data$region != .env$mainReg) regiETP <- rbind(regiETP,"United States")
  if ("FE|Transport|Pass|Aviation|International"|"FE|Transport|Pass|Aviation|Domestic") .env$vars <- rbind(.env$vars, "FE|Transport|Pass|Aviation")
  if ("ES|Transport|Pass|Aviation|International"|"ES|Transport|Pass|Aviation|Domestic") .env$vars <- rbind(.env$vars, "ES|Transport|Pass|Aviation")
  
  dRegiETPorig <- dRegiETPorig %>%
    filter(regiETP, .env$xVar, .env$vars) %>%
    droplevels()
  
  
  
  regions <- levels(dRegiScen$region)
  
  warnMissingVars(dMainScen, vars)
  if (NROW(dMainScen) == 0) {
    warning("Nothing to plot.", call. = FALSE)
    return(invisible(NULL))
  }
  
  label <- paste0("[", paste0(levels(d$unit), collapse = ","), "]")
  xLabel <- paste0(xVar, " [", paste0(levels(d$unit.x), collapse = ","), "]")
  
  p1 <- dMainScen %>%
    ggplot(aes(.data$value.x, .data$value)) +
    geom_line(aes(linetype = .data$scenario)) +
    facet_wrap(vars(.data$variable), scales = scales) +
    theme_minimal() +
    expand_limits(y = 0) +
    ylab(label) + xlab(xLabel)
  p2 <- dRegiScen %>%
    ggplot(aes(.data$value.x, .data$value, color = .data$region)) +
    geom_line(aes(linetype = .data$scenario)) +
    facet_wrap(vars(.data$variable), scales = scales) +
    theme_minimal() +
    scale_color_manual(values = plotstyle(regions)) +
    expand_limits(y = 0) +
    ylab(label) + xlab(xLabel)
  if (showHistorical) {
    p1 <- p1 +
      geom_point(data = dMainHist, aes(shape = .data$model)) +
      geom_line(data = dMainHist, aes(group = paste0(.data$model, .data$region)), alpha = 0.5)
    p2 <- p2 +
      geom_point(data = dRegiHist, aes(shape = .data$model)) +
      geom_line(data = dRegiHist, aes(group = paste0(.data$model, .data$region)), alpha = 0.5)
  }
  if (showETPorig) {
    p2 <- p2 +
      geom_point(data = dRegiETPorig, aes(shape = .data$model)) +
      geom_line(data = dRegiETPorig, aes(group = paste0(.data$model, .dRegiETPorig$region)), alpha = 0.5)
  }
  # Add markers for certain years.
  if (length(yearsByVariable) > 0) {
    p1 <- p1 +
      geom_point(
        data = dMainScen %>%
          filter(.data$period %in% .env$yearsByVariable) %>%
          mutate(year = factor(.data$period)),
        mapping = aes(.data$value.x, .data$value, shape = .data$year))
    p2 <- p2 +
      geom_point(
        data = dRegiScen %>%
          filter(.data$period %in% .env$yearsByVariable) %>%
          mutate(year = factor(.data$period)),
        mapping = aes(.data$value.x, .data$value, shape = .data$year))
  }
  
  # Show plots.
  print(p1)
  cat("\n\n")
  print(p2)
  cat("\n\n")
  
  return(invisible(NULL))
}
