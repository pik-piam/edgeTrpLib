# Generates a warning if some of the variable names in the character vector vars
# are not entries of the variables column of the data frame data.
warnMissingVars <- function(data, vars) {
  available <- vars %in% unique(data$variable)
  missingVars <- vars[!available]
  if (length(missingVars) > 0)
    warning("Variables not found: ", paste(missingVars, collapse = ", "),
            call. = FALSE)
}

# Helper function to extract the legend of a ggplot object.
getLegend <- function(plt) {
  tmp <- ggplot_gtable(ggplot_build(plt))
  legIdx <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(legIdx) == 0) return(NULL)
  tmp$grobs[[legIdx[1]]]
}

# Helper function. Changes the value of variables given in numerators by dividing
# by denominator and multiplying conversionFactor. Sets unit of these variables to newUnit.
calacuateRatio <- function(data,
                           numerators,
                           denominator,
                           newUnit = "1",
                           conversionFactor = 1) {
    data %>%
      filter(variable == denominator) %>%
      rename(denom_value = value) %>%
      select(model, scenario, region, period, denom_value) ->
      denom
    data %>%
      filter(variable %in% numerators) %>%
      left_join(denom, c('model', 'scenario', 'region', 'period')) %>%
      mutate(value = value / denom_value * conversionFactor,
             unit = newUnit)
  }


# Creates 2 area plots (main region + others) and to bar plots (main region +
# others) of the variables specified in vars over time. For area plots,
# faceting is done by region and scenario; for bar plots over region. If a
# variables is given in tot, this is shown as a black line. If not the sum of
# the values of vars is drawn. If fill=TRUE, the values of vars are divided by
# the values of tot to show share of total. The plots arranged and shown and
# NULL is returned invisibly.
showAreaAndBarPlots <- function(data,
                                vars,
                                tot = NULL,
                                fill = FALSE) {
  # This function uses the 'global' variables: mainReg, yearsBarPlot.
  
  if (fill) {
    if (is.null(tot))
      stop("fill=TRUE without explicit tot variable is not implemented yet")
    data %>%
      calacuateRatio(vars, tot) ->
      data
  }
  
  data %>%
    filter(variable %in% vars, scenario != "historical") ->
    d
  warnMissingVars(d, vars)
  if (NROW(d) == 0) {
    warning("Nothing to plot.", call. = FALSE)
    return(invisible(NULL))
  }
  
  # order variables by mean value
  d %>%
    mutate(variable = fct_reorder(variable, value, mean, na.rm = TRUE)) ->
    d
  
  # Create plots.
  
  d %>%
    filter(region == mainReg) %>%
    mipArea(scales = "free_y", total = is.null(tot)) +
    theme(legend.position = "none") ->
    p1
  
  d %>%
    filter(region == mainReg, period %in% yearsBarPlot) %>%
    mipBarYearData() +
    theme(legend.position = "none") ->
    p2
  
  d %>%
    filter(region != mainReg, period %in% yearsBarPlot) %>%
    mipBarYearData() +
    ylab(NULL) +
    guides(fill = guide_legend(reverse = TRUE, ncol = 3)) ->
    p3
  
  d %>%
    filter(region != mainReg) %>%
    mipArea(scales = "free_y", total = is.null(tot)) +
    guides(fill = guide_legend(reverse = TRUE)) ->
    p4
  
  # Add black lines in area plots from variable tot if provided.
  if (!is.null(tot) && !fill) {
    p1 <- p1 +
      geom_line(
        data = data %>% filter(region == mainReg, variable == tot, scenario != "historical"),
        mapping = aes(period, value),
        size = 1.3
      )
    p4 <- p4 +
      geom_line(
        data = data %>% filter(region != mainReg, variable == tot, scenario != "historical"),
        mapping = aes(period, value),
        size = 1.3
      )
  }
  
  # Show plots.
  grid.arrange(p1, p2, p3, layout_matrix = rbind(c(1, 3), c(2, 3)))
  cat("\n\n")
  plot(p4)
  cat("\n\n")
  
  return(invisible(NULL))
}

# Two lineplots are shown (main region + others), depicting the values in
# vars over time. Faceting is done by region. For scales, choose either
# "free_y" or "fixed". The plots arranged and shown and NULL is returned
# invisibly.
showLinePlots <- function(data, vars = NULL, scales = "free_y") {
  # This function uses the 'global' variables: mainReg
  
  if (!is.null(vars)) {
    d <- filter(data, variable %in% vars)
    label <- paste0(vars, " [", paste0(unique(d$unit), collapse = ","), "]")
  } else {
    d <- data
    label <- paste0(paste0(unique(d$variable), collapse = ","),
                    " [",
                    paste0(unique(d$unit), collapse = ","),
                    "]")
  }
  d %>%
    filter(region == mainReg, scenario != "historical") ->
    dMainScen
  d %>%
    filter(region == mainReg, scenario == "historical") ->
    dMainHist
  d %>%
    filter(region != mainReg, scenario != "historical") ->
    dRegiScen
  d %>%
    filter(region != mainReg, scenario == "historical") ->
    dRegiHist
  
  if (!is.null(vars))
    warnMissingVars(bind_rows(dMainScen, dRegiScen), vars)
  if (NROW(dMainScen) == 0 && NROW(dRegiScen) == 0) {
    warning("Nothing to plot.", call. = FALSE)
    return(invisible(NULL))
  }
  if (NROW(dMainScen) == 0) {
    p1 <- ggplot() + theme_minimal()
  } else {
    dMainScen %>%
      mipLineHistorical(
        x_hist = d %>% filter(region == mainReg, scenario == "historical"),
        ylab = label,
        scales = scales,
        plot.priority = c("x_hist", "x", "x_proj")
      ) ->
      p1
  }
  if (NROW(dRegiScen) == 0) {
    p2 <- ggplot() + theme_minimal()
  } else {
    dRegiScen %>%
      mipLineHistorical(
        x_hist = d %>% filter(region != mainReg, scenario == "historical"),
        ylab = NULL,
        scales = scales,
        plot.priority = c("x_hist", "x", "x_proj"),
        facet.ncol = 3
      ) ->
      p2
  }
  
  # If a legend of the plots can be used as common legend for both plots,
  # show that legend below mainReg-plot and only that legend.
  mainHistModels <- unique(dMainHist$model)
  regiHistModels <- unique(dRegiHist$model)
  if (length(mainHistModels) == 0 || identical(mainHistModels, regiHistModels)) {
    lgnd <- getLegend(p2)
  } else if (length(regiHistModels) == 0) {
    lgnd <- getLegend(p1)
  } else {
    lgnd <- NULL
  }
  if (!is.null(lgnd)) {
    p1 <- arrangeGrob(
      p1 + theme(legend.position = "none"),
      lgnd,
      ncol = 1,
      heights = c(0.76, 0.24)
    )
    p2 <- p2 + theme(legend.position = "none")
  }
  
  # Show plots.
  grid.arrange(p1, p2, nrow = 1)
  cat("\n\n")
  
  return(invisible(NULL))
}


# Creates a line plot showing single line plot of vars over time
# Additionally target values given in variables of the form
# <vars>|target|<sth> are shown. The plot is shown and NULL is returned
# invisibly.
showLinePlotsWithTarget <- function(data, vars, scales = "free_y") {
  vars %>%
    paste0("|target|") %>%
    str_replace_all(fixed("|"), fixed("\\|")) %>%
    paste0(collapse = "|") ->
    targetPattern
  
  data %>%
    filter(str_detect(variable, targetPattern)) ->
    dTar
  regionsWithTarget <- unique(dTar$region)
  data %>%
    filter(variable %in% vars, region %in% regionsWithTarget) ->
    d
  warnMissingVars(d, vars)
  if (NROW(d) == 0) {
    warning("Nothing to plot.", call. = FALSE)
    return(invisible(NULL))
  }
  
  label <- paste0(vars, " [", paste0(unique(d$unit), collapse = ","), "]")
  d %>%
    filter(scenario != "historical") %>%
    mipLineHistorical(
      x_hist = d %>% filter(scenario == "historical"),
      ylab = label,
      scales = scales,
      plot.priority = c("x_hist", "x", "x_proj"),
      facet.ncol = 3
    ) +
    geom_hline(
      data = dTar,
      aes(yintercept = value),
      linetype = 2,
      color = "coral"
    ) +
    geom_vline(
      data = dTar,
      aes(xintercept = period),
      linetype = 2,
      color = "coral"
    ) +
    geom_text(data = dTar, aes(
      x = max(d$period) - (max(d$period) - min(d$period)) / 4,
      y = value,
      label = paste(variable, period)
    )) ->
    p
  
  # Show plot.
  print(p)
  cat("\n\n")
  
  return(invisible(NULL))
}


# Creates two plots (main region + others) with the values of vars over time.
# Different regions are shown in the same plot. Faceting is done by variable.
# The plots arranged and shown and NULL is returned invisibly.
showMultiLinePlots <- function(data, vars, scales = "fixed") {
  # This function uses the 'global' variables: mainReg, regions.
  
  data %>%
    filter(variable %in% vars) ->
    d
  d %>%
    filter(region == mainReg, scenario != "historical") ->
    dMainScen
  d %>%
    filter(region == mainReg, scenario == "historical") ->
    dMainHist
  d %>%
    filter(region != mainReg, scenario != "historical") ->
    dRegiScen
  d %>%
    filter(region != mainReg, scenario == "historical") ->
    dRegiHist
  
  warnMissingVars(dMainScen, vars)
  if (NROW(dMainScen) == 0) {
    warning("Nothing to plot.", call. = FALSE)
    return(invisible(NULL))
  }
  
  label <- paste0("[", paste0(unique(d$unit), collapse = ","), "]")
  
  dMainScen %>%
    ggplot(aes(period, value)) +
    geom_line(aes(linetype = scenario)) +
    geom_point(data = dMainHist, aes(shape = model)) +
    geom_line(data = dMainHist, aes(group = paste0(model, region)), alpha = 0.5) +
    facet_wrap(vars(variable), scales = scales) +
    theme_minimal() +
    ylim(0, NA) +
    ylab(label) ->
    p1
  
  dRegiScen %>%
    ggplot(aes(period, value, color = region)) +
    geom_line(aes(linetype = scenario)) +
    geom_point(data = dRegiHist, aes(shape = model)) +
    geom_line(data = dRegiHist, aes(group = paste0(model, region)), alpha = 0.5) +
    facet_wrap(vars(variable), scales = scales) +
    theme_minimal() +
    scale_color_manual(values = plotstyle(regions)) +
    ylim(0, NA) +
    ylab(label) ->
    p2
  
  # Show plots.
  print(p1)
  cat("\n\n")
  print(p2)
  cat("\n\n")
  
  return(invisible(NULL))
}


# Same as showMultiLinePlots() but with GDP on x-axis. When plotting by GDP,
# data from historical.mif is only shown for years where historical GDP is
# available. The plots arranged and shown and NULL is returned invisibly.
showMultiLinePlotsByGDP <- function(data, vars, scales = "fixed") {
  # This function uses the 'global' variables: mainReg.
  
  data %>%
    filter(variable %in% vars) ->
    d
  d %>%
    filter(region == mainReg, scenario != "historical") ->
    dMainScen
  d %>%
    filter(region == mainReg, scenario == "historical") ->
    dMainHist
  d %>%
    filter(region != mainReg, scenario != "historical") ->
    dRegiScen
  d %>%
    filter(region != mainReg, scenario == "historical") ->
    dRegiHist
  
  warnMissingVars(dMainScen, vars)
  if (NROW(dMainScen) == 0) {
    warning("Nothing to plot.", call. = FALSE)
    return(invisible(NULL))
  }
  
  label <- paste0("[", paste0(unique(d$unit), collapse = ","), "]")
  
  dMainScen %>%
    ggplot(aes(gdp, value)) +
    geom_line(aes(linetype = scenario)) +
    geom_point(data = dMainHist, aes(shape = model)) +
    geom_line(data = dMainHist, aes(group = paste0(model, region)), alpha = 0.5) +
    facet_wrap(vars(variable), scales = scales) +
    theme_minimal() +
    ylim(0, NA) +
    ylab(label) + xlab("GDP PPP pCap (kUS$2005)") ->
    p1
  
  dRegiScen %>%
    ggplot(aes(gdp, value, color = region)) +
    geom_line(aes(linetype = scenario)) +
    geom_point(data = dRegiHist, aes(shape = model)) +
    geom_line(data = dRegiHist, aes(group = paste0(model, region)), alpha = 0.5) +
    facet_wrap(vars(variable), scales = scales) +
    theme_minimal() +
    scale_color_manual(values = plotstyle(regions)) +
    ylim(0, NA) +
    ylab(label) + xlab("GDP PPP pCap (kUS$2005)") ->
    p2
  
  # Show plots.
  print(p1)
  cat("\n\n")
  print(p2)
  cat("\n\n")
  
  return(invisible(NULL))
}
