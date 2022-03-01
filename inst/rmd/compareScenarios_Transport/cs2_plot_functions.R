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

