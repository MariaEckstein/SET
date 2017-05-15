add_response_times_to_plot = function(ggplot.object, RT_data, rtbox = .008, dist = .008) {
  
  ### Add y-position to the response time data (for 3 groups, nrow == 9; for 2 groups, nrow == 6)
  if (nrow(RT_data == 9)) {
    RT_data$y = rep(c(rtbox - 2*dist, rtbox - dist, rtbox), 3)
  } else if (nrow(RT_data == 6)) {
    RT_data$y = rep(c(rtbox - 2*dist, rtbox - dist, rtbox), 2)
  }
  RT_data_long = melt(RT_data, id.vars = c("pupilpatterngroup", "bigspan", "y"))
  
  ### Add response times and box around response times to the plot
  ggplot.object +
    geom_point(data = RT_data, aes(x = RT_median, y = y, color = bigspan), inherit.aes = F) +
    geom_line(data = subset(RT_data_long, variable %in% c("RT_lower", "RT_upper")), aes(x = value, y = y)) +
    geom_rect(aes(xmin = 6, xmax = 9, ymin = (rtbox - 3*dist), ymax = rtbox + dist), color = "black", fill = "transparent", inherit.aes = F)
}
