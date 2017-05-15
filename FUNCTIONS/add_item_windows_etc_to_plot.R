add_item_windows_etc_to_plot = function(ggplot.object, yaxmin = .15, yaxmax = .25, text_size = 3, faceti) {
  
  # Create dataframes that specify fixation periods in between items, when oddball is presented, and time of response prompt
  fixation_periods = data.frame(xmin = c(-2, seq(1, 5.5, 1.5), 6.5),
                                xmax = c(0, seq(1.5, 6, 1.5), 9),
                                ymin = rep(-Inf, 6),
                                ymax = rep(Inf, 6))
  response_prompt  = data.frame(x = c(6, 6),
                                y = c(-Inf, Inf))
  oddballs         = data.frame(oddball = c("SET", "noSET (3)", "noSET (4)"),
                                xmin    = c(0,     3,           4.5),
                                xmax    = c(0,     4,           5.5),
                                ymin    = rep(-Inf, 3),
                                ymax    = rep(Inf, 3))
  # Add the stuff to the plot
  ggplot.object = ggplot.object +
    geom_rect(data = fixation_periods, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = .2, inherit.aes = FALSE) +
    labs(x = 'Time (s)', y = 'TEPR (mm)', color = ' ', fill = ' ') +
    geom_line(data = response_prompt, aes(x = x, y = y), size = 1, inherit.aes = FALSE) +
    annotate('text', x = 0.5, y = yaxmin, label = 'Item1', size = text.size, inherit.aes = FALSE) +
    annotate('text', x = 2.0, y = yaxmin, label = 'Item2', size = text.size, inherit.aes = FALSE) +
    annotate('text', x = 3.5, y = yaxmin, label = 'Item3', size = text.size, inherit.aes = FALSE) +
    annotate('text', x = 5.0, y = yaxmin, label = 'Item4', size = text.size, inherit.aes = FALSE) +
    annotate('text', x = 6.75,y = yaxmin, label = 'Resp.', size = text.size, inherit.aes = FALSE) +
    annotate('text', x = 8.0, y = yaxmin, label = 'Fix.',  size = text.size, inherit.aes = FALSE) +
    scale_x_continuous(breaks = seq(0, 9, 1.5), limits = c(0, 9))# +
    # scale_y_continuous(breaks = seq(yaxmin, yaxmax, .05), labels = round(seq(yaxmin, yaxmax, .05), 2))
  
  if (faceti == "oddball") {
    ggplot.object = ggplot.object +
      geom_rect(data = oddballs, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = "red"), alpha = .2, inherit.aes = FALSE) +
      scale_fill_manual(values = c(span_colors(4), oddball_window_colors(1)), guide = FALSE)
  }
  ggplot.object
}
