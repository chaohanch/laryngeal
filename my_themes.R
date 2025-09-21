erp_theme <- theme(
  axis.line.x = element_blank(),
  axis.line.y = element_blank(),
  # axis.title.x = element_blank(),
  # axis.title.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  legend.title = element_blank(),
  legend.direction = "vertical",
  legend.margin = margin(0,0,0,0),
  # text = element_text(size = 14),
  # title = element_text(size = 12),
  # plot.title = element_text(hjust = 0.5,
  #                           margin = margin(b = 0)),
)



zero_lines <- list(
  geom_vline(xintercept = 0, linetype = "solid", alpha = 0.2),
  geom_hline(yintercept = 0, linetype = "solid", alpha = 0.2)
)