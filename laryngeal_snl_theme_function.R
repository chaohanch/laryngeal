# erp descriptive plot function
myfunc_erp_plot <- function(data, title, sig_times, ylimit) {
  ggplot(data, aes(time, amp)) +
    stat_summary(fun.data = mean_cl_normal, geom = "ribbon", linewidth = 1,
                 aes(fill = stim_role),
                 alpha = 0.3, show.legend = FALSE) +
    stat_summary(fun = mean, geom = "line", linewidth = 1, aes(color = stim_role)) +
    
    # colors and fill
    scale_color_manual(values = c(
      "standard"="#0571b0",
      "deviant"="#ca0020"
    )) +
    scale_fill_manual(values = c(
      "standard"="#0571b0",
      "deviant"="#ca0020"
    )) +
    
    # add lines for x and y zeros
    zero_lines +
    
    # rug plot
    geom_rug(data = data.frame(time = sig_times),
             aes(x = time),
             sides = "b",
             color = "black",
             inherit.aes = FALSE
    ) +
    
    # y limits
    coord_cartesian(ylim = ylimit) +
    
    scale_x_continuous(
      breaks = seq(0, 700, by = 200),  # Specify the desired breaks
      # labels = c("(ms)", 0, 200, 400, 600, 800)
    ) +
    scale_y_continuous(
      # labels = number_format(accuracy = 0.1),
      breaks = c(-2, -1, 0, 1, 2),  # Specify the desired breaks
      # labels = c(-1, 0, 1, "(μV)")
    ) +
    labs(
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      # axis.title.x = element_blank(),
      # axis.title.y = element_text(angle = 90, vjust = 0, hjust = -0.1),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      # axis.text.x = element_blank(),
      # legend.text = element_blank(),
      legend.title = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.2, 0.9),
      # legend.margin = margin(0,0,0,0),
      text = element_text(size = 16),
      # title = element_text(size = 12),
      # plot.title = element_text(hjust = 0.5,
      #                           margin = margin(b = 0)),
    ) +
    labs(x = "Time (ms)", y = "Amplitude (μV)", title = title)
}


# extract data function
myfunct_erp_getdata <- function(ppt_group, conds, chans) {
  df_abs <- df_erp %>%
    filter(condition %in% conds,
           group %in% ppt_group) %>%
    mutate( amp = rowMeans(across(all_of(chans))) ) %>%
    select(participant, group, condition, time, amp) %>%
    mutate(stim_role = case_when(
      endsWith(condition, "_stan") ~ "standard",
      endsWith(condition, "_devi") ~ "deviant",
      .default = NA)) %>%
    mutate(stim_role = factor(stim_role, levels = c("standard", "deviant"))) %>%
    droplevels()
  
  return(df_abs)
}


# correlation plot
myfunc_cor_plot <- function(data, y_label) {
  ggplot(data = data,
         mapping = aes(x = slope, y = amp, 
                       group = group, color = group, fill = group
         )) +
    facet_wrap(vars(poa)) +
    scale_color_manual(values = c("F0"="#BC770B", "VOT"="#7030EB")) +
    scale_fill_manual(values = c("F0"="#BC770B", "VOT"="#7030EB")) +
    geom_point(size = 1.2, alpha = 0.8) +
    # coord_cartesian(ylim = c(-7, 7),
    #                 xlim = c(0, 10)) +
    # scale_x_continuous(breaks = seq(1,10, 1)) +
    geom_smooth(method = "lm",
                se = TRUE,
                linewidth = 0.5,
                alpha = 0.1,
                na.rm = TRUE) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      # axis.title.x = element_blank(),
      # axis.title.y = element_text(angle = 90, vjust = 0, hjust = -0.1),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      # axis.text.x = element_blank(),
      # legend.text = element_blank(),
      legend.title = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.1, 0.85),
      # legend.margin = margin(0,0,0,0),
      text = element_text(size = 16),
      # title = element_text(size = 12),
      # plot.title = element_text(hjust = 0.5,
      #                           margin = margin(b = 0)),
    ) +
    labs(x = "Cue weight", y = y_label)
}


# add zero lines ####
zero_lines <- list(
  geom_vline(xintercept = 0, linetype = "solid", alpha = 0.2),
  geom_hline(yintercept = 0, linetype = "solid", alpha = 0.2)
)

