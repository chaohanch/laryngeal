library(tidyverse)
library(cowplot)
library(ggpubr)
library(magick)
library(lmerTest)
library(car)
library(emmeans)

source("~/Documents/GitHub/laryngeal/laryngeal_snl_theme_function.R")

# participants to include ####
df_ppt <- read_delim("~/OneDrive - University of Toronto/Projects/Laryngeal/data_analysis/participants_include.txt", delim = "\t") %>%
  filter(include == 1)

df_erp <- read_delim("~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/erp_all.txt", delim = "\t") %>%
  filter(participant %in% df_ppt$participant)

# draw stimuli ####
df_stim <- tibble(
  "F0" = c( rep(c(74, 79, 83), each = 6), rep(c(132, 140, 148), each = 3) ),
  "VOT" = c( rep(c(5, 8, 11, 47, 50, 53), times = 3), rep(c(5, 8, 11), times = 3) ),
  "category" = c( rep(c("g", "k"), each = 3, time = 3), rep(c("k"), times = 9) ),
) %>%
  mutate(category = factor(category, levels = c("g", "k"), labels = c("intended /g/", "intended /k/")))

fig <-
  ggplot(data = df_stim) +
  geom_point(aes(x = VOT, y = F0, color = category), size = 3) +
  scale_x_continuous(breaks = c(5, 8, 11, 47, 50, 53), guide = guide_axis(angle = 90)) +
  scale_y_continuous(breaks = c(74, 79, 83, 132, 140, 148)) +
  scale_color_manual(values = c("#F8766D", "#00BA38")) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    text = element_text(size = 16),
    # title = element_text(size = 12),
    legend.position = "inside",
    legend.position.inside = c(0.5, 0.5),
    legend.title = element_blank()) + # change the position of legend. "none" to hide legend
  labs(title = "Dorsal", x = "VOT")
fig
# ggsave(plot = fig, width = 4, height = 4, units = "in", dpi = 300, filename = "OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/snl_stims.jpg")

## glottal stims ####
df_stim <- tibble(
  "F0" = c( rep(c(74, 79, 83), each = 6), rep(c(132, 140, 148), each = 3) ),
  "VOT" = c( rep(c(80, 83, 86, 122, 125, 128), times = 3), rep(c(80, 83, 86), times = 3) ),
  "category" = c( rep(c("h"), times = 27) ),
) %>%
  mutate(category = factor(category, levels = c("h"), labels = c("/h/")))

fig <-
  ggplot(data = df_stim) +
  geom_point(aes(x = VOT, y = F0, color = category), size = 3) +
  scale_x_continuous(breaks = c(80, 83, 86, 122, 125, 128), guide = guide_axis(angle = 90)) +
  scale_y_continuous(breaks = c(74, 79, 83, 132, 140, 148)) +
  scale_color_manual(values = c("#AFABAB")) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    text = element_text(size = 16),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(color="white"),
    # title = element_text(size = 12),
    legend.position = "inside",
    legend.position.inside = c(0.5, 0.5),
    legend.title = element_blank()) + # change the position of legend. "none" to hide legend
  labs(title = "Glottal", x = " ", y = " ")
fig
# ggsave(plot = fig, width = 4, height = 4, units = "in", dpi = 300, filename = "OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/snl_stims_glottal.jpg")


# behavioral results ####

## categorical function ####
df_norm <- read_delim(file = "~/OneDrive - University of Toronto/Projects/Laryngeal/data_analysis/df_behav_norm.txt")
fig <-
  ggplot(df_norm, aes(cue_value, resp)) + 
  facet_wrap(~expName) +
  # geom_point() + 
  # inidividual function
  stat_smooth(aes(cue_value, resp, group = part), geom = "line", alpha = 0.7, color = "grey", method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  # average over all individuals
  stat_smooth(aes(color = expName, fill = expName), method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  scale_color_manual(values = c(
    "F0"="#BC770B",
    "VOT"="#7030EB"
  )) +
  scale_fill_manual(values = c(
    "F0"="#BC770B",
    "VOT"="#7030EB"
  )) +
  labs(
    x = "Normalized cue value",
    y = "Proportion of /k/ responses",
    title = "Fitted logistic regression curve"
  ) +
  # inidividual
  theme_bw() +
  theme(
    strip.background = element_blank(),
    text = element_text(size = 16),
    # title = element_text(size = 12),
    legend.position = "none",
    legend.title = element_blank()) # change the position of legend. "none" to hide legend
fig
# ggsave(plot = fig, width = 6, height = 4.5, units = "in", dpi = 300, filename = "~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/snl_behav_categorical_function.jpg")


## threshold distribution ####

### english ####
df_thresh_yj <- read_delim(file = "~/OneDrive - University of Toronto/Projects/Laryngeal/data_analysis/df_thresh_english.txt", delim = "\t")

fig_dist <-
  ggplot(data = df_thresh_yj, aes(group = group, fill = group)) +
  geom_histogram(aes(x = as.numeric(slope)), binwidth = 0.1, position = "identity", alpha = 0.5, color = "white") +
  # geom_density(aes(x = as.numeric(slope)), alpha = 0.5, color = NA) +
  scale_fill_manual(values = c(
    "F0"="#BC770B",
    "VOT"="#7030EB"
  )) +
  # scale_x_continuous(limits = c(-0.5, 2.5)) +
  # scale_x_continuous(
  #   breaks = seq(-0.2, 0.8, by = 0.2),  # Specify the desired breaks
  #   # labels = c("(ms)", 0, 200, 400, 600, 800)
  # ) +
  scale_y_continuous(
    breaks = seq(0, 10, by = 2),  # Specify the desired breaks
    # labels = c("(ms)", 0, 200, 400, 600, 800)
  ) +
  # coord_cartesian(xlim = c(-1, 2.5)) +
  theme_bw() +
  theme(
    # change font size of non-title/non-label texts
    text = element_text(size = 16),
    # change font size of title/label texts
    # title = element_text(size = 18)
    legend.title = element_blank(),
    legend.position = c(0.2, 0.8)
  ) + 
  labs(x = "Cue weight/Slope", # x-axis label
       y = "Number of participants",
       title = "Indiviual slope distribution") # y-axis label
fig_dist
# ggsave(plot = fig_dist, width = 4, height = 5, units = "in", dpi = 300, filename = "~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/snl_behav_threshold.jpg")

### korean ####
df_thresh_yj <- read_delim(file = "~/OneDrive - University of Toronto/Projects/Laryngeal/Seoul Data/data_analysis/df_thresh_korean.txt", delim = "\t")

fig_dist <-
  ggplot(data = df_thresh_yj, aes(group = group, fill = group)) +
  geom_histogram(aes(x = as.numeric(slope)), binwidth = 0.1, position = "identity", alpha = 0.5, color = "white") +
  # geom_density(aes(x = as.numeric(slope)), alpha = 0.5, color = NA) +
  scale_fill_manual(values = c(
    "F0"="#BC770B",
    "VOT"="#7030EB"
  )) +
  # scale_x_continuous(limits = c(-0.5, 2.5)) +
  # scale_x_continuous(
  #   breaks = seq(-0.2, 0.8, by = 0.2),  # Specify the desired breaks
  #   # labels = c("(ms)", 0, 200, 400, 600, 800)
  # ) +
  scale_y_continuous(
    breaks = seq(0, 10, by = 2),  # Specify the desired breaks
    # labels = c("(ms)", 0, 200, 400, 600, 800)
  ) +
  # coord_cartesian(xlim = c(-1, 2.5)) +
  theme_bw() +
  theme(
    # change font size of non-title/non-label texts
    text = element_text(size = 16),
    # change font size of title/label texts
    # title = element_text(size = 18)
    legend.title = element_blank(),
    legend.position = c(0.2, 0.8)
  ) + 
  labs(x = "Cue weight/Slope", # x-axis label
       y = "Number of participants",
       title = "Indiviual slope distribution") # y-axis label
fig_dist

# demographic
df_demo <- read_delim("~/OneDrive - University of Toronto/Projects/Laryngeal/Seoul Data/data_analysis/background questionnaire.txt", delim = "\t") %>%
  select(`Participant ID`, `Language 2: Speaking`:`Language 2: Writing`) %>%
  separate(col = `Participant ID`, into = c("number", "stim")) %>%
  unite(col = ppt, stim,number, sep="_") %>%
  pivot_longer(cols = `Language 2: Speaking`:`Language 2: Writing`, names_to = "language_aspect", values_to = "language_level")

df_cor <- df_thresh_yj %>%
  inner_join(df_demo, by = "ppt")

sort(unique(df_thresh_yj$ppt))
sort(unique(df_demo$ppt))


# check ppt numbers
length(unique(df_cor[df_cor$group=="VOT",]$ppt))
length(unique(df_cor[df_cor$group=="F0",]$ppt))

# correlation plot
fig <-
  ggplot(data = df_cor,
         mapping = aes(x = language_level, y = slope, 
         )) +
  facet_grid(group ~ language_aspect) +
  geom_point(size = 1, alpha = 0.5) +
  # coord_cartesian(ylim = c(-7, 7),
  #                 xlim = c(0, 10)) +
  # scale_x_continuous(breaks = seq(1,10, 1)) +
  geom_smooth(method = "lm",
              se = TRUE,
              linewidth = 0.5,
              alpha = 0.1,
              na.rm = TRUE) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0, #adjust the label in x axis
           label.y.npc = 0.2, #adjust the label in y axis
           size = 3) +
  # labs(x = "Rating", y = "Normalized modeled peak") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "top")
print(fig)
# save 

# erp descriptive plot ####

# vot, dorsal, short ####
chans <- c('FC1', 'C3', 'CP5', 'CP1', 'Pz', 'P3', 'P7', 'O1', 'Oz', 'O2', 'P4', 'P8', 'CP6',
          'CP2', 'Cz', 'C4')
ppt_group <- "VOT"
conds <- c("dorsal_lowStan_highDevi_stan", "dorsal_highStan_lowDevi_devi")
df_abs <- myfunct_erp_getdata(ppt_group, conds, chans)

# absolute waves
ylimit <- c(-3, 3)
title <- "VOT group: dorsal shortVOT (/ga/) deviant (*)"
sig_times <- df_abs$time[df_abs$time >= 308 & df_abs$time <= 464]
fig_abs <- myfunc_erp_plot(df_abs, title, sig_times, ylimit)

# add topo
fig_topo <- image_read("~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/topo_VOT_iMMN_dorsal_shortVOT_0.jpg")

# combine figures
fig <-
  ggdraw() +
  draw_plot(fig_abs, x = 0, y = 0, width = 1, height = 1) +
  draw_image(fig_topo, x = 3/5, y = 3/5, width = 1/4, height = 1/4)
fig
# ggsave(plot = fig, width = 7, height = 5, units = "in", dpi = 300, filename = "~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/snl_vot_dorsal_short.jpg")

# vot, dorsal, long ####
chans <- c('Cz', 'Fz', 'FC1', 'FC2', 'CP1', 'CP2', 'C3', 'C4', 'Pz')
ppt_group <- "VOT"
conds <- c("dorsal_highStan_lowDevi_stan", "dorsal_lowStan_highDevi_devi")
df_abs <- myfunct_erp_getdata(ppt_group, conds, chans)

# absolute waves
ylimit <- c(-3, 3)
title <- "VOT group: dorsal longVOT (/ka/) deviant (n.s.)"
sig_times <- df_abs$time[df_abs$time >= NA & df_abs$time <= NA]
fig_abs <- myfunc_erp_plot(df_abs, title, sig_times, ylimit)

# add topo
fig_topo <- image_read("~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/topo_blank.jpg")

# combine figures
fig <-
  ggdraw() +
  draw_plot(fig_abs, x = 0, y = 0, width = 1, height = 1) +
  draw_image(fig_topo, x = 3/5, y = 3/5, width = 1/4, height = 1/4)
fig
# ggsave(plot = fig, width = 7, height = 5, units = "in", dpi = 300, filename = "~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/snl_vot_dorsal_long.jpg")


# vot, glottal short ####
chans <- c('Cz', 'Fz', 'FC1', 'FC2', 'CP1', 'CP2', 'C3', 'C4', 'Pz')
ppt_group <- "VOT"
conds <- c("glottal_lowStan_highDevi_stan", "glottal_highStan_lowDevi_devi")
df_abs <- myfunct_erp_getdata(ppt_group, conds, chans)

# absolute waves
ylimit <- c(-3, 3)
title <- "VOT group: glottal shortVOT (/ha/) deviant (n.s.)"
sig_times <- df_abs$time[df_abs$time >= NA & df_abs$time <= NA]
fig_abs <- myfunc_erp_plot(df_abs, title, sig_times, ylimit)

# add topo
fig_topo <- image_read("~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/topo_blank.jpg")

# combine figures
fig <-
  ggdraw() +
  draw_plot(fig_abs, x = 0, y = 0, width = 1, height = 1) +
  draw_image(fig_topo, x = 3/5, y = 3/5, width = 1/4, height = 1/4)
fig
ggsave(plot = fig, width = 7, height = 5, units = "in", dpi = 300, filename = "~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/snl_vot_glottal_short.jpg")


# vot, glottal, long ####
chans <- c('Cz', 'Fz', 'FC1', 'FC2', 'CP1', 'CP2', 'C3', 'C4', 'Pz')
ppt_group <- "VOT"
conds <- c("glottal_highStan_lowDevi_stan", "glottal_lowStan_highDevi_devi")
df_abs <- myfunct_erp_getdata(ppt_group, conds, chans)

# absolute waves
ylimit <- c(-3, 3)
title <- "VOT group: glottal longVOT (/ha/) deviant (n.s.)"
sig_times <- df_abs$time[df_abs$time >= NA & df_abs$time <= NA]
fig_abs <- myfunc_erp_plot(df_abs, title, sig_times, ylimit)

# add topo
fig_topo <- image_read("~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/topo_blank.jpg")

# combine figures
fig <-
  ggdraw() +
  draw_plot(fig_abs, x = 0, y = 0, width = 1, height = 1) +
  draw_image(fig_topo, x = 3/5, y = 3/5, width = 1/4, height = 1/4)
fig
ggsave(plot = fig, width = 7, height = 5, units = "in", dpi = 300, filename = "~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/snl_vot_glottal_long.jpg")



# f0, dorsal, high ####
chans <- c('Fz', 'F3', 'FC5', 'FC1', 'C3', 'CP5', 'CP1', 'Pz', 'P3', 'P7', 'O1', 'Oz', 'O2', 'P4', 'CP6', 'CP2', 'Cz', 'C4', 'FC6', 'FC2', 'F4')
ppt_group <- "F0"
conds <- c("dorsal_highStan_lowDevi_stan", "dorsal_lowStan_highDevi_devi")
df_abs <- myfunct_erp_getdata(ppt_group, conds, chans)

# absolute waves
ylimit <- c(-3, 3)
title <- "F0 group: dorsal highF0 (/ka/) deviant (*)"
sig_times <- df_abs$time[df_abs$time >= 192 & df_abs$time <= 280]
fig_abs <- myfunc_erp_plot(df_abs, title, sig_times, ylimit)

# add topo
fig_topo <- image_read("~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/topo_F0_iMMN_dorsal_highF0_1.jpg")

# combine figures
fig <-
  ggdraw() +
  draw_plot(fig_abs, x = 0, y = 0, width = 1, height = 1) +
  draw_image(fig_topo, x = 3/5, y = 3/5, width = 1/4, height = 1/4)
fig
ggsave(plot = fig, width = 7, height = 5, units = "in", dpi = 300, filename = "~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/snl_f0_dorsal_high.jpg")


# f0, dorsal, low ####
chans <- c('Cz', 'Fz', 'FC1', 'FC2', 'CP1', 'CP2', 'C3', 'C4', 'Pz')
ppt_group <- "F0"
conds <- c("dorsal_lowStan_highDevi_stan", "dorsal_highStan_lowDevi_devi")
df_abs <- myfunct_erp_getdata(ppt_group, conds, chans)

# absolute waves
ylimit <- c(-3, 3)
title <- "F0 group: dorsal lowF0 (/ga/) deviant (n.s.)"
sig_times <- df_abs$time[df_abs$time >= NA & df_abs$time <= NA]
fig_abs <- myfunc_erp_plot(df_abs, title, sig_times, ylimit)

# add topo
fig_topo <- image_read("~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/topo_blank.jpg")

# combine figures
fig <-
  ggdraw() +
  draw_plot(fig_abs, x = 0, y = 0, width = 1, height = 1) +
  draw_image(fig_topo, x = 3/5, y = 3/5, width = 1/4, height = 1/4)
fig
ggsave(plot = fig, width = 7, height = 5, units = "in", dpi = 300, filename = "~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/snl_f0_dorsal_low.jpg")


# f0, glottal, high ####
chans <- c('Fz', 'F3', 'FC5', 'FC1', 'C3', 'CP5', 'CP1', 'Pz', 'P3', 'P4', 'CP6', 'CP2', 'Cz', 'C4', 'FC6', 'FC2', 'F4')
ppt_group <- "F0"
conds <- c("glottal_highStan_lowDevi_stan", "glottal_lowStan_highDevi_devi")
df_abs <- myfunct_erp_getdata(ppt_group, conds, chans)

# absolute waves
ylimit <- c(-3, 3)
title <- "F0 group: glottal highF0 (/ha/) deviant (*)"
sig_times <- df_abs$time[df_abs$time >= 244 & df_abs$time <= 344]
fig_abs <- myfunc_erp_plot(df_abs, title, sig_times, ylimit)

# add topo
fig_topo <- image_read("~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/topo_F0_iMMN_glottal_highF0_0.jpg")

# combine figures
fig <-
  ggdraw() +
  draw_plot(fig_abs, x = 0, y = 0, width = 1, height = 1) +
  draw_image(fig_topo, x = 3/5, y = 3/5, width = 1/4, height = 1/4)
fig
ggsave(plot = fig, width = 7, height = 5, units = "in", dpi = 300, filename = "~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/snl_f0_glottal_high.jpg")


# f0, glottal, low ####
chans <- c('Cz', 'Fz', 'FC1', 'FC2', 'CP1', 'CP2', 'C3', 'C4', 'Pz')
ppt_group <- "F0"
conds <- c("glottal_lowStan_highDevi_stan", "glottal_highStan_lowDevi_devi")
df_abs <- myfunct_erp_getdata(ppt_group, conds, chans)

# absolute waves
ylimit <- c(-3, 3)
title <- "F0 group: glottal lowF0 (/ha/) deviant (n.s.)"
sig_times <- df_abs$time[df_abs$time >= NA & df_abs$time <= NA]
fig_abs <- myfunc_erp_plot(df_abs, title, sig_times, ylimit)

# add topo
fig_topo <- image_read("~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/topo_blank.jpg")

# combine figures
fig <-
  ggdraw() +
  draw_plot(fig_abs, x = 0, y = 0, width = 1, height = 1) +
  draw_image(fig_topo, x = 3/5, y = 3/5, width = 1/4, height = 1/4)
fig
ggsave(plot = fig, width = 7, height = 5, units = "in", dpi = 300, filename = "~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/snl_f0_glottal_low.jpg")


# gam modeling english ####

df_gam_hasPeak <- read_delim("~/OneDrive - University of Toronto/Projects/Laryngeal/data_analysis/gam/df_gam_hasPeak_english.txt", delim = "\t") #%>%
  filter(half_area_latency >= 150 & half_area_latency <= 600)

# check sample size
df_gam_hasPeak %>% group_by(group, poa, direction) %>% dplyr::summarize(n = n())

# plot data
df_plot <- df_gam_hasPeak

# stats data
df_mod <- df_gam_hasPeak %>%
  mutate(group = factor(group, levels = c("VOT", "F0")),
         poa = factor(poa, levels = c("dorsal", "glottal")),
         direction = factor(direction, levels = c("highStan_lowDevi", "lowStan_highDevi")))
# set contrast
F0_VOT <- c(-1/2, 1/2)
contrasts(df_mod$group) <- cbind(F0_VOT)

dorsal_glottal <- c(-1/2, 1/2)
contrasts(df_mod$poa) <- cbind(dorsal_glottal)

HighLow_LowHigh <- c(-1/2, 1/2)
contrasts(df_mod$direction) <- cbind(HighLow_LowHigh)

## normalized modeled peak ####
# plot
fig <- 
  ggplot(data = df_plot) +
  facet_wrap(~group) +
  geom_boxplot(aes(x = poa, y = NMP, fill = direction)) +
  theme_bw() +
  theme(legend.position = "bottom")
print(fig)

# model
mod <- lmer(NMP ~ group*poa*direction + (1|ppt), data = df_mod)
summary(mod)
Anova(mod, type="III")
pair_comp <- emmeans(mod, pairwise ~ group | poa)
test(pair_comp)

## fractional area latency ####
# plot
fig <- 
  ggplot(data = df_plot) +
  facet_wrap(~group) +
  geom_boxplot(aes(x = poa, y = half_area_latency, fill = direction)) +
  theme_bw() +
  theme(legend.position = "bottom")
print(fig)

# model
mod <- lmer(half_area_latency ~ group*poa*direction + (1|ppt), data = df_mod)
summary(mod)
Anova(mod, type="III")
pair_comp <- emmeans(mod, pairwise ~ group | poa)
test(pair_comp)

# Brain-behavioral correlation english ####

df_thresh <- read_delim(file = "~/OneDrive - University of Toronto/Projects/Laryngeal/data_analysis/df_thresh_english.txt", delim = "\t")
df_gam_hasPeak <- read_delim("~/OneDrive - University of Toronto/Projects/Laryngeal/data_analysis/gam/df_gam_hasPeak_english.txt", delim = "\t") %>%
  filter(half_area_latency >= 150 & half_area_latency <= 600)

df_cor <- df_thresh %>%
  mutate(slope = as.numeric(slope)) %>%
  inner_join(df_gam_hasPeak, by = c("ppt", "group"))

## NMP ####
df_nmp <- df_cor %>%
  select(slope:direction, NMP) %>%
  pivot_wider(names_from = direction, values_from = NMP) %>%
  mutate(amp = rowMeans(across(c(highStan_lowDevi, lowStan_highDevi))),
         poa = factor(poa, levels = c("dorsal", "glottal"), labels = c("Dorsal", "Glottal")))

fig <- myfunc_cor_plot(df_nmp, "Normalized modeled peak") +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0, #adjust the label in x axis
           label.y.npc = 0.2, #adjust the label in y axis
           size = 4.5, show.legend = FALSE)
print(fig)
# save 
ggsave(plot = fig, width = 7, height = 4, units = "in", dpi = 300, filename = "~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/snl_cor_nmp.jpg")

## half area latency ####

df_hal <- df_cor %>%
  select(slope:direction, half_area_latency) %>%
  pivot_wider(names_from = direction, values_from = half_area_latency) %>%
  mutate(amp = rowMeans(across(c(highStan_lowDevi, lowStan_highDevi))),
         poa = factor(poa, levels = c("dorsal", "glottal"), labels = c("Dorsal", "Glottal")))

fig <- myfunc_cor_plot(df_hal, "Half-area latency") +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0, #adjust the label in x axis
           label.y.npc = 0, #adjust the label in y axis
           size = 4.5, show.legend = FALSE)
print(fig)
# save 
# ggsave(plot = fig, width = 7, height = 4, units = "in", dpi = 300, filename = "~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/snl_cor_hal.jpg")


# gam modeling korean ####

df_gam_hasPeak <- read_delim("~/OneDrive - University of Toronto/Projects/Laryngeal/data_analysis/gam/df_gam_hasPeak_korean.txt", delim = "\t") #%>%
  filter(half_area_latency >= 150 & half_area_latency <= 600)

# check sample size
df_gam_hasPeak %>% group_by(group, poa, direction) %>% dplyr::summarize(n = n())

# plot data
df_plot <- df_gam_hasPeak

# stats data
df_mod <- df_gam_hasPeak %>%
  mutate(group = factor(group, levels = c("VOT", "F0")),
         poa = factor(poa, levels = c("dorsal", "glottal")),
         direction = factor(direction, levels = c("highStan_lowDevi", "lowStan_highDevi")))
# set contrast
F0_VOT <- c(-1/2, 1/2)
contrasts(df_mod$group) <- cbind(F0_VOT)

dorsal_glottal <- c(-1/2, 1/2)
contrasts(df_mod$poa) <- cbind(dorsal_glottal)

HighLow_LowHigh <- c(-1/2, 1/2)
contrasts(df_mod$direction) <- cbind(HighLow_LowHigh)

## normalized modeled peak ####
# plot
fig <- 
  ggplot(data = df_plot) +
  facet_wrap(~group) +
  geom_boxplot(aes(x = poa, y = NMP, fill = direction)) +
  theme_bw() +
  theme(legend.position = "bottom")
print(fig)

# model
mod <- lmer(NMP ~ group*poa*direction + (1|ppt), data = df_mod)
summary(mod)
Anova(mod, type="III")
pair_comp <- emmeans(mod, pairwise ~ group | poa)
test(pair_comp)

## fractional area latency ####
# plot
fig <- 
  ggplot(data = df_plot) +
  facet_wrap(~group) +
  geom_boxplot(aes(x = poa, y = half_area_latency, fill = direction)) +
  theme_bw() +
  theme(legend.position = "bottom")
print(fig)

# model
mod <- lmer(half_area_latency ~ group*poa*direction + (1|ppt), data = df_mod)
summary(mod)
Anova(mod, type="III")
pair_comp <- emmeans(mod, pairwise ~ group | poa)
test(pair_comp)

# Brain-behavioral correlation korean ####

df_thresh <- read_delim(file = "~/OneDrive - University of Toronto/Projects/Laryngeal/Seoul Data/data_analysis/df_thresh_korean.txt", delim = "\t")
df_gam_hasPeak <- read_delim("~/OneDrive - University of Toronto/Projects/Laryngeal/data_analysis/gam/df_gam_hasPeak_korean.txt", delim = "\t") # %>%
  # filter(half_area_latency >= 150 & half_area_latency <= 600)

df_cor <- df_thresh %>%
  mutate(slope = as.numeric(slope)) %>%
  inner_join(df_gam_hasPeak, by = c("ppt", "group"))

## NMP ####
df_nmp <- df_cor %>%
  select(slope:direction, NMP) %>%
  pivot_wider(names_from = direction, values_from = NMP) %>%
  mutate(amp = rowMeans(across(c(highStan_lowDevi, lowStan_highDevi))),
         poa = factor(poa, levels = c("dorsal", "glottal"), labels = c("Dorsal", "Glottal")))

fig <- myfunc_cor_plot(df_nmp, "Normalized modeled peak") +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           # label.x.npc = 0, #adjust the label in x axis
           # label.y.npc = 0.2, #adjust the label in y axis
           size = 4.5, show.legend = FALSE)
print(fig)
# save 
ggsave(plot = fig, width = 7, height = 4, units = "in", dpi = 300, filename = "~/OneDrive - University of Toronto/Projects/Laryngeal/presentations/SNL/figures/snl_cor_nmp.jpg")

## half area latency ####

df_hal <- df_cor %>%
  select(slope:direction, half_area_latency) %>%
  pivot_wider(names_from = direction, values_from = half_area_latency) %>%
  mutate(amp = rowMeans(across(c(highStan_lowDevi, lowStan_highDevi))),
         poa = factor(poa, levels = c("dorsal", "glottal"), labels = c("Dorsal", "Glottal")))

fig <- myfunc_cor_plot(df_hal, "Half-area latency") +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = 0, #adjust the label in x axis
           label.y.npc = 0, #adjust the label in y axis
           size = 4.5, show.legend = FALSE)
print(fig)

# gam modeling combined ####
df_gam_hasPeak_english <- read_delim("~/OneDrive - University of Toronto/Projects/Laryngeal/data_analysis/gam/df_gam_hasPeak_english.txt", delim = "\t") %>%
  mutate(language = "english") %>%
  mutate(ppt = paste0(language, "_", ppt))
df_gam_hasPeak_korean <- read_delim("~/OneDrive - University of Toronto/Projects/Laryngeal/data_analysis/gam/df_gam_hasPeak_korean.txt", delim = "\t") %>%
  mutate(language = "korean") %>%
  mutate(ppt = paste0(language, "_", ppt))

df_gam_hasPeak_combined <- rbind(df_gam_hasPeak_english, df_gam_hasPeak_korean)


# data for model and plot
df_mod <- df_gam_hasPeak_combined %>%
  # filter(poa=="glottal") %>%
  filter(group=="VOT") %>%
  droplevels() %>%
  mutate(
    # group = factor(group, levels = c("VOT", "F0")),
    language = factor(language, levels = c("english", "korean")),
    poa = factor(poa, levels = c("dorsal", "glottal")),
    direction = factor(direction, levels = c("highStan_lowDevi", "lowStan_highDevi")))
# set contrast
VOT_F0 <- c(-1/2, 1/2)
contrasts(df_mod$group) <- cbind(VOT_F0)

dorsal_glottal <- c(-1/2, 1/2)
contrasts(df_mod$poa) <- cbind(dorsal_glottal)

ENG_KOR <- c(-1/2, 1/2)
contrasts(df_mod$language) <- cbind(ENG_KOR)

HighLow_LowHigh <- c(-1/2, 1/2)
contrasts(df_mod$direction) <- cbind(HighLow_LowHigh)

## normalized modeled peak ####
# plot
fig <- 
  ggplot(data = df_mod) +
  facet_wrap(~poa) +
  geom_boxplot(aes(x = language, y = NMP, fill = direction)) +
  theme_bw() +
  theme(legend.position = "bottom")
print(fig)

# model
mod <- lmer(NMP ~ poa*language*direction + (1|ppt), data = df_mod)
summary(mod)
Anova(mod, type="III")

emmeans(mod, pairwise ~ language | direction)

mod <- lmer(NMP ~ group*language*direction + (1|ppt), data = df_mod)
summary(mod)
Anova(mod, type="III")



# simple effect
mod <- lmer(NMP ~ language*direction + (1|ppt), data = df_mod[df_mod$group=="F0",])
summary(mod)
Anova(mod, type="III")

mod <- lmer(NMP ~ language*direction + (1|ppt), data = df_mod[df_mod$poa=="glottal",])
summary(mod)
Anova(mod, type="III")
