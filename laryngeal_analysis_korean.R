# code for behavioral data adapted based on Yunjung's script

library(tidyverse)
library(lmerTest)
# library(ez)
# library(cowplot)
# library(ggpubr)
# library(ggrepel)
# library(gridExtra)
# library(scales)

source("~/Documents/GitHub/laryngeal/my_themes.R")


setwd("~/Library/CloudStorage/OneDrive-UniversityofToronto/Projects/Laryngeal/")

# Korean ####

# bad participants
bad_ppts <- c(
  "F0_0008" # missing behavioral data
)

## behavioral ####

# draw stimuli ####
df_stim <- tibble(
  "F0" = c( rep(c(74, 79, 83), each = 6), rep(c(132, 140, 148), each = 3) ),
  "VOT" = c( rep(c(5, 8, 11, 47, 50, 53), times = 3), rep(c(47, 50, 53), times = 3) ),
  "category" = c( rep(c("g", "?"), each = 3, time = 3), rep(c("k"), times = 9) )
) %>%
  mutate(category = factor(category, levels = c("g", "k", "?"), labels = c("intended /g/", "intended /k/", "?")))

fig <-
  ggplot(data = df_stim) +
  geom_point(aes(x = VOT, y = F0, color = category), size = 3) +
  scale_x_continuous(breaks = c(5, 8, 11, 47, 50, 53)) +
  scale_y_continuous(breaks = c(74, 79, 83, 132, 140, 148)) +
  theme_bw() +
  theme(
    text = element_text(size = 16),
    # title = element_text(size = 12),
    legend.position = c(0.5, 0.5),
    legend.title = element_blank()) # change the position of legend. "none" to hide legend
fig


# get info from behavioral data

# collect files
all_files <- list.files("Seoul Data/behav/", pattern = "\\.csv$")

# trim and combine files and code participant
all=NULL
for (file in all_files) {
  
  ppt <- paste0( strsplit(file, "_")[[1]][3], "_", strsplit(file, "_")[[1]][2] )
  if (ppt %in% bad_ppts) {next}
    
  # single ppt data
  tmp <- read.csv(paste0("Seoul Data/behav/", file)) %>%
    rename(any_of(c(soundstimuli = "F0sounds", soundstimuli = "VOTsounds"))) %>%
    select(practice_sounds, category, repetition, soundstimuli, correctAns, prac_key_resp.keys, exptrial_key_resp.keys, expName) %>%
    mutate(part = ppt)
  
  # add in data
  all = rbind(all, tmp)
    
}

# practice (ha vs. a) check -- out of 8 trials, several people got one wrong and one person got two wrong but their behavioral data look fine. So, we'll keep them. 
df_practice <- all %>%
  filter(practice_sounds != "") %>%
  mutate(prac_resp = ifelse(prac_key_resp.keys %in% c("m"), "ha", "a"),
         target = ifelse(grepl("_ha_", practice_sounds), "ha", "a"),
         acc = ifelse(target == prac_resp, 1, 0))
xtabs(~practice_sounds+prac_resp, df_practice)

# accuracy
df_practice %>%
  group_by(part) %>%
  summarize(accuracy = mean(acc)) %>%
  ungroup() %>%
  print(n = Inf)

# main behavioral data
df_dat <- all %>%
  filter(soundstimuli != "") %>%
  group_by(soundstimuli) %>%
  mutate(
    f0 = as.numeric(strsplit(soundstimuli,"_")[[1]][9]),
    vot = as.numeric(strsplit(soundstimuli,"_")[[1]][11]),
    Response = ifelse(exptrial_key_resp.keys %in% c("z"), "ka", "ga"),
    resp = ifelse(exptrial_key_resp.keys %in% c("z"), 1, 0),
    cat = ifelse(category=="low_low","ga","ka"),
    target = ifelse(category == "low_low","g","k")
  )
xtabs(~cat+resp+part, df_dat)

# graphs
# overall
df_sum <- df_dat %>%
  group_by(expName, f0, vot) %>%
  summarize(mean_k = mean(resp))
fig <-
  ggplot(df_sum, aes(vot, f0, fill=mean_k)) + 
  facet_grid(. ~ expName) +
  geom_tile()+
  scale_fill_gradient(low = "white", high="darkgreen")
fig
ggsave(plot = fig, width = 5, height = 4, units = "in", dpi = 300, filename = paste0("figures/behav_meanK_color_korean.jpg"))

# individual
df_sum <- df_dat %>%
  group_by(expName, part, f0, vot) %>%
  summarize(mean_k = mean(resp))
fig <-
  ggplot(df_sum, aes(vot, f0, fill=mean_k)) + 
  facet_grid(expName ~ part) +
  geom_tile()+
  scale_fill_gradient(low = "white", high="darkgreen")
fig
ggsave(plot = fig, width = 40, height = 4, units = "in", dpi = 300, filename = paste0("figures/behav_individual_meanK_color_korean.jpg"))

fig <-
  ggplot(df_dat, aes(target, resp)) + 
  facet_grid(expName ~ part) +
  stat_summary(fun=mean, geom="bar", position=position_dodge()) +
  stat_summary(fun.data=mean_se, geom="errorbar", position=position_dodge()) +
  ylab("/k/ response")
fig
ggsave(plot = fig, width = 40, height = 4, units = "in", dpi = 300, filename = paste0("figures/behav_individual_meanK_bar_korean.jpg"))


# normalize data
df_norm <- df_dat %>%
  group_by(expName) %>%
  mutate(f0_n = as.numeric(scale(f0)),
         vot_n = as.numeric(scale(vot)),
         cue_value = ifelse(expName == "F0", f0_n, vot_n))

# plot functions
fig <-
  ggplot(df_norm, aes(cue_value, resp)) + 
  facet_wrap(~expName) +
  # geom_point() + 
  # inidividual function
  stat_smooth(aes(cue_value, resp, group = part), geom = "line", alpha = 0.7, color = "grey", method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  # average over all individuals
  stat_smooth(aes(color = expName, fill = expName), method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  scale_color_manual(values = c(
    "F0"="#156082",
    "VOT"="#E97132"
  )) +
  scale_fill_manual(values = c(
    "F0"="#156082",
    "VOT"="#E97132"
  )) +
  labs(
    x = "Normalized cue value",
    y = "Proportion of /k/ responses"
  ) +
  # inidividual
  theme_bw() +
  theme(
    text = element_text(size = 16),
    # title = element_text(size = 12),
    legend.position = "none",
    legend.title = element_blank()) # change the position of legend. "none" to hide legend
fig
ggsave(plot = fig, width = 5.5, height = 4, units = "in", dpi = 300, filename = paste0(paste0("figures/behav_categorical_function_korean.jpg")))


# inidividual slopes, plot density, t test

## Get the subject ID
ppt_list <- unique(df_dat$part)
# initialize an summary dataframe
df_thresh <- data.frame(matrix(ncol = 4, nrow = length(ppt_list)))
colnames(df_thresh) <- c("ppt", "thresh", "slope", "group")
df_thresh$ppt <- ppt_list

for (ppt in ppt_list) {
  
  # get experiment group
  cue_group <- strsplit(ppt, split = "_")[[1]][1]
  
  # Extract the data for each subject
  ppt_df <- df_norm %>%
    filter(part == ppt) %>%
    droplevels()
  # Get the summary data for a single subject
  ppt_summary <- ppt_df %>%
    group_by(expName, part, cue_value) %>%
    dplyr::summarize(k_perc = mean(resp)) %>%
    ungroup()
  
  ## Run the generalized linear model to get the coefficients for the sigmoid function
  ppt_model <- glm(resp ~ cue_value, data = ppt_df, family = "binomial")
  
  # Get the intercept for the sigmoid function: 
  b <- as.numeric(ppt_model$coefficients[1])
  # Get the slope for the sigmoid function:
  m <- as.numeric(ppt_model$coefficients[2])
  # Fit the sigmoid function to the summarized data
  ppt_summary$fitted_k_perc <- exp(b + m * ppt_summary$cue_value) / (1 + exp(b + m * ppt_summary$cue_value))
  # Get the Threshold (the VOT value when Percentage /t/ is 0.5)
  # When P = 0.5, In(P/(1-P)) = 0 = b + mx, so:
  thresh <- -b/m
  # Get the slope of the curve at the Threshold:
  slope <- (exp(b + m * thresh) * m) / (exp(b + m * thresh) + 1)^2
  # # Get the Uncertainty Region (UR), which is the inverse of the slope:
  # UR <- 1/slope_thresX
  
  ## Create a new summarized dataframe for each subject
  df_thresh[df_thresh$ppt == ppt, ] <- c(ppt, thresh, slope, cue_group)
  
}

# plot distribution
fig_dist <-
  ggplot(data = df_thresh, aes(group = group, fill = group)) +
  # geom_histogram(aes(x = as.numeric(slope)), binwidth = 0.1, position = "identity", alpha = 0.5, color = "white") +
  geom_density(aes(x = as.numeric(slope)), alpha = 0.5, color = NA) +
  scale_fill_manual(values = c(
    "F0"="#156082",
    "VOT"="#E97132"
  )) +
  scale_x_continuous(limits = c(-0.5, 2.5)) +
  # coord_cartesian(xlim = c(-1, 2.5)) +
  theme_bw() +
  theme(
    # change font size of non-title/non-label texts
    text = element_text(size = 16),
    # change font size of title/label texts
    # title = element_text(size = 18)
    legend.title = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.8)
  ) + 
  labs(x = "Slope", # x-axis label
       y = "Number of participants") # y-axis label
fig_dist
ggsave(plot = fig_dist, width = 4.5, height = 4, units = "in", dpi = 300, filename = paste0(paste0("figures/behav_slope_distribution_english.jpg")))


# t test
t.test(as.numeric(slope) ~ group, data = df_thresh)

# YJ inidividual slopes, plot density, t test

subset(df_dat, df_dat$expName == "VOT")->VOT
subset(df_dat, df_dat$expName == "F0")->F0
VOT$vot.n = scale(VOT$vot)
F0$f0.n = scale(F0$f0)

glmer(resp~vot.n+(1+vot.n|part),VOT,family="binomial")->fit1
summary(fit1)
glmer(resp~f0.n+(1+f0.n|part),F0,family="binomial")->fit2
summary(fit2)
VOT.slopes = ranef(fit1)$part[2] + fixef(fit1)[2]
F0.slopes = ranef(fit2)$part[2] + fixef(fit2)[2]
VOT.slopes$ppt = row.names(VOT.slopes)
F0.slopes$ppt = row.names(F0.slopes)
df_slope_korean <- VOT.slopes %>%
  rename(slope = vot.n) %>%
  mutate(group = "VOT")


tmp_vot <- VOT.slopes %>%
  mutate(group = "VOT") %>%
  dplyr::rename("slope" = vot.n)
tmp_f0 <- F0.slopes %>%
  mutate(group = "F0") %>%
  dplyr::rename("slope" = f0.n)
df_thresh_yj <- rbind(tmp_vot, tmp_f0) %>%
  mutate(language = "korean")
write_delim(x = df_thresh_yj, file = "Seoul Data/data_analysis/df_thresh_korean.txt", delim = "\t")

# plot slope distribution
fig_dist <-
  ggplot(data = df_thresh_yj, aes(group = group, fill = group)) +
  geom_histogram(aes(x = as.numeric(slope)), binwidth = 0.1, position = "identity", alpha = 0.5, color = "white") +
  # geom_density(aes(x = as.numeric(slope)), alpha = 0.5, color = NA) +
  scale_fill_manual(values = c(
    "F0"="#156082",
    "VOT"="#E97132"
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
    legend.position = c(0.8, 0.8)
  ) + 
  labs(x = "Cue weight", # x-axis label
       y = "Number of participants") # y-axis label
fig_dist
ggsave(plot = fig_dist, width = 4.5, height = 4, units = "in", dpi = 300, filename = paste0(paste0("figures/behav_slope_distribution_english.jpg")))

# t test
t.test(as.numeric(slope) ~ group, data = df_thresh_yj)
