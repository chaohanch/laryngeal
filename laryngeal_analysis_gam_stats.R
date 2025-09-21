library(tidyverse)
library(lmerTest)
library(ggpubr)
# library(corrplot)
# library(ez)
# library(Hmisc)
# library(car)
# library(emmeans)

# load data ####
df_gam <- read.table(file = "~/OneDrive - University of Toronto/Projects/Laryngeal/Seoul Data/data_analysis/gam/df_gam.txt", header = TRUE)
# df_gam <- read.table(file = "~/OneDrive - University of Toronto/Projects/Laryngeal/data_analysis/gam/df_gam.txt", header = TRUE)


df_ppt <- read_delim("~/OneDrive - University of Toronto/Projects/Laryngeal/data_analysis/participants_include.txt", delim = "\t") %>%
  filter(include == 1)

# add group info and pick participants
df_gam_complete <- df_gam %>%
  filter(ppt %in% df_ppt$participant) %>%
  separate(col = ppt, into = c("group", "ppt_num"), remove = FALSE) %>%
  separate(col = condition, into = c("poa", "stan", "devi", "stim")) %>%
  unite(col = "direction", c("stan", "devi")) %>%
  # convert hasPeak to 1 and 0
  mutate(hasPeak_numeric = as.numeric(hasPeak))


# plotting hasPeak
ggplot(data = df_gam_complete) +
  facet_wrap(~group) +
  geom_bar(aes(x = poa, y = hasPeak_numeric, fill = direction),
           stat = "summary", fun = mean, position=position_dodge()) +
  coord_cartesian(ylim = c(0.8, 1)) +
  theme_bw()

no_peak_ppts <- df_gam_complete %>%
  group_by(ppt) %>%
  dplyr::summarize(peak_mean = mean(hasPeak_numeric)) %>%
  ungroup() %>%
  filter(peak_mean < 1) %>%
  droplevels()

df_gam_hasPeak <- df_gam_complete %>%
  # filter(hasPeak == "TRUE") %>%
  filter(hasPeak == "TRUE" & !(ppt %in% no_peak_ppts$ppt) ) %>%
  # filter(half_area_latency >= 150 & half_area_latency <= 600) %>%
  droplevels()

write_delim(x = df_gam_hasPeak, file = "~/OneDrive - University of Toronto/Projects/Laryngeal/data_analysis/gam/df_gam_hasPeak_korean.txt", delim = "\t")

# check sample size
df_gam_hasPeak %>% group_by(group, poa, direction) %>% dplyr::summarize(n = n())

# plotting and stats ####

## plot data ####
df_plot <- df_gam_hasPeak

## stats data ####
df_mod <- df_gam_hasPeak %>%
  mutate(group = factor(group, levels = c("VOT", "F0")),
         poa = factor(poa, levels = c("dorsal", "glottal")),
         direction = factor(direction, levels = c("highStan_lowDevi", "lowStan_highDevi")))
# set contrast
F0_VOT <- c(-1/2, 1/2)
contrasts(df_mod$group) <- cbind(F0_VOT)

dorsal_glottal <- c(-1/2, 1/2)
contrasts(df_mod$poa) <- cbind(dorsal_glottal)

highLOW_lowHIGH <- c(-1/2, 1/2)
contrasts(df_mod$poa) <- cbind(highLOW_lowHIGH)

## traditional erp ####
fig <-
  ggplot(data = df_plot) +
  facet_wrap(~group) +
  geom_boxplot(aes(x = poa, y = trad_erp, fill = direction)) +
  theme_bw() +
  theme(legend.position = "bottom")
print(fig)
# ggsave(plot = fig, width = 5, height = 5, units = "in", dpi = 300, filename = "~/OneDrive - University of Toronto/Projects/Yas accent/figures/gam/gam_trad_erp.png")

# mod <- lm(trad_erp ~ group, data = df_mod)
mod <- lmer(trad_erp ~ group*poa*direction + (1|ppt), data = df_mod)
summary(mod)
Anova(mod, type="III")
pair_comp <- emmeans(mod, pairwise ~ direction | group)
test(pair_comp)


## gam erp ####
fig <-
  ggplot(data = df_plot) +
  facet_wrap(~group) +
  geom_boxplot(aes(x = poa, y = gam_erp, fill = direction)) +
  theme_bw() +
  theme(legend.position = "bottom")
print(fig)
# ggsave(plot = fig, width = 5, height = 5, units = "in", dpi = 300, filename = "~/OneDrive - University of Toronto/Projects/Yas accent/figures/gam/gam_trad_erp.png")

# mod <- lm(gam_erp ~ group, data = df_mod)
mod <- lmer(gam_erp ~ group*condition + (1|ppt), data = df_mod)
summary(mod)
Anova(mod, type = "III")
pair_comp <- emmeans(mod, pairwise ~ group | condition)
test(pair_comp)


## modeled area ####
fig <- 
  ggplot(data = df_plot) +
  facet_wrap(~group) +
  geom_boxplot(aes(x = poa, y = area, fill = direction)) +
  theme_bw() +
  theme(legend.position = "bottom")
print(fig)

# mod <- lm(area ~ group, data = df_mod)
mod <- lmer(area ~ group*condition + (1|ppt), data = df_mod)
summary(mod)
Anova(mod, type="III")
# pair_comp <- emmeans(mod, pairwise ~ group)
pair_comp <- emmeans(mod, pairwise ~ group | condition)
test(pair_comp)

## modeled peak ####
fig <- 
  ggplot(data = df_plot) +
  facet_wrap(~group) +
  geom_boxplot(aes(x = poa, y = peak_height, fill = direction)) +
  theme_bw() +
  theme(legend.position = "bottom")
print(fig)

# mod <- lm(peak_height ~ group, data = df_mod)
mod <- lmer(peak_height ~ group*condition + (1|ppt), data = df_mod)
summary(mod)
Anova(mod)
# pair_comp <- emmeans(mod, pairwise ~ group)
pair_comp <- emmeans(mod, pairwise ~ group | condition)
test(pair_comp)
pair_comp <- emmeans(mod, pairwise ~ group)
test(pair_comp)

## normalized modeled peak ####
fig <- 
  ggplot(data = df_plot) +
  facet_wrap(~group) +
  geom_boxplot(aes(x = poa, y = NMP, fill = direction)) +
  theme_bw() +
  theme(legend.position = "bottom")
print(fig)

# mod <- lm(NMP ~ group, data = df_mod)
mod <- lmer(NMP ~ group*condition + (1|ppt), data = df_mod)
summary(mod)
Anova(mod, type="III")
# pair_comp <- emmeans(mod, pairwise ~ group)
pair_comp <- emmeans(mod, pairwise ~ group | condition)
test(pair_comp)

## modeled fractional area latency ####
fig <- 
  ggplot(data = df_plot) +
  facet_wrap(~group) +
  geom_boxplot(aes(x = poa, y = half_area_latency, fill = direction)) +
  theme_bw() +
  theme(legend.position = "bottom")
print(fig)
# mod <- lm(half_area_latency ~ group, data = df_mod)
mod <- lmer(half_area_latency ~ group*condition + (1|ppt), data = df_mod)
summary(mod)
Anova(mod, type="III")
# pair_comp <- emmeans(mod, pairwise ~ group)
pair_comp <- emmeans(mod, pairwise ~ group | condition)
test(pair_comp)

## modeled peak latency ####
fig <- 
  ggplot(data = df_plot) +
  facet_wrap(~group) +
  geom_boxplot(aes(x = poa, y = peak_time, fill = direction)) +
  theme_bw() +
  theme(legend.position = "bottom")
print(fig)

# mod <- lm(peak_time ~ group, data = df_mod)
mod <- lmer(peak_time ~ group*condition + (1|ppt), data = df_mod)
summary(mod)
Anova(mod, type="III")
# pair_comp <- emmeans(mod, pairwise ~ group)
pair_comp <- emmeans(mod, pairwise ~ group | condition)
test(pair_comp)



# Brain-behavioral correlation ####
# read in reading data

df_thresh <- read_table(file = "data_analysis/df_thresh_english.txt")

df_cor <- df_thresh %>%
  mutate(slope = as.numeric(slope)) %>%
  inner_join(df_gam_hasPeak, by = c("ppt", "group"))

fig <-
  ggplot(data = df_cor,
         mapping = aes(x = slope, y = peak_time, 
                       group = group, color = group, fill = group
                       )) +
  facet_grid(poa ~ direction) +
  geom_point(size = 1, alpha = 0.8) +
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
  theme_bw() +
  theme(legend.title = element_blank())
print(fig)
# save 
ggsave(plot = fig, width = 12, height = 7, units = "in", dpi = 300, filename = "~/OneDrive - University of Toronto/Projects/Yas accent/figures/correlation/NMP.png")

# regression

# set contrast
levels(df_rating$condition)
en_ch <- c(-1/2, 1/2)
contrasts(df_rating$condition) <- cbind(en_ch)

levels(df_rating$group)
MONO_BILI <- c(-1/3, -1/3, 2/3)
ENG_CHI <- c(-1/2, 1/2, 0)
contrasts(df_rating$group) <- cbind(MONO_BILI, ENG_CHI)

mod <- lmer(NMP ~ group * condition * scale(likelihood_chinese) + (1|ppt), data = df_rating)
summary(mod)
Anova(mod, type = "III")

pair_comp <- emtrends(mod, pairwise ~ group | condition, var = "likelihood_chinese")
# slope significance
test(pair_comp)
# slope difference
pair_comp
# effect size
eff_size(pair_comp$emtrends, sigma = sigma(mod), edf = df.residual(mod), method = "identity")
