## Analyse data from participants in the primed conditions.

library(dplyr)
library(lazyeval)  # lazy evaluation used in bysubj() function [summarise_]
library(ggplot2)
library(lme4)
library(boot)  # for inv.logit()

#  ------------------------------------------------------------------------
#  Load data and process
#  ------------------------------------------------------------------------

# The data is created in the script "processing/compute_dependent_measures.R"
# There is the normal and the liberally coded version, see script for difference

# load
d <- read.csv("data/data_DVs.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
head(d)
# adjust Condition and Group levels for plotting
d$Condition <- factor(d$Condition, levels = c("Path", "Control", "Manner"))
d$Group <- factor(d$Group, levels = c("NS", "L2"))

# participant data
ppts <- read.csv("data/participants.csv", fileEncoding = "UTF-8",
                 stringsAsFactors = FALSE)
head(ppts)

# add speakers' clozescore to d:
d <- left_join(d, ppts %>% select(Subject, ClozeScore))
head(d)
tail(d)



#  ------------------------------------------------------------------------
#  Functions
#  ------------------------------------------------------------------------

# source functions to compute collinearity diagnostics
source("analysis/functions/kappa_mer_fnc.R")
source("analysis/functions/vif_mer_fnc.R")

# Convenience function to compute by-speaker averages for a given variable
bysubj <- function(df, myvar = NULL, varname = NULL) {
  out <- df %>% 
    group_by(Subject, Group, Condition, ClozeScore) %>%
    # to pass variable name as a string, use standard evaluation, see
    # https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html
    # (NB: I don't get how this is working, but seems to work)
    summarise_(M = interp(~ mean(var), var = as.name(myvar)))
  # if varname argument is supplied it names the computed column
  if (! is.null(varname)) names(out)[length(out)] <- varname
  out
}
bysubj(d, "P_anyw") %>% head
bysubj(d, "P_anyw", "P_anyw") %>% head


## plotting functions

# plot group patterns -- emphasis put between conditions in each group
f_plot <- function(df, varname = NULL) {
  p <- ggplot(df, aes(x = Group, y = M, colour = Condition)) +
    geom_point(position = position_jitterdodge()) +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = .85, width = .5,
                 position = "dodge") +
    ylim(0,1) +
    ylab(paste("Proportion of descriptions\nexpressing", varname)) + 
    ggtitle(varname)
  p
}

# here the emphasis is put between groups (NS vs L2)
f_plot2 <- function(df, varname = NULL, avg_type = "mean") {
  p <- ggplot(df, aes(x = Condition, y = M, colour = Group, group = Group,
                      shape = Group)) +
    geom_point(position = position_jitterdodge(), size = 2, alpha = .5) +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = 1.5,
                 position = "dodge", width = .5) +
    stat_summary(fun.y = avg_type, geom = "line",  size = 1,
                 position = position_dodge(width = .5)) +
    ylim(0,1) +
    ylab(paste("Proportion of descriptions\nexpressing", varname)) + 
    ggtitle(varname)
  p
}


# Plot effect of proficiency (cloze score)
f_plot_prof <- function(df, varname = NULL) {
  df <- df[df$Group == "L2", ]
  p <- ggplot(df, aes(x = ClozeScore, y = M, colour = Condition)) +
    geom_jitter(height = 0) +
    geom_smooth(method = "lm") +
    # ylim(0,1) +
    ylab(paste("Proportion of descriptions\nexpressing", varname)) + 
    ggtitle(varname)
  p
}


#  ------------------------------------------------------------------------
#  Logit mixed models and corresponding plots
#  ------------------------------------------------------------------------

# Create data subsets for the different comparisons -----------------------


# Data set for model fitting ----------------------------------------------

# Make some adjustments to the dataset for model fitting:
d_fm <- d
# contrast coding for group
contrasts(d_fm$Group) <- - contr.sum(2)
colnames(contrasts(d_fm$Group)) <- "L2_vs_NS"
contrasts(d_fm$Group)
# dummy coding for conditions, using control condition as reference
d_fm$Condition <- factor(d_fm$Condition, levels = c("Control", "Path", "Manner"))
colnames(contrasts(d_fm$Condition)) <- c("P_vs_C", "M_vs_C")
contrasts(d_fm$Condition)



# Path anywhere -----------------------------------------------------------

# plot all conditions for reference
# f_plot(bysubj(d, "P_anyw"), "Path anywhere")
f_plot2(bysubj(d, "P_anyw"), "Path anywhere")

# fit model
fm_pan <- glmer(P_anyw ~ Group * Condition + (1 | Subject) + (1 | VideoName),
                data = d_fm, family = "binomial",
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(fm_pan)
kappa.mer(fm_pan)
vif.mer(fm_pan)

# Manner anywhere ---------------------------------------------------------

# plot all conditions for reference
# f_plot(bysubj(d, "M_anyw"), "Manner anywhere")
f_plot2(bysubj(d, "M_anyw"), "Manner anywhere")

# fit model
fm_man <- glmer(M_anyw ~ Group * Condition + (1 | Subject) + (1 | VideoName),
                data = d_fm, family = "binomial",
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(fm_man)
kappa.mer(fm_man)
vif.mer(fm_man)


# Path in the verb --------------------------------------------------------

# plot all conditions for reference
# f_plot(bysubj(d, "P_V"), "Path in V")
f_plot2(bysubj(d, "P_V"), "Path in V")

# fit model
fm_pve <- glmer(P_V ~ Group * Condition + (1 | Subject) + (1 | VideoName),
                data = d_fm, family = "binomial", 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(fm_pve)
kappa.mer(fm_pve)
vif.mer(fm_pve)


# Manner in the verb ------------------------------------------------------

# plot all conditions for reference
# f_plot(bysubj(d, "M_V"), "Manner in V")
f_plot2(bysubj(d, "M_V"), "Manner in V")

# fit model
fm_mve <- glmer(M_V ~ Group * Condition + (1 | Subject) + (1 | VideoName),
                data = d_fm, family = "binomial",
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(fm_mve)
kappa.mer(fm_mve)
vif.mer(fm_mve)


# Path as adjunct ---------------------------------------------------------

# plot all conditions for reference
# f_plot(bysubj(d, "P_adj"), "Path as adjunct")
f_plot2(bysubj(d, "P_adj"), "Path as adjunct")

# fit model
fm_pad <- glmer(P_adj ~ Group * Condition + (1 | Subject) + (1 | VideoName),
                data = d_fm, family = "binomial",
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(fm_pad)
kappa.mer(fm_pad)
vif.mer(fm_pad)


# Manner as adjunct -------------------------------------------------------

# plot all conditions for reference
# f_plot(bysubj(d, "M_adj"), "Manner as adjunct")
f_plot2(bysubj(d, "M_adj"), "Manner as adjunct")

# fit model
fm_mad <- glmer(M_adj ~ Group * Condition + (1 | Subject) + (1 | VideoName),
                data = d_fm, family = "binomial",
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(fm_mad)
kappa.mer(fm_mad)
vif.mer(fm_mad)



#  ------------------------------------------------------------------------
#  Logit mixed models for proficiency effect (cloze scores)
#  ------------------------------------------------------------------------

# subset L2 data
d_l2 <- d %>% filter(Group == "L2")
# center ClozeScore
d_l2$cCloze <- scale(d_l2$ClozeScore)
# Code the Condition factor so that Control is the reference group
d_l2$Condition <- factor(d_l2$Condition, levels = c("Control", "Path", "Manner"))
colnames(contrasts(d_l2$Condition)) <- c("P_vs_C", "M_vs_C")
contrasts(d_l2$Condition)


# Path anywhere -----------------------------------------------------------

# Path anywhere
f_plot_prof(bysubj(d, "P_anyw"), "Path anywhere")

## likelihood ratio test to test significance of proficiency
# model with proficiency
fm_pan_prof <- glmer(P_anyw ~ Condition * cCloze + (1 | Subject) + (1 | VideoName),
                     data = d_l2, family = "binomial",
                     control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# model without proficiency
fm_pan_prof_null <- glmer(P_anyw ~ Condition + (1 | Subject) + (1 | VideoName),
                          data = d_l2, family = "binomial",
                          control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
anova(fm_pan_prof_null, fm_pan_prof)
# if significant, go ahead and interpret model with proficiency
summary(fm_pan_prof)
kappa.mer(fm_pan_prof)
vif.mer(fm_pan_prof)


# Manner anywhere -----------------------------------------------------------

# Manner anywhere
f_plot_prof(bysubj(d, "M_anyw"), "Manner anywhere")

## likelihood ratio test to test significance of proficiency
# model with proficiency
fm_man_prof <- glmer(M_anyw ~ Condition * cCloze + (1 | Subject) + (1 | VideoName),
                     data = d_l2, family = "binomial",
                     control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# model without proficiency
fm_man_prof_null <- glmer(M_anyw ~ Condition + (1 | Subject) + (1 | VideoName),
                          data = d_l2, family = "binomial",
                          control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
anova(fm_man_prof_null, fm_man_prof)  # trend towards significance

# if significant, go ahead and interpret model with proficiency
summary(fm_man_prof)
kappa.mer(fm_man_prof)
vif.mer(fm_man_prof)


# Path in verb ------------------------------------------------------------

# simple plot
f_plot_prof(bysubj(d, "P_V"), "Path in V") + geom_text(aes(label = Subject))
f_plot_prof(bysubj(d, "P_V"), "Path in V")

## likelihood ratio test to test significance of proficiency
# model with proficiency
fm_pve_prof <- glmer(P_V ~ Condition * cCloze + (1 | Subject) + (1 | VideoName),
                     data = d_l2, family = "binomial",
                     control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# model without proficiency
fm_pve_prof_null <- glmer(P_V ~ Condition + (1 | Subject) + (1 | VideoName),
                          data = d_l2, family = "binomial",
                          control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
anova(fm_pve_prof_null, fm_pve_prof)
# if significant, go ahead and interpret model with proficiency
summary(fm_pve_prof)
kappa.mer(fm_pve_prof)
vif.mer(fm_pve_prof)



# Manner in verb ------------------------------------------------------------

# simple plot
f_plot_prof(bysubj(d, "M_V"), "Manner in V")

## likelihood ratio test to test significance of proficiency
# model with proficiency
fm_mve_prof <- glmer(M_V ~ Condition * cCloze + (1 | Subject) + (1 | VideoName),
                     data = d_l2, family = "binomial",
                     control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# model without proficiency
fm_mve_prof_null <- glmer(M_V ~ Condition + (1 | Subject) + (1 | VideoName),
                          data = d_l2, family = "binomial",
                          control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
anova(fm_mve_prof_null, fm_mve_prof)  # not significant

# # if significant, go ahead and interpret model with proficiency
# summary(fm_mve_prof)
# kappa.mer(fm_mve_prof)
# vif.mer(fm_mve_prof)


# Path in adjunct ---------------------------------------------------------

# simple plot
f_plot_prof(bysubj(d, "P_adj"), "Path as adjunct")

## likelihood ratio test to test significance of proficiency
# model with proficiency
fm_pad_prof <- glmer(P_adj ~ Condition * cCloze + (1 | Subject) + (1 | VideoName),
                     data = d_l2, family = "binomial",
                     control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# model without proficiency
fm_pad_prof_null <- glmer(P_adj ~ Condition + (1 | Subject) + (1 | VideoName),
                          data = d_l2, family = "binomial",
                          control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
anova(fm_pad_prof_null, fm_pad_prof)  # not significant

# # if significant, go ahead and interpret model with proficiency
# summary(fm_pad_prof)
# kappa.mer(fm_pad_prof)
# vif.mer(fm_pad_prof)


# Manner in adjunct ---------------------------------------------------------

# simple plot
f_plot_prof(bysubj(d, "M_adj"), "Manner as adjunct")

## likelihood ratio test to test significance of proficiency
# model with proficiency
fm_mad_prof <- glmer(M_adj ~ Condition * cCloze + (1 | Subject) + (1 | VideoName),
                     data = d_l2, family = "binomial",
                     control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# model without proficiency
fm_mad_prof_null <- glmer(M_adj ~ Condition + (1 | Subject) + (1 | VideoName),
                          data = d_l2, family = "binomial",
                          control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
anova(fm_mad_prof_null, fm_mad_prof)

# if significant, go ahead and interpret model with proficiency
summary(fm_mad_prof)
kappa.mer(fm_mad_prof)
vif.mer(fm_mad_prof)



#  ------------------------------------------------------------------------
#  Trial-by-trial averages
#  ------------------------------------------------------------------------

# Anywhere ----------------------------------------------------------------

# path anywhere
trial_p_anyw <- d %>%
  group_by(Group, Condition, VideoTrial) %>%
  summarise(P = mean(P_anyw))
ggplot(trial_p_anyw, aes(x = VideoTrial, y = P, colour = Condition)) +
  geom_point() +
  facet_grid(. ~ Group) +
  geom_smooth() +
  ylim(0,1.05) +
  ggtitle("Path anywhere")

# manner anywhere
trial_m_anyw <- d %>%
  group_by(Group, Condition, VideoTrial) %>%
  summarise(M = mean(M_anyw))
ggplot(trial_m_anyw, aes(x = VideoTrial, y = M, colour = Condition)) +
  geom_point() +
  facet_grid(. ~ Group) +
  geom_smooth() +
  ylim(0,1) +
  ggtitle("Manner anywhere")


# Main verb ---------------------------------------------------------------

# path in verb
trial_p_V <- d %>%
  group_by(Group, Condition, VideoTrial) %>%
  summarise(P = mean(P_V))
ggplot(trial_p_V, aes(x = VideoTrial, y = P, colour = Condition)) +
  geom_point() +
  facet_grid(. ~ Group) +
  geom_smooth() +
  ylim(0,1.05) +
  ggtitle("Path in verb")

# manner in verb
trial_m_V <- d %>%
  group_by(Group, Condition, VideoTrial) %>%
  summarise(M = mean(M_V))
ggplot(trial_m_V, aes(x = VideoTrial, y = M, colour = Condition)) +
  geom_point() +
  facet_grid(. ~ Group) +
  geom_smooth() +
  ylim(-0.05,1) +
  ggtitle("Manner in verb")


# As adjuncts -------------------------------------------------------------

# path as adjunct
trial_p_adj <- d %>%
  group_by(Group, Condition, VideoTrial) %>%
  summarise(P = mean(P_adj))
ggplot(trial_p_adj, aes(x = VideoTrial, y = P, colour = Condition)) +
  geom_point() +
  facet_grid(. ~ Group) +
  geom_smooth() +
  ylim(-0.05, 1) +
  ggtitle("Path as adjunct")

# manner as adjunct
trial_m_adj <- d %>%
  group_by(Group, Condition, VideoTrial) %>%
  summarise(M = mean(M_adj))
ggplot(trial_m_adj, aes(x = VideoTrial, y = M, colour = Condition)) +
  geom_point() +
  facet_grid(. ~ Group) +
  geom_smooth() +
  ylim(-0.05,1) +
  ggtitle("Manner as adjunct")



#  ------------------------------------------------------------------------
#  Correlation plots
#  ------------------------------------------------------------------------

d_bysubj <- bysubj(d, "P_anyw", "P_anyw") %>%
  left_join(bysubj(d, "P_V", "P_V")) %>%
  left_join(bysubj(d, "P_adj", "P_adj")) %>%
  left_join(bysubj(d, "M_anyw", "M_anyw")) %>%
  left_join(bysubj(d, "M_V", "M_V")) %>%
  left_join(bysubj(d, "M_adj", "M_adj"))

library(GGally)
ggpairs(d_bysubj, columns = c("P_anyw", "P_V", "P_adj"))
ggpairs(d_bysubj[, 5:10])

