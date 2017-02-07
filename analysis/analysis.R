## Analyse data from participants in the primed conditions.

library(dplyr)
library(lazyeval)  # lazy evaluation used in bysubj() function [summarise_]
library(ggplot2)
library(lme4)
library(boot)  # for inv.logit()
library(knitr)  # for kable()

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



# Outliers? ---------------------------------------------------------------

# L2 speakers Cloze scores
cloze <- ppts %>%
  filter(Group == "L2") %>%
  select(Subject, Condition, ClozeScore)
# z-scores
cloze$zClozeScore <- scale(cloze$ClozeScore)
# Subjects ordered by clozescore
ppts_ordered <- cloze[order(cloze$zClozeScore), "Subject"]
# check out
cloze[ppts_ordered, ]

# Consider ClozeScore = 5 as outlier
threshold5 <- cloze[cloze$ClozeScore < 5, "Subject"]
# Consider ClozeScore = 10 as outlier
threshold10 <- cloze[cloze$ClozeScore < 10, "Subject"]

# Exclude participants? (Uncomment to exclude)
d <- d[!d$Subject %in% threshold5, ]
# d <- d[!d$Subject %in% threshold10, ]

rm(cloze, ppts_ordered, threshold10, threshold5)

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

# here the emphasis is put between groups (NS vs L2)
f_plot <- function(df, varname = NULL, avg_type = "mean") {
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

# # plot group patterns -- emphasis put between conditions in each group
# f_plot2 <- function(df, varname = NULL) {
#   p <- ggplot(df, aes(x = Group, y = M, colour = Condition)) +
#     geom_point(position = position_jitterdodge()) +
#     stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = .85, width = .5,
#                  position = "dodge") +
#     ylim(0,1) +
#     ylab(paste("Proportion of descriptions\nexpressing", varname)) + 
#     ggtitle(varname)
#   p
# }


# Plot effect of proficiency (cloze score)
f_plot_prof <- function(df, varname = NULL) {
  df <- df[df$Group == "L2", ]
  p <- ggplot(df, aes(x = ClozeScore, y = M, colour = Condition)) +
    geom_jitter(height = 0) +
    geom_smooth(method = "lm") +
    ylab(paste("Proportion of descriptions\nexpressing", varname)) + 
    ggtitle(varname)
  p
}

# Plot trial by trial averages
f_plot_trial <- function(df, varname = NULL) {
  if(is.null(varname)) stop("Specify varname (i.e., the name of the DV)")
  p <- ggplot(df, aes(x = VideoTrial, y = Avg, colour = Group)) +
    geom_point() +
    facet_grid(. ~ Condition) +
    geom_smooth() +
    ylim(-.05,1.05) +
    ylab(paste("Proportion of descriptions\nexpressing", varname)) + 
    ggtitle(varname)
  p
}

# Plot trial by trial averages, but only for one of the conditions
f_plot_trial2 <- function(df, varname = NULL) {
  if(is.null(varname)) stop("Specify varname (i.e., the name of the DV)")
  p <- ggplot(df, aes(x = VideoTrial, y = Avg, colour = Group)) +
    geom_point() +
    # facet_grid(. ~ Condition) +
    geom_smooth() +
    ylim(-.05,1.05) +
    ylab(paste("Proportion of descriptions\nexpressing", varname)) + 
    ggtitle(varname)
  p
}


# create a table of the summary of fixed effects of a model
fm_table <- function(fm) {
  m <- round(summary(fm)$coefficients, 3)
  tb <- as.data.frame(m)
  names(tb) <- c("Estimate", "SE", "z-value", "p-value")
  kable(tb)
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
# center predictor VideoTrial (without as.vector() it's turned into a matrix)
d_fm$cVideoTrial <- as.vector(scale(d_fm$VideoTrial, scale = FALSE))


# Path anywhere -----------------------------------------------------------

# # plot all conditions for reference
# # f_plot2(bysubj(d, "P_anyw"), "Path anywhere")
# f_plot(bysubj(d, "P_anyw"), "Path anywhere")
# 
# # fit model
# fm_pan <- glmer(P_anyw ~ Group * Condition + (1 | Subject) + (1 | VideoName),
#                 data = d_fm, family = "binomial",
#                 control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# summary(fm_pan)
# kappa.mer(fm_pan)
# vif.mer(fm_pan)


# Manner anywhere ---------------------------------------------------------

# # plot all conditions for reference
# # f_plot2(bysubj(d, "M_anyw"), "Manner anywhere")
# f_plot(bysubj(d, "M_anyw"), "Manner anywhere")
# 
# # fit model
# fm_man <- glmer(M_anyw ~ Group * Condition + (1 | Subject) + (1 | VideoName),
#                 data = d_fm, family = "binomial",
#                 control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# summary(fm_man)
# kappa.mer(fm_man)
# vif.mer(fm_man)


# Path in the verb --------------------------------------------------------

# plot all conditions for reference
# f_plot2(bysubj(d, "P_V"), "Path in V")
f_plot(bysubj(d, "P_V"), "Path in V")

# fit model
fm_pve <- glmer(P_V ~ Group * Condition + (1 | Subject) + (1 | VideoName),
                data = d_fm, family = "binomial", 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(fm_pve)
kappa.mer(fm_pve)
vif.mer(fm_pve)

fm_table(fm_pve)

# # which r.e. structure is justified?
# fm_pve1 <- glmer(P_V ~ Group * Condition + (1 | Subject) + (1 + Group | VideoName),
#                 data = d_fm, family = "binomial", 
#                 control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# summary(fm_pve1)
# anova(fm_pve, fm_pve1)
# 
# fm_pve2 <- glmer(P_V ~ Group * Condition + (1 | Subject) + (1 + Condition | VideoName),
#                 data = d_fm, family = "binomial", 
#                 control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# summary(fm_pve2)
# anova(fm_pve, fm_pve2)
# 
# fm_pve3 <- glmer(P_V ~ Group * Condition + (1 | Subject) + (1 + Group * Condition | VideoName),
#                 data = d_fm, family = "binomial", 
#                 control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# summary(fm_pve3)
# anova(fm_pve, fm_pve3)


# Manner in the verb ------------------------------------------------------

# plot all conditions for reference
# f_plot2(bysubj(d, "M_V"), "Manner in V")
f_plot(bysubj(d, "M_V"), "Manner in V")

# fit model
fm_mve <- glmer(M_V ~ Group * Condition + (1 | Subject) + (1 | VideoName),
                data = d_fm, family = "binomial",
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(fm_mve)
kappa.mer(fm_mve)
vif.mer(fm_mve)

fm_table(fm_mve)


# Path as adjunct ---------------------------------------------------------

# plot all conditions for reference
# f_plot2(bysubj(d, "P_adj"), "Path as adjunct")
f_plot(bysubj(d, "P_adj"), "Path as adjunct")

# fit model
fm_pad <- glmer(P_adj ~ Group * Condition + (1 | Subject) + (1 | VideoName),
                data = d_fm, family = "binomial",
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(fm_pad)
kappa.mer(fm_pad)
vif.mer(fm_pad)


# Manner as adjunct -------------------------------------------------------

# plot all conditions for reference
# f_plot2(bysubj(d, "M_adj"), "Manner as adjunct")
f_plot(bysubj(d, "M_adj"), "Manner as adjunct")

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

# # Path anywhere
# f_plot_prof(bysubj(d, "P_anyw"), "Path anywhere")
# 
# ## likelihood ratio test to test significance of proficiency
# # model with proficiency
# fm_pan_prof <- glmer(P_anyw ~ Condition * cCloze + (1 | Subject) + (1 | VideoName),
#                      data = d_l2, family = "binomial",
#                      control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# # model without proficiency
# fm_pan_prof_null <- glmer(P_anyw ~ Condition + (1 | Subject) + (1 | VideoName),
#                           data = d_l2, family = "binomial",
#                           control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# anova(fm_pan_prof_null, fm_pan_prof)
# # if significant, go ahead and interpret model with proficiency
# summary(fm_pan_prof)
# kappa.mer(fm_pan_prof)
# vif.mer(fm_pan_prof)


# Manner anywhere -----------------------------------------------------------

# # Manner anywhere
# f_plot_prof(bysubj(d, "M_anyw"), "Manner anywhere")
# 
# ## likelihood ratio test to test significance of proficiency
# # model with proficiency
# fm_man_prof <- glmer(M_anyw ~ Condition * cCloze + (1 | Subject) + (1 | VideoName),
#                      data = d_l2, family = "binomial",
#                      control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# # model without proficiency
# fm_man_prof_null <- glmer(M_anyw ~ Condition + (1 | Subject) + (1 | VideoName),
#                           data = d_l2, family = "binomial",
#                           control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# anova(fm_man_prof_null, fm_man_prof)  # trend towards significance
# 
# # if significant, go ahead and interpret model with proficiency
# summary(fm_man_prof)
# kappa.mer(fm_man_prof)
# vif.mer(fm_man_prof)


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

fm_table(fm_pve_prof)


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

fm_table(fm_mad_prof)



#  ------------------------------------------------------------------------
#  Trial-by-trial averages
#  ------------------------------------------------------------------------

# Anywhere ----------------------------------------------------------------

# # path anywhere
# trial_p_anyw <- d %>%
#   group_by(Group, Condition, VideoTrial) %>%
#   summarise(Avg = mean(P_anyw))
# f_plot_trial(trial_p_anyw, "Path anywhere")
# 
# # manner anywhere
# trial_m_anyw <- d %>%
#   group_by(Group, Condition, VideoTrial) %>%
#   summarise(Avg = mean(M_anyw))
# f_plot_trial(trial_m_anyw, "Manner anywhere")


# Main verb ---------------------------------------------------------------

# path in verb
trial_p_V <- d %>%
  group_by(Group, Condition, VideoTrial) %>%
  summarise(Avg = mean(P_V))
f_plot_trial(trial_p_V, "Path in verb")
# only for path primed
f_plot_trial2(trial_p_V %>% filter(Condition == "Path"), "Path in verb (Path-priming)")


# manner in verb
trial_m_V <- d %>%
  group_by(Group, Condition, VideoTrial) %>%
  summarise(Avg = mean(M_V))
f_plot_trial(trial_m_V, "Manner in verb")
# only for manner primed
f_plot_trial2(trial_m_V %>% filter(Condition == "Manner"), "Manner in verb (Manner-priming)")


# As adjuncts -------------------------------------------------------------

# path as adjunct
trial_p_adj <- d %>%
  group_by(Group, Condition, VideoTrial) %>%
  summarise(Avg = mean(P_adj))
f_plot_trial(trial_p_adj, "Path as adjunct")

# manner as adjunct
trial_m_adj <- d %>%
  group_by(Group, Condition, VideoTrial) %>%
  summarise(Avg = mean(M_adj))
f_plot_trial(trial_m_adj, "Manner as adjunct")



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

