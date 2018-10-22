## Plot coefficients by subject (with ranef adjustments) to see if L2 speakers
## come to resemble natives as they increase in proficiency.

# This is based on TFJ's suggestion (Jaeger, Florian <fjaeger@UR.Rochester.edu>,
# Subj: Re: testing for interactions in GAMs, ons 2018-09-26 20:07). Quote:

# "Taking a step back, we can define native-likeness in any way we want. E.g.,
# to give an arbitrary example, if ---after inspection of the native data--we
# think it's best captured by the linear (GLMM-derived) coefficient of 
# prime:trial over the first 5 trials for path priming and for the same
# coefficient for the first 10 trials of manner priming, we could do that. E.g.
# imagine comparing all NS and L2 subjects' coefficients for these two effects
# against the mean of the NS subjects (bonus points: all subjects plotted on a
# landscape of x = coef for path prime : trial  [coef + subject-specific
# adjustment of coef from NS and L2 model, respectively]; y = coef for manner
# prime : trial; shape = NS vs L2; color = black for NS & gradient based on
# proficiency for L2 subjects; subject-specific CIs along x & y optional). The
# likelihood measure was meant as a nice concise summary, and you're right it
# puts a pretty high bar on 'similarity'. Still worth trying, in my view, but
# a null result is indeed less informative."

# Notes:
# - The likelihood measure mentioned above is explored in script "analysis/
#   Q3_follow-up_compare-LL.R" -- results were difficult to interpret
# - The exact idea presented above (a landscpae as TFJ describes) is not 
#   feasible given the data, because VerbType and Condition are both between-
#   subject variables

library("dplyr")
library("lme4")
# library("purrr")
library("sjPlot")
library("ggplot2")



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Load data ---------------------------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

## Load data file
d <- read.csv("data/data_gamms_baseline-doublecounted.csv",
              stringsAsFactors = FALSE)
head(d)

# Convenience function to process Path/Manner subsets of data for GLMMs:
prepare_df <- function(df = NULL, verbtype = NULL, trials = NULL) {
  df <- df %>% 
    filter(VerbType == verbtype & Trial %in% trials) %>%
    select(Subject, Condition = Condition_bin, Group, Trial, cTrial, VideoName,
           ClozeScore, Used)
  df$cTrial <- df$Trial - mean(df$Trial)
  df$Subject <- factor(df$Subject)
  df$Condition <- factor(df$Condition)
  contrasts(df$Condition) <- - contr.sum(2)
  colnames(contrasts(df$Condition)) <- "primed_vs_baseline"
  df$Group <- factor(df$Group)
  contrasts(df$Group) <- contr.sum(2)
  colnames(contrasts(df$Group)) <- "L2_vs_NS"
  df
}


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Path verb model ---------------------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Based on visual inspection of plots in main_analyses.html, we define the
# trials of interest for the path comparison to be Trials = 1-13
d_p <- prepare_df(d, "P_V", 1:13)
head(d_p)

## Fit GLMM
# Failures to converge are commented out below; we simplify by-item random
# effects and take the first model that converges.

# fm_path1 <- glmer(
#   Used ~ Condition * Group * cTrial + (1 + cTrial | Subject) + 
#     (1 + Condition * Group * cTrial | VideoName),
#   data = d_p, family = 'binomial', 
#   control = glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5))
#   )
# 
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   unable to evaluate scaled gradient
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

# fm_path2 <- glmer(
#   Used ~ Condition * Group * cTrial + (1 + cTrial | Subject) +
#     (1 + Group * cTrial | VideoName),
#   data = d_p, family = 'binomial',
#   control = glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5))
#   )
# 
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   Model failed to converge with max|grad| = 0.0700389 (tol = 0.001, component 1)
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   Model is nearly unidentifiable: very large eigenvalue
#  - Rescale variables?

# # fit model
# fm_path <- glmer(
#   Used ~ Condition * Group * cTrial + (1 + cTrial | Subject) +
#     (1 + Group + cTrial | VideoName),
#   data = d_p, family = 'binomial',
#   control = glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5))
#   )
# saveRDS(fm_path, file = "analysis/gamms/glmm_path.rds")

# or load if already saved to disk
fm_path <- readRDS("analysis/gamms/glmm_path.rds")

summary(fm_path)



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Plot path verb model ----------------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# sjPlot::plot_model - see
# https://www.rdocumentation.org/packages/sjPlot/versions/2.6.0/topics/plot_model
# Plotting marginal effects
# https://strengejacke.github.io/sjPlot/articles/plot_marginal_effects.html
# Plotting interactions
# https://strengejacke.github.io/sjPlot/articles/plot_interactions.html

# fixed effects
plot_model(fm_path)
plot_model(fm_path, type = "eff", terms = "cTrial")
plot_model(fm_path, type = "eff", terms = c("cTrial", "Group"))
plot_model(fm_path, type = "eff", terms = c("cTrial", "Group", "Condition"))
plot_model(fm_path, type = "pred", terms = c("cTrial", "Group", "Condition"))  # Why different CIs?

# random effects
plot_model(fm_path, type = "re")
plot_model(fm_path, type = "diag")


# Add predictions (in log-odds) to data frame
d_p$Predicted <- predict(fm_path)

d_p %>%
  group_by(Condition, Group, Trial) %>%
  summarise(Y = mean(Predicted)) %>%
  ggplot(aes(x = Trial, y = Y, shape = Condition, colour = Group)) +
  geom_point() +
  geom_line(aes(linetype = Condition))

d_p %>%
  group_by(Condition, Group, Trial, Subject) %>%
  summarise(Y = mean(Predicted)) %>%
  ggplot(aes(x = Trial, y = Y, shape = Condition, colour = Group)) +
  geom_jitter(height = 0, width = .2, alpha = .2) +
  # geom_point(alpha = .2) +
  geom_smooth(aes(linetype = Condition))



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Extract by-subject coefficient estimates --------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Functions are sourced from separate script
source("analysis/functions/extract-subject-estimates_glmer_fnc.R")

# Take the data frame and for each subject (see row names) output their 
# individual model matrix
(mm_p <- factors2modelmatrix(d_p)) %>% head

# The by-subject coefficients estimated from the model (which add random effect
# adjustments / BLUPs to fixed effects) are obtained with the coef() function 
# (see Ben Bolker's answer:
# https://stackoverflow.com/questions/26198958/extracting-coefficients-and-their-standard-error-from-lme)
head(coef(fm_path)$Subject)
# Note that the column names match exactly those in the model matrix, so we can
# matrix multiply by the transpose of the coefficients



ggplot(sbj_p, aes(x = Intercept, y = cTrial, colour = Group)) +
  geom_point() + 
  facet_grid(. ~ Condition)

coef(fm_path)$Subject %>% head
coef(fm_path)$Subject %>% str
rownames(coef(fm_path)$Subject)

sbj_coefs_p <- coef(fm_path)$Subject
sbj_p <- d_p %>%
  head(sbj_p)

model.matrix(fm_path) %>% head




# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Manner verb model -------------------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Based on visual inspection of plots in main_analyses.html, we define the
# trials of interest for the path comparison to be Trials = 10-32
d_m <- prepare_df(d, "M_V", 10:32)

## Fit GLMM
# Failures to converge are commented out below; we simplify by-item random
# effects and take the first model that converges.

# fm_manner1 <- glmer(
#   Used ~ Condition * Group * cTrial + (1 + cTrial | Subject) + 
#     (1 + Condition * Group * cTrial | VideoName),
#   data = d_m, family = 'binomial', 
#   control = glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5))
#   )
# 
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   unable to evaluate scaled gradient
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

# fm_manner2 <- glmer(
#   Used ~ Condition * Group * cTrial + (1 + cTrial | Subject) +
#     (1 + Group * cTrial | VideoName),
#   data = d_m, family = 'binomial',
#   control = glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5))
#   )
# 
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   Model failed to converge with max|grad| = 0.0700389 (tol = 0.001, component 1)
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   Model is nearly unidentifiable: very large eigenvalue
#  - Rescale variables?

# # fit model
# fm_manner <- glmer(
#   Used ~ Condition * Group * cTrial + (1 + cTrial | Subject) +
#     (1 + Group + cTrial | VideoName),
#   data = d_m, family = 'binomial',
#   control = glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5))
# )
# saveRDS(fm_manner, file = "analysis/gamms/glmm_manner.rds")

# or load if already saved to disk
fm_manner <- readRDS("analysis/gamms/glmm_manner.rds")


summary(fm_manner)

plot_model(fm_manner)

