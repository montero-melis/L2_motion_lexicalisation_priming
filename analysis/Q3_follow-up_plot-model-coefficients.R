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
# - The exact suggestion presented above (a landscpae as TFJ describes) is not
#   feasible given the current design, because VerbType and Condition are
#   both between-subject variables, so coef for Manner and Path cannot appear
#   on the same plot.


library("dplyr")
library("lme4")
library("sjPlot")
library("ggplot2")
library("tidyr")



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Load data ---------------------------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# Load data file
d <- read.csv("data/data_gamms_baseline-doublecounted.csv",
              stringsAsFactors = FALSE)
head(d)

# For the plots below, we set ClozeScore of native speakers arbitrarily to 40
max(d$ClozeScore, na.rm = TRUE)  # Max score among L2ers (no overlap)
d[d$Group == "NS", "ClozeScore"] <- 40


# Convenience function to process Path/Manner subsets of data for GLMMs:
prepare_df <- function(df = NULL, verbtype = NULL, trials = NULL, group = NULL) {
  df <- df %>% 
    filter(VerbType == verbtype & Trial %in% trials) %>%
    select(Subject, Condition = Condition_bin, Group, Trial, cTrial, VideoName,
           ClozeScore, Used)
  # Select group if argument is passed (not NULL)
  if (! is.null(group) ) {
    df <- df %>% filter(Group == group) 
  } else {
    df$Group <- factor(df$Group)
    contrasts(df$Group) <- contr.sum(2)
    colnames(contrasts(df$Group)) <- "L2_vs_NS"
  }
  df$cCloze <- df$ClozeScore - mean(df$ClozeScore)
  df$cTrial <- df$Trial - mean(df$Trial)
  df$Subject <- factor(df$Subject)
  df$Condition <- factor(df$Condition)
  contrasts(df$Condition) <- - contr.sum(2)
  colnames(contrasts(df$Condition)) <- "primed_vs_baseline"
  df
}



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Path verb model ---------------------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Based on visual inspection of plots in main_analyses.html, we define the
# trials of interest for the path comparison to be Trials = 1-13
d_p <- prepare_df(d, "P_V", 1:13)
head(d_p)
tail(d_p)

## Fit GLMM

# Failures to converge are commented out below; we progressively simplify
# by-item random effects and take the first model that converges.

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

# # FINAL model:
# fm_path <- glmer(
#   Used ~ Condition * Group * cTrial + (1 + cTrial | Subject) +
#     (1 + Condition + Group + cTrial | VideoName),
#   data = d_p, family = 'binomial',
#   control = glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5))
#   )
# saveRDS(fm_path, file = "analysis/gamms/glmm_path.rds")

# load it if already saved to disk
fm_path <- readRDS("analysis/gamms/glmm_path.rds")
summary(fm_path)



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Plot path verb model ----------------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Add predictions (in log-odds) to data frame
d_p$Predicted <- predict(fm_path)

# The wiggly lines probably come from the random effects by items
d_p %>%
  group_by(Condition, Group, Trial) %>%
  summarise(Y = mean(Predicted)) %>%
  ggplot(aes(x = Trial, y = Y, shape = Condition, colour = Group)) +
  geom_point() +
  geom_line(aes(linetype = Condition))

# Another option is to fit smooths on the predictions for each trial
d_p %>%
  group_by(Condition, Group, Trial, Subject) %>%
  summarise(Y = mean(Predicted)) %>%
  ggplot(aes(x = Trial, y = Y, shape = Condition, colour = Group)) +
  geom_jitter(height = 0, width = .2, alpha = .2) +
  geom_smooth(aes(linetype = Condition))


## Use sjPlot::plot_model - see documentation:

# https://www.rdocumentation.org/packages/sjPlot/versions/2.6.0/topics/plot_model
# Plotting marginal effects
# https://strengejacke.github.io/sjPlot/articles/plot_marginal_effects.html
# Plotting interactions
# https://strengejacke.github.io/sjPlot/articles/plot_interactions.html

# fixed effects
plot_model(fm_path)
plot_model(fm_path, type = "eff", terms = "cTrial")
plot_model(fm_path, type = "eff", terms = c("cTrial", "Group"))
plot_model(fm_path, type = "eff", terms = c("cTrial", "Condition"))
# Why different CIs for type = "eff" vs "pred"?
plot_model(fm_path, type = "eff",  terms = c("cTrial", "Condition", "Group"))
plot_model(fm_path, type = "pred", terms = c("cTrial", "Condition", "Group"))
# From link above: "type = 'eff' ... is similar to type = 'pred', however,
# discrete predictors are held constant at their proportions (not reference 
# level)." 
# Now, since all predictors are centred or contrast-coded, it shouldn't make
# much of a difference?

# random effects
plot_model(fm_path, type = "re")
plot_model(fm_path, type = "diag")



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Extract by-subject coefficient estimates: Path --------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Functions are sourced from separate script
source("analysis/functions/extract-subject-estimates_glmer_fnc.R")

# Take the data frame and, for each subject (see row names), output their
# individual model matrix
(mm_p <- factors2modelmatrix(d_p)) %>% head

# We can easily obtain the subject-specific estimates, combining fixefs and
# ranefs:
head(coef(fm_path)$Subject)

# The following function combines the model estimates (fixef and ranef) into
# two values per subject: 1) their intercept and 2) their slope for Trial:
(sbj_p <- get_ppt_estimate(mm_p, fm_path, df = d_p)) %>% head


## Plot these estimates

# It's difficult to see if L2ers get more native-like with a landscape plot...
ggplot(sbj_p, aes(x = Intercept, y = Trial, colour = ClozeScore, shape = Group)) +
  geom_point(size = 2) + 
  facet_grid(. ~ Condition)

# ... So let's break it down further
# long format:
sbj_pl <- gather(sbj_p, coef, estimate, Intercept : Trial)

ggplot(sbj_pl, aes(x = ClozeScore, y = estimate, colour = Group)) +
  geom_jitter(alpha = .3, size = 1.5) +
  facet_grid(coef ~ Condition, scales = "free_y") +
  geom_smooth(method = "lm") +
  ggtitle("Path model") +
  stat_summary(
    data = sbj_pl %>% filter(Group == "NS"),
    fun.data = mean_cl_boot,
    geom = "errorbar",
    size = 1.5, width = 3
  )


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Manner verb model -------------------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Based on visual inspection of plots in main_analyses.html, we define the
# trials of interest for the path comparison to be Trials = 10-32
d_m <- prepare_df(d, "M_V", 10:32)


## Fit GLMM

# # Maximal model *does* converge
# fm_manner <- glmer(
#   Used ~ Condition * Group * cTrial + (1 + cTrial | Subject) +
#     (1 + Condition * Group * cTrial | VideoName),
#   data = d_m, family = 'binomial',
#   control = glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5))
#   )
# saveRDS(fm_manner, file = "analysis/gamms/glmm_manner.rds")

# or load if already saved to disk
fm_manner <- readRDS("analysis/gamms/glmm_manner.rds")
summary(fm_manner)



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Plot manner verb model --------------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Add predictions (in log-odds) to data frame
d_m$Predicted <- predict(fm_manner)

# Even more wiggly lines than for path (bc more complex by-item ranef structure?)
d_m %>%
  group_by(Condition, Group, Trial) %>%
  summarise(Y = mean(Predicted)) %>%
  ggplot(aes(x = Trial, y = Y, shape = Condition, colour = Group)) +
  geom_point() +
  geom_line(aes(linetype = Condition))

# Another option is to fit smooths on the predictions by trial
d_m %>%
  group_by(Condition, Group, Trial, Subject) %>%
  summarise(Y = mean(Predicted)) %>%
  ggplot(aes(x = Trial, y = Y, shape = Condition, colour = Group)) +
  geom_jitter(height = 0, width = .2, alpha = .2) +
  geom_smooth(aes(linetype = Condition))


## Using sjPlot::plot_model()

# fixed effects
plot_model(fm_manner)
plot_model(fm_manner, type = "eff", terms = "cTrial")
plot_model(fm_manner, type = "eff", terms = c("cTrial", "Group"))
plot_model(fm_manner, type = "eff", terms = c("cTrial", "Condition"))
plot_model(fm_manner, type = "eff", terms = c("cTrial", "Condition", "Group"))
plot_model(fm_manner, type = "pred", terms = c("cTrial", "Condition", "Group"))  # Why different CIs?

# random effects
plot_model(fm_manner, type = "re")
plot_model(fm_manner, type = "diag")



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Extract by-subject coefficient estimates: Manner ------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Take the data frame and for each subject (see row names) output their 
# individual model matrix
(mm_m <- factors2modelmatrix(d_m)) %>% head

# We can easily obtain the subject-specific estimates, combining fixefs and
# ranefs:
head(coef(fm_manner)$Subject)

# The following function comptues two estimates for each subject: their intercept
# and their slope for Trial (combining fixef and by-subject ranefs):
(sbj_m <- get_ppt_estimate(mm_m, fm_manner, df = d_m)) %>% head


# It's difficult to see if L2ers get more native-like with a landscape plot...
ggplot(sbj_m, aes(x = Intercept, y = Trial, colour = ClozeScore, shape = Group)) +
  geom_point() + 
  facet_grid(. ~ Condition)

# ... So let's break it down further
# long format:
sbj_ml <- gather(sbj_m, coef, estimate, Intercept : Trial)

ggplot(sbj_ml, aes(x = ClozeScore, y = estimate, colour = Group)) +
  geom_jitter(alpha = .3, size = 1.5) +
  facet_grid(coef ~ Condition, scales = "free_y") +
  geom_smooth(method = "lm") +
  ggtitle("Manner model") +
  stat_summary(
    data = sbj_pl %>% filter(Group == "NS"),
    fun.data = mean_cl_boot,
    geom = "errorbar",
    size = 1.5, width = 3
  )


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Modelling L2 proficiency (Cloze scores) as fixef ------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

d_l2_p <- prepare_df(d, "P_V", 1:13, "L2")
d_l2_m <- prepare_df(d, "M_V", 10:32, "L2")


## PATH model

# fm_l2_path1 <- glmer(
#   Used ~ Condition * cTrial * cCloze +
#     (1 + Condition * cTrial * cCloze | VideoName) +
#     (1 + cTrial | Subject),
#   data = d_l2_p, family = 'binomial',
#   control = glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5))
#   )
# 
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   unable to evaluate scaled gradient
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   Model failed to converge: degenerate  Hessian with 2 negative eigenvalues

# # FINAL model:
# fm_l2_path <- glmer(
#   Used ~ Condition * cTrial * cCloze +
#     (1 + Condition + cTrial * cCloze | VideoName) +
#     (1 + cTrial | Subject),
#   data = d_l2_p, family = 'binomial',
#   control = glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5))
# )
# saveRDS(fm_l2_path, file = "analysis/gamms/glmm_l2_path.rds")

# Load it if already fit
fm_l2_path <- readRDS("analysis/gamms/glmm_l2_path.rds")
summary(fm_l2_path)

plot_model(fm_l2_path, type = "eff",
           terms = c("cTrial", "Condition", "cCloze [-15,-10,-5,0,5,10]"))



## MANNER model

# fm_l2_manner1 <- glmer(
#   Used ~ Condition * cTrial * cCloze +
#     (1 + Condition * cTrial * cCloze | VideoName) +
#     (1 + cTrial | Subject),
#   data = d_l2_m, family = 'binomial',
#   control = glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5))
# )
# 
# Warning messages:
# 1: In optwrap(optimizer, devfun, start, rho$lower, control = control,  :
#   convergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   unable to evaluate scaled gradient
# 3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   Model failed to converge: degenerate  Hessian with 2 negative eigenvalues


# fm_l2_manner2 <- glmer(
#   Used ~ Condition * cTrial * cCloze +
#     (1 + Condition + cTrial * cCloze | VideoName) +
#     (1 + cTrial | Subject),
#   data = d_l2_m, family = 'binomial',
#   control = glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5))
# )
# 
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   unable to evaluate scaled gradient
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   Model failed to converge: degenerate  Hessian with 2 negative eigenvalues


# fm_l2_manner3 <- glmer(
#   Used ~ Condition * cTrial * cCloze +
#     (1 + Condition * cTrial + cCloze | VideoName) +
#     (1 + cTrial | Subject),
#   data = d_l2_m, family = 'binomial',
#   control = glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5))
# )
# 
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   unable to evaluate scaled gradient
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   Model failed to converge: degenerate  Hessian with 3 negative eigenvalues


# # FINAL model:
# fm_l2_manner <- glmer(
#   Used ~ Condition * cTrial * cCloze +
#     (1 + Condition + cTrial + cCloze | VideoName) +
#     (1 + cTrial | Subject),
#   data = d_l2_m, family = 'binomial',
#   control = glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5))
# )
# saveRDS(fm_l2_manner, file = "analysis/gamms/glmm_l2_manner.rds")

# Load it if already fit
fm_l2_manner <- readRDS("analysis/gamms/glmm_l2_manner.rds")
summary(fm_l2_manner)

plot_model(fm_l2_manner, type = "eff", 
           terms = c("cTrial", "Condition", "cCloze [-15,-10,-5,0,5,10]"))
