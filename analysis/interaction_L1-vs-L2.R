## Script to test whether there is a difference in adaptation at the group
## level when comparing L1 and L2 speakers.

# Background:

# In the paper (draft6 from 180601), we write:
# 
# "Thus, learners differed from native speakers in the direction one would
# expect if they were basing their L2 expectations on a mixture of their L1
# and L2 experience. Even if learners did not show a statistically stronger
# adaptation to path verbs than to manner verbs, they showed this numerical
# tendency, whereas native speakers showed the opposite tendency (they adapted
# to manner verbs but not to path verbs)."
# 
# Florian (comments on above draft) notes that this leads to the "classic 
# interaction fallacy. We can’t conclude from different patterns, that they are
# significantly different. This needs to be acknowleged, or better (required, 
# I’d say) L1 vs. L2 should be included in the/a follow-up analysis."
# 
# Since this interaction analysis is difficult to implement in the GAM 
# framework, I carry out a mixed logistic regression to ask this. 
# 
# Note, however, that this is not ideal because the trends in the data are not
# linear (in log-odds space).
# Additionally, it should be noted that we will be testing a 4-way interaction:
# VerbType (Path/Manner) x PrimeCondition (baseline/primed) x
# LanguageGroup (L1/L2) x Trial (1 through 32).

# To reuse some of the code in "analysis/supplementary-info.Rmd" that sets up
# the data, take advantage of the function "analysis/functions/rmd2rscript.R"!



#  ------------------------------------------------------------------------
#  Set up data
#  ------------------------------------------------------------------------


library(dplyr)
library(lme4)
library(ggplot2)
library(tidyr)
# library(mgcv)  # GAMs and GAMMs (Wood 2006)	
# library(itsadug)	
# library(boot)  # for inv.logit()	
# library(knitr)  # for kable()	
# library(lazyeval)  # lazy evaluation used in bysubj() function [summarise_]	
# library(effects)	
# library(xtable)	


setwd("analysis/")

#' ## Data loading and processing	
#' 	
#' ### Load data	
#' 	
#' 	
##  Load data	
# The data is created in the script 'processing/compute_dependent_measures.R'	
# There is the normal and the liberally coded version (see script for difference).	
# Here I use the normal coding.	

# load	
d <- read.csv('../data/data_DVs.csv', fileEncoding = 'UTF-8', stringsAsFactors = TRUE)	
# simplify somewhat	
d <- d %>%	
  select(Subject:VideoName, P_V, M_V) %>%	
  rename(Trial = VideoTrial)	

head(d)
str(d)

# Rename "Control" condition to "Baseline"	
levels(d$Condition)[levels(d$Condition) == "Control"] <- "Baseline"	

# participant data	
ppts <- read.csv("../data/participants.csv", fileEncoding = "UTF-8",	
                 stringsAsFactors = TRUE)	

# No audio data recorded for Subject 14 (L2) due to experimental error; exclude	
ppts <- ppts[ppts$Subject != 14, ]	
# transform ClozeScore to z-score (as.vector prevents it from becoming a matrix)	
ppts$zClozeScore <- as.vector(scale(ppts$ClozeScore))	
# center ClozeScore but not scaling (as.vector prevents it from becoming a matrix)	
ppts$cClozeScore <- as.vector(scale(ppts$ClozeScore, scale = FALSE))	

# # add speakers' clozescore to d:	
# d <- left_join(d, ppts %>% select(Subject, ClozeScore, zClozeScore, cClozeScore))	

# Subject needs to be a factor	
d$Subject <- factor(d$Subject)	

# in Group, let the native speakers (NS) be the reference group	
d$Group <- factor(d$Group, levels = c('NS', 'L2'))	


# # Add info about prime verbs to the data file (by joining two dataframes)	
# # Load table with priming verbs and then join	
# primes <- read.csv("../data/priming-verbs.csv")	
# head(primes)	
# d <- left_join(d, primes)	

head(d)	
str(d)	


#' ### Data for GAMs comparing NS and L2 speakers	
#' 	
#' - We will subset the data so that we can run one model for native speakers and another one for L2 speakers	
#' - In each model we compare Baseline Path vs Exposed Path and Baseline Manner vs Exposed Manner	
#' 	
#' Below: Subset data, define factors, set coding scheme, etc.	
#' 	
#' 	
## Single factor to model VerbType * Condition, using dummy coding:	
# (The way to analyze crossed factors is to combine their levels into 	
# a single factor, see Wood's comment here	
# http://grokbase.com/t/r/r-help/113qaadxt4/r-how-to-add-in-interaction-terms-in-gamm)	

# First need to convert data to long format:	
d_long <- gather(d, VerbType, Used, P_V:M_V)	
# single factor
d_long$VbType_Cond <- with(d_long, interaction(VerbType, Condition))	

# For the subjects  to be properly fitted as random smooths in the GAMs	
# we have to "pretend" that a baseline subject is two different subjects,	
# one for the comparison to path-primed, the other to manner-primed participants;	
# this may not be ideal statistically, but not doing this would turn the random	
# smooths into random intercepts, which is much worse.	
d_long$Subject <- with(d_long, interaction(Subject, VerbType))	

## Subset data for model fitting:
# We are removing observations that	
# correspond to path verbs produced in the manner-primed condition or to manner	
# verbs produced in the path-primed condition)	
d_mod <- d_long %>% filter(! VbType_Cond %in% c("P_V.Manner", "M_V.Path"))	
# drop unused factors for subject (I think this is important for gam fitting)	
d_mod$Subject <- factor(d_mod$Subject)	

# Use contrast coding to compare groups	
contrasts(d_mod$Group) <- - contr.sum(2) / 2	
colnames(contrasts(d_mod$Group)) <- "L2_vs_NS"	
contrasts(d_mod$Group)	


## Specify some global parameters	

# adjust figure heght/width when not going with default (espec. for 2x2 plots)	
myfighe_NS_L2 <- 6	
myfighe_L2_prof <- 6	
myfigwi <- 7	


## Source somewhat more complex functions	

# # source functions to compute collinearity diagnostics	
# source("functions/kappa_mer_fnc.R")	
# source("functions/vif_mer_fnc.R")	
# 
# # Function that plots mixed model-estimates in logit space from baseline	
# # conditions, including speaker estimates	
# source("functions/plot_glmm_fnc.R")	
# 
# # source multiplot function	
# source("functions/multiplot_fnc.R")	
# 
# # Function used to load models if they have already been saved,	
# # rather than fitting them anew each time the script is called	
# source("functions/load_or_fit_fnc.R")	
# 
# # Two functions to a) plot the differences between NS and L2 speakers from GAMMs,	
# # and b) plot the effects by L2 speakers' proficiency from GAMMs	
# source("functions/plot_gams_fnc.R")	


## Simpler convenience functions:	

# # print deviance explained as percentage	
# dev_expl <- function(fm) {	
#   devi <- summary(fm)$dev.expl	
#   paste0(round(100 * devi, 1), '% dev. explained')	
# }	

# create a neat table of the summary of fixed effects of a mixed model	
glmm_tb <- function(fm) {	
  m <- round(summary(fm)$coefficients, 3)	
  tb <- as.data.frame(m)	
  names(tb) <- c("Estimate", "SE", "z-value", "p-value")	
  kable(tb)	
}	


#' # Analyses and results	
#' 	
#' 	


#' ## Differences in adaptation over time (trial-by-trial analysis using GAMs)	
#' 	
#' 	
#' ### Native speakers -- trial-by-trial	
#' 	
#' Previous models (not shown here) showed that random *intercepts* by items made sense, but not random slopes.	
#' We add the former but not the latter.	
#' 	
#' Thus, the model is specified as:	
#' 	
#' `PathVerb ~ VbType_Cond + s(Trial, by = VbType_Cond) + s(Trial, Subject, bs = 'fs') + s(VideoName, bs = 're')`	
#' 	
#' Predictors are:	
#' 	
#' - `VbType_Cond`: Group-Condition interaction as a fixed effect	
#' - `s(Trial, by = VbType_Cond)`: A smooth function of `Trial` allowing the function to differ for each level of Group-Condition (thin plate regression splines, the default)	
#' - `s(Trial, Subject, bs = 'fs')`: Factor smooths for `Subject` to capture non-linear random effects of speakers	
#' - `s(VideoName, bs = 're')`: Random intercepts by items (i.e., `VideoName`)	
#' 	
#' 	
#' 	
# the expression that is passed to load_or_fit()	
gam_ns.expr <- "bam(Used ~ VbType_Cond + s(Trial, by = VbType_Cond) +	
s(Trial, Subject, bs = 'fs') + s(VideoName, bs = 're'),	
data = d_ns,	
family = 'binomial')"	
# load model or fit	
load_or_fit("gam_ns", gam_ns.expr)	
#' 	
#' 	
#' Summary of the model:	
#' 	
#' 	
summary(gam_ns)	
#' 	
#' 	
#' 	
#' Summary using gamtabs:	
#' 	
#' 	
#' 	
gam_tb(gam_ns)	
#' 	
#' 	
#' 	
#' Significance of the different terms in the model:	
#' 	
#' 	
anova(gam_ns)	
#' 	
#' 	
#' 	
#' Plot model estimates by condition, random smooth adjustments by speakers and QQ-plot of random by-item intercepts:	
#' 	
#' 	
## plot	
plot_smooth(gam_ns, view = 'Trial', plot_all = 'VbType_Cond', rm.ranef=TRUE)	
#' 	
#' 	
#' 	
#' 	
# show by-speaker random smooths and by-item random intercepts	
par(mfrow = c(1, 2))	
plot(gam_ns, select = 5)	
plot(gam_ns, select = 6)	
#' 	
#' 	
#' 	
#' 	
#' Plot differences between conditions (model estimates):	
#' 	
#' 	
plot_gam_main(gam_ns, "NS")  # choose this on Linux machine	
# on my old windows computer:	
# plot_gam_main(gam_ns, "NS", mark.diff = FALSE)  # use of mark.diff is a hack to	
# avoid an error that will stop the whole thing when I compile it on my work pc	
#' 	
#' 	
#' 	
#' 	
# save to disk for thesis chapter	
tiff(file = "figures/gam_natives.tiff", width = 5, height = 5.2, units = "in", pointsize = 10, res = 800)	
plot_gam_main(gam_ns, "NS")	
dev.off()	
#' 	
#' 	
#' 	
#' ### L2-speaker adaptation -- trial-by-trial	
#' 	
#' Previous models (not shown here) showed that random *intercepts* by items made sense, but not random slopes.	
#' We add the former but not the latter.	
#' 	
#' Thus, the model is specified as:	
#'   	
#'   `MannerVerb ~ VbType_Cond + s(Trial, by = VbType_Cond) + s(Trial, Subject, bs = 'fs') + s(VideoName, bs = 're')`	
#' 	
#' Predictors are:	
#'   	
#'   - `VbType_Cond`: Group-Condition interaction as a fixed effect	
#' - `s(Trial, by = VbType_Cond)`: A smooth function of `Trial` allowing the function to differ for each level of Group-Condition (thin plate regression splines, the default)	
#' - `s(Trial, Subject, bs = 'fs')`: Factor smooths for `Subject` to capture non-linear random effects of speakers	
#' - `s(VideoName, bs = 're')`: Random intercepts by items (i.e., `VideoName`)	
#' 	
#' 	
#' 	
# the expression that is passed to load_or_fit()	
gam_l2.expr <- "bam(Used ~ VbType_Cond + s(Trial, by = VbType_Cond) +	
s(Trial, Subject, bs = 'fs') + s(VideoName, bs = 're'),	
data = d_l2,	
family = 'binomial')"	
# load model or fit	
load_or_fit("gam_l2", gam_l2.expr)	
#' 	
#' 	
#' Summary of the model:	
#'   	
#' 	
summary(gam_l2)	
#' 	
#' 	
#' 	
#' Summary using gamtabs:	
#'   	
#' 	
gam_tb(gam_l2)	
#' 	
#' 	
#' 	
#' 	
#' Significance of the different terms in the model:	
#'   	
#' 	
anova(gam_l2)	
#' 	
#' 	
#' 	
#' Plot model estimates by condition, random smooth adjustments by speakers and QQ-plot of random by-item intercepts:	
#'   	
#' 	
## plot	
plot_smooth(gam_l2, view = 'Trial', plot_all = 'VbType_Cond', rm.ranef=TRUE)	
#' 	
#' 	
#' 	
#' 	
# show by-speaker random smooths and by-item random intercepts	
par(mfrow = c(1, 2))	
plot(gam_l2, select = 5)	
plot(gam_l2, select = 6)	
#' 	
#' 	
#' 	
#' 	
#' Plot differences between conditions (model estimates):	
#'   	
#' 	
plot_gam_main(gam_l2, "L2")	
#' 	
#' 	
#' 	
#' 	
# save to disk for thesis chapter	
tiff(file = "figures/gam_L2.tiff", width = 5, height = 5.2, units = "in", pointsize = 10, res = 800)	
plot_gam_main(gam_l2, "L2")	
dev.off()	
#' 	
#' 	
#' 	
#' ### L2-speaker adaptation -- beginning of experiment	
#' 	
#' We now model only the linear part of the curve for Path and Manner, so that we can run a mixed logit model and test for differences in slopes between path and manner conditions.	
#' 	
#' 	
# linear part corresponds to trials 1-9	
d_l2_beg <- d_l2 %>% filter(Trial %in% 1:9)	
# This time we want to analyze it as a 2 x 2 design: 	
# VerbType (Path vs Manner) x Condition (Exposed vs Baseline)	
# The two factors interact with themselves and with Trial (centred)	

# VerbType -- use contrast coding	
d_l2_beg$VerbType <- factor(d_l2_beg$VerbType)	
contrasts(d_l2_beg$VerbType) <- - contr.sum(2) / 2	
colnames(contrasts(d_l2_beg$VerbType)) <- "P_vs_M"	
contrasts(d_l2_beg$VerbType)	

# Condition (now becomes a binary variable: Path/Manner become "Primed")	
levels(d_l2_beg$Condition)[levels(d_l2_beg$Condition) %in% c("Path", "Manner")] <- "Primed"	
levels(d_l2_beg$Condition)	
table(d_l2_beg$Condition)  # roughly balanced	
# contrast coding	
contrasts(d_l2_beg$Condition) <- - contr.sum(2) / 2	
colnames(contrasts(d_l2_beg$Condition)) <- "Primed_vs_Baseline"	
contrasts(d_l2_beg$Condition)	

# Centre Trial	
d_l2_beg$cTrial <- d_l2_beg$Trial - mean(d_l2_beg$Trial)	

# Note that some items (i.e. VideoName) might now be missing	
length(unique(d_l2_beg$VideoName))  # but they aren't	
sort(table(d_l2_beg$VideoName))	
# broken down by verbtype...	
with(d_l2_beg, table(VerbType, VideoName))	
# ... and by condition	
with(d_l2_beg, table(Condition, VideoName))	


head(d_l2_beg)	
#' 	
#' 	
#' We can now fit the logit mixed model	
#' 	
#' 	
# The following snippet fitted different models to analyze the first trials;	
# in the end we choose the more conservative one that still converges 	
# -- here glmm_adapt_beg3b; this is the one that is fitted below and reported.	

# # minimal random effects	
# glmm_adapt_beg_min <- glmer(Used ~ Condition * VerbType * cTrial +	
#                           (1 | Subject) + (1 | VideoName),	
#                         data = d_l2_beg, family = "binomial",	
#                         control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))	
# summary(glmm_adapt_beg_min)	

# progressively adding r.e. ...	

# # 1 random slope	
# glmm_adapt_beg1a <- glmer(Used ~ Condition * VerbType * cTrial +	
#                           (1 + cTrial | Subject) + (1 | VideoName),	
#                         data = d_l2_beg, family = "binomial",	
#                         control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))	
# summary(glmm_adapt_beg1a)	
# 	
# glmm_adapt_beg1b <- glmer(Used ~ Condition * VerbType * cTrial +	
#                           (1 | Subject) + (1 + Condition | VideoName),	
#                         data = d_l2_beg, family = "binomial",	
#                         control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))	
# summary(glmm_adapt_beg1b)	
# 	
# # fails to converge:	
# # glmm_adapt_beg1c <- glmer(Used ~ Condition * VerbType * cTrial +	
# #                           (1 | Subject) + (1 + VerbType | VideoName),	
# #                         data = d_l2_beg, family = "binomial",	
# #                         control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))	
# # summary(glmm_adapt_beg1c)	
# 	
# # 2 random slopes	
# glmm_adapt_beg2a <- glmer(Used ~ Condition * VerbType * cTrial +	
#                           (1 + cTrial | Subject) + (1 + Condition | VideoName),	
#                         data = d_l2_beg, family = "binomial",	
#                         control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))	
# summary(glmm_adapt_beg2a)	
# 	
# # fails to converge:	
# # glmm_adapt_beg2b <- glmer(Used ~ Condition * VerbType * cTrial +	
# #                           (1 + cTrial | Subject) + (1 + VerbType | VideoName),	
# #                         data = d_l2_beg, family = "binomial",	
# #                         control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))	
# # summary(glmm_adapt_beg2b)	

# glmm_adapt_beg2c <- glmer(Used ~ Condition * VerbType * cTrial +	
#                           (1 | Subject) + (1 + Condition + VerbType | VideoName),	
#                         data = d_l2_beg, family = "binomial",	
#                         control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))	
# summary(glmm_adapt_beg2c)	

# 3 random slopes	

# fails to converge:	
# glmm_adapt_beg3a <- glmer(Used ~ Condition * VerbType * cTrial +	
#                           (1 + cTrial | Subject) + (1 + Condition + VerbType | VideoName),	
#                         data = d_l2_beg, family = "binomial",	
#                         control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))	
# summary(glmm_adapt_beg3a)	

# glmm_adapt_beg3b <- glmer(Used ~ Condition * VerbType * cTrial +	
#                           (1 | Subject) + (1 + Condition * VerbType | VideoName),	
#                         data = d_l2_beg, family = "binomial",	
#                         control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))	
# summary(glmm_adapt_beg3b)	


# maximal random effects	

# fails to converge	
# glmm_adapt_beg_max <- glmer(Used ~ Condition * VerbType * cTrial +	
#                           (1 + cTrial | Subject) + (1 + Condition * VerbType | VideoName),	
#                         data = d_l2_beg, family = "binomial",	
#                         control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))	
# summary(glmm_adapt_beg_max)	

# # compare all converging models	
# anova(glmm_adapt_beg_min, glmm_adapt_beg1a, glmm_adapt_beg1b, glmm_adapt_beg2a, 	
#       glmm_adapt_beg2c, glmm_adapt_beg3b)	

# # compare the one that seems to contribute most with the null	
# anova(glmm_adapt_beg_min, glmm_adapt_beg2c, glmm_adapt_beg3b)	

# summary(glmm_adapt_beg2c)	
# summary(glmm_adapt_beg3b)	

# In the end we choose the more conservative model: glmm_adapt_beg3b	
#' 	
#' 	
#' 	
#' 	
# The final model to analyze adaptation to path vs manner verbs during the	
# beginning (=linear in log-odds space) phase of the experiment	
# the expression that is passed to load_or_fit()	
glmm_adapt_beg.expr <- "glmer(Used ~ Condition * VerbType * cTrial +	
(1 | Subject) + (1 + Condition * VerbType | VideoName),	
data = d_l2_beg, family = 'binomial',	
control = glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5)))"	
# load model or fit	
load_or_fit("glmm_adapt_beg", glmm_adapt_beg.expr)	
#' 	
#' 	
#' 	
#' 	
#' Plot 	
#' 	
#' 	
plot(allEffects(glmm_adapt_beg))	
#' 	
#' 	
#' 	
#' 	
#' 	
#' ## Changes in adaptation with increasing L2 proficiency	
#' 	
#' 	
#' ### Adaptation to path verbs vs proficiency	
#' 	
#' TO DO:	
#' 	
#' Model comparison (like for manner below) to test whether the addition of the interaction term 	
#' `te(Trial, ClozeScore, by = Condition)`	
#' significantly improves the model.	
#' 	
#' 	
fm_p_L2prof.expr <- "bam(Used ~	
Condition +	
te(Trial, ClozeScore, by = Condition) +	
s(Trial, Subject, bs = 'fs') +	
s(VideoName, bs = 're'),	
data = dp_l2,	
family = 'binomial')"	
# load model or fit	
load_or_fit("fm_p_L2prof", fm_p_L2prof.expr)	
summary(fm_p_L2prof)	
#' 	
#' 	
#' Summary using gamtabs:	
#'   	
#' 	
gam_tb(fm_p_L2prof)	
#' 	
#' 	
#' 	
#' 	
# show by-speaker random smooths and by-item random intercepts	
par(mfrow = c(1, 2))	
plot(fm_p_L2prof, select = 3)	
plot(fm_p_L2prof, select = 4)	
#' 	
#' 	
#' 	
#' 	
#' 	
plot_L2_profic(fm_p_L2prof, primed_cond = 'Path')	
#' 	
#' 	
#' 	
#' 	
# save to disk for thesis chapter	
tiff(file = "figures/gam_L2_prof-path.tiff", width = 5.5, height = 4, units = "in", pointsize = 9.5, res = 800)	
plot_L2_profic(fm_p_L2prof, primed_cond = 'Path')	
dev.off()	
#' 	
#' 	
#' 	
#' We can also plot the differences between conditions:	
#' 	
#' 	
plot_L2_profic_diff(fm_p_L2prof, "Path")	
#' 	
#' 	
#' 	
#' 	
# save to disk for thesis chapter	
tiff(file = "figures/gam_L2_prof-path_diff.tiff", width = 5.5, height = 4, units = "in", pointsize = 9.5, res = 800)	
plot_L2_profic_diff(fm_p_L2prof, primed_cond = 'Path')	
dev.off()	
#' 	
#' 	
#' 	
#' 	
#' 	
#' 	
# difference plot in 3D	
plot_diff2(fm_p_L2prof, view = c('Trial', 'ClozeScore'),	
           comp = list(Condition = c('Path', 'Baseline')),	
           rm.ranef = TRUE)	
#' 	
#' 	
#' 	
#' 	
#' ### Adaptation to manner verbs vs proficiency	
#' 	
#' 	
#' 	
fm_m_L2prof.expr <- "bam(Used ~ Condition +	
te(Trial, ClozeScore, by = Condition) +	
s(Trial, Subject, bs = 'fs') +	
s(VideoName, bs = 're'),	
data = dm_l2,	
family = 'binomial')"	
# load model or fit	
load_or_fit("fm_m_L2prof", fm_m_L2prof.expr)	
summary(fm_m_L2prof)	
#' 	
#' 	
#' 	
#' Although the interaction terms are not significant in the model summary, we can run model comparison to see if adding the 3-way interaction with proficiency significantly improves the model:	
#' 	
#' 	
fm_m_L2prof_no3way.expr <- "bam(Used ~ Condition + 	
s(Trial, by = Condition) + 	
s(ClozeScore, by = Condition) + 	
s(Trial, Subject, bs = 'fs') +	
s(VideoName, bs = 're'),	
data = dm_l2,	
family = 'binomial')"	
# load model or fit	
load_or_fit("fm_m_L2prof_no3way", fm_m_L2prof_no3way.expr)	
summary(fm_m_L2prof_no3way)	
#' 	
#' 	
#' 	
# compare the models with itsadug::compareML()	
# (see email correspondence with Florian J. Subject: "Our skype on L2 adaptation pape" (15 Feb 2017))	
itsadug::compareML(fm_m_L2prof, fm_m_L2prof_no3way)	
#' 	
#' 	
#' 	
#' Summary using gamtabs:	
#'   	
#' 	
gam_tb(fm_m_L2prof)	
#' 	
#' 	
#' 	
#' 	
# show by-speaker random smooths and by-item random intercepts	
par(mfrow = c(1, 2))	
plot(fm_m_L2prof, select = 3)	
plot(fm_m_L2prof, select = 4)	
#' 	
#' 	
#' 	
#' 	
plot_L2_profic(fm_m_L2prof, primed_cond = 'Manner')	
#' 	
#' 	
#' 	
# save to disk for thesis chapter	
tiff(file = "figures/gam_L2_prof-manner.tiff", width = 5.5, height = 4, units = "in", pointsize = 9.5, res = 800)	
plot_L2_profic(fm_m_L2prof, primed_cond = 'Manner')	
dev.off()	
#' 	
#' 	
#' 	
plot_L2_profic_diff(fm_m_L2prof, primed_cond = 'Manner')	
#' 	
#' 	
#' 	
# save to disk for thesis chapter	
tiff(file = "figures/gam_L2_prof-manner_diff.tiff", width = 5.5, height = 4, units = "in", pointsize = 9.5, res = 800)	
plot_L2_profic_diff(fm_m_L2prof, primed_cond = 'Manner')	
dev.off()	
#' 	
#' 	
#' 	
# difference plot in 3D	
plot_diff2(fm_m_L2prof, view = c('Trial', 'ClozeScore'),	
           comp = list(Condition = c('Manner', 'Baseline')), rm.ranef = TRUE)	
#' 	
#' 	
