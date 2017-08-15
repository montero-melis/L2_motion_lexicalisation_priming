library(dplyr)
packageVersion('dplyr')
library(tidyr)
packageVersion('tidyr')  # useful for gather() to convert from wide to long
library(mgcv)  # GAMs and GAMMs (Wood 2006)
packageVersion('mgcv')  # GAMs and GAMMs (Wood 2006)
library(itsadug)
packageVersion('itsadug')
library(lme4)
packageVersion('lme4')
library(ggplot2)
packageVersion('ggplot2')
library(boot)  # for inv.logit()
packageVersion('boot')  # for inv.logit()
library(knitr)  # for kable()
packageVersion('knitr')  # for kable()
library(lazyeval)  # lazy evaluation used in bysubj() function [summarise_]
packageVersion('lazyeval')
library(effects)
library(xtable)

# for the path specifications to work
setwd("analysis")
getwd()

## ------------------------------------------------------------------------
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
# head(d)
# str(d)

# Rename "Control" condition to "Baseline"
levels(d$Condition)[levels(d$Condition) == "Control"] <- "Baseline"

# participant data
ppts <- read.csv("../data/participants.csv", fileEncoding = "UTF-8",
                 stringsAsFactors = TRUE)
# head(ppts)
# participants by group
with(ppts, table(Group))
with(ppts, table(Group, Gender))
with(ppts, table(Group, Condition))
# age
ppts %>%
  group_by(Group) %>%
  summarise(Mage = mean(Age, na.rm = T), SDage = sd(Age, na.rm = T))
# Cloze scores
mean(ppts$ClozeScore, na.rm=T)
sd(ppts$ClozeScore, na.rm=T)

# No audio data recorded for Subject 14 (L2) due to experimental error; exclude
ppts <- ppts[ppts$Subject != 14, ]
# transform ClozeScore to z-score (as.vector prevents it from becoming a matrix)
ppts$zClozeScore <- as.vector(scale(ppts$ClozeScore))
# center ClozeScore but not scaling (as.vector prevents it from becoming a matrix)
ppts$cClozeScore <- as.vector(scale(ppts$ClozeScore, scale = FALSE))

# add speakers' clozescore to d:
d <- left_join(d, ppts %>% select(Subject, ClozeScore, zClozeScore, cClozeScore))

# Subject needs to be a factor
d$Subject <- factor(d$Subject)

# in Group, let the native speakers (NS) be the reference group
d$Group <- factor(d$Group, levels = c('NS', 'L2'))


# Add info about prime verbs to the data file (by joining two dataframes)
# Load table with priming verbs and then join
primes <- read.csv("../data/priming-verbs.csv")
head(primes)
d <- left_join(d, primes)


head(d)
str(d)

## ------------------------------------------------------------------------
# Data for translation task
transl <- read.csv("../data/L2_translation-task_scored.csv", fileEncoding = "UTF-8")
getwd()

## ------------------------------------------------------------------------
# Outliers? ---------------------------------------------------------------

# L2 speakers Cloze scores
cloze <- ppts %>%
  filter(Group == "L2") %>%
  select(Subject, Condition, ClozeScore, zClozeScore)
# Subjects ordered by clozescore
head(cloze[order(cloze$zClozeScore), ])
tail(cloze[order(cloze$zClozeScore), ])

## ------------------------------------------------------------------------
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

## Subset data:
# Data for analysis of Native speakers (note we are removing observations that
# correspond to path verbs produced in the manner-primed condition or to manner
# verbs produced in the path-primed condition)
d_ns <- d_long %>% filter(Group == "NS" & ! VbType_Cond %in% c("P_V.Manner", "M_V.Path"))
# drop unused factors for subject (I think this is important for gam fitting)
d_ns$Subject <- factor(d_ns$Subject)
# Data for analysis of L2 speakers
d_l2 <- d_long %>% filter(Group == "L2" & ! VbType_Cond %in% c("P_V.Manner", "M_V.Path"))
d_l2$Subject <- factor(d_l2$Subject)

# Order levels correctly in each data frame
d_ns$VbType_Cond <- factor(d_ns$VbType_Cond, levels = c("P_V.Baseline", "P_V.Path",
                                                      "M_V.Baseline", "M_V.Manner"))
d_l2$VbType_Cond <- factor(d_l2$VbType_Cond, levels = c("P_V.Baseline", "P_V.Path",
                                                      "M_V.Baseline", "M_V.Manner"))
# note that log-odds of path verbs in baseline condition becomes the reference
# level in both groups (NS and L2 speakers)
contrasts(d_ns$VbType_Cond)
contrasts(d_l2$VbType_Cond)

## ------------------------------------------------------------------------
# subset L2 data only from Path/Baseline and Manner/Baseline conditions respectively
dp_l2 <- d_l2 %>% filter(VerbType == "P_V")
dm_l2 <- d_l2 %>% filter(VerbType == "M_V")

## ---- echo = T-----------------------------------------------------------
# Subset data from baseline condition only
d_basel <- d %>% filter(Condition == "Baseline")
# And from baseline for L2 speakers only
d_basel_l2 <- d %>% filter(Condition == "Baseline" & Group == "L2")

# Use contrast coding to compare groups
contrasts(d_basel$Group) <- - contr.sum(2) / 2
colnames(contrasts(d_basel$Group)) <- "L2_vs_NS"
contrasts(d_basel$Group)

## ------------------------------------------------------------------------
## Specify some global parameters

# adjust figure heght/width when not going with default (espec. for 2x2 plots)
myfighe_NS_L2 <- 5
myfighe_L2_prof <- 5
myfigwi <- 8

## ------------------------------------------------------------------------
## Source somewhat more complex functions

# source functions to compute collinearity diagnostics
source("functions/kappa_mer_fnc.R")
source("functions/vif_mer_fnc.R")

# Function that plots mixed model-estimates in logit space from baseline
# conditions, including speaker estimates
source("functions/plot_glmm_fnc.R")

# source multiplot function
source("functions/multiplot_fnc.R")

# Function used to load models if they have already been saved,
# rather than fitting them anew each time the script is called
source("functions/load_or_fit_fnc.R")

# Two functions to a) plot the differences between NS and L2 speakers from GAMMs,
# and b) plot the effects by L2 speakers' proficiency from GAMMs
source("functions/plot_gams_fnc.R")

## ------------------------------------------------------------------------
## Simpler convenience functions:

# print deviance explained as percentage
dev_expl <- function(fm) {
  devi <- summary(fm)$dev.expl
  paste0(round(100 * devi, 1), '% dev. explained')
}


## ------------------------------------------------------------------------
# By condition and verb-type 
transl %>% group_by(Condition, Type) %>%
  summarise(ProportionCorrect= round(sum(Score) / n(), 2)) %>%
  kable

# By condition and verb
transl %>% group_by(Condition, Type, Target_verb) %>%
  summarise(ProportionCorrect= round(sum(Score) / n(), 2)) %>%
  kable

## ------------------------------------------------------------------------
#  ------------------------------------------------------------------------
#  Descriptives (by-subject plots)
#  ------------------------------------------------------------------------

mywidth <- 7
myheight <- 3

# ggplot theme
mytheme <- theme_bw() + 
  theme(#text = element_text(size = 10),
    # panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(color="black"),
    axis.line.y = element_line(color="black"))

by_subj <- transl %>% group_by(Subject, Condition, Type, ClozeScore) %>%
  summarise(Perc = round(sum(Score) / n(), 2))

ggplot(by_subj, aes(x = ClozeScore, y = Perc, colour = Type)) +
  geom_jitter(height = 0, alpha = .5) +
  facet_grid(. ~ Condition) +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Proportion of correct translations") +
  mytheme

## ----fit_glmm_translation_data-------------------------------------------
#  ------------------------------------------------------------------------
#  Significance test (GLMM)
#  ------------------------------------------------------------------------

# It really makes most sense to focus on the control/baseline condition only
d_fm_baseline <- transl[transl$Condition == "Control", ]
# contrast coding for Type (Path vs Manner verb)
contrasts(d_fm_baseline$Type) <- contr.sum(2)
colnames(contrasts(d_fm_baseline$Type)) <- "MannerV_vs_PathV"
contrasts(d_fm_baseline$Type)
# center cloze scores
d_fm_baseline$cClozeScore <- as.vector(scale(d_fm_baseline$ClozeScore, scale = FALSE))

# Model treats the individual verbs as random effects
fm_transl <- glmer(Score ~ Type * cClozeScore + 
                     (1 | Subject) + (1 | Target_verb),
                   data = d_fm_baseline, family = "binomial")
summary(fm_transl)

fm_transl_eff <- allEffects(fm_transl)
plot(fm_transl_eff, type = "link", 
     xlab = "Proficiency\n(centred cloze score)",
     ylab = "Log-odds of correct\ntranslation of verb meaning",
     main = "Translation task")


## ------------------------------------------------------------------------
# Descriptive data for all analysed conditions
# table (will need to be appropriately formatted in report)
tb_descriptive <- d_long %>%
  group_by(VerbType, Condition, Group) %>%
  summarise(N = sum(Used),
            TotalN = n(),
            Percentage = round(100 * sum(Used) / n(), 1))
kable(tb_descriptive)

## ---- echo = TRUE--------------------------------------------------------
glmm_pve <- glmer(P_V ~ Group + (1 | Subject) + (1 + Group | VideoName),
                data = d_basel, family = "binomial", 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
glmm_tb(glmm_pve)

## ------------------------------------------------------------------------
summary(glmm_pve)

## ------------------------------------------------------------------------
kappa.mer(glmm_pve)
vif.mer(glmm_pve)

## ---- echo = TRUE--------------------------------------------------------
glmm_mve <- glmer(M_V ~ Group + (1 | Subject) + (1 + Group | VideoName),
                data = d_basel, family = "binomial", 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
glmm_tb(glmm_mve)

## ------------------------------------------------------------------------
summary(glmm_mve)

## ------------------------------------------------------------------------
kappa.mer(glmm_mve)
vif.mer(glmm_mve)



## Plot baseline condition

# redefine some parameters which are defined in "plot_glmm_fnc.R"
mytheme <- theme_bw() + 
  theme(text = element_text(size = 14),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        plot.title = element_text(hjust = 0.5)  # center title plot
  )

## ---- fig.width = myfigwi, fig.height = 3--------------------------------
# save relevant plots
plot_basel_path <- plot_glmm(glmm_pve, d = d_basel, DV = "Path")
plot_basel_manner <- plot_glmm(glmm_mve, d = d_basel, DV = "Manner")
# combine them
multiplot(plot_basel_path, plot_basel_manner, cols = 2)

## ------------------------------------------------------------------------
# save to disk for eurosla
tiff(file = "figures/eurosla/baseline.tiff", width = myfigwi, height = 4, units = "in", res = 800)
multiplot(plot_basel_path, plot_basel_manner, cols = 2)
dev.off()


## ------------------------------------------------------------------------
glmm_pve_prof <- glmer(P_V ~ cClozeScore + (1 | Subject) + (1 | VideoName),
                       data = d_basel_l2, family = "binomial", 
                       control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

## ------------------------------------------------------------------------
summary(glmm_pve_prof)

## ------------------------------------------------------------------------
glmm_mve_prof <- glmer(M_V ~ cClozeScore + (1 | Subject) + (1 | VideoName),
                       data = d_basel_l2, family = "binomial", 
                       control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

## ------------------------------------------------------------------------
summary(glmm_mve_prof)

## ---- fig.width = myfigwi, fig.height = 3.5------------------------------
# I use this approach to plot model estimates for cClozeScore
# http://stats.stackexchange.com/questions/135255/obtaining-adjusted-predicted-proportions-with-lme4-using-the-glmer-function
# and using the following advice to combine them into a multiplot:
# http://stackoverflow.com/questions/15227184/combine-two-plots-created-with-effects-package-in-r
eff_p <- effect("cClozeScore", glmm_pve_prof)
eff_m <- effect("cClozeScore", glmm_mve_prof)

plot(eff_p, type = "link", ylim = myylims, 
     xlab = "Proficiency\n(centred cloze score)",
     ylab = "Log-odds\nof path verb",
     main = "Path verbs vs. proficiency",
     ticks.y = list(at = c(-2, 0)),
     rug = F, key.args=list(space="right"),
     row = 1,col = 1, nrow = 1,ncol = 2, 
     more = TRUE)
plot(eff_m, type = "link", ylim = myylims,
     xlab = "Proficiency\n(centred cloze score)",
     ylab = "Log-odds\nof manner verb",
     main = "Manner verbs vs. proficiency",
     rug=F, key.args=list(space="right"),
     row = 1,col = 2, nrow = 1, ncol = 2)

# save to disk for eurosla
tiff(file = "figures/eurosla/baseline_L2-proficiency.tiff", width = myfigwi, height = 4, units = "in", res = 800)
plot(eff_p, type = "link", ylim = myylims, 
     xlab = "Proficiency\n(centred cloze score)",
     ylab = "Log-likelihood\nof path verb",
     main = "Path verbs vs. proficiency",
     ticks.y = list(at = c(-2, 0)),
     rug = F, key.args=list(space="right"),
     row = 1,col = 1, nrow = 1,ncol = 2, 
     more = TRUE)
plot(eff_m, type = "link", ylim = myylims,
     xlab = "Proficiency\n(centred cloze score)",
     ylab = "Log-likelihood\nof manner verb",
     main = "Manner verbs vs. proficiency",
     rug=F, key.args=list(space="right"),
     row = 1,col = 2, nrow = 1, ncol = 2)
dev.off()



## ----gam_ns--------------------------------------------------------------
# the expression that is passed to load_or_fit()
gam_ns.expr <- "bam(Used ~ VbType_Cond + s(Trial, by = VbType_Cond) +
                    s(Trial, Subject, bs = 'fs') + s(VideoName, bs = 're'),
                  data = d_ns,
                  family = 'binomial')"
# load model or fit
load_or_fit("gam_ns", gam_ns.expr)

## ------------------------------------------------------------------------
summary(gam_ns)


## ---- fig.height = myfighe_NS_L2, fig.width = myfigwi, echo = FALSE, results = 'hide'----
plot_gam_main(gam_ns, "NS")  # choose this on Linux machine

## ------------------------------------------------------------------------
# save to disk for EuroSLA
tiff(file = "figures/eurosla/gam_natives.tiff", width = myfigwi, height = myfighe_NS_L2,
     units = "in", pointsize = 12, res = 800)
plot_gam_main(gam_ns, "NS")
dev.off()

## ----gam_l2--------------------------------------------------------------
# the expression that is passed to load_or_fit()
gam_l2.expr <- "bam(Used ~ VbType_Cond + s(Trial, by = VbType_Cond) +
                    s(Trial, Subject, bs = 'fs') + s(VideoName, bs = 're'),
                  data = d_l2,
                  family = 'binomial')"
# load model or fit
load_or_fit("gam_l2", gam_l2.expr)

## ------------------------------------------------------------------------
summary(gam_l2)



## ---- fig.height = myfighe_NS_L2, fig.width = myfigwi, echo = FALSE, results = 'hide'----
plot_gam_main(gam_l2, "L2")

## ------------------------------------------------------------------------
# save to disk for EuroSLA
tiff(file = "figures/eurosla/gam_L2.tiff", width = myfigwi, height = myfighe_NS_L2,
     units = "in", pointsize = 12, res = 800)
plot_gam_main(gam_l2, "L2")
dev.off()

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
# The final model to analyze adaptation to path vs manner verbs during the
# beginning (=linear in log-odds space) phase of the experiment
# the expression that is passed to load_or_fit()
glmm_adapt_beg.expr <- "glmer(Used ~ Condition * VerbType * cTrial +
                          (1 | Subject) + (1 + Condition * VerbType | VideoName),
                        data = d_l2_beg, family = 'binomial',
                        control = glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5)))"
# load model or fit
load_or_fit("glmm_adapt_beg", glmm_adapt_beg.expr)

## ----fig.height = myfighe_NS_L2, fig.width = myfigwi---------------------
plot(allEffects(glmm_adapt_beg))





# Effects of L2 proficiency -----------------------------------------------

myheight_prof_plots <- 2.8
mywidth_prof_plots <- 9
mypointsize_prof_plots <- 14


## ------------------------------------------------------------------------
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


## ---- fig.height = myfighe_L2_prof, fig.width = myfigwi, results='hide'----
plot_L2_profic(fm_p_L2prof, primed_cond = 'Path')
plot_L2_profic_eurosla(fm_p_L2prof, primed_cond = 'Path', nb_plots = 3)  
plot_L2_profic_eurosla(fm_p_L2prof, primed_cond = 'Path', nb_plots = 4)  # default nb_plots

## ------------------------------------------------------------------------
# save to disk for EuroSLA
tiff(file = "figures/eurosla/gam_L2_prof-path.tiff", width = mywidth_prof_plots, 
     height = myheight_prof_plots, units = "in", pointsize = mypointsize_prof_plots, res = 800)
plot_L2_profic_eurosla(fm_p_L2prof, primed_cond = 'Path', nb_plots = 4)
dev.off()

## Plot DIFFERENCE between conditions (= adaptation effect)

## ---- fig.height = myfighe_L2_prof, fig.width = myfigwi, results='hide'----
plot_L2_profic_diff(fm_p_L2prof, "Path")
plot_L2_profic_diff_eurosla(fm_p_L2prof, "Path", nb_plots = 3)
plot_L2_profic_diff_eurosla(fm_p_L2prof, "Path", nb_plots = 4)

## ------------------------------------------------------------------------
# save to disk for EuroSLA
tiff(file = "figures/eurosla/gam_L2_prof-path_diff.tiff", width = mywidth_prof_plots, 
     height = myheight_prof_plots, units = "in", pointsize = mypointsize_prof_plots, res = 800)
plot_L2_profic_diff_eurosla(fm_p_L2prof, "Path", nb_plots = 4)
dev.off()


## Now for MANNER verbs

## ------------------------------------------------------------------------
fm_m_L2prof.expr <- "bam(Used ~ Condition +
te(Trial, ClozeScore, by = Condition) +
s(Trial, Subject, bs = 'fs') +
s(VideoName, bs = 're'),
data = dm_l2,
family = 'binomial')"
# load model or fit
load_or_fit("fm_m_L2prof", fm_m_L2prof.expr)
summary(fm_m_L2prof)


## ---- fig.height = myfighe_L2_prof, fig.width = myfigwi, results='hide'----
plot_L2_profic(fm_m_L2prof, primed_cond = 'Manner')
plot_L2_profic_eurosla(fm_m_L2prof, primed_cond = 'Manner', nb_plots = 3)  
plot_L2_profic_eurosla(fm_m_L2prof, primed_cond = 'Manner', nb_plots = 4)  # default nb_plots


## ------------------------------------------------------------------------
# save to disk for EuroSLA
tiff(file = "figures/eurosla/gam_L2_prof-manner.tiff", width = mywidth_prof_plots, 
     height = myheight_prof_plots, units = "in", pointsize = mypointsize_prof_plots, res = 800)
plot_L2_profic_eurosla(fm_m_L2prof, primed_cond = 'Manner', nb_plots = 4)  # default nb_plots
dev.off()

# Now plot DIFFERENCES

## ---- fig.height = myfighe_L2_prof, fig.width = myfigwi, results='hide'----
plot_L2_profic_diff(fm_m_L2prof, primed_cond = 'Manner')
plot_L2_profic_diff_eurosla(fm_m_L2prof, "Manner", nb_plots = 3)
plot_L2_profic_diff_eurosla(fm_m_L2prof, "Manner", nb_plots = 4)


## ------------------------------------------------------------------------
# save to disk for EuroSLA
tiff(file = "figures/eurosla/gam_L2_prof-manner_diff.tiff", width = mywidth_prof_plots, 
     height = myheight_prof_plots, units = "in", pointsize = mypointsize_prof_plots, res = 800)
plot_L2_profic_diff_eurosla(fm_m_L2prof, "Manner", nb_plots = 4)
dev.off()




