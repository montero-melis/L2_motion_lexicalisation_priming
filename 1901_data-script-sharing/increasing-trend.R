## Look into increasing trend for path verb production in baseline condition

# Reviewer 2 from BLC pointed out that
# "The L1 native dataset has an unexpected pattern such that the rates of path
# verbs increase over the course of the experiment. [...] This may be contribu-
# ting to the small priming effects and what looks like an inverse preference
# effect. Especially given that this doesn’t happen in the L2 group to the same
# extent, I think this is worth mentioning as a limitation of the dataset."

# We here look into the reviewer's observation that this pattern holds for L1
# speakers more than for L2 speakers.

library(dplyr)
library(mgcv)     # v. 1.8-17
library(itsadug)  # v. 2.2

source("1901_data-script-sharing/load_or_fit_fnc.R")

# load data -- select only baseline participants
d <- read.csv('1901_data-script-sharing/data.csv', fileEncoding = 'UTF-8', 
              stringsAsFactors = TRUE) %>%
  filter(Condition == "Baseline") %>%
  rename(PathVerb = P_V)  # make the DV more transparent

# Make sure Subject is a factor
d$Subject <- factor(d$Subject)
# in Group, let the native speakers (NS) be the reference group
d$Group <- factor(d$Group, levels = c('NS', 'L2'))
head(d, 3)

# Model path verb use as a function of Trial and Group
gam_basel_increase.expr <- 
"bam(PathVerb ~ Group + s(Trial, by = Group) + s(Trial, Subject, bs = 'fs') +
  s(Trial, VideoName, bs = 'fs'),
data = d, family = 'binomial')"
# load or fit (took about 3.3 minutes on my HP laptop)
load_or_fit("gam_basel_increase", gam_basel_increase.expr,
            alternPath = "1901_data-script-sharing/fitted_models/")


par(mfrow=c(1,2))
# plot the estimates + confidence intervals per group
plot_smooth(gam_basel_increase, view = "Trial", plot_all = "Group",
            rm.ranef = TRUE, hide.label = TRUE, rug = FALSE)
# plot their difference
plot_diff(gam_basel_increase, view = "Trial", rm.ranef = TRUE,
          comp = list(Group = c("NS", "L2")), hide.label = TRUE)
par(mfrow=c(1,1))
