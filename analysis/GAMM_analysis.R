# Analysis of time trends (trial-by-trial) using GAMMS 
# (cf. Wood 2006; Baayen et al. 2017; Winter & Wieling 2016)

library(mgcv)  # GAMs and GAMMs (Wood 2006)
library(itsadug)
library(dplyr)
# library(lazyeval)  # lazy evaluation used in bysubj() function [summarise_]
library(ggplot2)
# library(lme4)
# library(boot)  # for inv.logit()
# library(knitr)  # for kable()


#  ------------------------------------------------------------------------
#  Load data and process
#  ------------------------------------------------------------------------

# The data is created in the script "processing/compute_dependent_measures.R"
# There is the normal and the liberally coded version, see script for difference

# load
d <- read.csv("data/data_DVs.csv", fileEncoding = "UTF-8", stringsAsFactors = TRUE)
head(d)
# adjust Condition and Group levels for plotting
d$Condition <- factor(d$Condition, levels = c("Path", "Control", "Manner"))
d$Group <- factor(d$Group, levels = c("NS", "L2"))

# simplify
d <- d %>% select(Subject:VideoName, P_V, M_V)
head(d)

#  ------------------------------------------------------------------------
#  Follow tutorial by Winter & Wieling (2016)
#  ------------------------------------------------------------------------

plot(d$VideoTrial, jitter(d$P_V))  # not informative

# this is more informative
ggplot(d, aes(x = VideoTrial, y = P_V, colour = Group)) +
  geom_jitter(height = .1, width = .1, alpha = .3) +
  facet_grid(. ~ Condition) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

ggplot(d, aes(x = VideoTrial, y = M_V, colour = Group)) +
  geom_jitter(height = .1, width = .1, alpha = .3) +
  facet_grid(. ~ Condition) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))


## Fitting a logistic GAM:

mb <- bam(P_V ~ s(VideoTrial) + # fixed effects
            s(VideoTrial, Subject, bs = 're'),		# random effects
          data = d, method = 'fREML',
          family = 'binomial' # necessary for fitting a logistic GAM
)              

# add interaction by condition
mb1 <- bam(P_V ~ Condition + s(VideoTrial, by = Condition) + # fixed effects
            s(VideoTrial, Subject, bs = 're'),		# random effects
          data = d, method = 'fREML',
          family = 'binomial' # necessary for fitting a logistic GAM
)              

summary(mb1)
summary(mb1)$p.table
summary(mb1)$s.table


par(mfrow = c(1, 1))
plot_smooth(mb1, 'VideoTrial',
            main = 'Effect of t(ime): logits (log-odds)')

plot_smooth(mb1, 'VideoTrial', plot_all = "Condition",
            main = 'Effect of t(ime): logits (log-odds)')

plot_diff(mb1, 'VideoTrial', comp = list(Condition = c("Path", "Control")))
plot_diff(mb1, 'VideoTrial', comp = list(Condition = c("Manner", "Control")))

acf_resid(mb1)

# create Condition-Group interaction
d$GroupCond <- with(d, interaction(Group, Condition))
# add interaction by condition-group
mb2 <- bam(P_V ~ GroupCond + s(VideoTrial, by = GroupCond) + # fixed effects
             s(VideoTrial, Subject, bs = 're'),		# random effects
           data = d, method = 'fREML',
           family = 'binomial' # necessary for fitting a logistic GAM
)              


par(mfrow = c(1, 1))
plot_smooth(mb2, 'VideoTrial',
            main = 'Effect of t(ime): logits (log-odds)')

plot_smooth(mb2, 'VideoTrial', plot_all = "GroupCond",
            main = 'Effect of t(ime): logits (log-odds)')

plot_diff(mb2, view = 'VideoTrial', comp = list(GroupCond = c("L2.Control", "NS.Control")), rm.ranef = T)

plot_diff(mb2, view = 'VideoTrial', comp = list(GroupCond = c("NS.Path", "NS.Control")), rm.ranef = T)
plot_diff(mb2, view = 'VideoTrial', comp = list(GroupCond = c("L2.Path", "L2.Control")), rm.ranef = T)


plot_smooth(mb2, view = 'VideoTrial', cond = list(GroupCond = c("L2.Control"), GroupCond = c("NS.Control")), rm.ranef = T)



acf_resid(mb2)





par(mfrow = c(1, 3))

plot_smooth(mb1, view = 'VideoTrial', cond = list(Condition = "Path"), rm.ranef = T,
            main = 'Effect of t(ime): logits (log-odds)')

plot_smooth(mb1, view = 'VideoTrial', cond = list(Condition = "Control"), rm.ranef = T,
            main = 'Effect of t(ime): logits (log-odds)')

plot_smooth(mb1, view = 'VideoTrial', cond = list(Condition = "Manner"), rm.ranef = T,
            main = 'Effect of t(ime): logits (log-odds)')


par(mfrow = c(1, 1))


mb <- bam(iconicity_binary ~ s(t, k = 5) + # fixed effects
            s(t, dyad, bs = 'fs', k = 5, m = 1),		# random effects
          data = dyads, method = 'fREML',
          family = 'binomial' # necessary for fitting a logistic GAM
)              


## Plot the effect of t(ime); first set up plotting window for two plots:

par(mfrow = c(1, 2))

## First plot; plot dependent variable in logits:

plot_smooth(mb, 'VideoTrial', rm.ranef = T,
            main = 'Effect of t(ime): logits (log-odds)')

## Second plot; Plot dependent variable in probabilities:

plot_smooth(mb, 'VideoTrial', rm.ranef = T,
            main = 'Effect of t(ime): probabilities', transform = plogis)


