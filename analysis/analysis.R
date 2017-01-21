## Analyse data from participants in the primed conditions.

library(dplyr)
library(lazyeval)  # lazy evaluation used in bysubj() function [summarise_]
library(ggplot2)
library(lme4)

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
ppts <- read.csv("data/participants.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
head(ppts)

# add speakers' clozescore to d:
d <- left_join(d, ppts %>% select(Subject, ClozeScore))
head(d)
tail(d)



#  ------------------------------------------------------------------------
#  Functions
#  ------------------------------------------------------------------------


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

# f_plot2 <- function(df, varname = NULL) {
#   p <- ggplot(df, aes(x = Condition, y = M, colour = Group)) +
#     geom_point(position = position_jitterdodge()) +
#     stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = .85, width = .5,
#                  position = "dodge") +
#     ggtitle(varname)
#   p
# }


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
#  Plots
#  ------------------------------------------------------------------------

# group patterns
f_plot(bysubj(d, "P_anyw"), "Path anywhere")
f_plot(bysubj(d, "P_V"), "Path in V")
f_plot(bysubj(d, "P_adj"), "Path in adjunct")

f_plot(bysubj(d, "M_anyw"), "Manner anywhere")
f_plot(bysubj(d, "M_V"), "Manner in V")
f_plot(bysubj(d, "M_adj"), "Manner in adjunct")

# proficiency
f_plot_prof(bysubj(d, "P_anyw"), "Path anywhere")
f_plot_prof(bysubj(d, "P_V"), "Path in V")
f_plot_prof(bysubj(d, "P_adj"), "Path in adjunct")

f_plot_prof(bysubj(d, "M_anyw"), "Manner anywhere")
f_plot_prof(bysubj(d, "M_V"), "Manner in V")
f_plot_prof(bysubj(d, "M_adj"), "Manner in adjunct")



#  ------------------------------------------------------------------------
#  Logit mixed models and corresponding plots
#  ------------------------------------------------------------------------


# Create data subsets for the different comparisons -----------------------

# Path vs control
d_pc <- d %>% filter(Condition != "Manner")
# contrast coding for group
contrasts(d_pc$Group) <- - contr.sum(2)
colnames(contrasts(d_pc$Group)) <- "L2_vs_NS"
contrasts(d_pc$Group)
# contrast coding for conditions
d_pc$Condition <- factor(d_pc$Condition)
contrasts(d_pc$Condition) <- contr.sum(2)
colnames(contrasts(d_pc$Condition)) <- "P_vs_C"
contrasts(d_pc$Condition)

# Manner vs control
d_mc <- d %>% filter(Condition != "Path")
# contrast coding for group
contrasts(d_mc$Group) <- - contr.sum(2)
colnames(contrasts(d_mc$Group)) <- "L2_vs_NS"
contrasts(d_mc$Group)
# contrast coding for conditions
d_mc$Condition <- factor(d_mc$Condition)
contrasts(d_mc$Condition) <- - contr.sum(2)
colnames(contrasts(d_mc$Condition)) <- "M_vs_C"
contrasts(d_mc$Condition)


# Path in the verb --------------------------------------------------------

# plot all conditions for reference
f_plot(bysubj(d, "P_V"), "Path in V")

# Path-primed vs Control
fm_pve_pc <- glmer(P_V ~ Group * Condition + (1 | Subject) + (1 | VideoName),
                   data = d_pc, family = "binomial")
summary(fm_pve_pc)

# Manner-primed vs Control
fm_pve_mc <- glmer(P_V ~ Group * Condition + (1 | Subject) + (1 | VideoName),
                   data = d_mc, family = "binomial")
summary(fm_pve_mc)


# Path as adjunct ---------------------------------------------------------

# plot all conditions for reference
f_plot(bysubj(d, "P_adj"), "Path as adjunct")

# Path-primed vs Control
fm_pad_pc <- glmer(P_adj ~ Group * Condition + (1 | Subject) + (1 | VideoName),
                   data = d_pc, family = "binomial")
summary(fm_pad_pc)

# Manner-primed vs Control
fm_pad_mc <- glmer(P_adj ~ Group * Condition + (1 | Subject) + (1 | VideoName),
                   data = d_mc, family = "binomial")
summary(fm_pad_mc)


# Manner in the verb ------------------------------------------------------

# plot all conditions for reference
f_plot(bysubj(d, "M_V"), "Manner in V")

# Manner-primed vs Control
fm_mve_mc <- glmer(M_V ~ Group * Condition + (1 | Subject) + (1 | VideoName),
                                data = d_mc, family = "binomial")
summary(fm_mve_mc)

# Path-primed vs Control
fm_mve_pc <- glmer(M_V ~ Group * Condition + (1 | Subject) + (1 | VideoName),
                   data = d_pc, family = "binomial")
summary(fm_mve_pc)


# Manner as adjunct -------------------------------------------------------

# plot all conditions for reference
f_plot(bysubj(d, "M_adj"), "Manner as adjunct")


# Manner-primed vs Control
fm_mad_mc <- glmer(M_adj ~ Group * Condition + (1 | Subject) + (1 | VideoName),
                   data = d_mc, family = "binomial")
summary(fm_mad_mc)

# Path-primed vs Control
fm_mad_pc <- glmer(M_adj ~ Group * Condition + (1 | Subject) + (1 | VideoName),
                   data = d_pc, family = "binomial")
summary(fm_mad_pc)



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


# Path expression ---------------------------------------------------------

# Path in Verb
f_plot_prof(bysubj(d, "P_V"), "Path in V") + geom_text(aes(label = Subject))
fm_pve_prof <- glmer(P_V ~ Condition * cCloze + (1 | Subject) + (1 | VideoName),
                     data = d_l2, family = "binomial")
summary(fm_pve_prof)
# likelihood ratio test
fm_pve_prof_null <- glmer(P_V ~ Condition + (1 | Subject) + (1 | VideoName),
                     data = d_l2, family = "binomial")
summary(fm_pve_prof_null)
anova(fm_pve_prof_null, fm_pve_prof)

# Path in adjunct
f_plot_prof(bysubj(d, "P_adj"), "Path in adjunct")
fm_pad_prof <- glmer(P_adj ~ Condition * cCloze + (1 | Subject) + (1 | VideoName),
                     data = d_l2, family = "binomial")
summary(fm_pad_prof)
# likelihood ratio test
fm_pad_prof_null <- glmer(P_adj ~ Condition + (1 | Subject) + (1 | VideoName),
                     data = d_l2, family = "binomial")
anova(fm_pad_prof, fm_pad_prof_null)

# Path anywhere
f_plot_prof(bysubj(d, "P_anyw"), "Path anywhere")
fm_pan_prof <- glmer(P_anyw ~ Condition * cCloze + (1 | Subject) + (1 | VideoName),
                     data = d_l2, family = "binomial")
summary(fm_pan_prof)


# Manner expression ---------------------------------------------------------

# Manner in Verb
f_plot_prof(bysubj(d, "M_V"), "Manner in V")
fm_mve_prof <- glmer(M_V ~ Condition * cCloze + (1 | Subject) + (1 | VideoName),
                     data = d_l2, family = "binomial")
summary(fm_mve_prof)

# Manner in adjunct
f_plot_prof(bysubj(d, "M_adj"), "Manner in adjunct")
fm_mad_prof <- glmer(M_adj ~ Condition * cCloze + (1 | Subject) + (1 | VideoName),
                     data = d_l2, family = "binomial")
summary(fm_mad_prof)

# Manner anywhere
f_plot_prof(bysubj(d, "M_anyw"), "Manner anywhere")
fm_man_prof <- glmer(M_anyw ~ Condition * cCloze + (1 | Subject) + (1 | VideoName),
                     data = d_l2, family = "binomial")
summary(fm_man_prof)



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

