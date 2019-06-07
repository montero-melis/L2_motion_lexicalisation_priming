## Function to plot mixed model-estimates in logit space from baseline
## conditions, including speaker estimates.

# Relies on the following packages:

# require(ggplot2)
# require(arm)
# require(dplyr)

## plot NS vs L2 speakers

# theme for ggplot
mytheme <- theme_bw() + 
  theme(text = element_text(size = 12),
        # panel.border = element_blank(),
        panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        plot.title = element_text(hjust = 0.5)  # center title plot
        )


## plot_basel() function is similar to plot_glmm(), except that it plots the
## results when GLMM is run as a single analysis (with indicator variable
## VerbType), rather than as two separate analyses for path and manner verbs.
plot_basel <- function(fm, d, DV = NULL, nb_sims = 10000) {
  # fm: the fitted glmer model
  # d: the dataframe used to fit the model
  # nb_sims: number of simulations when using arm::sim()
  
  # get confidence intervals from models
  # simulate the coefficients using arm::sim and extract fixed effects
  set.seed(987654)  # make confidence intervals reproducible
  s <- arm::sim(fm, n.sims = nb_sims)@fixef
  # Add simulation values for different conditions
  estim <- data.frame(
    P_NS = s[, 1] - s[, 2] - s[, 3] + s[, 4],
    P_L2 = s[, 1] - s[, 2] + s[, 3] - s[, 4],
    M_NS = s[, 1] + s[, 2] - s[, 3] - s[, 4],
    M_L2 = s[, 1] + s[, 2] + s[, 3] + s[, 4]
  )
  # compute 95% confidence intervals and put into df
  confint <- data.frame(t(sapply(estim, quantile, probs = c(.025, .5, .975))))
  names(confint) <- c("lower", "median", "upper")
  confint$VerbType <- factor(rep(c("Path verb", "Manner verb"), each = 2),
                             levels = c("Path verb", "Manner verb"))
  confint$Group <- factor(c("Native\nspeakers", "L2 learners"),
                          levels = c("Native\nspeakers", "L2 learners"))

  # Now extract subject estimates from model
  # (this is hacky, but couldn't figure out a better way)
  model_pred <- dplyr::select(d, Subject:Group, VerbType)  # the data rows
  model_pred <- cbind(model_pred, predict(fm))  # combine with model predictions per row
  names(model_pred)[length(model_pred)] <- "predicted"  # rename last column
  # average over subjects
  model_pred <- model_pred  %>% group_by(Subject, Group, VerbType) %>%
    summarise(Pred = mean(predicted))
  # Make sure factor levels in model_pred are the same as in confint
  model_pred$Group <- factor(model_pred$Group, levels = c("NS", "L2"),
                             labels = c("Native\nspeakers", "L2 learners"))
  model_pred$VerbType <- factor(model_pred$VerbType, levels = c("P_V", "M_V"),
                                labels = c("Path verb", "Manner verb"))

  # plot using ggplot
  p <- ggplot(confint, aes(x = Group, y = median)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .25, size = 1.5) +
    facet_grid(. ~ VerbType) +
    ylab("Log-odds of verb") +
    xlab("") +
    ylim(-5.2, 5.2)
  set.seed(123987)  # make horizontal jitter reproducible
  p <- p + 
    geom_jitter(data = model_pred, aes(x = Group, y = Pred),
                       height = 0, width = .25, size = 2, alpha = .3) +
    facet_grid(. ~ VerbType)
  # final touch
  p + mytheme + geom_hline(yintercept = 0)
}

