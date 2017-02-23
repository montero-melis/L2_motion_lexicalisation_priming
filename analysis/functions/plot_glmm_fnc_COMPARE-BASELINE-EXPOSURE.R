## Function to plot mixed model-estimates in logit space from baseline
## conditions and exposed conditions, including speaker estimates

# NB1: This was the old function I used when I was comparing baseline
# and exposed participants using mixed models. I don't need it anymore
# if I'm analysing baseline on their own first. But I keep this function
# in case I should need it again, as it took me a lot of time to get it
# to work.
# NB2: This function is rather hacky.

require(ggplot2)
require(arm)
require(dplyr)

# theme for ggplot
mytheme <- theme_bw() + 
  theme(text = element_text(size = 9),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(size = .01, color="#CCCCCC"),
        panel.background = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"))

plot_glmm <- function(fm, d, exposed = NULL, ylims = NULL, nb_sims = 1000) {
  # fm: the fitted glmer model
  # d: the dataframe used to fit the model
  # exposed: either "Path" or "Manner" denoting exposed condition
  # ylims: y-limits for ggplot
  
  # provide Path or Manner to exposed argument
  cond_name <- exposed
  myylab <- paste0("Log-likelihood of ", exposed, "-verb")
  
  # get confidence intervals from models
  # simulate the coefficients using arm::sim and extract fixed effects
  s <- arm::sim(fm, n.sims = nb_sims)@fixef
  # Add simulation values for different conditions
  estim <- data.frame(
    co_NS = s[, 1] - .5 * s[, 2],
    co_L2 = s[, 1] + .5 * s[, 2],
    ex_NS = s[, 1] - .5 * s[, 2] + s[, 3] - .5 * s[, 4],
    ex_L2 = s[, 1] + .5 * s[, 2] + s[, 3] + .5 * s[, 4])
  # compute 95% confidence intervals and put into df
  confint <- data.frame(t(sapply(estim, quantile, probs = c(.025, .5, .975))))
  names(confint) <- c("lower", "median", "upper")
  confint$Condition <- rep(c("Control", cond_name), each = 2)
  confint$Group <- factor(c("NS", "L2"), levels = c("NS", "L2"))
  
  # Get subject estimates from model
  # (this is a hack, but couldn't figure out a better way)
  model_pred <- dplyr::select(d, Subject:Condition)  # the data rows
  model_pred <- cbind(model_pred, predict(fm))  # combine with model estimates
  names(model_pred)[length(model_pred)] <- "predicted"
  # now average over subjects
  model_pred <- model_pred  %>% group_by(Subject, Group, Condition) %>%
    summarise(Pred = mean(predicted))

  # plot using ggplot
  p <- ggplot(confint, aes(x = Condition, y = median, colour = Group)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), position = "dodge", width = .5,
                  size = 1.5) +
    geom_line(aes(group = Group), position = position_dodge(width = .5), size = 1) +
    ylab(myylab) +
    ylim(ylims)
  p <- p + geom_point(data = model_pred, aes(x = Condition, y = Pred, colour = Group),
                      position = position_jitterdodge(jitter.height = 0, dodge.width = 0.5),
                      size = 2, alpha = .5)
  p + mytheme
}


# E.g. call like this
# plot_glmm(glmm_pve, d = dp_lme4, exposed = "Path", ylims = c(-5.5, 5))
# plot_glmm(glmm_mve, d = dm_lme4, exposed = "Manner", ylims = c(-5.5, 5))

