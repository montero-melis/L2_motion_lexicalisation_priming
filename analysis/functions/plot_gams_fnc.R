## Functions to plot results from GAMs

require(mgcv)
require(itsadug)


# function to plot the differences between Path/Manner and Baseline
# There are two models to be plotted, one for NS, one for L2 speakers
plot_gam_main <- function(fm, mygroup = NULL, 
                          ylim1 = c(-4, 6), ylim2 = c(-.5, 6), ...) {
  layout(matrix(1:4, ncol = 2, byrow=TRUE), heights = c(1.5, 1))
  par(mai = c(.7, .8, .5, 0.2))
  mycex <- 0.9
  # plot path verbs Baseline
  plot_smooth(fm, view = 'Trial', cond = list(VbType_Cond = 'P_V.Baseline'),
              col = 'blue', lty = 2, rug = FALSE, ylim = ylim1, rm.ranef=TRUE,
              main = paste0(mygroup, ': Path verbs'), ylab = 'Log-odds\nof path verb',
              hide.label = TRUE)
  # plot Path Verbs in path-primed condition
  plot_smooth(fm, view = 'Trial', cond = list(VbType_Cond = 'P_V.Path'),
              col = 'red', rug = FALSE, rm.ranef = TRUE, add = TRUE,
              hide.label = TRUE)
  # add legend
  legend(x = 1, y = ylim1[2] + 0.75, legend = c('Path-primed', 'Baseline'),
         col = c('red', 'blue'), lty = 1:2, cex = mycex, box.lty = 0)
  
  # Manner verbs Baseline
  plot_smooth(fm, view = 'Trial', cond = list(VbType_Cond = 'M_V.Baseline'),
              col = 'blue', lty = 2, rug = FALSE, ylim = ylim1, rm.ranef = TRUE,
              main = paste0(mygroup, ': Manner verbs'), ylab = 'Log-odds\nof manner verb',
              hide.label = TRUE)
  # Manner verbs in manner-primed condition
  plot_smooth(fm, view = 'Trial', cond = list(VbType_Cond = 'M_V.Manner'),
              col = 'red', rug = FALSE, rm.ranef = TRUE, add = TRUE,
              hide.label = TRUE)
  # add legend
  legend(x = 1, y = ylim1[2] + 0.75, legend = c('Manner-primed', 'Baseline'),
         col = c('red', 'blue'), lty = 1:2, cex = mycex, box.lty = 0)
  
  # Now plot the estimated differences with itsadug::plot_diff()
  plot_diff(fm, view = 'Trial', comp = list(VbType_Cond = c('P_V.Path', 'P_V.Baseline')),
            ylim = ylim2, rm.ranef=TRUE, 
            main = 'Path-primed \u2212 Baseline',
            ylab = 'Diff. in log-odds\nof path verb', hide.label = TRUE,
            ...)  # a hack I need to set the mark.diff argument as F, bc of some bug?
  plot_diff(fm, view = 'Trial', comp = list(VbType_Cond = c('M_V.Manner', 'M_V.Baseline')),
            ylim = ylim2, rm.ranef=TRUE, 
            main = 'Manner-primed \u2212 Baseline',
            ylab = 'Diff. in log-odds\nof manner verb', hide.label = TRUE)
}


# function to plot the effects by L2 speakers' proficiency from GAMs
plot_L2_profic <- function(fm, primed_cond = NULL, ylim1 = c(-6, 8),
                           cloze_range = c(10, 35)) {
  cloze_scores <- seq(cloze_range[1], cloze_range[2], length.out = 6)
  # we want to have one common legend for all plots
  layout(rbind(1, matrix(2:7, ncol=3, byrow=TRUE)), heights = c(1, 6, 6))
  # add common legend
  par(mai=c(0,0,0,0))
  plot.new()
  legend(x = "center", ncol = 2, legend = c(paste0(primed_cond, '-primed'), 'Baseline'),
         col = c('red', 'blue'), lty = 1:2, box.lty = 0, cex = 1.5)
  # Now make plot for the model estimates at the different cloze scores in cloze_scores
  # par(mai=rep(0.2, 4))
  par(mai = c(.4, .6, .3, 0))
  for(cloze in cloze_scores) {
    plot_smooth(fm, view = 'Trial', cond = list(Condition = 'Baseline', ClozeScore = cloze),
                col = 'blue', lty = 2, rug = FALSE, ylim = ylim1, rm.ranef = TRUE,
                hide.label = TRUE, ylab = paste0('Log-odds\nof ', tolower(primed_cond), ' verb'),
                main = paste('L2 proficiency =', cloze))
    plot_smooth(fm, view = 'Trial', cond = list(Condition = primed_cond, ClozeScore = cloze),
                col = 'red', rug = FALSE, rm.ranef = TRUE, hide.label = TRUE, add = TRUE)
  }
}


# adapt function for EuroSLA slides
plot_L2_profic_eurosla <- function(fm, primed_cond = NULL, ylim1 = c(-6, 8),
                           cloze_range = c(10, 35), nb_plots = 4) {
  cloze_scores <- seq(cloze_range[1], cloze_range[2], length.out = nb_plots)
  # we want to have one common legend for all plots
  layout(rbind(1, matrix(2:(nb_plots + 1), ncol = nb_plots, byrow=TRUE)), heights = c(1, 6))
  # add common legend
  par(mai=c(0,0,0,0))
  plot.new()
  legend(x = "center", ncol = 2, legend = c(paste0(primed_cond, '-primed'), 'Baseline'),
         col = c('red', 'blue'), lty = 1:2, box.lty = 0, cex = 1.5)
  # Now make plot for the model estimates at the different cloze scores in cloze_scores
  # par(mai=rep(0.2, 4))
  par(mai = c(.4, .6, .3, 0), mgp = c(1.6, .8, 0))
  for(cloze in cloze_scores) {
    plot_smooth(fm, view = 'Trial', cond = list(Condition = 'Baseline', ClozeScore = cloze),
                col = 'blue', lty = 2, rug = FALSE, ylim = ylim1, rm.ranef = TRUE,
                hide.label = TRUE, ylab = paste0('Log-odds\nof ', tolower(primed_cond), ' verb'),
                main = paste('L2 proficiency =', round(cloze)))
    plot_smooth(fm, view = 'Trial', cond = list(Condition = primed_cond, ClozeScore = cloze),
                col = 'red', rug = FALSE, rm.ranef = TRUE, hide.label = TRUE, add = TRUE)
  }
}


# Adaptation of the previous function when a SINGLE model is fit to path and
# manner verbs (rather than fitting two separate models).
# Note, however, that we will still call the function twice, once for each
# comparison, even if the comparison is extracted from the estimates of the
# same model.
plot_L2_profic_singlemodel <- function(fm, primed_cond = NULL, ylim1 = c(-6, 8),
                                   cloze_range = c(10, 35), nb_plots = 4) {
  cloze_scores <- seq(cloze_range[1], cloze_range[2], length.out = nb_plots)
  # Choose the relevant verb type based on argument "primed_cond"
  if (! primed_cond %in% c("Path", "Manner")) {
    stop("argument primed_con has to be either Path or Manner")
  } else if (primed_cond == "Path") {
    myverbtype <- "P_V"
  } else {
    myverbtype <- "M_V"
  }
  baseline_cond <- paste(myverbtype, "Baseline", sep = ".")
  my_primed_cond <- paste(myverbtype, primed_cond, sep = ".")
  # we want to have one common legend for all plots
  layout(rbind(1, matrix(2:(nb_plots + 1), ncol = nb_plots, byrow=TRUE)), heights = c(1, 6))
  # add common legend
  par(mai=c(0,0,0,0))
  plot.new()
  legend(x = "center", ncol = 2, legend = c(paste0(primed_cond, '-primed'), 'Baseline'),
         col = c('red', 'blue'), lty = 1:2, box.lty = 0, cex = 1.5)
  # Now make plot for the model estimates at the different cloze scores in cloze_scores
  # par(mai=rep(0.2, 4))
  par(mai = c(.4, .6, .3, 0), mgp = c(1.6, .8, 0))
  for(cloze in cloze_scores) {
    plot_smooth(fm, view = 'Trial', cond = list(VbType_Cond = baseline_cond, ClozeScore = cloze),
                col = 'blue', lty = 2, rug = FALSE, ylim = ylim1, rm.ranef = TRUE,
                hide.label = TRUE, ylab = paste0('Log-odds\nof ', tolower(primed_cond), ' verb'),
                main = paste('L2 proficiency =', round(cloze)))
    plot_smooth(fm, view = 'Trial', cond = list(VbType_Cond = my_primed_cond, ClozeScore = cloze),
                col = 'red', rug = FALSE, rm.ranef = TRUE, hide.label = TRUE, add = TRUE)
  }
}



# function to plot the effects by L2 speakers' proficiency from GAMs
# this time showing DIFFERENCES
plot_L2_profic_diff <- function(fm, primed_cond = NULL, ylim1 = c(-2.5, 8),
                                cloze_range = c(10, 35)) {
  cloze_scores <- seq(cloze_range[1], cloze_range[2], length.out = 6)
  mytitle <- paste0('Adaptation effect as a function of L2 proficiency:\n', primed_cond, '-primed \u2212 baseline condition ')
  myylab <- paste0('Diff. in log-odds\nof ', tolower(primed_cond), ' verb')
  
  # Now make plot for the model estimates at the different cloze scores in cloze_scores
  layout(matrix(1:6, ncol=3, byrow=TRUE))
  par(mai = c(.5, .8, .7, .2), mgp = c(2.3, .8, 0))
  for(cloze in cloze_scores) {
    plot_diff(fm, view = 'Trial', comp = list(Condition = c(primed_cond, 'Baseline')),
              cond = list(ClozeScore = cloze),
              ylim = ylim1, hide.label = TRUE, ylab = myylab, main = "")
    title(main = paste('L2 proficiency =', cloze), font.main = 1, line = 1)
    
  }
  # https://www.r-bloggers.com/two-tips-adding-title-for-graph-with-multiple-plots-add-significance-asterix-onto-a-boxplot/
  # mtext("Difference plots", outer = TRUE, cex = 1.5)
  # mtext("My 'Title' in a strange place", side = 3, line = -1, outer = TRUE)
  title(mytitle, line = -2, outer = TRUE, cex = 1)
}


# adapt function for EuroSLA slides
plot_L2_profic_diff_eurosla <- function(fm, primed_cond = NULL, ylim1 = c(-2.5, 8),
                                cloze_range = c(10, 35), nb_plots = 4) {
  cloze_scores <- seq(cloze_range[1], cloze_range[2], length.out = nb_plots)
  mytitle <- paste0(primed_cond, '-primed \u2212 baseline condition ')
  myylab <- paste0('Diff. in log-odds\nof ', tolower(primed_cond), ' verb')
  
  # Now make plot for the model estimates at the different cloze scores in cloze_scores
  layout(matrix(1:nb_plots, ncol = nb_plots, byrow=TRUE))
  par(mai = c(.4, .6, .7, 0), mgp = c(1.6, .8, 0))
  for(cloze in cloze_scores) {
    plot_diff(fm, view = 'Trial', comp = list(Condition = c(primed_cond, 'Baseline')),
              cond = list(ClozeScore = cloze),
              ylim = ylim1, hide.label = TRUE, ylab = myylab, main = "")
    title(main = paste('L2 proficiency =', round(cloze)), font.main = 1, line = 1)
    
  }
  # https://www.r-bloggers.com/two-tips-adding-title-for-graph-with-multiple-plots-add-significance-asterix-onto-a-boxplot/
  # mtext("Difference plots", outer = TRUE, cex = 1.5)
  # mtext("My 'Title' in a strange place", side = 3, line = -1, outer = TRUE)
  title(mytitle, line = -1, outer = TRUE, cex = 1)
}


# Function to plot the effects by L2 speakers' proficiency from GAMs this
# time showing DIFFERENCES. It's an adaptation of plot_L2_profic_diff()
# function to be used when a SINGLE model is fit to path and manner verbs
# (rather than fitting two separate models).
# Note, however, that we will still call the function twice, once for each
# comparison, even if the comparison is extracted from the estimates of the
# same model.
plot_L2_profic_diff_singlemodel <- function(
  fm,
  primed_cond = NULL, 
  ylim1 = c(-2.5, 8.5),
  cloze_range = c(10, 35),
  nb_plots = 4
  ) {
  cloze_scores <- seq(cloze_range[1], cloze_range[2], length.out = nb_plots)
  # Choose the relevant verb type based on argument "primed_cond"
  if (! primed_cond %in% c("Path", "Manner")) {
    stop("argument primed_con has to be either 'Path' or 'Manner'")
  } else if (primed_cond == "Path") {
    myverbtype <- "P_V"
  } else {
    myverbtype <- "M_V"
  }
  my_primed_cond <- paste(myverbtype, primed_cond, sep = ".")
  baseline_cond <- paste(myverbtype, "Baseline", sep = ".")
  mytitle <- paste0(primed_cond, ' priming \u2212 baseline')
  myylab <- paste0('Diff. in log-odds\nof ', tolower(primed_cond), ' verb')
  
  # Now make plot for the model estimates at the different cloze scores in cloze_scores
  layout(matrix(1:nb_plots, ncol = nb_plots, byrow=TRUE))
  par(mai = c(.4, .6, .7, 0), mgp = c(1.6, .8, 0))
  print(my_primed_cond)
  print(baseline_cond)
  for(cloze in cloze_scores) {
    plot_diff(fm, view = 'Trial', comp = list(VbType_Cond = c(my_primed_cond, baseline_cond)),
              cond = list(ClozeScore = cloze),
              ylim = ylim1, hide.label = TRUE, ylab = myylab, main = "")
    title(main = paste('L2 proficiency =', round(cloze)), font.main = 1, line = 1)
    
  }
  # https://www.r-bloggers.com/two-tips-adding-title-for-graph-with-multiple-plots-add-significance-asterix-onto-a-boxplot/
  # mtext("Difference plots", outer = TRUE, cex = 1.5)
  # mtext("My 'Title' in a strange place", side = 3, line = -1, outer = TRUE)
  title(mytitle, line = -2, outer = TRUE, cex = 1)
}





## OLD:

# # function to plot the differences between NS and L2 speakers from GAMs
# # NB: not used anymore if I'm modelling each group (NS, L2) separately
# plot_NS_L2 <- function(fm, primed_cond = NULL, ylim1 = c(-4, 6), ylim2 = c(-.5, 6), ...) {
#   layout(matrix(1:4, ncol = 2, byrow=TRUE), heights = c(1.5, 1))
#   par(mai = c(.7, .8, .5, 0.2))
#   mycex <- 0.9
#   NS_primed <- paste0('NS.', primed_cond)
#   L2_primed <- paste0('L2.', primed_cond)
#   # plot NS Baseline
#   plot_smooth(fm, view = 'Trial', cond = list(GroupCondit = 'NS.Baseline'),
#               col = 'blue', lty = 2, rug = FALSE, ylim = ylim1, rm.ranef=TRUE,
#               main = 'Native speakers', ylab = paste0('Log-odds\nof ', primed_cond, " verb"),
#               hide.label = TRUE)
#   # plot NS primed
#   plot_smooth(fm, view = 'Trial', cond = list(GroupCondit = NS_primed),
#               col = 'red', rug = FALSE, rm.ranef = TRUE, add = TRUE,
#               hide.label = TRUE)
#   # add legend
#   legend(x = 1, y = ylim1[2] + 0.75, legend = c(paste0(primed_cond, '-exposed'), 'Baseline'),
#          col = c('red', 'blue'), lty = 1:2, cex = mycex, box.lty = 0)
  
#   # L2 speakers Baseline
#   plot_smooth(fm, view = 'Trial', cond = list(GroupCondit = 'L2.Baseline'),
#               col = 'blue', lty = 2, rug = FALSE, ylim = ylim1, rm.ranef = TRUE,
#               main = 'L2 speakers', ylab = paste0('Log-odds\nof ', primed_cond, " verb"),
#               hide.label = TRUE)
#   # L2 speakers primed
#   plot_smooth(fm, view = 'Trial', cond = list(GroupCondit = L2_primed),
#               col = 'red', rug = FALSE, rm.ranef = TRUE, add = TRUE,
#               hide.label = TRUE)
#   # add legend
#   legend(x = 1, y = ylim1[2] + 0.75, legend = c(paste0(primed_cond, '-exposed'), 'Baseline'),
#          col = c('red', 'blue'), lty = 1:2, cex = mycex, box.lty = 0)
  
#   # Now plot the estimated differences with itsadug::plot_diff()
#   plot_diff(fm, view = 'Trial', comp = list(GroupCondit = c(NS_primed, 'NS.Baseline')),
#             ylim = ylim2, rm.ranef=TRUE, 
#             main = paste0('NS: ', primed_cond, '-exposed',  ' - Baseline'),
#             ylab = paste('Diff. in log-odds\nof ', primed_cond, 'verb'), hide.label = TRUE,
#             ...)  # a hack I need to set the mark.diff argument as F, bc of some bug?
#   plot_diff(fm, view = 'Trial', comp = list(GroupCondit = c(L2_primed, 'L2.Baseline')),
#             ylim = ylim2, rm.ranef=TRUE, 
#             main = paste0('L2: ', primed_cond, '-exposed',  ' - Baseline'),
#             ylab = paste('Diff. in log-odds\nof ', primed_cond, 'verb'), hide.label = TRUE)
# }

