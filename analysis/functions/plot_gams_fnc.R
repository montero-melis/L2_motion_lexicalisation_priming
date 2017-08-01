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
  # plot Path-Verbs Baseline
  plot_smooth(fm, view = 'Trial', cond = list(VbType_Cond = 'P_V.Baseline'),
              col = 'blue', lty = 2, rug = FALSE, ylim = ylim1, rm.ranef=TRUE,
              main = paste0(mygroup, ': Path verbs'), ylab = 'Log-odds\nof path-verb',
              hide.label = TRUE)
  # plot Path-Verbs path-primed
  plot_smooth(fm, view = 'Trial', cond = list(VbType_Cond = 'P_V.Path'),
              col = 'red', rug = FALSE, rm.ranef = TRUE, add = TRUE,
              hide.label = TRUE)
  # add legend
  legend(x = 1, y = ylim1[2] + 0.75, legend = c('Path-exposed', 'Baseline'),
         col = c('red', 'blue'), lty = 1:2, cex = mycex, box.lty = 0)
  
  # Manner-verbs Baseline
  plot_smooth(fm, view = 'Trial', cond = list(VbType_Cond = 'M_V.Baseline'),
              col = 'blue', lty = 2, rug = FALSE, ylim = ylim1, rm.ranef = TRUE,
              main = paste0(mygroup, ': Manner verbs'), ylab = 'Log-odds\nof manner-verb',
              hide.label = TRUE)
  # Manner-verbs manner-primed
  plot_smooth(fm, view = 'Trial', cond = list(VbType_Cond = 'M_V.Manner'),
              col = 'red', rug = FALSE, rm.ranef = TRUE, add = TRUE,
              hide.label = TRUE)
  # add legend
  legend(x = 1, y = ylim1[2] + 0.75, legend = c('Manner-exposed', 'Baseline'),
         col = c('red', 'blue'), lty = 1:2, cex = mycex, box.lty = 0)
  
  # Now plot the estimated differences with itsadug::plot_diff()
  plot_diff(fm, view = 'Trial', comp = list(VbType_Cond = c('P_V.Path', 'P_V.Baseline')),
            ylim = ylim2, rm.ranef=TRUE, 
            main = 'Path-exposed - Baseline',
            ylab = 'Diff. in log-odds\nof path-verb', hide.label = TRUE,
            ...)  # a hack I need to set the mark.diff argument as F, bc of some bug?
  plot_diff(fm, view = 'Trial', comp = list(VbType_Cond = c('M_V.Manner', 'M_V.Baseline')),
            ylim = ylim2, rm.ranef=TRUE, 
            main = 'Manner-exposed - Baseline',
            ylab = 'Diff. in log-odds\nof manner-verb', hide.label = TRUE)
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
  legend(x = "center", ncol = 2, legend = c(paste0(primed_cond, '-exposed'), 'Baseline'),
         col = c('red', 'blue'), lty = 1:2, box.lty = 0, cex = 1.5)
  # Now make plot for the model estimates at the different cloze scores in cloze_scores
  # par(mai=rep(0.2, 4))
  par(mai = c(.7, .8, .5, 0))
  for(cloze in cloze_scores) {
    plot_smooth(fm, view = 'Trial', cond = list(Condition = 'Baseline', ClozeScore = cloze),
                col = 'blue', lty = 2, rug = FALSE, ylim = ylim1, rm.ranef = TRUE,
                hide.label = TRUE, ylab = paste0('Log-odds\nof ', primed_cond, " verb"),
                main = paste('Cloze score =', cloze))
    plot_smooth(fm, view = 'Trial', cond = list(Condition = primed_cond, ClozeScore = cloze),
                col = 'red', rug = FALSE, rm.ranef = TRUE, hide.label = TRUE, add = TRUE)
  }
}


# function to plot the effects by L2 speakers' proficiency from GAMs
# this time showing DIFFERENCES [FUN UNDER CONSTRUCTION 170801]
plot_L2_profic_diff <- function(fm, primed_cond = NULL, ylim1 = c(-6, 8),
                                cloze_range = c(10, 35)) {
  cloze_scores <- seq(cloze_range[1], cloze_range[2], length.out = 6)
  # we want to have one common legend for all plots
  layout(rbind(1, matrix(2:7, ncol=3, byrow=TRUE)), heights = c(1, 6, 6))
  # add common legend
  par(mai=c(0,0,0,0))
  plot.new()
  legend(x = "center", ncol = 2, legend = c(paste0(primed_cond, '-exposed'), 'Baseline'),
         col = c('red', 'blue'), lty = 1:2, box.lty = 0, cex = 1.5)
  # Now make plot for the model estimates at the different cloze scores in cloze_scores
  # par(mai=rep(0.2, 4))
  par(mai = c(.7, .8, .5, 0))
  for(cloze in cloze_scores) {
    plot_diff(fm, view = 'Trial', comp = list(Condition = c(primed_cond, 'Baseline')),
              cond = list(ClozeScore = cloze))#,
    #             col = 'blue', lty = 2, rug = FALSE, ylim = ylim1, rm.ranef = TRUE,
    #             hide.label = TRUE, ylab = paste0('Log-odds\nof ', primed_cond, " verb"),
    #             main = paste('Cloze score =', cloze))
    # plot_smooth(fm, view = 'Trial', cond = list(Condition = primed_cond, ClozeScore = cloze),
    #             col = 'red', rug = FALSE, rm.ranef = TRUE, hide.label = TRUE, add = TRUE)
  }
}

plot_L2_profic_diff(fm_m_L2prof, "Manner")

plot_diff(fm_m_L2prof, view = 'Trial', comp = list(Condition = c('Manner', 'Baseline')))

plot_L2_profic(fm_m_L2prof_noarrastra, primed_cond = 'Manner')





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

