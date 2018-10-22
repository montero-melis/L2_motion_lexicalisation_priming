## Functions to be used in script "analysis/Q3_follow-up_plot-model-coefficients.R"

# These are taylor-made functions for the current project, but they are factored
# into a separate script for clarity in the script that wraps things up.

library(dplyr)


# factors2modelmatrix -----------------------------------------------------

# Given participant factor levels, output the right model matrix to multiply
# their model coefficients by:
factors2modelmatrix <- function(
  ppt_factors = NULL
) {
  # Subject-level predictors (NB: Expects certain columns to exist!)
  sbj_factors <- ppt_factors %>% select(Subject, Condition, Group) %>% unique
  # Adds cTrial as predictor for which to estimate mean effect
  sbj_factors$cTrial <- 1
  # Converts to model matrix (NB: Make sure factors are contrast coded!)
  mm <- model.matrix(~ 1 + Condition * Group * cTrial, sbj_factors)
  row.names(mm) <- sbj_factors$Subject
  mm
}

