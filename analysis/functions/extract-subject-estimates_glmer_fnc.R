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



# Get intercept and trial slope per subject -------------------------------


# For each subject, we want to obtain two estimates:
# 1) Subject-specific Intercept: adding fixef and ranefs specific to that subject
# 2) Subject-specific Trial effect: again adding fixef and ranefs

# Function to do the basic computation per subject:
# Takes two vectors, one represents the subject-specific predictors, the other
# the subject-specific estimates. It outputs a 2-valued vector: Subject-specific
# Intercept and Subject-specific Trial effect
compute_ppt_estimate <- function (
  sbj_pred = NULL,
  sbj_coef = NULL
  ) {
  # first divide sbj_coef into the intercept part and the cTrial slope:
  b <- diag(sbj_coef) %*% matrix(c(1,1,1,0,1,0,0,0,   # coefficients for intercept
                                   0,0,0,1,0,1,1,1),  # coefficients for cTrial slope
                                 ncol = 2)
  # Now predictor vector matrix-multiplies b to obtain the 2 estimates
  sbj_pred %*% b
}


# wrapper function to do the same thing for all subjects
# NB: The by-subject coefficients estimated from the model (which add random
# effect adjustments / BLUPs to fixed effects) are obtained with the coef() 
# function (see Ben Bolker's answer:
# https://stackoverflow.com/questions/26198958/extracting-coefficients-and-their-standard-error-from-lme)
get_ppt_estimate <- function (
  mm_sbj = NULL,  # output of factors2modelmatrix() 
  fm = NULL,  # we'll use output of coef(fm)$Subject
  df = NULL  # dataframe used to fit model to get subject info in last step
  ) {
  # Through matrix multiplication, we'll obtain the two estimates above:
  coefs <- coef(fm)$Subject

  # Sanity check: verify same subjects in both arrays
  if ( (sum(! row.names(mm_sbj) %in%  row.names(coefs) )  != 0 ) |
       (sum(! row.names(coefs)  %in%  row.names(mm_sbj) ) != 0 )) {
    stop ("Subjects are not the same!")
  } else {  # same row order in both (by subject)
    mm_sbj <- mm_sbj[rownames(coefs), ]
  }
  
  # Compute the 2 estimates for each subject using compute_ppt_estimate()
  ppt_estimates <- matrix(NA, nrow = nrow(mm_sbj), ncol = 2)
  for (S in 1 : nrow(mm_sbj)) {
    ppt_estimates[S, ] <- compute_ppt_estimate(mm_sbj[S, ], coefs[S, ])
  }
  out <- data.frame(ppt_estimates, Subject = rownames(mm_sbj))
  names(out)[1:2] <- c("Intercept", "Trial")
  # Combine with subject information
  out <- left_join(out, df %>% select(Subject, Condition, Group, ClozeScore) %>% unique)
  out %>% select(Subject:ClozeScore, Intercept, Trial)
}
