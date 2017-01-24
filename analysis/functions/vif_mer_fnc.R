## Function to compute VIF (collinearity diagnostic)

# Retrieved from
# https://github.com/aufrank/R-hacks/blob/master/mer-utils.R
# through HLP blog entry
# https://hlplab.wordpress.com/2011/02/24/diagnosing-collinearity-in-lme4/
# See also Baayen (2008)

vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}
