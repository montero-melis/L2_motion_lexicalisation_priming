## Function to compute Kappa's k (collinearity diagnostic)

# Retrieved from
# https://github.com/aufrank/R-hacks/blob/master/mer-utils.R
# through HLP blog entry
# https://hlplab.wordpress.com/2011/02/24/diagnosing-collinearity-in-lme4/
# See also Baayen (2008)

kappa.mer <- function (fit,
                       scale = TRUE, center = FALSE,
                       add.intercept = TRUE,
                       exact = FALSE) {
  X <- fit@pp$X
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  nrp <- sum(1 * (nam == "(Intercept)"))
  if (nrp > 0) {
    X <- X[, -(1:nrp), drop = FALSE]
    nam <- nam[-(1:nrp)]
  }
  
  if (add.intercept) {
    X <- cbind(rep(1), scale(X, scale = scale, center = center))
    kappa(X, exact = exact)
  } else {
    kappa(scale(X, scale = scale, center = scale), exact = exact)
  }
}
