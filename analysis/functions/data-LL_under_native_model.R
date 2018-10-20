# Compute log-likelihood of new data under native model

# this function assigns the subject IDs from the natives randomly to the 
# subjects in "d_new" (the data from which we want to predict); if you do
# this on many iterations you're getting a distribution for the likelihood
# of the new data under the native model, while assuming that the random
# adjustments in natives captures well the randomness of the group you're
# predicting from (in this case, importantly, L2 speakers).


################################################################
# High-level function
################################################################

# High-level function that wraps things up
mypredict2 <- function(
  N = 20,  # number of iterations
  d_nat = NULL,  # original df for native speakers
  d_non = NULL,  # original df for non-native speakers
  mygam_basel = NULL,  # baseline native speaker model (no interaction with trial)
  mygam_alter = NULL,  # alternative native speaker model (incl interaction with trial)
  print_each_step = FALSE  # for use with myprint() function to understand function
  ) {

  # myprint function to print intermediate objects
  myprint <- myprint(print_each_step)  # myprint is a function itself
    
  # Matrix that will contain the LL differences for each iteration
  m_nat <- matrix(nrow = length(unique(d_nat$Subject)), ncol = N)
  m_non <- matrix(nrow = length(unique(d_non$Subject)), ncol = N)
  myprint(m_nat[1:7,]); myprint(m_non[1:7,])
  
  # Add column to keep track of the real subjects before assigning them random IDs
  d_nat$TrueSubject <- d_nat$Subject
  d_non$TrueSubject <- d_non$Subject
  myprint(head(d_nat)); myprint(head(d_non))
  
  for (n in 1:N) {
    print(paste("iteration", n))
    
    # randomly sample from the Subject IDs of the native speaker group; note we
    # want to hold individual differences in each run constant. (Size argument
    # works bc it's the same number of unique subjects in both groups)
    rand_subj <- sample(unique(d_nat$Subject), replace = TRUE,
                        size = length(unique(d_nat$Subject)))
    # Override subject info by randomly assigning native subject IDs with repl.
    levels(d_nat$Subject) <- sample(rand_subj)
    levels(d_non$Subject) <- sample(rand_subj)
    myprint(head(d_nat)); myprint(head(d_non))
    
    # Using functions defined separately below, we compute first the mean LL
    # of a subject's data under each model (using get_mean_LL()), and then
    # the difference between those LLs under each of the models, baseline/null
    # mode or alternative/full model (using LL_difference())
    nat_diff <- LL_difference(get_mean_LL(d_nat, mygam_basel), 
                              get_mean_LL(d_nat, mygam_alter))
    non_diff <- LL_difference(get_mean_LL(d_non, mygam_basel), 
                              get_mean_LL(d_non, mygam_alter))
    myprint(head(nat_diff)); myprint(head(non_diff))

    # put values into corresponding iteration column of respective matrices:
    m_nat[, n] <- nat_diff %>% pull(LL_diff)
    m_non[, n] <- non_diff %>% pull(LL_diff)
    myprint(m_nat[1:7,]); myprint(m_non[1:7,])
  }
  
  # N random assignments lead to a distribution of mean LL by subject
  # Summarize result of iterations as 3 quantiles: .05, .5, and .95:
  nat_distrib <- t(apply(m_nat, 1, quantile, probs = c(.05, .5, .95)))
  non_distrib <- t(apply(m_non, 1, quantile, probs = c(.05, .5, .95)))
  
  # arrange the distribution of LL differences back into a data frame
  # natives
  df_nat <- d_nat %>% group_by(TrueSubject, Condition, VbType_Cond, ClozeScore) %>% summarise()
  df_nat$Group <- "NS"
  df_nat <- data.frame(df_nat, nat_distrib)
  # non-natives
  df_non <- d_non %>% group_by(TrueSubject, Condition, VbType_Cond, ClozeScore) %>% summarise()
  df_non$Group <- "L2"
  df_non <- data.frame(df_non, non_distrib)
  
  # combine
  out <- rbind(df_nat, df_non)
  # rename columns
  names(out)[1] <- "Subject"
  names(out)[6:8] <- paste("quant", c("05", "50", "95"), sep = "")
  # add the number of iterations on which this is based
  out$nbIterations <- N
  # good to go!
  out
}


################################################################
# Lower-level functions / routines used by mypredict2()
################################################################

# myprint function to print intermediate objects -- function outputs a function
# which prints objects or not depending on "printout" argument.
myprint <- function(printout = FALSE) {
  if (isTRUE(! printout)) { f <- function(x) {}
  } else if (isTRUE(printout)) {
    f <- function (x) {
      cat("\n"); print(paste("This is ", deparse(substitute(x)))); cat("\n")
      print(x)
      }
  } else { stop("'printout' argument has to be a logical value!") }
  f
}
# FUN <- myprint(T)
# FUN(10)
# FUN <- myprint(F)
# FUN(10)
# rm(FUN)
# FUN <- myprint(10)
# FUN(10)


# Obtain the mean LL of all subject's data assuming some (native) model
# This function expects:
# - a Subject column with levels matching those of the native speaker group
# - a TrueSubject column which keeps track of the actual Subject
get_mean_LL <- function(
  my_newdata = NULL,
  assumed_model = NULL
  ) {
  # predict participant data under native model (including subject smooths)
  my_newdata$predicted <- predict.gam(assumed_model, newdata = my_newdata,
                                      type = "response")
  # Log the probability of each observation (=trial) under the current model,
  # giving us the log-likelihood (LL) of an observation under the model:
  my_newdata$LL <- with(my_newdata, log(ifelse(Used == 1, predicted, 1 - predicted)))
  # Average these log-probabilities by participant, to obtain the mean LL of
  # a participant's data under the native model
  mean_sbj_LL <- my_newdata %>% 
    group_by(TrueSubject) %>%
    summarise(LL = mean(LL))
  mean_sbj_LL
}
# d_ns2 <- d_ns
# d_ns2$TrueSubject <- d_ns2$Subject
# x <- get_mean_LL(d_ns2, gam_ns)
# y <- get_mean_LL(d_ns2, gam_ns)


# Function to calculate difference between two LLs for the two models
LL_difference <- function(
  ll_basel = NULL,
  ll_alter = NULL
  ) {
  # Check that the TrueSubject columns match exactly
  mismatch <- sum(as.character(ll_basel$TrueSubject) != as.character(ll_alter$TrueSubject))
  if (mismatch != 0) {stop ("These are not the same subjects!")}
  comparison <- data.frame(
    TrueSubject = ll_basel$TrueSubject,
    LL_basel = ll_basel$LL,
    LL_alter = ll_alter$LL
    )
  comparison$LL_diff <- with(comparison, LL_alter - LL_basel)
  comparison
}

# run this
# mypredict2(N = 2, d_ns, d_l2, mygam_basel = gam_ns, 
#           mygam_alter = gam_ns_no_trialinter, 
#           print_each_step = T)
# mypredict2(N = 2, d_ns, d_l2, mygam_basel = gam_ns, 
#           mygam_alter = gam_ns_no_trialinter, 
#           print_each_step = F)
