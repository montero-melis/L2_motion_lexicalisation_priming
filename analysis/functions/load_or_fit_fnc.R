# Function used to load models if they have already been saved,
# rather than fitting them anew each time the script is called

require(mgcv)

load_or_fit <- function(fm_name, fm_code, forcefit = FALSE, ...) {
  rel_path <- './gamms/'
  # the string in argument fm_code may contain '\n'; remove them
  fm_code <- gsub('\n', '', fm_code)
  
  # check first if there is a saved rda object with that name; if so, load it
  model_exists <- paste0(fm_name, '.rda') %in% list.files(path = rel_path)
  # if forcefit = T it will always fit the model
  if((!forcefit) & model_exists) {
    load(paste0(rel_path, fm_name, '.rda'), .GlobalEnv)
    
  } else {  # fit it and save it to right folder
    # record the time it takes to fit
    fm_time <- paste0(fm_name, ".t")
    ptm <- proc.time()
    assign(fm_name, eval(parse(text = fm_code), ...), envir = .GlobalEnv)
    assign(fm_time, proc.time() - ptm, envir = .GlobalEnv)
    # both the model and the time it took can be saved as same file
    save(list = c(fm_name, fm_time), file = paste0(rel_path, fm_name, '.rda'))
    # print a brief model summary when it has been fitted and time to fit
    fm <- get(fm_name)
    fm_t <- get(fm_time)
    print(fm)
    print(fm_t)
  }
}


# A function to fit models using a sort of two-fold cross-validation approach
# for the baseline data (instead of double-counting baseline participants).
# The function randomly takes half of the baseline participants and assigns
# them to the VerbType = MannerVerb, and the other half to VerbType = PathVerb;
# it then fits a model to that first fold using the load_or_fit function;
# then it inverts the assignment and fits another model.
# All models are saved to disk with appropriate names, which include the random
# seed employed for the assignment

# NB: For now it will only work as expected if we analyze either ONLY the native
# speakers or ONLY the L2ers, but not if we mix the two groups; this is because
# it will split all baseline participants into two folds, irrespective of group.
load_or_fit_2foldx <- function(fm_name, fm_code, myrandseed = NULL) {
  
  # randomly choose a random seed if not given as argument
  if (is.null(myrandseed)) {
    myrandseed <- base::sample.int(999, 1)
  }

  # load data used to fit the model (regex to match the data argument in fm_code)
  datafile <- gsub(".*data\\s?=\\s?(.*?),.*", "\\1", fm_code)
  df <- eval(parse(text = datafile))  # it will eventually look for it in .GlobalEnv
  
  # unique baseline participants
  doubled_ppts <- unique(as.character(df$Subject[df$Condition == "Baseline"]))
  unique_ppts <- unique(gsub("(.*)\\.[PM]_V", "\\1", doubled_ppts))  # keeps ppt ID only
  
  # assign half of them to each fold
  set.seed(myrandseed)
  fold1_ids <- base::sample(unique_ppts, floor(length(unique_ppts) / 2))
  fold2_ids <- unique_ppts[!unique_ppts %in% fold1_ids]
  # Data points to be excluded in each fold
  fold1_exclude <- c(paste(fold1_ids, ".P_V", sep =""),
                     paste(fold2_ids, ".M_V", sep =""))
  fold2_exclude <- c(paste(fold2_ids, ".P_V", sep =""),
                     paste(fold1_ids, ".M_V", sep =""))
  # full data set divided into two folds
  fold1 <- df[! df$Subject %in% fold1_exclude, ]
  fold2 <- df[! df$Subject %in% fold2_exclude, ]
  
  # load or fit model for each fold
  for (i in 1:2) {
    # replace the data argument in model call with folded data
    curr_fm_code <- gsub("data\\s?=\\s?.*?,", paste("data = fold", i, ",", sep =""), fm_code)
    # load model or fit
    curr_modelname <- paste(fm_name, "_2foldx_", myrandseed, "_fold", i, sep ="")
    # Take advantage of ellipsis (...) to pass the envir argument; allows for
    # looking up the data sets fold1 and fold2 in the current environment
    load_or_fit(curr_modelname, curr_fm_code, envir = environment())
  }
}