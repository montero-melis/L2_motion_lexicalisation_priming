# Function used to load models if they have already been saved,
# rather than fitting them anew each time the script is called.

load_or_fit <- function(
  fm_name,  # The name or the fitted model, which will also be the file name.
  fm_code,  # A string that will be evaluated as an expression (function call) in R
  forcefit = FALSE,  # If TRUE, the model will be fitted even if there is an object with that name.
  alternPath = NULL,  # Possibly indicate alternative path to look and save
  ...
  ) {

  # path to subfolder where fitted models will be saved and looked for
  if (is.null(alternPath)) {
    rel_path <- './fitted_models/'
  } else {
    rel_path <- alternPath
  }

  # the string in argument fm_code may contain '\n'; remove them
  fm_code <- gsub('\n', '', fm_code)
  
  # check first if there is a saved rda object with that name
  model_exists <- paste0(fm_name, '.rda') %in% list.files(path = rel_path)  # logical: exists?

  # NB: if forcefit = T it will always fit the model
  if((!forcefit) & model_exists) {
    load(paste0(rel_path, fm_name, '.rda'), .GlobalEnv)
    
    } else {  # fit it and save it to right folder
      cat("Fitting model ... ")
      
      # record the time it takes to fit
      fm_time <- paste0(fm_name, ".t")
      ptm <- proc.time()
      assign(fm_name, eval(parse(text = fm_code), ...), envir = .GlobalEnv)
      assign(fm_time, proc.time() - ptm, envir = .GlobalEnv)
      
      # both the model and the time it took can be saved as same file
      save(list = c(fm_name, fm_time), file = paste0(rel_path, fm_name, '.rda'))
      
      # print brief model summary when fitted and time it took to fit
      fm <- get(fm_name)
      fm_t <- get(fm_time)
      print(fm)
      print(fm_t)
    }
  }
