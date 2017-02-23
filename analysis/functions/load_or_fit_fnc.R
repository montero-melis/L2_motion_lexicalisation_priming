# Function used to load models if they have already been saved,
# rather than fitting them anew each time the script is called

require(mgcv)

load_or_fit <- function(fm_name, fm_code, forcefit = FALSE) {
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
    assign(fm_name, eval(parse(text = fm_code)), envir = .GlobalEnv)
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
