## Analyse data from participants in the control (no priming) condition.


library(dplyr)
# library(stringr)


#  ------------------------------------------------------------------------
#  Load data and simplify
#  ------------------------------------------------------------------------

# load transcriptions
tr_compl <- read.csv("data/data_verbal_transcribed.csv", fileEncoding = "UTF-8",
                     stringsAsFactors = FALSE)
head(tr_compl)

# simplify and keep only control participants; in this df include Target column
d_wtargets <- tr_compl %>%
  filter(Condition == "Control") %>%
  select(Subject:VideoName, Target)
head(d_wtargets)

# load annotated data
annot <- read.csv("data/data_annotated_long.csv", fileEncoding = "UTF-8",
                  stringsAsFactors = FALSE)
head(annot)


#  ------------------------------------------------------------------------
#  Path encoding
#  ------------------------------------------------------------------------

# How was Path expressed: in V, as an adjunct or both?
# this function will be passed on to dplyr
where_path <- function(df, liberal = FALSE) {
  mentions_path <- 
    if (liberal) {  # means that ?Path cases counted as Path
      df$SemComp %in% c("?Path", "Path")
      } else {
        df$SemComp == "Path"
      }
  path_constituents <- df$SyntCateg[mentions_path]
  # Is P expressed in the V (0=no, 1=yes)?
  P_V <- as.numeric(sum("V" == path_constituents) > 0)
  # Is P expressed as an adjunct (0=no, 1=yes)?
  P_Adjunct <- as.numeric(sum("Adjunct" == path_constituents) > 0)
  # Classify as one of 3 or 4 cases (depends on whether "mixed" an own category?)
  Path <- 
    if(P_V + P_Adjunct == 0) {
      "No path"
    } else if (P_V + P_Adjunct == 2) {
      "V+Adjunct"  # change to "mixed" for more fine-grained classification
    } else if (P_V & !P_Adjunct) {
      "V"
    } else if (!P_V & P_Adjunct) {
      "Adjunct"
    } else {
      stop("Oops!")
    }
  # dataframe as output, so it works with dplyr::out()
  data.frame(Path, stringsAsFactors = FALSE)
}

# do this for the annotated data in long format
path_annot <- annot %>%
  filter(Condition == "Control") %>%
  group_by(Subject, Condition, Group, VideoTrial, VideoName) %>%
  do(where_path(., liberal = TRUE)) 

head(path_annot)


# now merge back into particiant data
left_join(d_wtargets, path_annot)