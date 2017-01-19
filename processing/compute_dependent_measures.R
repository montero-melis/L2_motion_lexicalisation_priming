## Compute the DVs used in the analyses

# For each participant and item, compute the following binary variables (0/1):
# 1) P_anyw: Was Path expressed anywhere in the target?
# 2) P_V: Was Path expressed in the main verb of the target?
# 3) P_adj: Was Path expressed as an adjunct in the target?
# 4) M_anyw: Was Manner expressed anywhere in the target?
# 5) M_V: Was Manner expressed in the main verb of the target?
# 6) M_adj: Was Manner expressed as an adjunct in the target?


library(dplyr)


#  ------------------------------------------------------------------------
#  Load data
#  ------------------------------------------------------------------------

# load transcriptions
tr_compl <- read.csv("data/data_verbal_transcribed.csv", fileEncoding = "UTF-8",
                     stringsAsFactors = FALSE)
head(tr_compl)

# simplify but keep Target column for reference
tr_simpl <- tr_compl %>%
  select(Subject:VideoName, Target)
head(tr_simpl)

# load annotated data
annot <- read.csv("data/data_annotated_long.csv", fileEncoding = "UTF-8",
                  stringsAsFactors = FALSE)
head(annot)


#  ------------------------------------------------------------------------
#  Summarise how Path/Manner were encoded
#  ------------------------------------------------------------------------

# Use a function to answer:
# How were Path/Manner expressed, in V, as an adjunct, as both or not at all?
# This function will be passed on to dplyr's do()
where_component <- function(df, component = NULL, liberal = FALSE) {
  # component has to be one of "Path" or "Manner"
  # liberal specifies if borderline cases (?Path/?Manner) should be counted
  mentions <- 
    if (liberal) {  # means e.g. that ?Path cases are counted as Path
      df$SemComp %in% c(component, paste("?", component, sep = ""))
    } else {
      df$SemComp == component
    }
  constituents <- df$SyntCateg[mentions]
  # E.g. is P expressed in the V (0=no, 1=yes)?
  V <- as.numeric(sum("V" == constituents) > 0)
  # Is P expressed as an adjunct (0=no, 1=yes)?
  Adjunct <- as.numeric(sum("Adjunct" == constituents) > 0)
  # Classify as one of 4 cases
  InfoExpressed <- 
    if(V + Adjunct == 0) {
      paste("No", component)
    } else if (V + Adjunct == 2) {
      "V+Adjunct"
    } else if (V & !Adjunct) {
      "V"
    } else if (!V & Adjunct) {
      "Adjunct"
    } else {
      stop("Oops!")
    }
  # dataframe as output, so it works with dplyr::out()
  out <- data.frame(InfoExpressed, stringsAsFactors = FALSE)
  names(out) <- component
  out
}

## Compute from the annotated data in long format

# Path
path_annot <- annot %>%
  group_by(Subject, Condition, Group, VideoTrial, VideoName) %>%
  do(where_component(., component = "Path", liberal = FALSE)) 
head(path_annot)
# now merge back into particiant data
d <- left_join(tr_simpl, path_annot)
# Those with an NA in Path are cases of "No Path"
head(d[is.na(d$Path), ])
d[is.na(d$Path), "Path"] <- "No Path"
head(d)

# Manner
manner_annot <- annot %>%
  group_by(Subject, Condition, Group, VideoTrial, VideoName) %>%
  do(where_component(., component = "Manner", liberal = FALSE)) 
head(manner_annot)
# now merge back into particiant data
d <- left_join(d, manner_annot)
# Those with an NA in Manner are cases of "No Manner"
head(d[is.na(d$Manner), ])
d[is.na(d$Manner), "Manner"] <- "No Manner"
head(d)

# check unique values
levels(factor(d$Path))
levels(factor(d$Manner))


#  ------------------------------------------------------------------------
#  Create the actually used DVs
#  ------------------------------------------------------------------------

d$P_anyw <- ifelse(d$Path != "No Path", 1, 0)
d$P_V    <- ifelse(d$Path %in% c("V", "V+Adjunct"), 1, 0)
d$P_adj  <- ifelse(d$Path %in% c("Adjunct", "V+Adjunct"), 1, 0)
d$M_anyw <- ifelse(d$Manner != "No Manner", 1, 0)
d$M_V    <- ifelse(d$Manner %in% c("V", "V+Adjunct"), 1, 0)
d$M_adj  <- ifelse(d$Manner %in% c("Adjunct", "V+Adjunct"), 1, 0)

# check out random rows
d[sample(nrow(d), 10), ]


#  ------------------------------------------------------------------------
#  Save to disk with normal/liberal coding
#  ------------------------------------------------------------------------

# Normal coding (as done above)
write.csv(d, file = "data/data_DVs.csv", row.names = FALSE, fileEncoding = "UTF-8")



# Liberal coding ----------------------------------------------------------

# For liberal coding the steps above are repeated but liberal argument is set to TRUE

## Compute from the annotated data in long format

# Path
path_annot <- annot %>%
  group_by(Subject, Condition, Group, VideoTrial, VideoName) %>%
  do(where_component(., component = "Path", liberal = TRUE)) 
# now merge back into particiant data
d_lib <- left_join(tr_simpl, path_annot)
# Those with an NA in Path are cases of "No Path"
d_lib[is.na(d_lib$Path), "Path"] <- "No Path"

# Manner
manner_annot <- annot %>%
  group_by(Subject, Condition, Group, VideoTrial, VideoName) %>%
  do(where_component(., component = "Manner", liberal = TRUE)) 
# now merge back into particiant data
d_lib <- left_join(d_lib, manner_annot)
# Those with an NA in Manner are cases of "No Manner"
d_lib[is.na(d_lib$Manner), "Manner"] <- "No Manner"

# check unique values
levels(factor(d_lib$Path))
levels(factor(d_lib$Manner))

##  Create the actually used DVs
d_lib$P_anyw <- ifelse(d_lib$Path != "No Path", 1, 0)
d_lib$P_V    <- ifelse(d_lib$Path %in% c("V", "V+Adjunct"), 1, 0)
d_lib$P_adj  <- ifelse(d_lib$Path %in% c("Adjunct", "V+Adjunct"), 1, 0)
d_lib$M_anyw <- ifelse(d_lib$Manner != "No Manner", 1, 0)
d_lib$M_V    <- ifelse(d_lib$Manner %in% c("V", "V+Adjunct"), 1, 0)
d_lib$M_adj  <- ifelse(d_lib$Manner %in% c("Adjunct", "V+Adjunct"), 1, 0)

# write to disk
write.csv(d_lib, file = "data/data_DVs_liberal.csv", row.names = FALSE, fileEncoding = "UTF-8")

