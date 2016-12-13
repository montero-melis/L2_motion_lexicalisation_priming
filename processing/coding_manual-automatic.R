## Semi-automatic coding of transcriptions:
## It's basically manual coding done in a script, using regexp

library(dplyr)
library(stringr)


#  ------------------------------------------------------------------------
#  Data import and simplification
#  ------------------------------------------------------------------------

# load transcriptions
tr_compl <- read.csv("data/data_verbal_transcribed.csv", fileEncoding = "UTF-8",
               stringsAsFactors = FALSE)
head(tr_compl)
str(tr_compl)

# keep track of original row number in the transcribed data file (+1 to match)
tr_compl$rownb <- as.numeric(row.names(tr_compl)) + 1
# remove training items (VideoTrial == 0)
tr_compl <- tr_compl[tr_compl$VideoTrial != 0, ]
# Note there are no recordings for participant 14
tr_compl[tr_compl$Subject == "14", ]
# remove that participant
tr_compl <- tr_compl[tr_compl$Subject != "14", ]

# drop some columns to have a df that's easier to work with
tr <- tr_compl %>% select(rownb, Subject:VideoName, Target)

head(tr)
str(tr)


# # save all descriptions in randomized order
# set.seed(-3)
# tr_random <- tr[sample(nrow(tr)), ]
# # to file
# write.csv(tr_random, "processing/targets_random.csv", fileEncoding = "UTF-8")



#  ------------------------------------------------------------------------
#  Check out unique words
#  ------------------------------------------------------------------------

## How many unique words and their frequency?

# all descriptions into a single string vector of words
target <- unlist(strsplit(tr$Target, " "))
sort(table(target))
# count of unique words
length(unique(target))

rm(target)


#  ------------------------------------------------------------------------
#  Annotate data
#  ------------------------------------------------------------------------

# the data annotation is done by calling the script "annotation_regex.R"
source("processing/annotation_regex.R", encoding = "UTF-8")

# Add a column that shows the number of matches per target description
tr$NbMatches <- str_count(tr$Match, "%")
# remove the initial "%" (for first matches); leave "%" as separator only:
tr$Match <- str_replace(tr$Match, "^%", "")

head(tr)


#  ------------------------------------------------------------------------
#  Check coding
#  ------------------------------------------------------------------------

# We have to manually check the coding row per row to discover errors.
# These can be of the following types:
# - Missed targets, i.e. vocabulary that is not captured by a regex; subtypes:
#   - Missing vocabulary items (have to be added to Vocabulary)
#   - Target is in Vocabulary but is not captured, e.g. b/c of misspellings
# - False alarms, i.e. words erroneously identified as targets

# To spot errors, save data in random order to csv file which can then be
# checked on google drive.
# save all descriptions in randomized order
set.seed(33)
tr_random <- tr[sample(nrow(tr)), ]
# add columns needed for checking the coding:
tr_random$Checked_KS <- ""
tr_random$Comment_KS <- ""
tr_random$Comment_GMM <- ""

# to file
write.csv(tr_random, "processing/targets_random.csv", fileEncoding = "UTF-8")
rm(tr_random)


#  ------------------------------------------------------------------------
#  Annotated data in long format (one row per match)
#  ------------------------------------------------------------------------

# The basic function takes a row from the data frame tr as input. It outputs
# a new data frame with one row for each vocabulary match in the Target.

tolong <- function(df = tr) {
  out <- data.frame()
  # looping through each row is very slow, but it's transparent
  for (i in 1:nrow(df)) {
    if (df[i, "NbMatches"] > 0) {  # skip the row if no match
      # each match as a value of vector
      row_match <- unlist(str_split(df[i, "Match"], "%"))
      # split into the string matched and the associated vocabulary entry
      row_match_matrix <- str_split_fixed(row_match, ":", 2)
      # combine with that row's info
      curr_df <- data.frame(df[i, c("Subject", "Group", "Condition",
                                    "VideoTrial", "VideoName")],
                            row_match_matrix, row.names = NULL)
      out <- rbind(out, curr_df)
    }
  }
  names(out)[(length(out) - 1) : length(out)] <- c("Match", "VocabEntry")
  out
}

# small example
tr[33:39,]
tolong(tr[33:39,])

# do for the whole data set (slow ~ 15s)
tr_long <- tolong()


#  ------------------------------------------------------------------------
#  Merge long data with all vocabulary info
#  ------------------------------------------------------------------------

# Each row in tr_long is merged with vocabulary to obtain the columns:

# Subject: Subject identifier
# Group: L1 / L2
# Condition: priming condition (Path, Manner, Control)
# VideoTrial: Order in which this event was seen
# VideoName: scene (item) identifier
# (Target: The whole target description (for reference))
# Match: The specific match that was captured with the regex
# VocabEntry: The corresponding vocabulary entry associated to the Match
# Baseform: The base form (makes mainly sense for verbs)
# SemComp: The general semantic component expressed
# Meaning: The specific meaning expressed within the SemComp
# SyntCateg: The broad syntactic category -- wil be mostly main verb or adjunct
# SyntForm: The specific syntactic form








#  ------------------------------------------------------------------------
#  
#  ------------------------------------------------------------------------




#  ------------------------------------------------------------------------
#  
#  ------------------------------------------------------------------------




#  ------------------------------------------------------------------------
#  
#  ------------------------------------------------------------------------
