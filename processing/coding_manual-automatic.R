## Semi-automatic coding of transcriptions:
## It's basically manual coding done in a script, using regexp

library(dplyr)
library(stringr)


#  ------------------------------------------------------------------------
#  Data import and simplification
#  ------------------------------------------------------------------------

# load transcriptions
tr_compl <- read.csv("data/data_verbal_transcribed_full.csv", fileEncoding = "UTF-8",
               stringsAsFactors = FALSE)
head(tr_compl)
str(tr_compl)

# remove training items (VideoTrial == 0)
tr_compl <- tr_compl[tr_compl$VideoTrial != 0, ]
# Note there are no recordings for participant 14
tr_compl[tr_compl$Subject == "14", ]
# remove that participant
tr_compl <- tr_compl[tr_compl$Subject != "14", ]

# Remove observations where there is no data b/c of experimental error.
# First check that empty Target really corresponds to lack of recording:
tr_compl[tr_compl$Target == "", ]  # it does, see Comments
# So remove these
tr_compl <- tr_compl[tr_compl$Target != "", ]

# save simplified version to file
tr_compl %>% 
  select(Subject:Target_Object, Target) %>%
  write.csv("data/data_verbal_transcribed.csv", row.names = FALSE,
            fileEncoding = "UTF-8")

# because later I will be randomizing rows, create a column to keep track of
# original row number in the transcribed data file (+1 to match)
tr_compl$rownb <- as.numeric(row.names(tr_compl)) + 1

# drop unnecessary columns to have a df that's easier to work with
tr <- tr_compl %>% select(rownb, Subject:VideoName, Target)

head(tr)
str(tr)


# # save all descriptions in randomized order
# set.seed(-3)
# tr_random <- tr[sample(nrow(tr)), ]
# # to file
# write.csv(tr_random, "processing/targets_random.csv", fileEncoding = "UTF-8")



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
                            row_match_matrix,
                            row.names = NULL, stringsAsFactors = FALSE)
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
head(tr_long)
str(tr_long)


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

vocab <- read.csv("data/vocabulary.csv", fileEncoding = "UTF-8",
                  stringsAsFactors = FALSE)

# check that all vocabulary used in annotation is in the vocabulary sheet
sum(! unique(tr_long$VocabEntry) %in% vocab$VocabEntry)  # zero? If not:
mymissing <- ! unique(tr_long$VocabEntry) %in% vocab$VocabEntry
unique(tr_long$VocabEntry)[mymissing]
# ... and that all items in vocabulary sheet are used for annotation
sum(! vocab$VocabEntry %in% unique(tr_long$VocabEntry))  # zero? If not:
mymissing <- ! vocab$VocabEntry %in% unique(tr_long$VocabEntry)
vocab$VocabEntry[mymissing]
rm(mymissing)

# merge with vocabulary information
tr_long <- left_join(tr_long, vocab)
head(tr_long)

# tr_long now contains the annotated data, which can be saved to file
# Note, however, that it is not ready to use in the current format,
# because target descriptions which do not contain any annotated information
# do not appear in this data frame (each row represents one annotation).
# So to actually use it, it has to be merged back with the original list of
# participants and their utterances, excluding transcriptions for which there 
# was a technical error so that speech was not recorded (e.g., because the 
# participant pushed the space bar).
write.csv(tr_long, "data/data_annotated_long.csv", fileEncoding = "UTF-8",
          row.names = FALSE)


#  ------------------------------------------------------------------------
#  
#  ------------------------------------------------------------------------
