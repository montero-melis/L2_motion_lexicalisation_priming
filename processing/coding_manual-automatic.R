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

# remove training items (VideoTrial == 0)
tr_compl <- tr_compl[tr_compl$VideoTrial != 0, ]
# no recordings for participant 14
tr_compl[tr_compl$Subject == "14", ]
# keep track of original row number in the transcribed data file
tr_compl$rownb <- row.names(tr_compl)

# drop some columns to have a df that's easier to work with, and remove all 
# rows with empty transcriptions
tr <- tr_compl %>% filter(Target != "")  %>% 
  select(rownb, Subject:VideoName, Target)

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

tr[1:500,]

#  ------------------------------------------------------------------------
#  The basic function
#  ------------------------------------------------------------------------



# The basic function takes a row from the data frame tr as input. It outputs
# a new data frame with one row for each vocabulary match in the Target.
# Each of the rows in the output df has the following columns:

# Subject: Subject identifier
# VideoName: scene (item) identifier
# Target: The whole target description (for reference)
# Match: The specific match that was captured with the regex
# VocabEntry: The corresponding vocabulary entry associated to the Match
# Baseform: The base form (makes mainly sense for verbs)
# SemComp: The general semantic component expressed
# Meaning: The specific meaning expressed within the SemComp
# SyntCateg: The broad syntactic category -- wil be mostly main verb or adjunct
# SyntForm: The specific syntactic form

annotate <- function(df_row, regex) {
  str_detect(df_row$Target, regex)
  str_subset(df_row$Target, regex)
  str_extract(df_row$Target, regex)
}

annotate(tr[1, ], "baj.*o\\b")



mymatch <- function(regex = NULL, df = tr){
  df[with(df, str_detect(Target, regex)), ]
}

myextract <- function(regex = NULL, df = tr){
  df[with(df, str_extract(Target, regex)), ]
}


mymatch("desde abajo")
mymatch("para abajo")
mymatch("(?<!desde )abajo")

str_extract(tr$Target,"desde abajo")
str_extract(tr$Target,"(?<!desde )abajo")
str_extract(tr$Target,"(?<!(para)|(desde)|(hacia) )abajo")
str_extract(tr$Target,"((para)|(desde)|(hacia)) abajo")



mymatch("\\bsube\\b", tr[1:200,])



#  ------------------------------------------------------------------------
#  Identify and code main verbs in each target
#  ------------------------------------------------------------------------









#  ------------------------------------------------------------------------
#  
#  ------------------------------------------------------------------------




#  ------------------------------------------------------------------------
#  
#  ------------------------------------------------------------------------




#  ------------------------------------------------------------------------
#  
#  ------------------------------------------------------------------------
