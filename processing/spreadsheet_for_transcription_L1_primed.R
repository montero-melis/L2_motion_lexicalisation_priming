# Script to create a spreadsheet for transcription of the L1 priming data that
# is identical in form (i.e. same columns) as the one I have for the L2 priming
# data.


# Import the merged eprime file with trial-by-trial info from all L1 subjects
# (this has already been simplified somewhat from the raw exported version)
d <- read.csv("data/eprime_L1_primed_hopi_selected-columns.csv",
              fileEncoding = "UTF-8", stringsAsFactors = FALSE)


## edit columns so they are compatible with the existing spreadsheet for L2ers

# Subject ID's might overlap between L1 and L2 speakers; avoid confusion:
d$Subject <- paste("NS", d$Subject, sep = "_")
# identify this group as native speakers (NS)
d$Group <- "NS"
# Not needed
d$ExperimentName <- NULL
# transparent name for condition
unique(d[, 2:3])
d$Condition <- ifelse(d$Condition == 1, "Control",
                      ifelse(d$Condition == 2, "Manner", "Path"))
# not needed anymore
d$PrimeType <- NULL

# videonames all in the same column
names(d)[names(d) == "TargetVideo.SubTrial."] <- "VideoName"
d$VideoName[d$VideoName == "No Data"] <- "prt_meuche"  # training items

# VideoTrial is 0 for training and then 1 through 32:
names(d)[names(d) == "DescrDesignList"] <- "VideoTrial"
d$VideoTrial[d$VideoTrial == "No Data"] <- 0  # training items
d$VideoTrial <- as.numeric(d$VideoTrial)

# rename columns
names(d)[names(d) == "MannerPrimeSentence.SubTrial."] <- "MannerPrime"
names(d)[names(d) == "PathPrimeSentence.SubTrial."] <- "PathPrime"
names(d)[names(d) == "Prime_MannerVerb"] <- "MannerPrimeV"
names(d)[names(d) == "Prime_PathVerb"] <- "PathPrimeV"

# not needed
d$SessionDate <- NULL
d$SessionTime <- NULL
d$DescrTaskList <- NULL
d$Procedure.Trial. <- NULL
d$SubTrial <- NULL
# faster with column indices:
head(d[, c(6:7, 9, 11, 13:14, 16, 18:21)])
head(d[, -c(6:7, 9, 11, 13:14, 16, 18:21)])
d <- d[, -c(6:7, 9, 11, 13:14, 16, 18:21)]

# change "No Data" to "" (blank)
nodata <- function(v) {
  v[v == "No Data"] <- ""
  v
}
d[, 4:9] <- lapply(d[, 4:9], nodata)

# reorder columns so they match the existing transcription spreadsheet
d <- d[, c(1, 11, 2:3, 10, 5, 4, 7, 6, 8:9)]

# add necessary columns
d$Transcriber <- ""
d$Description <- ""
d$Target <- ""
d$Comments <- ""
d$FileName <- ""
d$Look <- ""
d$TrickyCase <- ""


###########################################################
## DON'T RUN THE FOLLOWING CODE!
###########################################################

# Now I take what had been transcribed as of 2016-06-28 and merge the spreadsheet
# with transcribed L2 data with the dataframe above for the L1 speakers.
# This requires some fiddling because I'm adding two variables that were not
# in the previous spreadsheet: Target_Ground and Target_Object

# load transcribed L2 data
l2 <- read.csv("data/data_verbal_transcribed_161028.csv",
              fileEncoding = "UTF-8", stringsAsFactors = FALSE)

## add information about grounds and objects to l2 df
# lookup table for grounds and objects of the videos
mylookup <- unique(d[, c("VideoName", "Target_Ground", "Target_Object")])
# join
library(dplyr)
l2 <- left_join(l2, mylookup)
# reorder columns
l2 <- l2[, c(1:9, 17:18, 10:16)]
# check
sum(names(d) != names(l2))  # identical column names

# bind the two dfs together
combi <- rbind(l2, d)

# Replace NAs in Look column with empty strings
combi$Look[is.na(combi$Look)] <- ""

write.csv(combi, "data/data_verbal_transcribed.csv", row.names = FALSE,
          fileEncoding = "UTF-8")
