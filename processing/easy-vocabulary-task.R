# Take the raw responses from the easy vocabulary task performed by L2 speakers
# before the priming manipulation and summarize them as by-subject scores.

getwd()

library(dplyr)

voc <- read.csv("data/easy_vocabulary-task_L2_raw.csv", fileEncoding = "UTF-8")
voc$Subject <- as.character(voc$Subject)
head(voc)

voc_subj <- voc %>% 
  group_by(Subject) %>%
  summarise(VocabTaskAcc = round(mean(Accuracy), 3))

write.csv(voc_subj, "data/easy_vocabulary-task_L2_by-subject.csv",
          row.names = FALSE)

# Add this info as a column to the data we'll share for the paper:
ppt <- read.csv("1901_data-script-sharing/participant-info.csv",
                stringsAsFactors = FALSE)
ppt2 <- left_join(ppt, voc_subj)
head(ppt2)
tail(ppt2)

# Save to disk
write.csv(ppt2, "1901_data-script-sharing/participant-info.csv", fileEncoding = "UTF-8", row.names = FALSE)
