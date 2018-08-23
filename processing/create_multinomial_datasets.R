## Florian asked for a wide and a long version of the dataset I used
## for analysis so he could try to fit a multinomial model.

library(dplyr)
library(tidyr)

# The data is created in the script 'processing/compute_dependent_measures.R'
# There is the normal and the liberally coded version (see script for difference).	
# Here I use the normal coding.	

## load	data
d <- read.csv('data/data_DVs.csv', fileEncoding = 'UTF-8', stringsAsFactors = TRUE)	
# simplify somewhat	
d <- d %>%	
  dplyr::select(Subject:Target, P_V, M_V) %>%	
  rename(Trial = VideoTrial)	
# Rename "Control" condition to "Baseline"	
levels(d$Condition)[levels(d$Condition) == "Control"] <- "Baseline"	
# Centre Trial	
d$cTrial <- round(d$Trial - mean(d$Trial), 2)
# check out
head(d)


## Add L2 proficiency scores:
  
# participant data
ppts <- read.csv("data/participants.csv", fileEncoding = "UTF-8",
                 stringsAsFactors = TRUE)
# No audio data recorded for Subject 14 (L2) due to experimental error; exclude
ppts <- ppts[ppts$Subject != 14, ]
ppts$Subject <- factor(ppts$Subject)

# center ClozeScore but not scaling (as.vector prevents it from becoming a matrix)
ppts$cClozeScore <- round(as.vector(scale(ppts$ClozeScore, scale = FALSE)), 2)

# Turn L2 proficiency into a categorical (ordered) variable.
# Do this by assigning cloze scores <= 40th percentile to "low",
# scores >= 60th percentile to "high", and the rest to "medium", 
# which shall not be analyzed in the group comparison followign up on Question 3
cutoff_prof <- quantile(ppts$ClozeScore, c(.4, .6), na.rm = TRUE)
ppts$Profic_categ <- with(ppts,
                          ifelse(ClozeScore <= cutoff_prof[1], "low prof",
                                 ifelse(ClozeScore >= cutoff_prof[2], "high prof", 
                                        "medium prof")))
# native speakers
ppts$Profic_categ[with(ppts, is.na(Profic_categ) & Group == "NS")] <- "native"
# Leads to the following nuber of observations:
table(ppts$Profic_categ)
# add speakers' clozescore and derived measures to d:
d <- left_join(d, ppts %>% dplyr::select(Subject, ClozeScore, cClozeScore, 
                                         Profic_categ))
# NB: Subject NS_1 is in dataframe "d" but missing from df "ppts", while Subject
# NS_202 is in "ppts" but missing from "d". Both were in the baseline condition.
# Subject needs to be a factor
d$Subject <- factor(d$Subject)
# check out
head(d)


## DV as a multinomial outcome

# One row per description
d$DV_multinom <- with(d, ifelse(P_V == 0 & M_V == 0, "None",
                                ifelse(P_V == 1 & M_V == 0, "Path_verb",
                                       ifelse(P_V == 0 & M_V == 1, "Manner_verb", "Both"))))

with(d, table(Group, DV_multinom, useNA = "ifany"))
with(d, table(Group, DV_multinom, Condition, useNA = "ifany"))

# have a look at a random sample
sample_rows <- sample(nrow(d), 10)
d[sample_rows, ] %>% select(Subject:M_V, DV_multinom)  # looking all right

# write to disk
d %>% select(Subject:Trial, cTrial, VideoName, ClozeScore:DV_multinom) %>%
  write.csv("data/data_verbs_multinomial.csv",
            fileEncoding = "UTF-8", row.names = FALSE)


## Convert data to long format:
d_long <- d %>%
  mutate(
    None = ifelse(P_V == 0 & M_V == 0, 1, 0),
    Path_verb = ifelse(P_V == 1 & M_V == 0, 1, 0),
    Manner_verb = ifelse(P_V == 0 & M_V == 1, 1, 0),
    Both = ifelse(P_V == 1 & M_V == 1, 1, 0)
   ) %>%
  gather(key = Value, value = Used, None:Both) %>%
  select(Subject:Trial, cTrial, VideoName, ClozeScore:Profic_categ, Value:Used) %>%
  arrange(Subject, Trial, Value) 
    
head(d_long)

# write to disk
d_long %>%
  write.csv("data/data_verbs_multinomial_long.csv",
            fileEncoding = "UTF-8", row.names = FALSE)
