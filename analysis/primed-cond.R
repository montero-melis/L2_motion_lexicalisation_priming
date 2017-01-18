## Analyse data from participants in the primed conditions.

library(dplyr)
library(ggplot2)

#  ------------------------------------------------------------------------
#  Load data and simplify
#  ------------------------------------------------------------------------

# load transcriptions
tr_compl <- read.csv("data/data_verbal_transcribed.csv", fileEncoding = "UTF-8",
                     stringsAsFactors = FALSE)
head(tr_compl)

# simplify and keep all participants; in this df include Target column
d_wtargets <- tr_compl %>%
  select(Subject:VideoName, Target)
head(d_wtargets)

# load annotated data
annot <- read.csv("data/data_annotated_long.csv", fileEncoding = "UTF-8",
                  stringsAsFactors = FALSE)
head(annot)


#  ------------------------------------------------------------------------
#  Summarise Path encoding
#  ------------------------------------------------------------------------

# How was Path expressed: in V, as an adjunct or both?
# this function will be passed on to dplyr
where_path <- function(df, liberal = FALSE) {
  mentions_path <- 
    if (liberal) {  # means that ?Path cases are counted as Path
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
      "V+Adjunct"
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
  group_by(Subject, Condition, Group, VideoTrial, VideoName) %>%
  do(where_path(., liberal = FALSE)) 

head(path_annot)

# now merge back into particiant data
d_wtargets <- left_join(d_wtargets, path_annot)
# Those with an NA in Path are cases of "No Path"
d_wtargets[is.na(d_wtargets$Path), ]
d_wtargets[is.na(d_wtargets$Path), "Path"] <- "No path"


#  ------------------------------------------------------------------------
#  Summarise Manner encoding
#  ------------------------------------------------------------------------

# How was Manner expressed: in V, as an adjunct or both?
# this function will be passed on to dplyr
where_manner <- function(df, liberal = FALSE) {
  mentions_manner <- 
    if (liberal) {  # means that ?Manner cases are counted as Manner
      df$SemComp %in% c("?Manner", "Manner")
    } else {
      df$SemComp == "Manner"
    }
  manner_constituents <- df$SyntCateg[mentions_manner]
  # Is P expressed in the V (0=no, 1=yes)?
  P_V <- as.numeric(sum("V" == manner_constituents) > 0)
  # Is P expressed as an adjunct (0=no, 1=yes)?
  P_Adjunct <- as.numeric(sum("Adjunct" == manner_constituents) > 0)
  # Classify as one of 3 or 4 cases (depends on whether "V+Adjunct" is a category?)
  Manner <- 
    if(P_V + P_Adjunct == 0) {
      "No manner"
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
  data.frame(Manner, stringsAsFactors = FALSE)
}

# do this for the annotated data in long format
manner_annot <- annot %>%
  group_by(Subject, Condition, Group, VideoTrial, VideoName) %>%
  do(where_manner(., liberal = FALSE)) 

head(manner_annot)

# now merge back into particiant data
d_wtargets <- left_join(d_wtargets, manner_annot)
# Those with an NA in Manner are cases of "No Manner"
d_wtargets[is.na(d_wtargets$Manner), ]
d_wtargets[is.na(d_wtargets$Manner), "Manner"] <- "No manner"

# order factor levels for plotting
d_wtargets$Path <- factor(d_wtargets$Path, levels = c("V", "Adjunct", "V+Adjunct", "No path"))
d_wtargets$Manner <- factor(d_wtargets$Manner, levels = c("V", "Adjunct", "V+Adjunct", "No manner"))

head(d_wtargets)


#  ------------------------------------------------------------------------
#  Descriptive tables/figures at group level
#  ------------------------------------------------------------------------

# simpler name for data frame without targets
d <- d_wtargets %>% select(-Target)

with(d, addmargins(table(Path, Manner, Group)))

my_ylim <- ylim(c(0, 600))

# Control
ggplot(d[d$Condition == "Control",], aes(x = Group)) +
  geom_bar() +
  facet_grid(. ~ Path) +
  ggtitle("Path encoding") +
  my_ylim

ggplot(d[d$Condition == "Control",], aes(x = Group)) +
  geom_bar() +
  facet_grid(. ~ Manner) +
  ggtitle("Manner encoding") +
  my_ylim

# Path-primed
ggplot(d[d$Condition == "Path",], aes(x = Group)) +
  geom_bar() +
  facet_grid(. ~ Path) +
  ggtitle("Path encoding") +
  my_ylim

ggplot(d[d$Condition == "Path",], aes(x = Group)) +
  geom_bar() +
  facet_grid(. ~ Manner) +
  ggtitle("Manner encoding") +
  my_ylim

# Manner-primed
ggplot(d[d$Condition == "Manner",], aes(x = Group)) +
  geom_bar() +
  facet_grid(. ~ Path) +
  ggtitle("Path encoding") +
  my_ylim

ggplot(d[d$Condition == "Manner",], aes(x = Group)) +
  geom_bar() +
  facet_grid(. ~ Manner) +
  ggtitle("Manner encoding") +
  my_ylim



#  ------------------------------------------------------------------------
#  Semantic componentes expressed anywhere -- by speakers
#  ------------------------------------------------------------------------

compute_average <- function(df, component = NULL, method = NULL) {
  if (is.null(component) || ! component %in% c("Path", "Manner")) {
    stop("Specify component as Path or Manner!")
  }
  if (is.null(method)) {
    stop("Specify a method for computing the average!")
  } else if (method == "anywhere") {
    hit <- c("V", "Adjunct", "V+Adjunct")
  } else if (method == "V") {
    hit <- c("V", "V+Adjunct")
  } else if (method == "Adjunct") {
    hit <- c("Adjunct", "V+Adjunct")
  } else {
    stop("The method for computing the average is not valid!")
  }
  # we need a trick to extract the column as a vector using dplyr, see
  # http://stackoverflow.com/questions/21618423/extract-a-dplyr-tbl-column-as-a-vector
  hits <- sum( (df %>% collect %>% .[[component]]) %in% hit)
  out <- data.frame(hits)
  names(out) <- paste(component, method, sep = "_")
  out
}

compute_average(d, "Path", "anywhere")

# Compute for each participant: a) #descriptions provided, b) #descriptions
# containing path anywhere/in the V/in an adjunt, c) #descriptions containing
# manner anywhere/in the V/in an adjunt

# a)
d_descr <- d %>% group_by(Subject, Group, Condition) %>% summarise(N = n())
# b)
d_p_any <- d %>% group_by(Subject, Group, Condition) %>%
  do(compute_average(., "Path", "anywhere"))
d_p_v <- d %>% group_by(Subject, Group, Condition) %>%
  do(compute_average(., "Path", "V"))
d_p_adj <- d %>% group_by(Subject, Group, Condition) %>%
  do(compute_average(., "Path", "Adjunct"))
# c)
d_m_any <- d %>% group_by(Subject, Group, Condition) %>%
  do(compute_average(., "manner", "anywhere"))
d_m_v <- d %>% group_by(Subject, Group, Condition) %>%
  do(compute_average(., "manner", "V"))
d_m_adj <- d %>% group_by(Subject, Group, Condition) %>%
  do(compute_average(., "manner", "Adjunct"))



d[, "Path"]
(d %>% group_by(Subject, Group, Condition))[, "Path"]


