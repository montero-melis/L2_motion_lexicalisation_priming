## Translation task: At the end of the experiment, all participants had to
## translate Spanish sentences to Swedish. All sentences were taken from the
## exposure phase, and they covered the 4 target verbs used in the experiment.

# - Path-exposed participants translated 4 path sentences
# - Manner-exposed participants translated 4 manner sentences
# - Participants in baseline condition transcribed all 8 (4P, 4M)

library(dplyr)
library(ggplot2)


#  ------------------------------------------------------------------------
#  Load data
#  ------------------------------------------------------------------------

transl <- read.csv("data/written_translation_task.csv", fileEncoding = "UTF-8",
                   stringsAsFactors = FALSE)
head(transl)
# get rid of comments
transl$Comment <- NULL
# More explicit factor levels
transl$Type <- factor(transl$Type, labels = c("Manner verb", "Path verb"))
# Subjects as character for later joining
transl$Subject <- as.character(transl$Subject)

# participant data -- for cloze scores
ppts <- read.csv("data/participants.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
head(ppts)

# add speakers' clozescore to d:
transl <- left_join(transl, ppts %>% select(Subject, ClozeScore))


#  ------------------------------------------------------------------------
#  Scoring
#  ------------------------------------------------------------------------

# Convenience functions to find unique responses
find_unique <- function(target_v = NULL, df = transl) {
  unique_tr <- df[df$Target_verb == target_v, "Translation"]
  sort(unique(unique_tr))
}

# and to score correct/wrong responses based on provided string vector
score_transl <- function(target_v = NULL, valid = NULL, df = transl) {
  rowid <- which(df$Target_verb == target_v)
  tr <- df$Translation[rowid]
  scores <- ifelse(tr %in% valid, 1, 0)
  df[rowid, "Score"] <- scores
  transl <<- df
  df
}

find_unique(target_v = "arrastrar")
score_transl("arrastrar", c("breda ut / skaka", "dra", "släpa"))

find_unique(target_v = "bajar")
score_transl("bajar", c("gå nedför", "gå ner från", "gå nerför", "komma nerför",
                        "stiga nedför", "sänka", "ta ned", "ta ner"))

find_unique(target_v = "cruzar")
score_transl("cruzar", c("gå genom", "gå igenom", "gå tvärs över", "gå över", 
                        "korsa", "passera"))

find_unique(target_v = "empujar")
score_transl("empujar", c("fösa", "knuffa", "puffa", "putta", "skjuta", "trycka"))

find_unique(target_v = "entrar")
score_transl("entrar", c("gå in i", "gå in på", "göra entré", "in i",
                         "komma in i", "komma in på", "stiga in i"))

find_unique(target_v = "rodar")
score_transl("rodar", c("rulla", "snurra"))

find_unique(target_v = "subir")
score_transl("subir", c("bestiga", "gå upp", "gå upp i", "gå upp på", 
                        "gå uppför", "höja", "klättra upp", "köra uppför",
                        "ta sig upp", "ta sig uppför", "ta upp", "åka uppför"))

find_unique(target_v = "tirar de")
score_transl("tirar de", c("dra", "dra från", "dra i"))


#  ------------------------------------------------------------------------
#  Descriptives (summary tables)
#  ------------------------------------------------------------------------

# By condition and verb
transl %>% group_by(Condition, Type, Target_verb) %>%
  summarise(Perc = round(sum(Score) / n(), 2))

# By condition and verb-type 
transl %>% group_by(Condition, Type) %>%
  summarise(Perc = round(sum(Score) / n(), 2))


#  ------------------------------------------------------------------------
#  Descriptives (by-subject plots)
#  ------------------------------------------------------------------------

mywidth <- 7
myheight <- 3

# ggplot theme
mytheme <- theme_bw() + 
  theme(#text = element_text(size = 10),
    # panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(color="black"),
    axis.line.y = element_line(color="black"))

by_subj <- transl %>% group_by(Subject, Condition, Type, ClozeScore) %>%
  summarise(Perc = round(sum(Score) / n(), 2))

ggplot(by_subj, aes(x = ClozeScore, y = Perc, colour = Type)) +
  geom_jitter(height = 0, alpha = .5) +
  facet_grid(. ~ Condition) +
  geom_smooth(se = FALSE) +
  ylab("Proportion of correct translations") +
  mytheme

ggsave("analysis/figures/translation-task_loess.pdf", width = mywidth,
       height = myheight)
ggsave("analysis/figures/translation-task_loess.png", width = mywidth,
       height = myheight)

ggplot(by_subj, aes(x = ClozeScore, y = Perc, colour = Type)) +
  geom_jitter(height = 0, alpha = .5) +
  facet_grid(. ~ Condition) +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Proportion of correct translations") +
  mytheme

ggsave("analysis/figures/translation-task_lm.pdf", width = mywidth,
       height = myheight)
ggsave("analysis/figures/translation-task_lm.png", width = mywidth,
       height = myheight)
