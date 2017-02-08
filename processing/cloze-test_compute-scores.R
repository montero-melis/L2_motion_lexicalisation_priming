## Code valid responses for Amazonas cloze test

library(plyr)
library(ggplot2)

# (This is taken from the excel sheet that Manne gave me, see 
# 'Dropbox/thesis/study2/data_files/cloze_amazonas_solution.xlsx')
# I've added the changes I suggested in an e-mail I sent to Manne 
# on 2014-10-28, 8:08 pm

valid <- list(
  c("del"),
  c("una"),
  c("de"),
  c("conviven", "habitan", "viven"),
  c("muy", "bastante", "super"),
  c("arboles", "bosques", "territorios", "montes"),
  c("animales", "insectos"),
  c("entre", "de"),
  c("activas", "peligrosas", "abundantes", "comunes", "frecuentes",
    "representadas", "numerosas", "pequeñas", "grandes", "habituales", 
    "venenosas", "trabajadoras", "importantes", "conocidas", "feroces",
    "pequenas"),
  c("en",  "de"),
  c("rio"),
  c("aguas", "desembocaduras", "profundidades", "cauces", "caudales",
    "afluentes"),
  c("se"),
  c("aguas", "orillas", "pozas", "lagunas", "corrientes"),
  c("en", "sobre"),
  c("la", "esta", "su"),
  c("pesar", "pezar"),
  c("ha"),
  c("sobre"),
  c("se"),
  c("la", "esta", "aquella"),
  c("duda"),
  c("peligro", "riesgo", "decadencia", "crisis"),
  c("partes"),
  c("en"),
  c("empresas", "emprezas", "tribus", "multinacionales", "industrias", 
    "fabricas", "corporaciones", "compañias", "personas", "madereras"),
  c("vendan"),
  c("con"),
  c("muchos", "los", "estos"),
  c("de"),
  c("los", "muchos"),
  c("medidas", "precauciones"),
  c("informe", "programa", "estudio", "documento", "proyecto", "plan", 
    "documental", "folleto", "tratado", "reporte", "articulo"),
  c("las"),
  c("segun"),
  c("que"),
  c("nunca", "jamas", "mas", "ya")
  )
# name each question Q1 through Q37
names(valid) <- paste("Q", 1:length(valid), sep =  "")
# make sure all lower case...
valid <- lapply(valid, tolower)
# ... and no leading or trailing whitespaces
valid <- lapply(valid, function (x) {gsub("^\\s+|\\s+$", "", x)})

head(valid)

## Response sheet
resp <- read.csv("data/clozetest_subject-responses.csv", strip.white = TRUE,
                 stringsAsFactors = FALSE)
# remove colums of NA's
resp <- resp[, colSums(is.na(resp)) != nrow(resp)]
# rename columns
names(resp) <- c("Subject", paste("Q", 1:37, sep=""))
# keep only rows for valid participants
resp <- resp[resp$Subject %in% 1:60, ]
# store subject ID's in separate vector
Subj <- resp$Subject
# everything to lower case and as matrix
resp <- sapply(resp, tolower)
# name rows with Subject ID's and remove Subject column
rownames(resp) <- resp[, "Subject"]
resp <- resp[, -1]

# Now correct the responses, assigning one point if response is in 'valid',
# 0 otherwise
score <- matrix(rep(NA, length(resp)), nrow = nrow(resp))
for (j in 1:37) {
  score[, j] <- as.numeric(resp[, j] %in% valid[[j]])
}
rm(j)

## Some general questions

# how well did Subjects do?
boxplot(apply(score, 1, sum), 
        main = "Percentage correct responses\nby subject (max 37)")

# How difficult were the items?
boxplot(apply(score, 2, sum),
        main = "Percentage correct responses\nby item (max 60)")

# sort items for difficulty:
q_difficulty <- apply(score, 2, sum)
names(q_difficulty) <- paste("Q", 1:37, sep = "")
sort(q_difficulty)

# An item that only 1 ppt got right?
which(apply(score, 2, sum) == 1)  # Q27 -- "vendan" (subjunctive form)

## total scores by subject
total <- data.frame(
  Subject = Subj,
  ClozeScore = apply(score, 1, sum)
  )

# write subject scores to file
write.csv(total, file = "data/clozetest_subject-scores.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
