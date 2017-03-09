### Data needs to be downloaded from http://wwbp.org/data.html 
### Section Word and Phrase Correlation, all but gender and age
### are used

vocab_dir <- paste0(getwd(),"/OpenVocabData")
ifelse(!dir.exists(vocab_dir), 
       print("Subdirectory /OpenVocabData does not exist"), 
       print(" /OpenVocabData exists"))


### Import all traits
scoringsList <- dir(path = vocab_dir, pattern = "top100.1to3grams.gender_age_controlled.rmatrix.csv")
scorings <- as.data.frame(matrix(NA, 0, 0))
for (i in 1:length(scoringsList)) {
  temp <- fread(input = paste0(vocab_dir, "/", scoringsList[i]))
  temp$category <- colnames(temp)[2]
  colnames(temp)[2] <- "weight"
  scorings <- bind_rows(scorings, temp)
  rm(temp)
}
rm(scoringsList, i, vocab_dir)
