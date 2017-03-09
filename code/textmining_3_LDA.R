# If comments have already been preprocessed, but
# object crawled_comments does not exist in current environemnt
commentsCorp <- readRDS("commentsCorpora.rds")

# Load libraries, install missing libraries
libList <- c("data.table", "dplyr", "tidyr", "tm", "SnowballC", "beepr", "topicmodels", "Matrix")
for (i in 1:length(libList)) {
  if (!libList[i] %in% installed.packages()) { 
    install.packages(libList[i])
  }
  library(libList[i], character.only = TRUE)
}
rm(i, libList)

DTM_comments <- DocumentTermMatrix(commentsCorp)
inspect(DTM_comments)
# <<DocumentTermMatrix (documents: 3491692, terms: 894271)>>
#   Non-/sparse entries: 45037731/3122473858801
# Sparsity           : 100%
# Maximal term length: 9993
# Weighting          : term frequency (tf)
DTM_comments2 <-(removeSparseTerms(DTM_comments, 0.95))
saveRDS(DTM_comments, "DTM_stemmed_comments.rds")

