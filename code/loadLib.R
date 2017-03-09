# Load libraries, install missing libraries
libList <- c("data.table", 
             "dplyr", 
             "tidyr", 
             "plotly",
             "glmnet",
             # "topicmodels", 
             # "SnowballC",
             "quanteda",
             "tm", 
             "beepr", 
             "stringr",
             "RColorBrewer"
             )
for (i in 1:length(libList)) {
  if (!libList[i] %in% installed.packages()) { 
    install.packages(libList[i])
  }
  library(libList[i], character.only = TRUE)
}
rm(i, libList)
