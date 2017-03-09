# Script takes output from 01-UserCollectionScript.py (*.csv files) 
# and parses them into a column vector of user names. Basic frequency
# statistics are returned to console as well.

# Get list of all .csv files containing user comments - one for each 
# user. Script expects to find them in subfolder /crawled_user_comments 
# of current working directory.

crawled_commentsWD <- paste0(getwd(), "/crawled_user_comments")
crawledUC_filelist <- list.files(crawled_commentsWD, pattern = ".csv")



# Loop through each csv

# fread is arguably R's fastest csv parser
# Still errors are to be expected, so we try to capture those
# with a tryCatch(). To minimize problems when binding rows
# due to different column types, we first try the loading
# algorithm on a small portion of the data from which to get
# target column classes for each imported csv.

crawled_comments_temp <- as.data.frame(matrix(NA, 0, 0))
for(i in 1:floor(0.001*length(crawledUC_filelist))) {
  temp <- fread(paste0(crawled_commentsWD, "/", crawledUC_filelist[i]), 
                header = T, sep = ",")
  crawled_comments_temp <- bind_rows(crawled_comments_temp, temp)
  rm(temp)
}
colTypes <- as.data.frame(summary.default(crawled_comments_temp), stringsAsFactors = FALSE) %>% 
  filter(Var2=="Mode") %>% mutate(Freq=as.character(Freq), Freq=gsub(pattern="logical", replacement="character", Freq))
rm(crawled_comments_temp, i)

error_catcher <- list()
crawled_comments <- as.data.frame(matrix(NA, 0, 0))
for(i in 1:length(crawledUC_filelist)) {
  temp <- fread(paste0(crawled_commentsWD, "/", crawledUC_filelist[i]), 
                header = T, sep = ",", fill = TRUE, 
                colClasses = c(colTypes$Freq))
  catchTheError <- tryCatch(
    {
      crawled_comments <- bind_rows(crawled_comments, temp)
      print(i/length(crawledUC_filelist))
      print(i)
      print(Sys.time())
    },
    error=function(e) e
  )
  if(inherits(catchTheError, "error")){
    error_catcher[[length(error_catcher)+1]] <- c(crawledUC_filelist[i], i, paste0(catchTheError))

  }
  rm(temp, catchTheError)
}
saveRDS(error_catcher, "error_catcher.RDS")
saveRDS(crawled_comments, "crawled_comments.RDS")
rm(colTypes)
print(paste0(round(length(error_catcher)/length(crawledUC_filelist)*100, 2), 
             "% of user imports returned an error."))



