# Script takes output from 01-UserCollectionScript.py (*.csv files) 
# and parses them into a column vector of user names. Basic frequency
# statistics are returned to console as well.

# Get list of all .csv files containing user comments - one for each 
# user. Script expects to find them in subfolder /crawled_user_comments 
# of current working directory.

crawled_comments_testWD <- path.expand(paste0(getwd(), "/crawled_user_comments_testdata"))
crawledUC_filelist <- list.files(path = crawled_comments_testWD, pattern = ".csv")



# Loop through each csv

# fread is arguably R's fastest csv parser
# Still errors are to be expected, so we try to capture those
# with a tryCatch(). To minimize problems when binding rows
# due to different column types, we first try the loading
# algorithm on a small portion of the data from which to get
# target column classes for each imported csv.

crawled_comments_test <- as.data.frame(matrix(NA, 0, 0))
for(i in 1:length(crawledUC_filelist)) {
  temp <- fread(paste0(crawled_comments_testWD, "/", crawledUC_filelist[i]), 
                header = T, sep = ",")
  findUnderstr <- substr(colnames(temp), 1, 1)=="_"
  colnames(temp)[findUnderstr] <- sub(pattern = "_",
                                                  replacement = "ed_",
                                                  colnames(temp)[findUnderstr]
  )
  rm(findUnderstr)
  temp <- temp %>% mutate_all(as.character)
  crawled_comments_test <- bind_rows(crawled_comments_test, temp)
  rm(temp)
}

saveRDS(crawled_comments_test, "crawled_comments_test.RDS")



