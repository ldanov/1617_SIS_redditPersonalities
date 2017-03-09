# Script takes output from 01-UserCollectionScript.py (*.csv files) 
# and parses them into a column vector of user names. Basic frequency
# statistics are returned to console as well.

# Clear current global environment
rm(list=ls())

# Get list of all .csv files containing usernames - one for each 
# subreddit. Script expects to find them in subfolder /crawled_users 
# of current working directory.

# Loop through each csv
crawled_users_filelist <- list.files(paste0(getwd(), "/crawled_users"), pattern = ".csv")
crawled_users <- as.data.frame(matrix(c(0),0,2))
colnames(crawled_users) <- c("SUBREDDIT", "USERS")
for(i in 1:length(crawled_users_filelist)) {
  temp <- fread(paste0(getwd(),"/crawled_users/", crawled_users_filelist[i]), header = F, sep = ",")
  temp <- as.data.frame(transpose(temp[]))
  temp <- as.data.frame(temp[!is.na(temp[,1]),])
  temp$USERS <- gsub("AUTHORS:  ", "", temp[,1])
  temp[,1] <- paste0(gsub(".csv", "",crawled_users_filelist[i]))
  colnames(temp)[1] <- "SUBREDDIT"
  crawled_users <- rbind(crawled_users, temp)
}
rm(temp, crawled_users_filelist, i)

# Return frequency of username across all subreddits
table(table(crawled_users$USERS))

# Create per user frequency table across each and all subreddits
crawled_users_stat <- summarise(group_by(crawled_users, USERS, SUBREDDIT), posts_in_subreddit=n()) %>% spread(key=SUBREDDIT, value = posts_in_subreddit, fill = 0)
crawled_users_stat <- summarise(group_by(crawled_users, USERS), posts_in_all_subreddits=n()) %>% left_join(crawled_users_stat)

# Return number of users per subreddit and as distinct within
# subreddit
bind_rows(summarize(group_by(crawled_users, SUBREDDIT), n(), n_distinct(USERS)), summarize(group_by(crawled_users), SUBREDDIT="all subreddits", n(), n_distinct(USERS)))

# Create csv with column vector of unique user names as input for crawler
crawled_users %>% filter(USERS!="[deleted]") %>% select(USERS) %>% distinct(USERS) %>% write.table("00unique_user_list.csv", row.names = FALSE, col.names = FALSE, sep=",")
