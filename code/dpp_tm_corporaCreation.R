# Text preprocessing

# Create wrapper loop for batch word stemming

# Tests showed faster times for removing punctuation, numbers
# and stopwords through dplyr compared with tm::tm_map

# The first attempt to stem a corpora of all comments tried to allocate
# all available RAM space to the task (30GB of maximum available 32GB RAM)
# Instead we will split the corpora into 500 smaller ones, stemming each,
# thus also not losing progress if an error is thrown.
# Meta data of each comment is added for identification purposes - 
# based on description from tm::extensions.pdf documentation
crawled_comments$group <- rep(1:500, length.out=nrow(crawled_comments))
corp_dir <- paste0(getwd(), "/comment_corpora")
ifelse(dir.exists(corp_dir), 
       print("Subdirectory /comment_corpora already exists"), 
       dir.create(corp_dir))
meta_data <- list(contents="body", heading="name", author="author")


for (i in 1:500) {
  temp <- crawled_comments %>% 
    select(name, author, body, group) %>% 
    filter(group==i) %>%
    select(-group) %>%
    mutate(body=tolower(body), 
           body=removePunctuation(body),
           body=removeNumbers(body),
           body=removeWords(body, stopwords("english")))
  customReader <- readTabular(mapping = meta_data)
  temp_Corp <- VCorpus(DataframeSource(temp), readerControl = list(reader=customReader))
  temp_Corp <- tm_map(temp_Corp, stemDocument)
  temp_Corp <- tm_map(temp_Corp, stripWhitespace)
  saveRDS(temp_Corp, paste0(corp_dir, "/tempCorp_", i, ".rds"))
  rm(temp, temp_Corp)
}; beepr::beep(2)

commentsCorp <- readRDS(paste0(corp_dir, "/tempCorp_", 1, ".rds"))
for (i in 2:500) {
  temp <- readRDS(paste0(corp_dir, "/tempCorp_", i, ".rds"))
  commentsCorp <- c(commentsCorp, temp)
  rm(temp)
}; beepr::beep(2)

saveRDS(commentsCorp, "commentsCorpora.rds")
rm(i)

