# Filter out filler rows that give boundaries between opposite traits in csv
scorings <- scorings %>% filter(V1!="") %>% rename(ngram=V1)

print(paste0("Highest p-value ", max(scorings$p)))

### Fist start by adding a whitespace to each ngram, as they are not stemmed
scorings$ngram <- paste0(scorings$ngram, " ")


### Escaping multiple hits on same variation
## youtube links: template looks like https://www.youtube.com/watch?v=
## all combinations of the template elements "www.youtube.com", "/watch?v=" 
## will be replaced by "youtube.com" and their average weight

youtube_vector <- c("\\v \\=", "youtube", "watch", "\\? \\v")

youtube_matrix <- as.data.frame(matrix(1:1000, 1000, 1))
for (i in 1:length(youtube_vector)) {
  temp <- as.data.frame(grepl(youtube_vector[i], scorings[,1]))
  colnames(temp)[1] <- paste0("Match_",i)
  youtube_matrix <- bind_cols(youtube_matrix, temp)
  rm(temp)
  
}
youtube_matrix %>% summarise_each(funs = c("sum"))
youtube_matrix <- youtube_matrix %>% 
  mutate(occurance=Match_1+Match_2+Match_3+Match_4) %>% 
  select(V1, occurance) %>%
  rename(row_nr=V1)
scorings <- bind_cols(scorings, youtube_matrix)
if (sum(rownames(scorings)==as.character(scorings$row_nr))==nrow(scorings)) print("All is fine")
rm(youtube_matrix, youtube_vector)

scorings[scorings$occurance>0,"occurance"] <- 1
temp <- scorings %>% 
  group_by(category) %>% 
  filter(occurance==1) %>% 
  summarise(occurance=1, weight_new=weighted.mean(weight, w=nchar(weight))) %>% 
  right_join(scorings) %>%
  filter(occurance==1) %>%
  mutate(ngram="youtube.com/watch?")

scorings$ngram[scorings$row_nr %in% temp$row_nr] <- temp$ngram
scorings$weight[scorings$row_nr %in% temp$row_nr] <- temp$weight_new
scorings <- select(scorings, -row_nr, -occurance)
rm(temp)

## Top-level .com domain
temp <- as.data.frame(grepl("\\. com", scorings[,1]))
temp$row_nr2 <- c(1:1000)
colnames(temp)[1] <- paste0("Match_web")


temp %>% summarise_each(funs = c("sum"))
## Only one occurance of ". com"
rm(temp)
scorings$ngram[grepl("\\. com", scorings[,1])] <- "\\.com"

## "http:" protocol link start
http_vector <- c(":/ /", "http", "/ www")

http_matrix <- as.data.frame(matrix(1:1000, 1000, 1))
for (i in 1:length(http_vector)) {
  temp <- as.data.frame(grepl(http_vector[i], scorings[,1]))
  colnames(temp)[1] <- paste0("Match_",i)
  http_matrix <- bind_cols(http_matrix, temp)
  rm(temp)
  
}
http_matrix %>% summarise_each(funs = c("sum"))
http_matrix <- http_matrix %>% 
  mutate(occurance=Match_1+Match_2+Match_3) %>% 
  select(V1, occurance) %>%
  rename(row_nr=V1)
http_matrix$occurance[http_matrix$occurance>0] <- 1
scorings <- bind_cols(scorings, http_matrix)
if (sum(rownames(scorings)==as.character(scorings$row_nr))==nrow(scorings)) print("All is fine")
rm(http_matrix, http_vector)


temp <- scorings %>% 
  group_by(category) %>% 
  filter(occurance==1) %>% 
  summarise(occurance=1, weight_new=weighted.mean(weight, w=nchar(weight))) %>% 
  right_join(scorings) %>%
  filter(occurance==1) %>%
  mutate(ngram="http://www.")

scorings$ngram[scorings$row_nr %in% temp$row_nr] <- temp$ngram
scorings$weight[scorings$row_nr %in% temp$row_nr] <- temp$weight_new
scorings <- select(scorings, -row_nr, -occurance)
rm(temp)

## Variations of "sigh" and "*"
sigh_matrix <- scorings %>% 
  filter(grepl("sigh", scorings[,1])) %>%
  group_by(category) %>%   
  summarise(ngram="sigh ", weight_new=weighted.mean(weight, w=nchar(weight))) 

scorings$ngram[grepl("sigh", scorings[,1])] <- "sigh"
for (i in 1:nrow(sigh_matrix)) {
  scorings$weight[scorings$ngram=="sigh" & scorings$category==sigh_matrix$category[i]] <- sigh_matrix$weight_new[i]
}

rm(sigh_matrix, i)

## ": " emoticons, unnecessary whitespaces
scorings$ngram <- gsub(": ", ":", scorings$ngram)
scorings$ngram <- gsub("@_ @", "@_@", scorings$ngram)
scorings$ngram <- gsub("> . >", ">.>", scorings$ngram)
scorings$ngram <- gsub("< / 3", "</3", scorings$ngram)
scorings$ngram <- gsub("< /", "</", scorings$ngram)
scorings$ngram <- gsub("^ -", "^-", scorings$ngram, fixed = TRUE)
scorings$ngram <- gsub("- ^", "-^", scorings$ngram, fixed = TRUE)
scorings$ngram <- gsub("t . t", "t.t", scorings$ngram, fixed = TRUE)
scorings$ngram <- gsub(" .", ".", scorings$ngram, fixed = TRUE)

### Escaping single special characters
scorings$ngram <- gsub("*", "\\*", scorings$ngram, fixed = TRUE)
scorings$ngram <- gsub(")", "\\)", scorings$ngram, fixed = TRUE)
scorings$ngram <- gsub("(", "\\(", scorings$ngram, fixed = TRUE)
scorings$ngram <- gsub("^ ^", "^^", scorings$ngram, fixed = TRUE)
scorings$ngram <- gsub("? ?", "??", scorings$ngram, fixed = TRUE)
scorings$ngram <- gsub("? ?", "??", scorings$ngram, fixed = TRUE)

scorings$ngram <- gsub("! !", "!!", scorings$ngram, fixed = TRUE)
scorings$ngram <- gsub("! !", "!!", scorings$ngram, fixed = TRUE)
scorings$ngram <- gsub(" !", "!", scorings$ngram, fixed = TRUE)
scorings$ngram <- gsub(" ? !", "?!", scorings$ngram, fixed = TRUE)
scorings$ngram <- gsub("?", "\\?", scorings$ngram, fixed = TRUE)
scorings$ngram <- gsub("!", "\\!", scorings$ngram, fixed = TRUE)

### Escaping parts of words
scorings$ngram[scorings$ngram=="ur"] <- " ur "
scorings$ngram[scorings$ngram=="im "] <- " im "
scorings$ngram[scorings$ngram=="im"] <- " im "

### Adding whitespaces to end of strings and removing accidentally created more than single whitespaces
scorings$ngram <- paste0(scorings$ngram, " ")
scorings$ngram <- gsub("\\s+", " ", scorings$ngram)

### Remove ending whitespace for exceptions
scorings$ngram[grepl("youtube.com/watch?", scorings$ngram )] <- "youtube.com/watch?"
scorings$ngram[grepl("http://www.", scorings$ngram )] <- "http://www."
scorings$ngram[grepl("\\.com", scorings$ngram)] <- "\\.com"

### Finally checking for duplicates and removing them as they would double weight for same amount of output
scorings <- scorings %>% select(-p) %>% distinct() 
scorings <- scorings %>% arrange(category, ngram, weight) %>% mutate(wordkey=1:nrow(.))

saveRDS(scorings, "scorings_dpp.RDS")
