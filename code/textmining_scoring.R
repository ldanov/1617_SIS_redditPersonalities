### Unfortunately there is no way viable way to remove quoted parent comments

openvocab_perauthor_n_words <- crawled_comments %>%  
  group_by(author) %>% 
  summarise(comments_concat=paste0(as.character(body), collapse = " "), 
            nwords=1+sum(stringr::str_count(comments_concat, " "))) %>%
  mutate(comments_concat=tolower(comments_concat))

for (i in scorings$wordkey) {
  temp <- openvocab_perauthor_n_words %>% select(author, comments_concat) %>%
    mutate_("stringr::str_count(comments_concat, .GlobalEnv$scorings[.GlobalEnv$i,1])")
  colnames(temp)[3] <- paste0("Word_",i)
  openvocab_perauthor_n_words <- left_join(openvocab_perauthor_n_words, temp, by = c("author", "comments_concat"))
  print(paste0("Word ", i, " finished at ", Sys.time()))
  rm(temp)
}; saveRDS(openvocab_perauthor_n_words, "openvocab_perauthor_n_words.RDS"); beep(4)

openvocab_stats <- openvocab_perauthor_n_words %>% 
  select(starts_with("Word_")) %>% 
  summarise_each(funs(sum)) %>% 
  t() %>% 
  as.data.frame() %>% 
  transmute(wordkey=as.integer(gsub("Word_", "", rownames(.))), n_occurances=V1) %>%
  left_join(scorings)


### Assessing ngram distibutions
n_word_distribution <- openvocab_stats %>% plot_ly(y=~n_occurances, x=~category, type = "box") %>%
  layout(boxmode = "group", yaxis=list(type="log"), title="Frequency distribution of words")


### Look at different versions of most common found ngrams, do they make sense?
openvocab_stats %>% filter(n_occurances>quantile(openvocab_stats$n_occurances, .99)) %>% arrange(n_occurances)
openvocab_stats %>% filter(n_occurances>quantile(openvocab_stats$n_occurances, .975)) %>% arrange(n_occurances)
openvocab_stats %>% filter(n_occurances>2000000) %>% arrange(n_occurances)
openvocab_stats %>% filter(n_occurances<5) %>% arrange(n_occurances)

### The top 1% words are excluded from scoring
openvocab_stats$weight[openvocab_stats$n_occurances>quantile(openvocab_stats$n_occurances, .99)] <- 0

### Calculate per author scores first code piece is wrong
# author_personality_score <- openvocab_perauthor_n_words %>% 
#   select(-comments_concat) %>% 
#   gather(Word, n_occurances, -author, -nwords) %>%
#   mutate(wordkey=as.integer(gsub(pattern="Word_", replacement="", x=Word))) %>%
#   select(-Word) %>%
#   left_join(openvocab_stats[,c("wordkey", "weight", "category")]) %>% 
#   group_by(author, category) %>%
#   summarise(score1=weighted.mean((n_occurances/nwords), w = weight),
#             score2=weighted.mean(n_occurances, w = weight))
author_personality_score <- openvocab_perauthor_n_words %>% 
  select(-comments_concat) %>% 
  gather(Word, n_occurances, -author, -nwords) %>%
  mutate(wordkey=as.integer(gsub(pattern="Word_", replacement="", x=Word))) %>%
  select(-Word) %>%
  left_join(openvocab_stats[,c("wordkey", "weight", "category")]) %>% 
  group_by(author, category) %>%
  summarise(score1=weighted.mean(weight, w = n_occurances),
            score2=sum(weight*(n_occurances/nwords))) %>%
  tidyr::replace_na(list(score1=0, score2=0))

saveRDS(author_personality_score, "author_personality_score.RDS")
author_personality_score %>% group_by(category) %>% summarise(min(score1), max(score1),
                                                              min(score2), max(score2))

### Results
per_score1 <- author_personality_score %>% plot_ly(y=~score1, x=~category, type = "box", boxpoints ="all", name = ~"avg per ngram") %>%
  layout(boxmode = "group", yaxis=list(type="linear")) 
per_score2 <- author_personality_score %>% plot_ly(y=~score2, x=~category, type = "box", boxpoints ="all", name = ~"avg per word") %>%
  layout(boxmode = "group", yaxis=list(type="linear"))



