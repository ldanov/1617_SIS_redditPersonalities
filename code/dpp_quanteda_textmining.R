# Text preprocessing with quanteda library

# Create a valid per author set to create corpus
comment_set <- crawled_comments %>% 
  group_by(author) %>% 
  summarise(comments_concat=paste0(as.character(body_textmining), collapse = " ")) %>%
  mutate(comments_concat=gsub("\\s+", " ", comments_concat), 
         nwords=1+sum(stringr::str_count(comments_concat, " ")))


cr_com_quant_corp <- corpus(x=comment_set$comments_concat)
docnames(cr_com_quant_corp) <- comment_set$author
saveRDS(cr_com_quant_corp, "cr_com_quant_corp.RDS")
rm(comment_set)

### As quanteda::dfm() yielded an error when used with older version
### of package "Matrix" it depends on, please be sure to check that
### package "Matrix" is latest available on CRAN version

# Create a document-frequency matrix from corpus
cr_com_quant_dfm <- dfm(cr_com_quant_corp,
                        tolower = TRUE,
                        removePunct = TRUE,
                        removeNumbers = TRUE,
                        removeSeparators = TRUE,
                        remove=stopwords("english"),
                        stem = TRUE,
                        verbose = TRUE); beep(2)
# Creating a dfm from a corpus ...
# ... lowercasing
# ... tokenizing
# ... found 4,482 documents, 791,153 features
# ... removed 174 features, from 174 supplied (glob) feature types
# ... stemming features (English), trimmed 132667 feature variants
# ... created a 4,482 x 658,312 sparse dfm
# ... complete. 
# Elapsed time: 67.1 seconds.
# > cr_com_quant_dfm
# Document-feature matrix of: 4,482 documents, 658,312 features (99.6% sparse).
saveRDS(cr_com_quant_dfm, "cr_com_quant_dfm.RDS")
WC_dfm_top1000 <- textplot_wordcloud(cr_com_quant_dfm, 
                                     max.words = 1000, 
                                     colors = brewer.pal(12, "Paired"), 
                                     scale = c(8, .5))
### Trimming the dfm to words found in minimum 1% of the authors
# cr_com_quant_dfm_trimmed1 <- dfm_trim(cr_com_quant_dfm, min_count=ceiling(4482*0.01))
# Document-feature matrix of: 4,482 documents, 27,125 features (91% sparse).
### Matrix is still too large, second approach will set n to 2000
saveRDS(cr_com_quant_dfm_trimmed1, "cr_com_quant_dfm_trimmed1.RDS")

### Trimming to top 2000 words
top_2000_features <- topfeatures(cr_com_quant_dfm, n = 2000, decreasing = TRUE)
cr_com_quant_dfm_trimmed2 <- dfm_select(cr_com_quant_dfm, 
                                        features=names(top_2000_features),
                                        selection="keep",
                                        valuetype="fixed")

saveRDS(cr_com_quant_dfm_trimmed2, "cr_com_quant_dfm_trimmed2.RDS")

# > format(object.size(cr_com_quant_corp),"MB")
# [1] "531.7 Mb"
# > format(object.size(cr_com_quant_dfm),"MB")
# [1] "182.9 Mb"
# > format(object.size(cr_com_quant_dfm_trimmed1),"MB")
# [1] "127.1 Mb"
# > format(object.size(cr_com_quant_dfm_trimmed2),"MB")
# [1] "63.2 Mb"

df_dfm <- as.data.frame(cr_com_quant_dfm_trimmed2) 
df_dfm$reddit_author <- row.names(df_dfm)
df_dfm <- df_dfm %>% left_join(openvocab_perauthor_n_words[,c("author", "nwords")], 
                                       by=c("reddit_author"="author")) %>% 
  gather(word, count, -reddit_author, -nwords) %>%
  mutate(count_adj=count/nwords)

df_dfm_all <- df_dfm %>% 
  select(-count) %>% 
  spread(key=word, value=count_adj, fill = 0)
