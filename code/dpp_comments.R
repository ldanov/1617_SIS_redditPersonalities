# Remove repeated columns
# R does not have a built in column checker. What can be done is transpose
# the data frame and use the unique() function, but the (now) matrix needs
# to be transposed back. Due to size it is not a efficient operation. 
# Insted we will first check whether JSON objects and their html parts are
# identical and if so remove the JSON version

# Very first step is renaming columns starting with underscore
findUnderstr <- substr(colnames(crawled_comments), 1, 1)=="_"
colnames(crawled_comments)[findUnderstr] <- sub(pattern = "_",
                                                replacement = "ed_",
                                                colnames(crawled_comments)[findUnderstr]
)
rm(findUnderstr)

cols_w_JSON <- colnames(crawled_comments)[grepl(colnames(crawled_comments), pattern = "JSON")]
cols_to_match <- gsub("JSON", "", colnames(crawled_comments)[grepl(colnames(crawled_comments), pattern = "JSON")])
toRem <- c()
# Are all JSON columns with JSON suffix removed also present
if (!FALSE %in% (cols_to_match %in% colnames(crawled_comments))) {
  for (i in 1:length(cols_w_JSON)) {
    if (identical(crawled_comments[,cols_w_JSON[i]], crawled_comments[,cols_to_match[i]])) {
      toRem <- c(toRem, cols_w_JSON[i])
    }
  }
}

crawled_comments <- crawled_comments %>% select_(.dots = c(paste0("-", toRem)))
rm(cols_to_match, cols_w_JSON, i, toRem)

# Next remove all columns containing a single variable
# First a soft test to speed up computation based on semi-random sample

toGoThrough <- c()
testSample <- dplyr::sample_n(crawled_comments, 30000)
for (i in 1:ncol(testSample)) {
  if(max(table(testSample[,i]))==nrow(testSample)){
    toGoThrough <- c(toGoThrough, i)
  }
}
rm(testSample)

# Columns that failed the soft test
colnames(crawled_comments)[toGoThrough]

# Now redo test only with failed columns
toRem <- c()
for (i in toGoThrough) {
  if (n_distinct(crawled_comments[,i])==1) {
    toRem <- c(toRem, colnames(crawled_comments)[i])
  }
}

# Columns to remove
toRem
# Columns that do contain more than one value
colnames(crawled_comments)[toGoThrough[!colnames(crawled_comments)[toGoThrough] %in% toRem]]

crawled_comments <- crawled_comments %>% select_(.dots = c(paste0("-", toRem))) %>% select(-V1)
rm(toRem, toGoThrough, i)

# Check for duplicate rows

crawled_comments <- crawled_comments %>% 
  distinct() %>%
  filter(!is.na(body)) %>%
  mutate(author_flair_css_class=gsub("", NA, author_flair_css_class),
         author_flair_text=gsub("", NA, author_flair_text),
         removal_reason=gsub("", NA, removal_reason),
         distinguished=gsub("", NA, distinguished),
         edited=gsub("False", NA, edited),
         over_18=as.logical(toupper(over_18)),
         score_hidden=as.logical(toupper(score_hidden)),
         archived=as.logical(toupper(archived)),
         stickied=as.logical(toupper(stickied)),
         nwords=1+str_count(body, " "),
         nlinks=str_count(body, " ?\\((http)(s?)(://)(.*?)\\)"),
         body_textmining=gsub(" ?\\((http)(s?)(://)(.*?)\\)", " ", body),
         body_textmining=gsub("\\s+", " ", body_textmining))


### Clean up commments for compatibility with scoring grams

## Emoticons and another multiple whitespace removal
crawled_comments$body <- gsub(": ", ":", crawled_comments$body)
crawled_comments$body <- gsub("@_ @", "@_@", crawled_comments$body)
crawled_comments$body <- gsub("> . >", ">.>", crawled_comments$body)
crawled_comments$body <- gsub("< / 3", "</3", crawled_comments$body)
crawled_comments$body <- gsub("< /", "</", crawled_comments$body)
crawled_comments$body <- gsub("^ -", "^-", crawled_comments$body, fixed = TRUE)
crawled_comments$body <- gsub("- ^", "-^", crawled_comments$body, fixed = TRUE)

# Save custom body text data frame
saveRDS(crawled_comments, "crawled_comments_dpp.rds")
beepr::beep(2)
