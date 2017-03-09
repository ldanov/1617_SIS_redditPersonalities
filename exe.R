# This is just the main command hub of each script
# For more info on each, look at code comments
# The import data section scripts end with the 
# creation of RDS files, which are read at the beginning
# of subsequent script sections. Thus, importing is only
# necessary to be run once.

# Clear current global environment
rm(list=ls())

## Load libraries
source(file=paste0(getwd(), "/code/loadLib.R"))



# Because the ngrams used for scoring can contain characters
# that we want to avoid in our text mining, we are creating 
# two versions of processed user comments - one for scoring
# and one for text mining/corpora analysis

## Import data

### import_users.R is part of the iterative process to
### create a list of users, which are then crawled with
### the reddit crawler bot (python implementation)

# source(file=paste0(getwd(), "/code/import_users.R"))

### import_comments.R imports all available user comments. 
### Its output is an uniquely named object called 
### "crawled_comments.RDS" that contains all comments and 
### associated metadata before any transformations and 
### preprocessing is applied. In that sense it needs to 
### only be run once, becase the imported data.frame is 
### then saved locally to wd() as "/crawled_comments.RDS"

# source(file=paste0(getwd(), "/code/import_comments.R"))

### DPP on scorings is done immediately after import as
### there is no need for two versions of preprocessing.
### Output of the two scripts is the scorings df and
### "scorings_dpp.RDS" saved in working directory.

# source(file=paste0(getwd(), "/code/import_scorings.R"))
# source(file=paste0(getwd(), "/code/dpp_scorings.R"))

scorings <- readRDS("scorings_dpp.RDS")


## Data preprocessing

### DPP commments has two goals: 
### 1) dpp comments as to capture emoticons and links, which
### are ngrams in the scoring table - mutated body column
### 2) dpp comments as to remove or mutate all punctuations and 
### links as to make them more computationally friendly for text
### mining and dtf matrix creation - body_textmining column

# If comments have already been processed into single file, but
# object crawled_comments does not exist in current environemnt

# source(file=paste0(getwd(), "/code/dpp_comments.R"), echo=TRUE)
crawled_comments <- readRDS("crawled_comments_dpp.rds")

### Size in environment
# > format(object.size(crawled_comments), "MB")
# [1] "4037 Mb"
### Size on disk as .RDS file
# > utils:::format.object_size(file.size("crawled_comments_dpp.rds"), "MB")
# [1] "954.1 Mb"

### Apply scoring as weighted sum of correlations
### Output is a author_personality_score df with
### scores over all of each author's comments
### Formulas used:
### sum each correlation, weighted by occurance,
### 1) across all occuring ngrams ()
### 2) across all words in comment
### where for each author: n_occurances is the sum of 
### occurances of scoring ngram in all comments;
### n_words is the count of all black spaces in all comments +1
### to account for no-whitespace comments (i.e. "trololololol"); 
### and weights is the correlation given from scorings table
source(file=paste0(getwd(), "/code/textmining_scoring.R"))

### Assessing n_gram distribution across categories
n_word_distribution

### Comparison of results of two formulas:
subpl <- subplot(per_score1, per_score2, shareY = TRUE) %>% 
  layout(title="Frequency distribution of authors' scores per trait<br>Same scale",
         legend = list(orientation = 'h'))

# subplot(per_score1, per_score2, shareY = FALSE) %>% 
#   layout(title="Frequency distribution of authors' scores per trait<br>Auto scale",
#          legend = list(orientation = 'h'))

## Text mining, word correlation and iterative scoring
source(file=paste0(getwd(), "/code/dpp_quanteda_textmining.R"))
WC_dfm_top1000
source(file=paste0(getwd(), "/code/textmining_reg_2.R"))