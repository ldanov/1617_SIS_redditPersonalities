
set.seed(10203)

for (trait in unique(scorings$category)) {
  
  temp_parameter_df <- crossing(word=unique(df_dfm$word)) %>%
    mutate(trait= trait, beta=NA, pvalue=NA)
  
  temp_predicted_score_df <- author_personality_score %>% 
    ungroup() %>%
    filter(category==trait) %>%
    select(-score2) %>% 
    arrange(author) %>%
    rename(starting_value=score1) %>%
    as.data.frame(.)
  
  word_arrange <- sample.int(length(temp_parameter_df$word))
  
  for (i in word_arrange) {
    tempx <- df_dfm %>% 
      filter(word==as.data.frame(temp_parameter_df)[i, "word"]) %>%
      arrange(reddit_author) %>% 
      select(count_adj) %>%
      as.matrix(.)
    tempy <- temp_predicted_score_df %>% 
      filter(category==trait) %>%
      arrange(author) %>%
      select(ncol(temp_predicted_score_df)) %>%
      as.matrix(.)
    
    ### glmnet does not take a vector as predictor
    # lambda_choice <- cv.glmnet(tempx, tempy, type.measure="deviance")$lambda.min
    # glmnet(x=tempx,
    #      y=tempy,
    #      family = "gaussian",
    #      alpha = 0,
    #      lambda = lambda_choice)
    
    temp_pred <- lm(tempy~ tempx -1)
    
    ### beta estimate and p-value
    temp_parameter_df[i, c("beta", "pvalue")] <- summary(temp_pred)$coefficients[c(1,4)]
    
    ### fitted values
    temp_predicted_score_df[,ncol(temp_predicted_score_df)+1] <- temp_pred$fitted.values
    colnames(temp_predicted_score_df)[ncol(temp_predicted_score_df)] <- paste0("pred_word_",i)
    ### 
    rm(tempx, tempy, temp_pred)
  }
  temp_predicted_score_df$last_iteration <- temp_predicted_score_df[,ncol(temp_predicted_score_df)]
  assign(paste0("pred_score_df_", trait), temp_predicted_score_df)
  assign(paste0("parameter_df_", trait), temp_parameter_df)
}
