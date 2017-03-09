df_dfm_all <- df_dfm_all %>% arrange(reddit_author)
tempx <- df_dfm_all %>% select(-reddit_author, -nwords) %>% as.matrix(.)
set.seed(10203)

for (trait in unique(scorings$category)) {
  
  temp_predicted_score_df <- author_personality_score %>% 
    ungroup() %>%
    filter(category==trait) %>%
    select(-score2) %>% 
    arrange(author)
  tempy <- temp_predicted_score_df$score1
  
  ### glmnet does not take a vector as predictor
  lambda_choice <- cv.glmnet(tempx, tempy, type.measure="deviance")$lambda.min
  temp_pred <- glmnet(x=tempx,
                        y=tempy,
                        family = "gaussian",
                        alpha = 0,
                        lambda = lambda_choice)
  
  ###
  saveRDS(file = paste0("temp_pred_", trait), object = temp_pred)
  assign(paste0("temp_pred_", trait), temp_pred)
  
}; beep(2)
objects <- ls(pattern = "temp_pred_")
library(xlsx)

df <- df_temp_pred_arg[1,]
df$trait <- NA

for (i in 1:length(objects)) {
  temp <- eval(parse(text=objects[i]))
  needed <- temp$beta
  summ_needed <- summary(temp$beta)
  tempdf <- data.frame(Origin = rownames(needed)[summ_needed$i],
                       Destination = colnames(needed)[summ_needed$j],
                       Beta = summ_needed$x)
  tempdf$trait <- substr(objects[i],11, nchar(objects[i]))
  assign(value = tempdf, x = paste0("df_",objects[i]))
  df <- bind_rows(df, tempdf)
  write.csv(tempdf, paste0("df_",objects[i], ".csv"), row.names = FALSE, sep = ";")
  
}
df <- df[!is.na(df$trait),]
plot_Df1_word <- df %>% select(-Destination) %>% group_by(trait) %>% top_n(n = 30, wt = abs(Beta))
plot_Df2_word <- df %>% select(-Destination) %>% group_by(trait) %>% top_n(n = -10, wt = Beta)

plot_Df1 <- plot_Df2_word %>% spread(key = trait, value = Beta, fill = NA)
plot_Df2 <- plot_Df2_word %>% spread(key = trait, value = Beta, fill = NA)
bind_cols(plot_Df1, plot_Df2)
