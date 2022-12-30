########################################
# train model on player level
#
#
########################################


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(skimr)
library(rsample)
library(ranger)
library(MLmetrics)
library(fastshap)


# Constants ---------------------------------------------------------------

data_folder <- "data"
output_folder <- "output"
model_save_name <- "rf0_player.rds"
results_save_name <- "df_results_player.rds"


# Functions ---------------------------------------------------------------

# Prediction wrapper
pred_func <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Read data ---------------------------------------------------------------

df_2model_player <- readr::read_rds(file.path(data_folder, "df_2model_player.rds"))



# Summarise data ----------------------------------------------------------

#skimr::skim(df_2model_player)
mean(df_2model_player$pff_sack)
hist(df_2model_player$dist_to_qb)


# Split data --------------------------------------------------------------


#stratify based on sack so proportion of occurences similar in train and test splits
data_split <- rsample::initial_split(df_2model_player, prop = 0.75, strata = "pff_sack")


df_train <- rsample::training(data_split)
df_test <- rsample::testing(data_split)

#verify stratification
mean(df_train$pff_sack)
mean(df_test$pff_sack)


# Model training ----------------------------------------------------------

#create vector of featrue cols then create formula

# #train base random forest model
system.time(
  rf0 <- ranger::ranger(pff_sack ~ dist_to_qb, data = df_train, probability = TRUE)
)

rf0[["data_train"]] <- df_train
rf0[["data_test"]] <- df_test
readr::write_rds(rf0, file.path(output_folder, model_save_name))

# rf0 <- readr::read_rds(file.path(output_folder, "rf0_player.rds"))
# df_train <- rf0$data_train
# df_test <- rf0$data_test

train_brier <- rf0$prediction.error
train_preds <- as_tibble(rf0$predictions) %>% rename(pred_sack = `1`, pred_no_sack = `0`)
train_logloss <- MLmetrics::LogLoss(y_pred = train_preds$pred_sack, y_true = df_train$pff_sack)

# Model testing -----------------------------------------------------------

test_preds <- pred_func(rf0, newdata = df_test) %>% 
  as_tibble(test_preds_obj$predictions) %>% 
  rename(pred_sack = V2, pred_no_sack = V1)

test_logloss <- MLmetrics::LogLoss(y_pred = test_preds$pred_sack, y_true = df_test$pff_sack)



# Model eval --------------------------------------------------------------

trn_w_preds <- df_train %>% 
  mutate(data_split = "train") %>% 
  bind_cols(train_preds)

tst_w_preds <- df_test %>% 
  mutate(data_split = "test") %>% 
  bind_cols(test_preds)

df_results <- bind_rows(trn_w_preds, tst_w_preds)
readr::write_rds(df_results, file.path(output_folder, results_save_name))

# #shap values
# df_shap <- explain(
#   rf0, 
#   #feature_names = ,
#   X = as.data.frame(df_train["pocket_size"]), 
#   nsim = 2,
#   pred_wrapper = pred_func,
#   #newdata = ,
#   adjust = TRUE
# )
# readr::write_rds(df_shap, file.path(output_folder, "df_shap.rds"))

# Apply results -----------------------------------------------------------

#biggest swings in sack prob from frame to frame, start to end of play
#prob from snap to result - players overcoming situation on either off or def side
#probs by defenders in box, num rushers, coverage, formation, down linemen, by features

