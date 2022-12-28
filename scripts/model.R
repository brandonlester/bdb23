########################################
# train model
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


# Functions ---------------------------------------------------------------

# Prediction wrapper
pred_func <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Read data ---------------------------------------------------------------

df_2model <- readr::read_rds(file.path(data_folder, "df_2model.rds"))



# Summarise data ----------------------------------------------------------

skimr::skim(df_2model)
mean(df_2model$sack)
hist(df_2model$pocket_size)


# Split data --------------------------------------------------------------

#stratify based on sack so proportion of occurences similar in train and test splits
data_split <- rsample::initial_split(df_2model, prop = 0.75, strata = "sack")
df_train <- rsample::training(data_split)
df_test <- rsample::testing(data_split)

#verify stratification
mean(df_train$sack)
mean(df_test$sack)


# Model training ----------------------------------------------------------

#create vector of featrue cols then create formula

# #train base random forest model
rf0 <- ranger::ranger(sack ~ pocket_size, data = df_train, probability = TRUE)

rf0[["data_train"]] <- df_train
rf0[["data_test"]] <- df_test
readr::write_rds(rf0, file.path(output_folder, "rf0.rds"))

#rf0 <- readr::read_rds(file.path(output_folder, "rf0.rds"))

train_brier <- rf0$prediction.error
train_preds <- as_tibble(rf0$predictions) %>% rename(pred_sack = `1`, pred_no_sack = `0`)
train_logloss <- MLmetrics::LogLoss(y_pred = train_preds$sack, y_true = df_train$pred_sack)

# Model testing -----------------------------------------------------------

test_preds <- pred_func(rf0, newdata = df_test) %>% 
  as_tibble(test_preds_obj$predictions) %>% 
  rename(pred_sack = V2, pred_no_sack = V1)

test_logloss <- MLmetrics::LogLoss(y_pred = test_preds$sack, y_true = df_test$pred_sack)



# Model eval --------------------------------------------------------------

trn_w_preds <- df_train %>% 
  mutate(data_split = "train") %>% 
  bind_cols(train_preds)

tst_w_preds <- df_test %>% 
  mutate(data_split = "test") %>% 
  bind_cols(test_preds)

df_results <- bind_rows(trn_w_preds, tst_w_preds)
readr::write_rds(df_results, file.path(output_folder, "df_results.rds"))

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

