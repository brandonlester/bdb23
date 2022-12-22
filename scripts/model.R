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


# Constants ---------------------------------------------------------------

data_folder <- "data"
output_folder <- "output"

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

# #train base random forest model
# rf0 <- ranger::ranger(sack ~ pocket_size, data = df_train, probability = TRUE)
# readr::write_rds(rf0, file.path(output_folder, "rf0.rds"))
rf0 <- readr::read_rds(file.path(output_folder, "rf0.rds"))

train_brier <- rf0$prediction.error
train_preds <- as_tibble(rf0$predictions) %>% rename(sack = `1`, no_sack = `0`)
train_logloss <- MLmetrics::LogLoss(y_pred = train_preds$sack, y_true = df_train$sack)

# Model testing -----------------------------------------------------------

test_preds_obj <- predict(rf0, data = df_test)
test_preds <- as_tibble(test_preds_obj$predictions) %>% rename(sack = V2, no_sack = V1)
test_logloss <- MLmetrics::LogLoss(y_pred = test_preds$sack, y_true = df_test$sack)



# Model eval --------------------------------------------------------------

#shap values
#learning curves
#other model performance


# Apply results -----------------------------------------------------------

#biggest swings in sack prob from frame to frame, start to end of play
#prob from snap to result - players overcoming situation on either off or def side
#probs by defenders in box, num rushers, coverage, formation, down linemen, by features

