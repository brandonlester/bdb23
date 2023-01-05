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

#df_2model <- readr::read_rds(file.path(data_folder, "df_2model.rds"))

df_block_2model <- readr::read_rds(file.path(data_folder, "df_block_2model.rds"))
df_rush_2model <- readr::read_rds(file.path(data_folder, "df_rush_2model.rds"))

# Summarise data ----------------------------------------------------------

skim_block <- skimr::skim(df_block_2model)
skim_rush <- skimr::skim(df_rush_2model)

feature_names <- c(
  #"gameId", 
  #"playId",
  #"nflId",
  #"frameId",
  #"time", 
  #"jerseyNumber",
  #"team",
  #"playDirection",
  #"x", #TODO???
  #"y", #TODO???
  #"s", #TODO???
  #"a", #TODO???
  #"dis", #TODO???
  #"o", #TODO???
  #"dir", #TODO???
  #"event",
  #"start_frame",
  #"end_frame",
  #"playerId",
  #"possessionTeam",
  #"side_of_ball",
  #"pff_nflIdBlockedPlayer",
  #"pressure_allowed",
  #"pressure_delivered",
  #"pff_positionLinedUp", #TODO???
  #"pff_role",
  #"near_oppo_pid",
  #"near_oppo_nflId",
  "near_oppo_dist",
  #"other_near_oppo_nflId",
  #"x_QB",
  #"y_QB",
  "dist_to_qb",
  "most_open_rec",
  "num_blockers",
  "num_rushers",
  "qb_to_sideline",
  "frames_from_start",
  "offenseFormation",
  "off_personnel",
  "def_personnel",
  "dropBackCategory",
  "coverage",
  "quarter",
  "down",
  "yardsToGo",
  "yards_to_endzone",
  "defendersInBox",
  "pff_playAction",
  "pocket_size"
)





# Split data --------------------------------------------------------------

model_list <- list(
  block = list(
    df_all = df_block_2model,
    target = "pressure_allowed"
  ),
  rush = list(
    df_all = df_rush_2model,
    target = "pressure_delivered"
  )
)
rm(df_block_2model)
rm(df_rush_2model)


create_formula <- function(target) as.formula(paste(paste0(target, " ~ "), paste(feature_names, collapse= "+")))

model_list$block$formula <- create_formula(model_list$block$target)
model_list$rush$formula <- create_formula(model_list$rush$target)



split_data <- function(df, pct, target) {
  require(rsample)
  ds <- initial_split(df, prop = pct, strata = target)
  return(list(trn = training(ds), tst = testing(ds)))
}

ds_block <- split_data(model_list$block$df_all, 0.75, model_list$block$target)
model_list$block$df_train <- ds_block[["trn"]]
model_list$block$df_test <- ds_block[["tst"]]
rm(ds_block)

ds_rush <- split_data(model_list$rush$df_all, 0.75, model_list$rush$target)
model_list$rush$df_train <- ds_rush[["trn"]]
model_list$rush$df_test <- ds_rush[["tst"]]
rm(ds_rush)

# Model training ----------------------------------------------------------

# system.time(
#   model_list$block$rf0 <- ranger::ranger(model_list$block$formula, data = model_list$block$df_train, probability = TRUE)
# ) #elapsed 475.89
# 
# system.time(
#   model_list$rush$rf0 <- ranger::ranger(model_list$rush$formula, data = model_list$rush$df_train, probability = TRUE)
# ) #elapsed 375.97
# 
# readr::write_rds(model_list, "data/model_list.rds")

#TODO - remove df_all since already have trn and test then re-write
model_list <- readr::read_rds("data/model_list.rds")






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

