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
model_list$block$df_all <- NULL
rm(ds_block)

ds_rush <- split_data(model_list$rush$df_all, 0.75, model_list$rush$target)
model_list$rush$df_train <- ds_rush[["trn"]]
model_list$rush$df_test <- ds_rush[["tst"]]
model_list$rush$df_all <- NULL
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


model_list <- readr::read_rds("data/model_list.rds")

obj <- model_list$block
mod_name <- "rf0"





get_predictions <- function(obj, mod_name) {
  mod <- obj[[mod_name]]
  
  trn_preds <- as_tibble(mod$predictions)
  names(trn_preds)[names(trn_preds) == 1] <- paste0("pred_", obj$target)
  names(trn_preds)[names(trn_preds) == 0] <- paste0("pred_no_", obj$target)
  
  tst_preds <- pred_func(mod, newdata = obj$df_test)
  tst_preds <- as_tibble(tst_preds)
  names(tst_preds)[names(tst_preds) == "V2"] <- paste0("pred_", obj$target)
  names(tst_preds)[names(tst_preds) == "V1"] <- paste0("pred_no_", obj$target)
  
  return(list(trn_preds = trn_preds, tst_preds = tst_preds))
}


block_preds <- get_predictions(model_list$block, mod_name = "rf0")
rush_preds <- get_predictions(model_list$rush, mod_name = "rf0")

model_list$block$preds_trn <- block_preds$trn_preds
model_list$block$preds_tst <- block_preds$tst_preds

model_list$rush$preds_trn <- rush_preds$trn_preds
model_list$rush$preds_tst <- rush_preds$tst_preds

brier_score <- function(y_pred, y_true) sum((y_pred - y_true)^2)/length(y_pred)

calc_metrics <- function(obj) {
  require(MLmetrics)
  
  pred_target_name <- paste0("pred_", obj$target)
  
  trn_ypred <- obj$preds_trn[[pred_target_name]]
  trn_ytrue <- obj$df_train[[obj$target]]
  tst_ypred <- obj$preds_tst[[pred_target_name]]
  tst_ytrue <- obj$df_test[[obj$target]]
  
  list(
    brier_trn = brier_score(y_pred = trn_ypred, y_true = trn_ytrue),
    logloss_trn = LogLoss(y_pred = trn_ypred, y_true = trn_ytrue),
    
    brier_tst = brier_score(y_pred = tst_ypred, y_true = tst_ytrue),
    logloss_tst = LogLoss(y_pred = tst_ypred, y_true = tst_ytrue)
  )
}

model_list$block$metrics <- calc_metrics(model_list$block)
model_list$rush$metrics <- calc_metrics(model_list$rush)

guess_brier <- 0.25

# Model eval --------------------------------------------------------------

bind_preds <- function(obj) {
  trn <- obj$df_train %>% 
    mutate(data_split = "train") %>% 
    bind_cols(obj$preds_trn)
  
  tst <- obj$df_test %>% 
    mutate(data_split = "test") %>% 
    bind_cols(obj$preds_tst)
  
  bind_rows(trn, tst)
}

model_list$block$df_all <- bind_preds(model_list$block)
model_list$rush$df_all <- bind_preds(model_list$rush)

model_list$block$df_train <- NULL
model_list$block$df_test <- NULL
model_list$block$preds_trn <- NULL
model_list$block$preds_tst <- NULL

model_list$rush$df_train <- NULL
model_list$rush$df_test <- NULL
model_list$rush$preds_trn <- NULL
model_list$rush$preds_tst <- NULL


readr::write_rds(model_list, "data/model_results.rds")


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