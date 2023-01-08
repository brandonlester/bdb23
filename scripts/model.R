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
library(tidymodels)
library(fastDummies)
#library(xgboost)

# Constants ---------------------------------------------------------------

data_folder <- "data"
output_folder <- "output"


# Functions ---------------------------------------------------------------

# Prediction wrapper
pred_func <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

create_formula <- function(target) as.formula(paste(paste0(target, " ~ "), paste(feature_names, collapse= "+")))


split_data <- function(df, pct, target) {
  require(rsample)
  ds <- initial_split(df, prop = pct, strata = target)
  return(list(trn = training(ds), tst = testing(ds)))
}

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

bind_preds <- function(obj) {
  trn <- obj$df_train %>% 
    mutate(data_split = "train") %>% 
    bind_cols(obj$preds_trn)
  
  tst <- obj$df_test %>% 
    mutate(data_split = "test") %>% 
    bind_cols(obj$preds_tst)
  
  bind_rows(trn, tst)
}

# Read data ---------------------------------------------------------------

#df_2model <- readr::read_rds(file.path(data_folder, "df_2model.rds"))

df_block_2model <- readr::read_rds(file.path(data_folder, "df_block_2model.rds"))
df_rush_2model <- readr::read_rds(file.path(data_folder, "df_rush_2model.rds"))
df_plays <- read_csv(file.path(data_folder, "plays.csv"))

# Summarise data ----------------------------------------------------------


skim_block <- skimr::skim(df_block_2model)
skim_rush <- skimr::skim(df_rush_2model)

feature_names <- c(
  #"pff_positionLinedUp", #???
  "offenseFormation",
  "num_rb",
  "num_te",
  "num_wr",
  "num_dl",
  "num_lb",
  "num_db",
  "dropBackCategory",
  "pff_passCoverageType",
  "near_oppo_dist",
  "dist_to_qb",
  "most_open_rec",
  "num_blockers",
  "num_rushers",
  "qb_to_sideline",
  "frames_from_start",
  "nearest_trench_dist",
  "quarter",
  "down",
  "yardsToGo",
  "yards_to_endzone",
  "defendersInBox",
  "pff_playAction",
  "pocket_size"
)

skim_block_features <- skimr::skim(select(df_block_2model, all_of(feature_names)))
skim_rush_features <- skimr::skim(select(df_rush_2model, all_of(feature_names)))


cols2dummy <- c("offenseFormation", "pff_passCoverageType", "dropBackCategory")



df_block_2model <- df_block_2model %>% 
  mutate(across(starts_with("num_"), as.integer)) %>% 
  filter(!is.na(nearest_trench_dist)) %>% 
  fastDummies::dummy_cols(select_columns = cols2dummy) %>% 
  mutate(pressure_allowed = as.factor(pressure_allowed))
  
df_rush_2model <- df_rush_2model %>% 
  mutate(across(starts_with("num_"), as.integer)) %>% 
  filter(!is.na(nearest_trench_dist)) %>% 
  fastDummies::dummy_cols(select_columns = cols2dummy) %>% 
  mutate(pressure_delivered = as.factor(pressure_delivered))

#update feature_names after dummy encoding
dummy_names <- names(df_block_2model)[str_detect(names(df_block_2model), paste(cols2dummy,collapse="|"))]
dummy_names <- dummy_names[!dummy_names %in% cols2dummy]
feature_names <- feature_names[!feature_names %in% cols2dummy]
feature_names <- c(feature_names, dummy_names)

# tidymodels training -----------------------------------------------------

block_data_split <- rsample::initial_split(df_block_2model, prop = 0.75, strata = pressure_allowed)
block_train <- training(block_data_split)
block_test <- testing(block_data_split)

rush_data_split <- rsample::initial_split(df_rush_2model, prop = 0.75, strata = pressure_delivered)
rush_train <- training(rush_data_split)
rush_test <- testing(rush_data_split)

feature_formula <- paste(feature_names, collapse = "+")
block_formula <- paste("pressure_allowed", feature_formula, sep = " ~ ")
rush_formula <- paste("pressure_delivered", feature_formula, sep = " ~ ")

block_recipe <- recipe(block_train, block_formula)
rush_recipe <- recipe(rush_train, rush_formula)

ranger_spec <- rand_forest(
  # trees = 1000,
  # mtry = tune(),
  # min_n = tune()
) %>%
  set_engine("ranger") %>%
  set_mode("classification")

block_cv <- vfold_cv(block_train, v = 10, strata = pressure_allowed)
rush_cv <- vfold_cv(rush_train, v = 10, strata = pressure_delivered)


# TRAIN MODELS ------------------------------------------------------------


system.time(
  block_results <- tune_grid(
    ranger_spec,
    preprocessor = block_recipe,
    resamples = block_cv,
    #grid = 20,
    metrics = metric_set(yardstick::mn_log_loss),
    control = control_grid(save_pred = TRUE)
  )
)

system.time(
  rush_results <- tune_grid(
    ranger_spec,
    preprocessor = rush_recipe,
    resamples = rush_cv,
    #grid = 20,
    metrics = metric_set(yardstick::mn_log_loss),
    control = control_grid(save_pred = TRUE)
  )
)

system.time(
  readr::write_rds(block_results, file.path(data_folder, "block_tidymodel_results.rds"))
)

system.time(
  readr::write_rds(rush_results, file.path(data_folder, "rush_tidymodel_results.rds"))
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

model_list$block$formula <- create_formula(model_list$block$target)
model_list$rush$formula <- create_formula(model_list$rush$target)

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

# df_train <- model_list$block$df_train
# rmatrix_features <-  df_train %>% select(all_of(feature_names)) %>% as.matrix()
# 
# xgbmatrix_train <- xgboost::xgb.DMatrix(data = rmatrix_features, label = df_train$pressure_allowed)
# 
# bstDMatrix <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")



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

block_preds <- get_predictions(model_list$block, mod_name = "rf0")
rush_preds <- get_predictions(model_list$rush, mod_name = "rf0")

model_list$block$preds_trn <- block_preds$trn_preds
model_list$block$preds_tst <- block_preds$tst_preds

model_list$rush$preds_trn <- rush_preds$trn_preds
model_list$rush$preds_tst <- rush_preds$tst_preds



model_list$block$metrics <- calc_metrics(model_list$block)
model_list$rush$metrics <- calc_metrics(model_list$rush)

guess_brier <- 0.25

# Model eval --------------------------------------------------------------



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