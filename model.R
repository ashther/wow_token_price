
library(dplyr)
library(timetk)
library(tidymodels)
library(RSQLite)

# Sys.setlocale("LC_TIME", "C")

# data -------------------------------------------------------------------

con <- dbConnect(SQLite(), 'db/wow_token_price.sqlite')
data_raw <- dbReadTable(con, 'wow_token_price') %>% 
  as_tibble() %>% 
  mutate(time = as.POSIXct(time, tz = 'Asia/Shanghai'))
dbDisconnect(con)

for (i in seq_len(18)) {
  data_raw[[sprintf('lag_%s', i)]] <- lag(data_raw$price, n = i)
}

data_raw <- drop_na(data_raw) %>% 
  select(-timestamp)

data_split <- initial_split(data_raw, strata = 'price')
data_train <- training(data_split)
data_test <- testing(data_split)

folds <- vfold_cv(data_train, strata = 'price')

# recipe -----------------------------------------------------------------

rec <- recipe(price ~ ., data = data_train) %>% 
  step_timeseries_signature(time) %>% 
  step_rm(time) %>% 
  step_rm(
    contains('iso'), contains('second'), contains('minute'), contains('xts')
  ) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_nzv(all_predictors()) %>% 
  step_corr(all_predictors()) %>% 
  step_lincomb(all_predictors()) %>% 
  step_normalize(all_numeric(), -all_outcomes())

data_prep <- prep(rec) %>% juice()

# model ------------------------------------------------------------------

model_glm <- linear_reg(penalty = 0.1) %>% 
  set_mode('regression') %>% 
  set_engine('glmnet')

model_rf <- rand_forest() %>% 
  set_mode('regression') %>% 
  set_engine('ranger')


# workflow ---------------------------------------------------------------

wf_glm <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(model_glm)

wf_rf <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(model_rf)


# compare ----------------------------------------------------------------

res_glm <- wf_glm %>% 
  fit_resamples(
    resamples = folds, 
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
    # metrics = metric_set(c(rmse))
  )
show_best(res_glm)

res_rf <- wf_rf %>% 
  fit_resamples(
    resamples = folds, 
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
    # metrics = metric_set(c(rmse))
  )
show_best(res_rf)


# tune -------------------------------------------------------------------

model <- rand_forest(mtry = tune(), trees = 500, min_n = tune()) %>% 
  set_mode('regression') %>% 
  set_engine('ranger', importance = 'impurity', keep.inbag = TRUE)

work_flow <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(model)

res <- work_flow %>% 
  tune_grid(
    resamples = folds, 
    grid = grid_regular(finalize(mtry(), data_prep), min_n(), levels = 5), 
    control = control_grid(verbose = TRUE, save_pred = TRUE)
    # metric = metric_set(rmse)
  )

param_best <- select_best(res)

model_fit <- work_flow %>% 
  finalize_workflow(param_best) %>% 
  fit(data_train)


# predict ----------------------------------------------------------------

predict(model_fit, data_test) %>% 
  bind_cols(select(data_test, price)) %T>% 
  mae(price, .pred) %>% 
  ggplot(aes(price, .pred)) + 
  geom_point(alpha = 0.3) + 
  geom_abline(slope = 1, lty = 2, color = 'red') + 
  coord_equal() + 
  ggdark::dark_theme_minimal()

last_nth <- function(df, col, n) {
  idx <- nrow(df) - n + 1
  if (idx <= 0) 
    return(NA_integer_)
  res <- df[[col]][idx]
  ifelse(purrr::is_empty(res), NA_integer_, res)
}

model_fit <- work_flow %>% 
  finalize_workflow(param_best) %>% 
  fit(filter(data_raw, time >= Sys.Date() - 7 & time < max(time) - 6 * 3600))

filter(data_raw, time >= Sys.Date() - 1) %>% {
  # temp <- mutate(., type = 'obs')
  # steps <- 6 * 3
  temp <- filter(., time < max(time) - 6 * 3600) %>%
    mutate(type = 'obs')
  steps <- nrow(.) - nrow(temp) + 6 * 3
  
  for (i in seq_len(steps)) {
    to_predict <- tibble(
      time = last(temp$time) + 1200, 
      price = NA_integer_
    )
    for (k in 1:18) {
      to_predict[[sprintf('lag_%s', k)]] <- last_nth(temp, 'price', k)
    }
    to_predict <- mutate(to_predict, across(contains('lag'), as.integer))
    to_predict$price <- as.integer(predict(model_fit, to_predict)$.pred[1])
    
    temp <- bind_rows(temp, mutate(to_predict, type = 'pred'))
  }
  
  transmute(., time, price, type = 'obs') %>% 
    bind_rows(select(temp, time, price, type))
} %>% 
  ggplot(aes(time, price)) + 
  geom_line(aes(color = type), size = 1.3) + 
  ggdark::dark_theme_minimal()
