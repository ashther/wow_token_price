
library(dplyr)
library(timetk)
library(tidymodels)
library(RSQLite)

# Sys.setlocale("LC_TIME", "C")

# global -----------------------------------------------------------------


last_nth <- function(df, col, n) {
  idx <- nrow(df) - n + 1
  if (idx <= 0) 
    return(NA_integer_)
  res <- df[[col]][idx]
  ifelse(purrr::is_empty(res), NA_integer_, res)
}

# param_best <- readRDS('config/param_best.rds')
# work_flow <- readRDS('config/work_flow.rds')


# data -------------------------------------------------------------------


con <- dbConnect(SQLite(), 'db/wow_token_price.sqlite')
data_raw <- dbReadTable(con, 'wow_token_price') %>% 
  as_tibble() %>% 
  mutate(time = as.POSIXct(time, tz = 'Asia/Shanghai')) %>% 
  select(-timestamp)
dbDisconnect(con)

for (i in seq_len(72)) {
  data_raw[[sprintf('lag_%s', i)]] <- lag(data_raw$price, n = i)
}
data_raw <- drop_na(data_raw)

# train ------------------------------------------------------------------

data_train <- filter(data_raw, time >= Sys.Date() - 30 & time < max(time) - 6 * 3600)
folds <- vfold_cv(data_train, strata = 'price')

rec <- recipe(price ~ ., data = data_train) %>% 
  step_timeseries_signature(time) %>% 
  step_rm(time) %>% 
  step_rm(
    contains('iso'), contains('second'), contains('minute'), contains('xts')
  ) %>% 
  step_interact(~ time_wday.lbl * time_hour) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_nzv(all_predictors()) %>% 
  step_corr(all_predictors()) %>% 
  step_lincomb(all_predictors()) %>% 
  step_normalize(all_numeric(), -all_outcomes())

data_prep <- prep(rec) %>% juice()

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

annotation <- collect_predictions(res) %>% 
  mae(price, .pred) %>% 
  pull(.estimate) %>% 
  round(0)

stdev <- collect_predictions(res) %>% 
  summarise(stdev = sd(price - .pred)) %>% 
  pull(stdev)

p1 <- collect_predictions(res) %>% 
  mutate(err = abs(price - .pred)) %>% 
  ggplot(aes(price, .pred)) + 
  geom_point(alpha = 0.05, color = 'purple') + 
  geom_abline(slope = 1, color = 'grey', lty = 2) + 
  geom_text(label = annotation, x = -Inf, y = Inf, 
            hjust = -1, vjust = 1, color = 'white') + 
  coord_equal() + 
  awtools::a_dark_theme()

param_best <- select_best(res)


# train new model with data before 6 hours ago
model_fit <- work_flow %>% 
  finalize_workflow(param_best) %>% 
  fit(data_train)

p3 <- pull_workflow_fit(model_fit) %>% 
  vip::vi() %>% 
  mutate(importance = Importance / max(Importance) * 100) %>% 
  head(6) %>% 
  ggplot(aes(reorder(Variable, importance), importance)) + 
  geom_col(fill = '#785d37') + 
  coord_flip() + 
  awtools::a_dark_theme() +
  labs(x = NULL)


# predict ----------------------------------------------------------------

# predict last 6 hours for comparing, and next 6 hours for explore
p2 <- filter(data_raw, time >= Sys.Date() - 2) %>% {

  real <- filter(., time >= max(time) - 6 * 3600) %>% 
    mutate(type = 'obs') %>% 
    select(time, price, type)
  temp <- filter(., time < max(time) - 6 * 3600) %>%
    mutate(type = 'history')
  steps <- nrow(.) - nrow(temp) + 6 * 3
  
  for (i in seq_len(steps)) {
    to_predict <- tibble(
      time = last(temp$time) + 1200, 
      price = NA_integer_
    )
    for (k in 1:72) {
      to_predict[[sprintf('lag_%s', k)]] <- last_nth(temp, 'price', k)
    }
    to_predict <- mutate(to_predict, across(contains('lag'), as.integer))
    to_predict$price <- as.integer(predict(model_fit, to_predict)$.pred[1])
    
    temp <- bind_rows(temp, mutate(to_predict, type = 'pred'))
  }
  
  bind_rows(real, select(temp, time, price, type))
  
} %>% 
  mutate(
    lo_80 = price - 1.28*stdev, hi_80 = price + 1.28*stdev, 
    lo_95 = price - 1.96*stdev, hi_95 = price + 1.96*stdev, 
  ) %>% 
  ggplot(aes(time, price)) + 
  geom_line(aes(color = type), size = 1.3) + 
  geom_ribbon(
    aes(ymin = lo_80, ymax = hi_80), 
    data = . %>% filter(type == 'pred'), 
    fill = '#596DD5', alpha = 0.35
  ) + 
  geom_ribbon(
    aes(ymin = lo_95, ymax = hi_95), 
    data = . %>% filter(type == 'pred'), 
    fill = '#596DD5', alpha = 0.1
  ) + 
  scale_y_continuous(labels = scales::comma) + 
  awtools::a_dark_theme()

library(patchwork)

p <- ((p1 / p3) | p2) + plot_layout(widths = c(1, 2))

saveRDS(p2$data, 'db/pred.rds')
ggsave('wow_token_price.png', p, width = 10, height = 6.18)
