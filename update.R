
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

param_best <- readRDS('config/param_best.rds')
work_flow <- readRDS('config/work_flow.rds')


# data -------------------------------------------------------------------


con <- dbConnect(SQLite(), 'db/wow_token_price.sqlite')
data_raw <- dbReadTable(con, 'wow_token_price') %>% 
  as_tibble() %>% 
  mutate(time = as.POSIXct(time, tz = 'Asia/Shanghai'))
dbDisconnect(con)

for (i in seq_len(18)) {
  data_raw[[sprintf('lag_%s', i)]] <- lag(data_raw$price, n = i)
}


# train ------------------------------------------------------------------

# train new model with data before 6 hours ago
model_fit <- work_flow %>% 
  finalize_workflow(param_best) %>% 
  fit(filter(data_raw, time >= Sys.Date() - 30 & time < max(time) - 6 * 3600))



# predict ----------------------------------------------------------------

# predict last 6 hours for comparing, and next 6 hours for explore
p <- filter(data_raw, time >= Sys.Date() - 2) %>% {

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
  scale_y_continuous(labels = scales::comma) + 
  ggdark::dark_theme_minimal()

ggsave('wow_token_price.png', p)
