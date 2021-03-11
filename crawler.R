
library(dplyr)
library(purrr)
library(rvest)
library(RSQLite)

wowTokenCrawler <- R6::R6Class(
  'wowTokenCrawler', 
  
  public = list(
    
    con = NULL, 
    
    initialize = function() {
      self$con = dbConnect(SQLite(), 'db/wow_token_price.sqlite')
    }, 
    
    finalize = function() {
      dbDisconnect(self$con)
    }, 
    
    walk = function() {
      data <- private$page_get()
      private$upsert(data)
    }
  ), 
  
  private = list(
    url_base = 'http://www.nfuwow.com/wowtoken/real.html', 
    
    page_get = function() {
      page <- read_html(private$url_base)
      mutate(
        private$page_parse(page), 
        time = as.character(time), 
        timestamp = as.character(Sys.time())
      )
    }, 
    
    page_parse = function(page) {
      html_node(page, '#area-data-cn') %>% 
        html_attr('data-json') %>% 
        jsonlite::fromJSON() %>% 
        as_tibble() %>% 
        mutate(
          time = as.POSIXct(strptime(time, format = '%m-%d %H:%M', tz = 'Asia/Shanghai')), 
          is_ok = time <= last(time), 
          time = if_else(!is_ok, time - 3600*24*365, time)
        ) %>% 
        select(time, price)
    }, 
    
    upsert = function(data) {
      tables <- dbListTables(self$con)
      if ('wow_token_price' %in% tables) {
        old <- dbReadTable(self$con, 'wow_token_price')
        new <- anti_join(data, old, by = 'time')
      } else {
        new <- data
      }
      
      dbWriteTable(self$con, 'wow_token_price', new, append = TRUE, row.names = FALSE)
    }
    
  )
)

crawler <- wowTokenCrawler$new()
crawler$walk()