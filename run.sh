#!/usr/lib64/R/bin/Rscript

source('crawler.R')
source('update.R')

Sys.setenv(RSTUDIO_PANDOC='/usr/lib/rstudio-server/bin/pandoc')
rmarkdown::render('index.Rmd', output_dir = 'docs', quiet = FALSE)
system('git add .')
system("git commit -m 'update data'")
system('git push')
