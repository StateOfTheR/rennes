
lapply(
  list.files('_posts/', recursive = TRUE, pattern = '.Rmd', 
                                  full.names = TRUE), rmarkdown::render)
rmarkdown::render_site(encoding = 'UTF-8')
