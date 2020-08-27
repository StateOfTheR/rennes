
lapply(
  list.files('_posts/', recursive = TRUE, pattern = '.Rmd', 
                                  full.names = TRUE), rmarkdown::render)
render_site()
