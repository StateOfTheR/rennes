### Given the problems encountered with lapply in xaringan files generation, avoid laply and go for a loop

for(f in   list.files('_posts/', recursive = TRUE, pattern = '.Rmd', 
                      full.names = TRUE))
  rmarkdown::render(f)

rmarkdown::render_site(encoding = 'UTF-8')
