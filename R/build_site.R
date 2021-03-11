

### Given the problems encountered with lapply in xaringan files generation, avoid lapply and go for a loop
## render post
for(f in   list.files('_posts/', recursive = TRUE, pattern = '.Rmd', 
                      full.names = TRUE))
  rmarkdown::render(f)
## render presentation
pres_list <- c("SOTR_ShinyApps_PYH.Rmd")
for(f in   pres_list)
  rmarkdown::render(file.path("_presentation",f))



rmarkdown::render_site(encoding = 'UTF-8')

dir.create("_site/presentation")
system('cp -R _presentation/* _site/presentation/.')
