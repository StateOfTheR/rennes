FROM rocker/geospatial:latest
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
 && apt-get install -y pandoc \
    pandoc-citeproc
RUN R -e "install.packages(c('tidyverse','wesanderson','lubridate','rmarkdown', 'stringr', 'ggplot2','tinytex','RefManageR', 'distill', 'bibtex'))"
RUN R -e "install.packages(c('shiny','kableExtra', 'shinydashboard', 'shinythemes','DT', 'plotly'))"
RUN R -e "install.packages(c('ggpubr','gganimate','GGally','magick','gifski'))"
RUN R -e "install.packages(c('rnaturalearth','rnaturalearthdata','rgeos'))"
RUN R -e "install.packages(c('car', 'emmeans'))"
RUN R -e "install.packages('FactoMineR')"
RUN R -e "remotes::install_github('yihui/xaringan')"
RUN R -e "remotes::install_github('gadenbuie/xaringanExtra')"
RUN R -e "remotes::install_github('EvaMaeRey/flipbookr')"
RUN R -e "install.packages('palmerpenguins')"
