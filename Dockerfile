from rocker/shiny-verse:4.4.2

run apt-get update && \
  apt-get install -y --no-install-recommends texlive texlive-latex-recommended texlive-fonts-extra qpdf tidy git

run R -e "install.packages(c('amap', 'dbscan', 'cowplot', 'fastICA', 'fpc', 'ggrepel', 'gtable', 'knitr', 'plyr', 'reshape2'))"

run R -e "install.packages(c('cli', 'R6'))"

run R -e "install.packages('covr')"

add ./ /opticskxi

run R -e "devtools::install('opticskxi', dependencies = TRUE)"
