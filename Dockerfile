from rocker/verse:4.2.3
run apt-get update && \
  apt-get install -y --no-install-recommends texlive texlive-fonts-extra qpdf && \
  R -e "install.packages(c('pkgbuild', 'roxygen2', 'testthat'))"
run R -e "install.packages(c('amap', 'dbscan', 'cowplot', 'fastICA', 'fpc', 'ggrepel', 'gtable', 'knitr', 'plyr', 'reshape2'))"
add ./ /opticskxi
run R -e "devtools::install('opticskxi')"
