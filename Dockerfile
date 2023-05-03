from rocker/verse:4.3.0
run apt-get update && \
  apt-get install -y --no-install-recommends texlive texlive-fonts-extra texlive-latex-recommended qpdf && \
  R -e "install.packages(c('pkgbuild', 'roxygen2', 'testthat'))"
run R -e "install.packages(c('amap', 'dbscan', 'cowplot', 'fastICA', 'fpc', 'ggrepel', 'gtable', 'knitr', 'plyr', 'reshape2'))"
add ./ /opticskxi
run R -e "devtools::install('opticskxi')"
