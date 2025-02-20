from rocker/shiny-verse:4.4.2

run apt-get update && \
  apt-get install -y --no-install-recommends texlive texlive-latex-recommended texlive-fonts-extra qpdf tidy git

# use this to save on Gitlab compute minutes, use above to update
#from thomaschln/knowledgegraphs:main

run R -e "install.packages(c('amap', 'dbscan', 'cowplot', 'fastICA', 'fpc', 'ggrepel', 'gtable', 'knitr', 'plyr', 'reshape2', 'text2vec'))"

add ./ /opticskxi

run R -e "devtools::install('opticskxi')"
