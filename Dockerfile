#from rocker/shiny-verse:4.3.3

#run apt-get update && \
#  apt-get install -y --no-install-recommends texlive texlive-latex-recommended texlive-fonts-extra qpdf tidy && \
#  R -e "install.packages(c('amap', 'dbscan', 'cowplot', 'fastICA', 'fpc', 'ggrepel', 'gtable', 'knitr', 'plyr', 'reshape2'))"

# use this to save on Gitlab compute minutes, use above to update
from thomaschln/knowledgegraphs:main

run R -e "install.packages(c('amap', 'dbscan', 'cowplot', 'fastICA', 'fpc', 'ggrepel', 'gtable', 'knitr', 'plyr', 'reshape2'))"

add ./ /opticskxi

run R -e "devtools::install('opticskxi')"
