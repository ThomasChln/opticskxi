from thomaschln/r-publish
run R -e "install.packages('pkgbuild')"
add ./ /opticskxi
run R -e "devtools::install('opticskxi', dependencies = TRUE)" && \
  cd opticskxi/vignettes && \
  R -e "Sweave('opticskxi.Rnw');tinytex::latexmk('opticskxi.tex')"
