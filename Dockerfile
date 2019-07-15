from thomaschln/r-publish
add ./ /opticskxi
run R -e "devtools::install('opticskxi', dependencies = TRUE)" && \
  cd opticskxi/vignettes && \
  R -e "Sweave('opticskxi.Rnw');tinytex::latexmk('opticskxi.tex')"
