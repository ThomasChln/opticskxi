# prepare the package for release
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: clean devtools_check

doc.pdf:
	R CMD Rd2pdf -o doc.pdf .

build:
	cd ..;\
	R CMD build --no-manual $(PKGSRC)

build-cran: clean
	cd ..;\
	R CMD build $(PKGSRC)

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build-cran
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz

submit: check
	cd ..;\
	mv $(PKGNAME)_$(PKGVERS).tar.gz $(PKGSRC)

roxygenise:
	R -e "roxygen2::roxygenise()"

devtools_test:
	R -e "devtools::test(reporter = testthat::ProgressReporter\$$new(show_praise = FALSE))"

devtools_check:
	R -e "devtools::check()"

vignette:
	cd vignettes;\
	R -e "Sweave('opticskxi.Rnw');tools::texi2pdf('opticskxi.tex')";\
	R -e "Sweave('ensemble_metrics.Rnw');tools::texi2pdf('ensemble_metrics.tex')"

clean:
	$(RM) doc.pdf
	cd vignettes;\
	$(RM) *.pdf *.aux *.bbl *.blg *.out *.tex *.log
