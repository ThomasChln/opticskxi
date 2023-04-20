# OPTICS k-Xi

[![CRAN version](http://www.r-pkg.org/badges/version/opticskxi)](https://cran.r-project.org/package=opticskxi)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/opticskxi)](https://cran.r-project.org/package=opticskxi)
[![Build Status](https://travis-ci.org/thomaschln/opticskxi.svg)](https://travis-ci.org/thomaschln/opticskxi)
[![Coverage status](https://codecov.io/gh/thomaschln/opticskxi/branch/master/graph/badge.svg)](https://codecov.io/github/thomaschln/opticskxi)

This R package provides a novel cluster extraction method for the OPTICS algorithm, OPTICS k-Xi, along with ggplot2 visualizations and a framework to compare clustering models with varying parameters using distance-based metrics.

## Summary

Density-based clustering methods are well adapted to the clustering of high-dimensional data and enable the discovery of core groups of various shapes despite large amounts of noise.

The opticskxi R package provides a novel density-based cluster extraction method, OPTICS k-Xi, and a framework to compare k-Xi models using distance-based metrics to investigate datasets with unknown number of clusters. The vignette first introduces density-based algorithms with simulated datasets, then presents and evaluates the k-Xi cluster extraction method. Finally, the models comparison framework is described and experimented on 2 genetic datasets to identify groups and their discriminating features.

The k-Xi algorithm is a novel OPTICS cluster extraction method that specifies directly the number of clusters and does not require fine-tuning of the steepness parameter as the OPTICS Xi method. Combined with a framework that compares models with varying parameters, the OPTICS k-Xi method can identify groups in noisy datasets with unknown number of clusters.

## Installation

Stable CRAN version, in R:

```r
  install.packages('opticskxi')
```

Development version, using the devtools package in R:

```r
  devtools::install_git('https://gitlab.com/thomaschln/opticskxi.git')
```

## Usage

Compute OPTICS profile and k-Xi clustering

```r
  data('multishapes')
  optics_shapes <- dbscan::optics(multishapes[1:2])
  kxi_shapes <- opticskxi(optics_shapes, n_xi = 5, pts = 30)
```

Visualize with ggplot2

```r
  ggplot_optics(optics_shapes)
  ggplot_kxi_profile(kxi_shapes)
```

Compare multiple k-Xi models in dataset with unknown number of clusters and visualize the best models:

* Compute k-Xi models with varying parameters and their distance-based metrics

```r
   data('hla')
   m_hla <- hla[-c(1:2)] %>% scale
   df_params_hla <- expand.grid(n_xi = 3:5, pts = c(20, 30, 40),
     dist = c('manhattan', 'euclidean', 'abscorrelation', 'abspearson'))
   df_kxi_hla <- opticskxi_pipeline(m_hla, df_params_hla)
```

* Visualize the metrics and OPTICS profiles of the models with highest average silhouette width

```r
   ggplot_kxi_metrics(df_kxi_hla, n = 8)
   gtable_kxi_profiles(df_kxi_hla) %>% plot
```

* Extract the second best model and visualize the clusters using PCA dimension reduction

```r
   best_kxi_hla <- get_best_kxi(df_kxi_hla, rank = 2)
   clusters_hla <- best_kxi_hla$clusters
   fortify_pca(m_hla, sup_vars = data.frame(Clusters = clusters_hla)) %>%
     ggpairs('Clusters', ellipses = TRUE, variables = TRUE)
```

See the [vignette](https://cran.r-project.org/web/packages/opticskxi/vignettes/opticskxi.pdf) for results and further details.

## Acknowledgements

This work was inspired by Jérôme Wojcik (Precision for Medicine) and Sviatoslav Voloshynovskiy (University of Geneva).

## License

This package is free and open source software, licensed under GPL-3.

## Citations

This package has 4 citations on [Google scholar](https://scholar.google.com/scholar?oi=bibs&hl=en&cites=16168867266574784239&as_sdt=5) (April 2023)
