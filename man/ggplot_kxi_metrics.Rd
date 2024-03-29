% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optics_plots.R
\name{ggplot_kxi_metrics}
\alias{ggplot_kxi_metrics}
\title{Ggplot OPTICS k-Xi metrics}
\usage{
ggplot_kxi_metrics(df_kxi, metric = c("avg.silwidth", "bw.ratio"), n = 8)
}
\arguments{
\item{df_kxi}{Data frame returned by opticskxi_pipeline}

\item{metric}{Vector of metrics to display from the df_kxi object}

\item{n}{Number of best models for the first metric to display}
}
\value{
ggplot
}
\description{
Plot metrics of a kxi_pipeline output
}
\seealso{
\link{opticskxi_pipeline}
}
