#' Gtable OPTICS k-Xi distance profiles
#'
#' Plot OPTICS distance profiles of k-Xi clustering models
#'
#' @param df_kxi Data frame returned by opticskxi_pipeline
#' @param metric Metric to choose best clustering model
#' @param rank   Ranks of models to plot, ordered by decreasing model metric
#' @param ...    Passed to ggplot_kxi_profile
#' @seealso \link{opticskxi_pipeline}
#' @export
gtable_kxi_profiles <- function(df_kxi, metric = 'avg.silwidth', rank = 1:4,
  ...) {
  n <- ceiling(sqrt(length(rank)))
  m_gg <- lapply(rank, .gtable_kxi_profile, df_kxi, metric, ...) %>% matrix(n)
  m_gg[, 1] %<>% lapply('+', labs(x = NULL))
  m_gg[2, ] %<>% lapply('+', labs(y = NULL))
  
  grDevices::pdf(NULL, 1, 1)
  on.exit(grDevices::dev.off())
  lapply(m_gg, ggplotGrob) %>% matrix(n, byrow = TRUE) %>%
    gtable::gtable_matrix('x', ., grid::unit(c(1, 1), 'null'),
      grid::unit(c(1, 1), 'null'))
}

.gtable_kxi_profile <- function(i, df_kxi, metric, ...) {
  df_kxi %<>% get_best_kxi(metric, i)
  ggplot_optics(df_kxi$optics, df_kxi$clusters, ...) +
    labs(title = kxi_model_name(df_kxi)) +
    theme(title = element_text(size = 10), legend.position = 'none')
}

kxi_model_name <- function(i) {
  lab <- paste(c('Xi', 'Pts'), c(i$n_xi, i$pts), sep = ':', collapse = ' ')
  i$dim_red <- if (i$dim_red != 'identity') paste(i$dim_red, i$n_dimred_comp)
  paste(i$dim_red, i$dist, lab, sep = ' ')
}

ggplot_kxi_profile <- function(kxi_obj, ...) {
}

#' Ggplot OPTICS k-Xi metrics
#'
#' Plot metrics of a kxi_pipeline output
#'
#' @param df_kxi Data frame returned by opticskxi_pipeline
#' @param metric Vector of metrics to display from the df_kxi object
#' @param n      Number of best models for the first metric to display
#' @return ggplot
#' @seealso \link{opticskxi_pipeline}
#' @export
ggplot_kxi_metrics <- function(df_kxi, metric = c('avg.silwidth', 'bw.ratio'),
  n = 8) {

  m_metrics <- as.matrix(as.data.frame(df_kxi$metrics))[metric, ]
  labels <- rownames(m_metrics) %>% sapply(switch, dunn = 'Dunn index',
      avg.silwidth = 'Avg. silhouette width', pearsongamma = 'Pearson gamma',
      bw.ratio = 'Between-within ratio', ch = 'CH')
  dimnames(m_metrics) <- list(labels, apply(df_kxi, 1, kxi_model_name))
  df_kxi <- m_metrics %>% `[`(, utils::tail(order(.[1, ]), n)) %>%
    reshape2::melt()

  ggplot(df_kxi, aes(value, Var2)) + geom_point() +
    facet_wrap(~ Var1, scales = 'free_x') +
    labs(x = 'Metrics values', y = 'Clustering models') + theme_bw() +
    theme(strip.background = element_blank(),
      panel.spacing.x = grid::unit(3, 'mm'),
      panel.grid.minor = element_blank())
}

#' Ggplot optics
#'
#' Plot OPTICS reachability plot.
#'
#' @param optics_obj   dbscan::optics object
#' @param groups       Optional vector defining groups of OPTICS observations
#' @param colors       If groups specified, vector of colors for each group
#' @param segment_size Size for geom_segment
#' @return ggplot
#' @seealso \link{opticskxi}
#' @examples
#' data('multishapes')
#' optics_obj <- dbscan::optics(multishapes[1:2])
#' ggplot_optics(optics_obj)
#' ggplot_optics(optics_obj,
#'   groups = opticskxi(optics_obj, n_xi = 5, pts = 30))
#' @export
ggplot_optics <- function(optics_obj, groups = NULL,
  colors = if (!is.null(groups)) nice_palette(groups),
  segment_size = 300 / nrow(df_optics)) {

  df_optics <- get_optics_df(optics_obj)
  if (!is.null(groups)) {
    groups %<>% factor
    groups[groups == 0] <- NA
    df_optics$Clusters <- groups[df_optics$id] %>% as.character
    mapping <- aes(x = .data$optics_id, y = .data$reachdist,
                   xend = .data$optics_id, color = .data$Clusters)
  } else {
    mapping <- aes(x = .data$optics_id, y = .data$reachdist,
                   xend = .data$optics_id)
  }
  df_optics$reachdist[1] <- df_optics$coredist[1]

  ggplot(df_optics, mapping) +
    geom_segment(yend = 0, linewidth = segment_size) +
    labs(x = 'Observations', y = 'Distance') +
    coord_cartesian(xlim = c(0, nrow(df_optics)), expand = FALSE) +
    theme_bw() + theme(panel.grid = element_blank(),
      strip.text = element_blank(), strip.background = element_blank()) +
    if (!is.null(groups)) {
      list(guides(color = guide_legend(ncol = 1 + (length(unique(groups)) > 5),
            override.aes = list(linewidth = 3))),
        scale_color_manual(values = colors, na.value = grDevices::grey(.5)))
    }
}
