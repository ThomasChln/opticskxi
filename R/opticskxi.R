
#' OPTICS k-Xi clustering algorithm
#'
#' For each largest distance differences on the OPTICS profile,
#' consecutive observations left and right on the OPTICS profile
#' (i.e. lower and higher OPTICS id) will be assigned to 2 different clusters
#' if their distance is below the distance of the edge point.
#' If above, observations are NA.
#' The pts parameter defines a minimum number of observations to form a
#' valley (i.e. cluster). If the number of observations in one valley is
#' smaller than pts, observations are set to NA.
#'
#' @param optics_obj Data frame returned by optics
#' @param n_xi       Number of clusters to define
#' @param pts        Minimum number of points per clusters
#' @param max_loop   Maximum iterations to find n_xi clusters
#' @param verbose    Print the ids of the largest difference considered and
#'                   cluster information if they define one
#' @return Vector of clusters
#' @seealso \link{opticskxi_pipeline}, \link{ggplot_optics}
#' @examples
#' data('multishapes')
#' optics_shapes <- dbscan::optics(multishapes[1:2])
#' kxi_shapes <- opticskxi(optics_shapes, n_xi = 5, pts = 30)
#' ggplot_optics(optics_shapes, groups = kxi_shapes)
#' ggpairs(cbind(multishapes[1:2], kXi = kxi_shapes), group = 'kXi')
#' @export
opticskxi <- function(optics_obj, n_xi, pts = optics_obj$minPts,
  max_loop = 50, verbose = FALSE) {

  df_optics <- get_optics_df(optics_obj)

  # get largest differences ids
  df_optics$reachdist[1] <- df_optics$coredist[1]
  diffs <- diff(df_optics$reachdist) 
  steep_ups <- diffs > 0
  ids <- order(abs(diffs), decreasing = TRUE)

  clusters <- rep(NA, nrow(df_optics))
  n_clust <- 0
  for (id in ids) {
    if (verbose) print(id)
    max_loop <- max_loop - 1

    valley_ids <- kxi_valley(df_optics, id, steep_ups[id])$optics_id

    if (length(valley_ids) >= pts) {
      # if the new cluster empties a previous one, skip to next difference
      tmp_clusters <- clusters
      tmp_clusters[valley_ids] %<>% paste0(n_clust + 1)
      tab_clusters <- table(tmp_clusters)
      if (any(tab_clusters < pts) || length(tab_clusters) == n_clust) next

      # else assign clusters
      n_clust <- n_clust + 1
      clusters[valley_ids] %<>% paste0(n_clust)
      if (verbose) print(paste0('Cluster ', n_clust, ': ', length(valley_ids)))
    }

    # if number of cluster equal to n_xi, stop
    if (n_clust == n_xi || max_loop == 0) break
  }

  clusters[order(df_optics$id)] %>% factor %>% as.numeric
}

get_optics_df <- function(optics_obj) {
  optics_obj[c('order', 'coredist', 'reachdist', 'predecessor')] %>%
    as.data.frame %>% cbind(id = seq_len(nrow(.)), .) %>% `[`(.$order, ) %>%
    cbind(optics_id = seq_len(nrow(.)), .)
}

# get dataframe of neighbors with smaller distance
# in steep downs both points are part of the cluster and first distance is threshold
# in steep ups only the first is part of the cluster and second distance is threshold
kxi_valley <- function(df_optics, steep_id, steep_up) {
  # if steep up take dist of second
  id_dist <- df_optics$reachdist[steep_id + steep_up]

  optics_id <- reachdist <- NULL
  if (steep_up) {
    # first previous id with larger distance is part of cluster
    id_min <- df_optics %$% which(reachdist > id_dist & optics_id < steep_id)
    id_min <- if (length(id_min)) max(id_min) else 1
    subset(df_optics, optics_id <= steep_id & optics_id >= id_min)
  } else {
    # first next id with larger distance is not part of cluster
    id_max <- df_optics %$% which(reachdist > id_dist & optics_id > steep_id)
    id_max <- if (length(id_max)) min(id_max) - 1 else nrow(df_optics)
    subset(df_optics, optics_id >= steep_id & optics_id <= id_max)
  }
}
