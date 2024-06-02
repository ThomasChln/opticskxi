
rm_huge_clusters = function(l_optics_clusters, max_size_ratio = 0.15) {

  clusters = l_optics_clusters$clusters
  huge_clusters = table(clusters) > max_size_ratio * length(clusters)
  huge_clusters = names(which(huge_clusters))

  if (length(huge_clusters) == 0) return(clusters)

  df_optics = get_optics_df(l_optics_clusters$optics)
  df_optics$reachdist[1] = df_optics$coredist[1]
  clusters = clusters[df_optics$id]

  for (huge_cluster in huge_clusters) {
    # cluster may have been removed in previous steps
    if (!huge_cluster %in% unique(clusters)) next

    huge_cluster_idxs = which(clusters == huge_cluster)
    clusters[huge_cluster_idxs] = NA
    median_dist = median(df_optics$reachdist[huge_cluster_idxs])

    # rm surrounding cluster (otherwise could use 1.1, 1.2 type clusters)
    clusters %<>% rm_neighbor_cluster_left(huge_cluster_idxs, median_dist,
					   df_optics$reachdist)
    clusters %<>% rm_neighbor_cluster_right(huge_cluster_idxs, median_dist,
					    df_optics$reachdist)
  }

  clusters %<>% factor %>% as.numeric

  clusters[order(df_optics$id)]
}

rm_neighbor_cluster_left = function(clusters, rm_cluster_idxs, median_dist,
				    reachdists) {

  if (rm_cluster_idxs[1] != 1) {
    left_cluster = clusters[rm_cluster_idxs[1] - 1]
    
    if (!is.na(left_cluster)) {
      left_cluster_idxs = which(clusters == left_cluster)
      median_dist_left = median(reachdists[left_cluster_idxs])

      if (median_dist_left > median_dist) {
        clusters[left_cluster_idxs] = NA
        # double recursion, quite proud of this one
        clusters %<>% rm_neighbor_cluster_left(left_cluster_idxs, median_dist,
					       reachdists)
        clusters %<>% rm_neighbor_cluster_right(left_cluster_idxs, median_dist,
					       	reachdists)
      }
    }
  }

  clusters
}

rm_neighbor_cluster_right = function(clusters, rm_cluster_idxs, median_dist, reachdists) {

  if (tail(rm_cluster_idxs, 1) != length(clusters)) {
    right_cluster = clusters[tail(rm_cluster_idxs, 1) + 1]
    
    if (!is.na(right_cluster)) {
      right_cluster_idxs = which(clusters == right_cluster)
      median_dist_right = median(reachdists[right_cluster_idxs])

      if (median_dist_right > median_dist) {
        clusters[right_cluster_idxs] = NA
        clusters %<>% rm_neighbor_cluster_left(right_cluster_idxs, median_dist, reachdists)
        clusters %<>% rm_neighbor_cluster_right(right_cluster_idxs, median_dist, reachdists)
      }
    }
  }

  clusters
}

