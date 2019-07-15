
#' Plot multiple axes of a data frame or a fortified dimension reduction.
#'
#' @param df_data   Data frame
#' @param group     Column name of the grouping of observations
#' @param axes      Axes to plot. If more than 2, plots all pair combinations
#' @param variables Logical, plot variable contributions of the dimension
#'                  reduction to the selected axes, only for 2 axes
#' @param n_vars    Maximum number of variable contributions to plot. By
#'                  default 0, for all variables.
#' @param ellipses  Logical, plot ellipses of groups
#' @param ...       Passed to ggplot2 stat_ellipse if ellipses are requested
#' @param title     String to add as title, default NULL
#' @param colors    Vector of colors for each group
#' @return ggmatrix
#' @seealso \link{fortify_pca}, \link{fortify_ica}
#' @examples
#' df_pca <- fortify_pca(iris[-5])
#' ggpairs(df_pca)
#' df_pca <- fortify_pca(iris[-5], sup_vars = iris[5])
#' ggpairs(df_pca, group = 'Species', ellipses = TRUE, variables = TRUE)
#' @export
ggpairs <- function(df_data, group = NULL, axes = 1:2, variables = FALSE,
  n_vars = 0, ellipses = FALSE, ..., title = NULL,
  colors = if (!is.null(group)) nice_palette(df_data[[group]])) {

  if (is.null(colors)) colors <- 'black'
  variables <- if (variables) variables_layers(df_data, axes, n_vars)

  if ('DIMRED_VARTYPE' %in% names(df_data)) {
    df_data %<>% subset(DIMRED_VARTYPE == 'OBS')
  }
  df_obs <- df_data[c(axes, if (!is.null(group)) match(group, names(df_data)))]

  # order nas first to plot them underneath
  if (!is.null(group)) df_obs %<>% order_df_by_nas(group)

  map <- list(x = 'x', y = 'y') %>% append(list(color = group)) %>%
    do.call(aes_string, .)
  df_gg <- names(df_obs)[seq_along(axes)] %>% utils::combn(2) %>%
    apply(2, combine_axes, df_obs) %>%  do.call(rbind, .) %>%
    cbind(df_obs[group], row.names = NULL)
  df_gg$axe1 %<>% factor(unique(.))
  df_gg$axe2 %<>% factor(unique(.))

  gg <- ggplot(df_gg, map) + geom_point() +
    facet_grid(axe2 ~ axe1, scales = 'free') +
    labs(x = NULL, y = NULL, title = title) + theme_bw(12) +
    scale_color_manual(values = colors, na.value = grDevices::grey(.5)) +
    theme(panel.grid.minor = element_blank(), 
      strip.background = element_blank(),
      panel.spacing = grid::unit(1.5, 'mm')) +
    append(if (ellipses) stat_ellipse(...), variables)

  if (length(axes) == 2) return(gg)

  grDevices::pdf(NULL, 1, 1)
  grob <- ggplotGrob(gg + theme(legend.position = 'bottom'))
  grDevices::dev.off()

  idx_pos <- (length(axes) - 1) %>% matrix(0, ., .) %>% upper.tri %>%
    which %>% `+`(1)
  grob$grobs[idx_pos] = lapply(idx_pos, grid::nullGrob)
  grob
}

order_df_by_nas <- function(df_obs, group) {
  df_obs[[group]] %<>% as.character
  if (any(is.na(df_obs[[group]]))) {
    nas <- df_obs[[group]] %>% is.na
    df_obs %<>% { rbind(subset(., nas), subset(., !nas)) }
  }
  df_obs
}

variables_layers <- function(df_data, axes, n_vars) {
  if (!'DIMRED_VARTYPE' %in% names(df_data)) {
    stop("Can't plot variables wthout a DIMRED_VARTYPE column.")
  }
  if (length(axes) != 2) stop("Can't plot variables for more than 2 axes.")
  df_var <- subset(df_data, DIMRED_VARTYPE == 'VAR')

  lam <- subset(df_data, DIMRED_VARTYPE == 'Explained_variance')[axes] %>%
    unlist
  if (length(lam) == 0) {
    lam <- apply(df_data[axes], 2, range) %>% diff %>% as.vector
  }
  df_var <- t(t(df_var[axes]) * sqrt(lam) * sqrt(nrow(df_var))) %>%
    cbind.data.frame(varname = df_var$DIMRED_VARNAME)

  if (n_vars > 0) {
    df_var_subset <- NULL
    for (axe in 1:2) {
      df_var_ordered <- df_var[order(abs(df_var[[axe]]), decreasing = TRUE), ]
      df_var_subset %<>% rbind(utils::head(df_var_ordered, n_vars))
    }
    df_var <- df_var_subset
  }

  map <- names(df_var) %>% { aes_string(xend = .[1], yend = .[2]) }
  segments <- geom_segment(map, df_var, x = 0, y = 0, color = 'black',
    arrow = grid::arrow(length = grid::unit(0.03, "npc")))

  map <- names(df_var) %>%
    { aes_string(x = .[1], y = .[2], label = 'varname') }
  labels <- ggrepel::geom_label_repel(map, df_var, color = 'black',
    segment.color = 'grey50')

  list(segments, labels)
}

combine_axes <- function(i, df_obs) {
  cbind(as.list(i), df_obs[, i], stringsAsFactors = FALSE) %>%
    stats::setNames(c('axe1', 'axe2', 'x', 'y'))
}

