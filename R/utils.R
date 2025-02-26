

if (getRversion() >= "2.15.1") {
  vars = c('.', 'x', 'rotation', 'sdev', 'DIMRED_VARTYPE', 'value', 'Var2',
    'dim_red', 'dist', 'n_xi', 'pts', 'n_dimred_comp', 'n_clusters', 'n_occur')
  utils::globalVariables(vars)
}

#' Print vignette table
#'
#' Print knitr::kable latex table with legend at bottom.
#'
#' @param table_obj Table object
#' @param label     Latex label
#' @return None, side-effect prints a Latex table
#' @export
print_vignette_table <- function(table_obj, label) {

  caption = if (label == 'Ensemble') {

    'In the first slot of the object returned by $ensemble\\_metrics$, we can investigate which metric voted for each model. Here we have the 10 models with highest sum of metrics ranks thresholded to 50. The top ensemble model was not the best model for any metric (otherwise the rank value would be 50), but was in the top 10 for 5 metrics (rank greater than 40), and was outside of the top 50 for 2 metrics (rank value set to 0).'

  } else {

    paste('Contingency table of disease status and k-Xi',
          'clustering of the', label,
          'dataset, with standardized Pearson residuals.')
  }
 
  caption %<>% paste0('\\caption{\\label{tab:', tolower(label), '}', ., '}')

  knitr::kable(table_obj, format = 'latex', booktabs = TRUE) %>%
    c('\\begin{table}[t]', '\\centering', ., caption, '\\end{table}') %>%
    cat(sep = '\n')
}


#' Residuals table
#'
#' Bind contingency table and Pearson Chi-squared residuals.
#'
#' @param ... Passed to contingency_table and chisq.test
#' @return Matrix
#' @export
residuals_table <- function(...) {
  stats::chisq.test(...)$stdres %>%
    `colnames<-`(paste('Residuals:', colnames(.))) %>% round(2) %>%
    rbind(matrix(NA, 2, ncol(.))) %>% cbind(contingency_table(...), .)
}

#' Contingency table
#'
#' Include NAs and add totals to table.
#'
#' @param ... Passed to table
#' @return Table object
#' @export
contingency_table <- function(...) {
  table(..., useNA = 'ifany') %>% cbind(Total = rowSums(.)) %>%
    rbind(Total = colSums(.))
}

#' Nice palette
#'
#' Color palette
#'
#' @param groups  Vector, each unique value will get a color
#' @param rainbow If TRUE, rainbow-like colors, else differentiate successive
#'                values
#' @return Vector of colors
#' @export
nice_palette <- function(groups, rainbow = FALSE) {
  if (is.null(groups)) return()
  groups %<>% unlist %>% unique %>% stats::na.omit() %>% factor(., .) %>%
    as.numeric
  max_grps <- max(groups)

  if (!rainbow) {
    step <- ceiling(max_grps / 2) - 1
    seq_colors <- seq_along(groups)
    evens <- seq_colors %% 2 == 0
    seq_colors[evens] <- seq_colors[evens] + step:(max_grps %% 2)
    seq_colors[!evens] <- seq_colors[!evens] - 0:step
    groups %<>% factor(seq_colors) %>% as.numeric
  }

  grDevices::hsv(groups / max_grps, .9, .7)
}
