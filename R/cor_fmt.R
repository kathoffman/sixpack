cors <- function(df, cor_method) {
  M <- Hmisc::rcorr(as.matrix(df), type=cor_method)
  out <- purrr::map(M, ~data.frame(.x))
  return(out)
}

cor_fmt_1g <- function(df,
                       cor_method){
  temp <- cors(df, cor_method = cor_method)
  temp <- purrr::map(temp, ~tibble::rownames_to_column(.x, var="measure1"))
  temp <- purrr::map_dfr(temp, ~tidyr::pivot_longer(.x, -measure1, "measure2"), .id="id")
  temp <- tidyr::pivot_wider(temp, names_from = id, values_from = value)
  temp$p <- temp$P; temp$P <- NULL
  out <- temp
  return(out)
}

#' Format correlations for two different groups for use in a heat map
#'
#' @param df A data frame or tibble containing only the columns you want to calculate the correlations of, and a group identifier column.
#' @param group A string containing the name of a column which identifies the two groups you want correlations computed.
#'
#' @importFrom dplyr arrange n filter rowwise row_number
#' @return  a tidy tibble ready for plotting
#'
#' @examples format_cor_2groups(mtcars, "am")
#'
cor_fmt_2g <- function(df, cor_method, group){
  stopifnot(length(levels(factor(df[[group]]))) == 2)
  g1 <- levels(factor(df[[group]]))[[1]]
  g2 <- levels(factor(df[[group]]))[[2]]
  g1_df <- df[df[[group]] == g1,]
  g1_df <- g1_df[,-which(names(g1_df) == group)]
  g2_df <- df[df[[group]] == g2,]
  g2_df <- g2_df[,-which(names(g2_df) == group)]
  g1_cor <- cor_fmt_1g(g1_df, cor_method = cor_method)
  g2_cor <- cor_fmt_1g(g2_df, cor_method = cor_method)
  g1_cor$key <- apply(g1_cor[ ,c("measure1","measure2")], 1, paste, collapse = "-")
  g1_cor <- dplyr::group_by(dplyr::arrange(g1_cor, measure1, measure2), key)
  g1_cor <- dplyr::filter(g1_cor, dplyr::row_number() == 1)
  g2_cor$key <- apply(g2_cor[ ,c("measure1","measure2")], 1, paste, collapse = "-")
  g2_cor <- dplyr::group_by(dplyr::arrange(g2_cor, measure1, measure2), key)
  g2_cor <- dplyr::filter(g2_cor, dplyr::row_number() == dplyr::n(), measure1 != measure2)
  out <- dplyr::full_join(g1_cor, g2_cor)
  return(out)
}


#' importFrom("stats", "p.adjust")
#'
#' @param df data frame or tibble of data
#' @param cor_method choose from "spearman" or "pearson"
#' @param col_list optional list of columns to compare with each other (if not all columns against all other columns)
#' @param mc_method mutliple comparison method; see ?p.adjust for more details
#' @param group column to split comparisons by; must contain two unique values
#'
#' @export
#' @return tibble with ready to format correlation data
cor_fmt <- function(df,
                    cor_method = c("spearman"),
                    col_list = NULL, # not yet implemented
                    mc_method = NULL,
                    group = NULL) {
  if (is.null(group) == T){
    temp <- cor_fmt_1g(df, cor_method = cor_method)
  }
  else {
    if(length(levels(factor(df[[group]]))) != 2) stop('`sixpack` only supports two groups at this time.')
    temp <- cor_fmt_2g(df, cor_method = cor_method, group = group)
  }
  if (is.null(mc_method) == F){
    temp$p <- p.adjust(temp$p, method = mc_method)
  }
   temp$sig_p <- ifelse(temp$p < .05, T, F)
   temp$p_if_sig = ifelse(temp$p < .05, temp$p, NA)
   temp$r_if_sig = ifelse(temp$p < .05, temp$r, NA)
  out <- temp
   return(out)
}
