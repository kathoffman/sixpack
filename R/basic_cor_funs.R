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
