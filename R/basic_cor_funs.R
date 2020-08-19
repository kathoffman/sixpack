cors <- function(df) {
  M <- Hmisc::rcorr(as.matrix(df))
  out <- purrr::map(M, ~data.frame(.x))
  return(out)
}

#' importFrom("stats", "p.adjust")
format_cors <- function(df, mult_comp = F){
  temp <- cors(df)
  temp <- purrr::map(temp, ~tibble::rownames_to_column(.x, var="measure1"))
  temp <- purrr::map(temp, ~tidyr::pivot_longer(.x, -measure1, "measure2"))
  temp <- dplyr::bind_rows(temp, .id = "id")
  temp <- tidyr::pivot_wider(temp, names_from = id, values_from = value)
  temp$p <- temp$P; temp$P <- NULL
  if (mult_comp == T){
    temp$p <- p.adjust(temp$p, method = "fdr")
  }
   temp$sig_p <- ifelse(temp$p < .05, T, F)
   temp$p_if_sig = ifelse(temp$p <.05, temp$p, NA)
   temp$r_if_sig = ifelse(temp$p <.05, temp$r, NA)
  out <- temp
   return(out)
}
