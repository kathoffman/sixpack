#' Format correlations for two different groups for use in a heat map
#'
#' @param df A data frame or tibble containing only the columns you want to calculate the correlations of, and a group identifier column.
#' @param group A string containing the name of a column which identifies the two groups you want correlations computed.
#'
#' @importFrom dplyr arrange n filter rowwise row_number
#' @return  a tidy tibble ready for plotting
#' @export
#'
#' @examples format_cor_2groups(mtcars, "am")
#'
format_cor_2groups <- function(df, group){
  stopifnot(length(levels(factor(df[[group]]))) == 2)
  g1 <- levels(factor(df[[group]]))[[1]]
  g2 <- levels(factor(df[[group]]))[[2]]
  g1_df <- df[df[[group]] == g1,]
  g1_df <- g1_df[,-which(names(g1_df) == group)]
  g2_df <- df[df[[group]] == g2,]
  g2_df <- g2_df[,-which(names(g2_df) == group)]
  g1_cor <- format_cors(g1_df, mult_comp = T)
  g2_cor <- format_cors(g2_df, mult_comp = T)
  g1_cor$key <- apply(g1_cor[ ,c("measure1","measure2")], 1, paste, collapse = "-")
  g1_cor <- dplyr::group_by(dplyr::arrange(g1_cor, measure1, measure2), key)
  g1_cor <- dplyr::filter(g1_cor, dplyr::row_number() == 1)
  g2_cor$key <- apply(g2_cor[ ,c("measure1","measure2")], 1, paste, collapse = "-")
  g2_cor <- dplyr::group_by(dplyr::arrange(g2_cor, measure1, measure2), key)
  g2_cor <- dplyr::filter(g2_cor, dplyr::row_number() == dplyr::n(), measure1 != measure2)
  out <- dplyr::full_join(g1_cor, g2_cor)
  return(out)
}

#' @param df  A data frame or tibble containing only the columns you want to calculate the correlations of, and a group identifier column.
#'
#' @param group A string containing the name of a column which identifies the two groups you want correlations computed.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_tile theme_classic coord_flip coord_equal labs guides scale_x_discrete scale_y_discrete scale_size  scale_alpha_discrete theme element_text
#' @return  ggplot object
#' @export
#'
#' @examples plot_cor_2groups(mtcars, "am")
plot_cor_2groups <- function(df, group){
  plot_df <-  format_cor_2groups(df, group)
  g1 <- levels(factor(df[[group]]))[[1]]
  g2 <- levels(factor(df[[group]]))[[2]]
  ggplot(plot_df, aes(measure1,measure2, col=r_if_sig, alpha=sig_p)) + ## to get the rect filled
    geom_tile(col="black", fill="white") +
    geom_point(aes(size = abs(r)), shape=16) +
    labs(x=g1,y=g2,col = "Spearman's\nCorrelation") +
    theme_classic()+
    coord_equal() +
    guides(alpha=F) +
    #scale_color_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1))  +
    viridis::scale_color_viridis(limits=c(-1,1), na.value="gray70")+
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    scale_size(range=c(1,6), guide=NULL) +
    scale_alpha_discrete(0,1)
}
