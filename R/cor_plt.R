#' @param df  A data frame or tibble containing only the columns you want to calculate the correlations of, and a group identifier column.
#'
#' @param group A string containing the name of a column which identifies the two groups you want correlations computed.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_tile theme_classic coord_flip coord_equal labs guides scale_x_discrete scale_y_discrete scale_size  scale_alpha_discrete theme element_text
#' @return  ggplot object
#' @export
#'
#' @examples plot_cor_2groups(mtcars, "am")
plot_cor_2groups <- function(df, cor_method, group){
  plot_df <-  cor_fmt(df, cor_method = cor_method, group = group)
  g1 <- levels(factor(df[[group]]))[[1]]
  g2 <- levels(factor(df[[group]]))[[2]]
  ggplot(plot_df, aes(measure1,measure2, col=r_if_sig, alpha=sig_p)) + ## to get the rect filled
    geom_tile(col="black", fill="white") +
    geom_point(aes(size = abs(r)), shape=16) +
    labs(x=g1,y=g2,col = "Spearman's\nCorrelation") +
    theme_classic()+
    coord_equal() +
    guides(alpha=F) +
    viridis::scale_color_viridis(limits=c(-1,1), na.value="gray70")+
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    scale_size(range=c(1,6), guide=NULL) +
    scale_alpha_discrete(0,1)
}
