#' @title ITN Correlation Plot
#'
#' @description This function plots the correlation between degree and strength scores
#' @param gs International Trade Network - igraph object
#' @export
#' @return Correlation plot
#' @examples
#' require(igraph)
#'
#' ##Create random International Trade Network (igraph object)
#' ITN<-erdos.renyi.game(75,0.05,directed = TRUE)
#'
#' ##Add edge weights
#' E(ITN)$weight<-runif(ecount(ITN), 0, 1)
#'
#' ##Plot correlation matrix between degree and strength scores.
#' corr_plot<-ITNcorr(ITN)
#'

ITNcorr<-function(gs){
  deg <- igraph::degree(gs, mode = "all")
  deg.in <- igraph::degree(gs, mode = "in")
  deg.out <- igraph::degree(gs, mode = "out")

  str <- igraph::strength(gs, mode = "all")
  str.in <- igraph::strength(gs, mode = "in")
  str.out <- igraph::strength(gs, mode = "out")

  degree_strength_df <- data.frame(deg, deg.in, deg.out, str.out, str.in, str)

  corr <- stats::cor(degree_strength_df)
  corr2<-corr[1:3,4:6]
  melted_cormat <- reshape2::melt(corr2)
  colnames(melted_cormat)<-c("Degree.Binary","Degree.Strength","Value")

  ggplot2::ggplot(data = melted_cormat,
                  ggplot2::aes(x=melted_cormat$Degree.Binary,
                               y=melted_cormat$Degree.Strength,
                               fill=melted_cormat$Value)) +
    ggplot2::geom_tile()+
    ggplot2::labs(x="Binary Degree",y="Degree Strength",
                  title="Correlation between binary degree and degree strength",
                  fill="Correlation Level")
}
