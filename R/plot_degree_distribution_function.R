#' @title Plot Degree Distribution
#'
#' @description This function plots degree distribution for any graph
#' @param graph igraph object
#' @param a mode - "in","out","all
#' @export
#' @return Panel of ITN degree distribution plots
#' @examples
#' require(igraph)
#' ##Create random International Trade Network (igraph object)
#' ITN<-erdos.renyi.game(75,0.05,directed = TRUE)
#'
#' ##Plot out degree distribution
#' plot_degree_distribution(ITN,"in")

plot_degree_distribution <-function(graph,a) {
  d <- igraph::degree(graph, mode = a)
  dd <- igraph::degree.distribution(graph, mode = a, cumulative = FALSE)
  degree <- 1:max(d)
  probability <- dd[-1]
  nonzero.position <- which(probability != 0)
  probability <- probability[nonzero.position]
  degree <- degree[nonzero.position]

  TYPE<-paste0("Degree_",a)
  TITLErev<-paste0("Degree Distribution_",a)
  plotdata<-cbind(probability,degree)
  plotdata<-as.data.frame(plotdata)
  colnames(plotdata)<-c("probability","degree")
  #graphics::plot(probability ~ degree, log = "xy", xlab = TYPE, ylab = "Probability (log)",
  #     col = 1, main = TITLErev)
  ggplot2::ggplot(plotdata, ggplot2::aes(x=degree, y=probability)) +
    ggplot2::geom_point(color="darkred")+
    ggplot2::geom_jitter(position = "jitter",color="darkred")+
    ggplot2::labs(title=TITLErev,
         x=TYPE, y = "Probability")+
    ggplot2::theme_gray()
}
