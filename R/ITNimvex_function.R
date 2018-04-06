#' @title ITN - Exports vs Imports Plot
#'
#' @description The following function produces a plot showing imports
#' (in degree) vs exports (out degree). This allows us to identify whether
#' in the ITN, countries that export high levels also import high levels.
#' The plot can be produced for either weighted or binary import and export ties.
#' @param gs International Trade Network - igraph object
#' @param weighted TRUE - plot import strength vs export strength. FALSE - Import count Vs export count
#' @export
#' @return Imports Vs Exports Plot
#' @examples
#' require(igraph)
#'
#' ##Create random International Trade Network (igraph object)
#' ITN<-erdos.renyi.game(75,0.05,directed = TRUE)
#'
#' ##Add edge weights
#' E(ITN)$weight<-runif(ecount(ITN), 0, 1)
#'
#' ##Plot binary import vs exports
#' imvex_plot<-ITNimvex(ITN,FALSE)

ITNimvex<-function(gs,weighted){
  deg.in <- igraph::degree(gs, mode = "in")
  deg.out <- igraph::degree(gs, mode = "out")

  str.in <- igraph::strength(gs, mode = "in")
  str.out <- igraph::strength(gs, mode = "out")

  degree_df <- data.frame(deg.in, deg.out, str.out, str.in)
  if (weighted==TRUE){
    ggplot2::ggplot(degree_df, ggplot2::aes(x=str.in, y=str.out)) +
      ggplot2::geom_jitter(position = "jitter",shape=18, color="darkblue")+
      #ggplot2::geom_point(shape=18, color="darkblue")+
      ggplot2::geom_smooth(method="lm",  linetype="dashed",
                  color="darkred")+
      ggplot2::labs(title="Imports Vs Exports (Strength)",
           x="Imports (In degree)", y = "Exports (Out degree)")+
      ggplot2::theme_classic()

  }
  else{
    ggplot2::ggplot(degree_df, ggplot2::aes(x=deg.in, y=deg.out)) +
      ggplot2::geom_jitter(position = "jitter",shape=18, color="darkblue")+
      #ggplot2::geom_point(shape=18, color="darkblue")+
      ggplot2::geom_smooth(method="lm",  linetype="dashed",
                  color="darkred")+
      ggplot2::labs(title="Imports Vs Exports (Count)",
           x="Imports (In degree)", y = "Exports (Out degree)")+
      ggplot2::theme_classic()

  }


}
