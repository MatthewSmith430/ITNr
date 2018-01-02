#' @title ITN Plots
#'
#' @description This function plots the ITN for a quick inspection
#' @param gs International Trade Network - igraph object
#' @export
#' @return Panel of ITN plots
#' @examples \dontrun{
#' ##Create random International Trade Network (igraph object)
#' ITN<-erdos.renyi.game(75,0.05,directed = TRUE)
#'
#' ##Plot set of network visualisations
#' ITNplotset(ITN)
#'
#' }
ITNplotset<-function(gs){
  graphics::par(mfrow=c(2,2))
  igraph::V(gs)$vertex.label<-NA
  cfg <- igraph::cluster_fast_greedy(igraph::as.undirected(gs))
  mem<-as.vector(igraph::membership(cfg))
  igraph::V(gs)$cluster.fast.greedy.mem<-mem

  graphics::plot(cfg,igraph::as.undirected(gs), edge.arrow.size=0,vertex.label=NA,
       layout=igraph::layout.fruchterman.reingold,vertex.size=10,
       main = "Cluster Fast Greedy ITN Plot")


  CSC<-igraph::cluster_spinglass(gs)
  CSCmem<-CSC$membership
  igraph::V(gs)$spinglass.com<-CSCmem

  graphics::plot(gs,vertex.label=NA,vertex.size=10,edge.arrow.size=0,
       edge.width=igraph::E(gs)$weight,
       layout=igraph::layout.fruchterman.reingold,
       vertex.color=igraph::V(gs)$spinglass.com,
       main="Spinglass ITN Plot")

  graphics::plot(gs,vertex.label=NA,vertex.size=10,edge.arrow.size=0,
       edge.width=igraph::E(gs)$weight,
       layout=igraph::layout.fruchterman.reingold,
       vertex.color=igraph::V(gs)$region,
       main="Region ITN Plot")

  graphics::plot(gs,vertex.label=NA,vertex.size=(igraph::degree(gs, mode="out")/2),
       edge.arrow.size=0,edge.width=igraph::E(gs)$weight,
       layout=igraph::layout.sphere,vertex.color=igraph::V(gs)$region,
       main="Region ITN Plot - Node Size Outdegree")

}
