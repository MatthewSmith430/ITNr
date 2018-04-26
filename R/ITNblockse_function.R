#' @title ITN Blockmodel & Structural Equivalence
#'
#' @description This function calculates block membership for ITN and strucutral equivalence between countries
#' @param gs International Trade Network - igraph object
#' @export
#' @return List object containing block memebrship and strucutural equivalence matrix results
#' @examples
#' require(igraph)
#' require(sna)
#' require(intergraph)
#'
#' ##Create random International Trade Network (igraph object)
#' ITN<-erdos.renyi.game(50,0.05,directed = TRUE)
#'
#' ##Add edge weights
#' E(ITN)$weight<-runif(ecount(ITN), 0, 1)
#'
#' ##Blockmodel & structural equivalence analysis
#' blockse<-ITNblock_se(ITN)
#'

ITNblock_se<-function(gs){
  h<-as.integer(igraph::vcount(gs)/3)
  gsnet<-intergraph::asNetwork(gs)
  ec <- sna::equiv.clust(gsnet, mode="digraph")#,
                   # plabels=sna::network.vertex.names(gsnet))
  bm <- sna::blockmodel(gsnet, ec, h=h)
  bm.mem<-bm$block.membership
  igraph::V(gs)$block.membership<-bm.mem

  MAT1<-igraph::as_adjacency_matrix(gs,attr="weight")
  MAT2<-as.matrix(MAT1)
  D<-blockmodeling::sedist(M=MAT2)
  D<-as.matrix(D)

  TAB<-igraph::get.data.frame(gs, what = "vertices")
  NAME<-TAB$name
  block.mem<-TAB$block.membership
  BLOCK<-cbind(NAME,block.mem)
  BLOCK<-as.data.frame(BLOCK)

  RESULTSblockse<-list(
    "Block.Membership"=BLOCK,
    "Structural.Equivalence.Matrix"=D
  )
  return(RESULTSblockse)
}
