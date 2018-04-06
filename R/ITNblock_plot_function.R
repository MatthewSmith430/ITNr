#' @title ITN Blockmodel Plot
#'
#' @description This function calculates block membership for the ITN and then plots the network, with node colour according to block membership.
#' @param gs International Trade Network - igraph object
#' @param LABEL Should labels be present - TRUE/FALSE
#' @export
#' @return Network Plot - nodes coloured based on block membership
#' @examples
#' require(igraph)
#' require(sna)
#' require(intergraph)
#'
#' ##Create random International Trade Network (igraph object)
#' ITN<-erdos.renyi.game(75,0.05,directed = TRUE)
#'
#' ##Add edge weights
#' E(ITN)$weight<-runif(ecount(ITN), 0, 1)
#'
#' ##Blockmodel plot
#' block_plot<-ITNblock_plot(ITN,FALSE)
#'

ITNblock_plot<-function(gs,LABEL){
  gsnet<-intergraph::asNetwork(gs)
  ec <- sna::equiv.clust(gsnet, mode="digraph")#,
                   # plabels=network.vertex.names(gsnet))
  bm <- sna::blockmodel(gsnet, ec, h=20)
  bm.mem<-bm$block.membership
  igraph::V(gs)$block.membership<-bm.mem


  colourCount <- length(unique(bm.mem))
  y<-grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Accent"))(colourCount)

  gNET<-intergraph::asNetwork(gs)

  if (LABEL==TRUE){
      GGally::ggnet2(gNET,
                     node.size=5,node.color = igraph::V(gs)$block.membership,color.palette = y,
                     color.legend = "Block",label = TRUE,
                     label.size = 2.5,edge.size = igraph::E(gs)$weight,
                     edge.color = "grey50",arrow.size =5 )+
        ggplot2::guides(size = FALSE)

      }
    else{
        GGally::ggnet2(gNET,
                       node.size=5,node.color = igraph::V(gs)$block.membership,color.palette = y,
                       color.legend = "Block",label=FALSE,
                       edge.size = igraph::E(gs)$weight,
                       edge.color ="grey50",arrow.size =5 )+
          ggplot2::guides(size = FALSE)

      }
    }

