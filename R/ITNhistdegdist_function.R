#' @title ITN Histogram Degree Distribution
#'
#' @description This function plots the histogram degree distribtuion for the ITN
#' @param gs International Trade Network - igraph object
#' @export
#' @return Panel of ITN histogram degree distribtuion plots
#' @examples
#' require(igraph)
#'
#' ##Create random International Trade Network (igraph object)
#' ITN<-erdos.renyi.game(75,0.05,directed = TRUE)
#'
#' ##Add edge weights
#' E(ITN)$weight<-runif(ecount(ITN), 0, 1)
#'
#' ##Plot degree distribution histogram
#' hist_deg_dist<-ITNhistdegdist(ITN)

ITNhistdegdist<-function(gs){
  net <- cbind(igraph::get.edgelist(gs, names=FALSE),igraph:: E(gs)$weight)
  net <- tnet::as.tnet(net, type="weighted one-mode tnet")
  WeightDegOut<-tnet::degree_w(net,measure=c("degree","output"), type="out")
  WeightDegIn<-tnet::degree_w(net,measure=c("degree","output"), type="in")
  WeightDegAll<-tnet::degree_w(net,measure=c("degree","output"), type="all")
  Dout<-WeightDegOut[,2]
  Din<-WeightDegIn[,2]
  Dall<-Din+Dout
  plotdata<-cbind(Dout,Din,Dall)
  plotdata<-as.data.frame(plotdata)
  colnames(plotdata)<-c("Dout","Din","Dall")
  OUTplot<-ggplot2::ggplot(data=plotdata, ggplot2::aes(Dout)) +
    ggplot2::geom_histogram(fill="darkblue",
                   alpha = 1,position = 'stack', stat = 'bin',
                   binwidth = 1) +
    ggplot2::labs(title="Out Degree Distribution") +
    ggplot2::labs(x="Out Degree", y="Frquency")+
    ggplot2::theme_gray()

  INplot<-ggplot2::ggplot(data=plotdata, ggplot2::aes(Din)) +
    ggplot2::geom_histogram(fill="darkblue",
                            alpha = 1,position = 'stack', stat = 'bin',
                            binwidth = 1) +
    ggplot2::labs(title="In Degree Distribution") +
    ggplot2::labs(x="In Degree", y="Frquency")+
    ggplot2::theme_gray()

  ALLplot<-ggplot2::ggplot(plotdata, ggplot2::aes(Dall)) +
    ggplot2::geom_histogram(fill="darkblue",
                            alpha = 1,position = 'stack', stat = 'bin',
                            binwidth = 1) +
    ggplot2::labs(title="Degree Distribution") +
    ggplot2::labs(x="Degree", y="Frquency")+
    ggplot2::theme_gray()

  cowplot::plot_grid(INplot,OUTplot,ALLplot,labels=c("A","B","C"),ncol=2)


}


