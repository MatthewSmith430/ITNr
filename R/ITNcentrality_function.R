#' @title ITN Centrality
#'
#' @description This function calculates a number of centrality metrics for the weighted International Trade Network (ITN)
#' @param gs International Trade Network - igraph object
#' @export
#' @return Table of centrality results (dataframe)
#' @examples
#' require(igraph)
#' ##Create random International Trade Network (igraph object)
#' ITN<-erdos.renyi.game(75,0.05,directed = TRUE)
#'
#' ##Add edge weights
#' E(ITN)$weight<-runif(ecount(ITN), 0, 1)
#'
#' ##Add vertex names
#' V(ITN)$name<-1:vcount(ITN)
#'
#' ##Calculate the centrality measures
#' ITNCENT<-ITNcentrality(ITN)
#'
ITNcentrality<-function(gs){
  if (sum(igraph::degree(gs)==0)==0){
    net <- cbind(igraph::get.edgelist(gs, names=FALSE), igraph::E(gs)$weight)
    net <- tnet::as.tnet(net, type="weighted one-mode tnet")

    WeightDegOut<-tnet::degree_w(net,measure=c("degree","output"), type="out")
    WeightDegIn<-tnet::degree_w(net,measure=c("degree","output"), type="in")
    WeightDegAll<-tnet::degree_w(net,measure=c("degree","output"), type="all")
    WeightBet<-tnet::betweenness_w(net)

    WeightDegOut<-WeightDegOut[order(WeightDegOut[,1]),]
    WeightDegIn<-WeightDegIn[order(WeightDegIn[,1]),]
    WeightDegAll<-WeightDegAll[order(WeightDegAll[,1]),]
    WeightBet<-WeightBet[order(WeightBet[,1]),]

    Weighted.Out.Degree<-WeightDegOut[,3]
    Weighted.In.Degree<-WeightDegIn[,3]
    Weighted.Degree.All<-WeightDegOut[,3]
    Binary.Out.Degree<-WeightDegOut[,2]
    Binary.In.Degree<-WeightDegIn[,2]
    Binary.Degree.All<-WeightDegOut[,2]
    Betweenness<-WeightBet[,2]
    Closeness<-igraph::closeness(gs, mode="all")
    Eigenvector<-igraph::eigen_centrality(gs)$vector
    Hub<-igraph::hub_score(gs, weights=NA)$vector
    Authority<-igraph::authority_score(gs, weights=NA)$vector

    NAMES<-igraph::V(gs)$name
    TAB<-cbind(NAMES,Weighted.Out.Degree,Binary.Out.Degree,
               Weighted.In.Degree,Binary.In.Degree,
               Weighted.Degree.All,Binary.Degree.All,
               Betweenness,Closeness,Eigenvector,Hub,Authority)
    myDF<-as.data.frame(TAB,stringsAsFactors =FALSE)
    myDF$NAMES<-as.character(myDF$NAMES)
    myDF$Weighted.Out.Degree<-as.numeric(myDF$Weighted.Out.Degree)
    myDF$Binary.Out.Degree<-as.numeric(myDF$Binary.Out.Degree)
    myDF$Weighted.In.Degree<-as.numeric(myDF$Weighted.In.Degree)
    myDF$Binary.In.Degree<-as.numeric(myDF$Binary.In.Degree)
    myDF$Weighted.Degree.All<-as.numeric(myDF$Weighted.Degree.All)
    myDF$Binary.Degree.All<-as.numeric(myDF$Binary.Degree.All)
    myDF$Betweenness<-as.numeric(myDF$Betweenness)
    myDF$Closeness<-as.numeric(myDF$Closeness)
    myDF$Eigenvector<-as.numeric(myDF$Eigenvector)
    myDF$Hub<-as.numeric(myDF$Hub)
    myDF$Authority<-as.numeric(myDF$Authority)
    myDF<-round_df(myDF,4)
    return(myDF)
  }
  else{
    ISOLATES<-igraph::V(gs)[igraph::degree(gs)==0]$id
    gs<-igraph::delete.vertices(gs, igraph::V(gs)[igraph::degree(gs)==0])
    net <- cbind(igraph::get.edgelist(gs, names=FALSE),igraph:: E(gs)$weight)
    net <- tnet::as.tnet(net, type="weighted one-mode tnet")

    WeightDegOut<-tnet::degree_w(net,measure=c("degree","output"), type="out")
    WeightDegIn<-tnet::degree_w(net,measure=c("degree","output"), type="in")
    WeightDegAll<-tnet::degree_w(net,measure=c("degree","output"), type="all")
    WeightBet<-tnet::betweenness_w(net)

    WeightDegOut<-WeightDegOut[order(WeightDegOut[,1]),]
    WeightDegIn<-WeightDegIn[order(WeightDegIn[,1]),]
    WeightDegAll<-WeightDegAll[order(WeightDegAll[,1]),]
    WeightBet<-WeightBet[order(WeightBet[,1]),]

    Weighted.Out.Degree<-WeightDegOut[,3]
    Weighted.In.Degree<-WeightDegIn[,3]
    Weighted.Degree.All<-WeightDegOut[,3]
    Binary.Out.Degree<-WeightDegOut[,2]
    Binary.In.Degree<-WeightDegIn[,2]
    Binary.Degree.All<-WeightDegOut[,2]
    Betweenness<-WeightBet[,2]
    Closeness<-igraph::closeness(gs, mode="all")
    Eigenvector<-igraph::eigen_centrality(gs)$vector
    Hub<-igraph::hub_score(gs, weights=NA)$vector
    Authority<-igraph::authority_score(gs, weights=NA)$vector

    NAMES<-igraph::V(gs)$name
    TAB<-cbind(NAMES,Weighted.Out.Degree,Binary.Out.Degree,
               Weighted.In.Degree,Binary.In.Degree,
               Weighted.Degree.All,Binary.Degree.All,
               Betweenness,Closeness,Eigenvector,Hub,Authority)
    TAB2<-as.matrix(TAB)
    mm<-matrix("NA",length(ISOLATES),12)
    mm[,1]<-ISOLATES
    colhead<-c("NAMES","Weighted.Out.Degree","Binary.Out.Degree",
               "Weighted.In.Degree","Binary.In.Degree",
               "Weighted.Degree.All","Binary.Degree.All",
               "Betweenness","Closeness","Eigenvector","Hub","Authority")
    colnames(mm)<-colhead
    TAB2<-rbind(TAB,mm)
    TAB2<-TAB2[order(TAB2[,1]),]
    myDF<-as.data.frame(TAB2,stringsAsFactors =FALSE)
    myDF$NAMES<-as.character(myDF$NAMES)
    myDF$Weighted.Out.Degree<-as.numeric(myDF$Weighted.Out.Degree)
    myDF$Binary.Out.Degree<-as.numeric(myDF$Binary.Out.Degree)
    myDF$Weighted.In.Degree<-as.numeric(myDF$Weighted.In.Degree)
    myDF$Binary.In.Degree<-as.numeric(myDF$Binary.In.Degree)
    myDF$Weighted.Degree.All<-as.numeric(myDF$Weighted.Degree.All)
    myDF$Binary.Degree.All<-as.numeric(myDF$Binary.Degree.All)
    myDF$Betweenness<-as.numeric(myDF$Betweenness)
    myDF$Closeness<-as.numeric(myDF$Closeness)
    myDF$Eigenvector<-as.numeric(myDF$Eigenvector)
    myDF$Hub<-as.numeric(myDF$Hub)
    myDF$Authority<-as.numeric(myDF$Authority)
    myDF<-round_df(myDF,4)
    return(myDF)
  }
}
