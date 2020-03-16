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
    WeightDegOut<-igraph::strength(gs, mode="out")
    WeightDegIn<-igraph::strength(gs, mode="in")
    WeightDegAll<-igraph::strength(gs, mode="total")
    WeightBet<-igraph::betweenness(gs, weights = igraph::E(gs)$weight,
                 normalized = TRUE)



    Weighted.Out.Degree<-WeightDegOut
    Weighted.In.Degree<-WeightDegIn
    Weighted.Degree.All<-WeightDegOut
    Binary.Out.Degree<-igraph::degree(gs,mode="out")
    Binary.In.Degree<-igraph::degree(gs,mode="in")
    Binary.Degree.All<-igraph::degree(gs,mode="total")
    Betweenness<-WeightBet
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

