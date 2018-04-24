#' @title ITN Centrality for binary networks
#'
#' @description This function calculates a number of centrality metrics for the binary International Trade Network (ITN)
#' @param gs International Trade Network - binary igraph object
#' @export
#' @return Table of centrality results (dataframe)
#' @examples
#' require(igraph)
#' ##Create random International Trade Network (igraph object)
#' ITN<-erdos.renyi.game(75,0.05,directed = TRUE)
#'
#'
#' ##Add vertex names
#' V(ITN)$name<-1:vcount(ITN)
#'
#' ##Calculate the centrality measures
#' ITNCENT<-ITNcentrality_binary(ITN)
#'
ITNcentrality_binary<-function(gs){
    Binary.Out.Degree<-igraph::degree(gs,mode="out")
    Binary.In.Degree<-igraph::degree(gs,mode="in")
    Binary.Degree.All<-igraph::degree(gs,mode="all")
    Betweenness<-igraph::betweenness(gs)
    Closeness<-igraph::closeness(gs, mode="all")
    Eigenvector<-igraph::eigen_centrality(gs)$vector
    Hub<-igraph::hub_score(gs, weights=NA)$vector
    Authority<-igraph::authority_score(gs, weights=NA)$vector

    NAMES<-igraph::V(gs)$name
    TAB<-cbind(NAMES,Binary.Out.Degree,
               Binary.In.Degree,
               Binary.Degree.All,
               Betweenness,Closeness,Eigenvector,Hub,Authority)
    TAB2<-as.matrix(TAB)
    mm<-matrix("NA",length(ISOLATES),12)
    mm[,1]<-ISOLATES
    colhead<-c("NAMES","Binary.Out.Degree",
              "Binary.In.Degree","Binary.Degree.All",
               "Betweenness","Closeness","Eigenvector",
              "Hub","Authority")
    colnames(mm)<-colhead
    TAB2<-rbind(TAB,mm)
    TAB2<-TAB2[order(TAB2[,1]),]
    myDF<-as.data.frame(TAB2,stringsAsFactors =FALSE)
    myDF$NAMES<-as.character(myDF$NAMES)
    myDF$Binary.Out.Degree<-as.numeric(myDF$Binary.Out.Degree)
    myDF$Binary.In.Degree<-as.numeric(myDF$Binary.In.Degree)
    myDF$Binary.Degree.All<-as.numeric(myDF$Binary.Degree.All)
    myDF$Betweenness<-as.numeric(myDF$Betweenness)
    myDF$Closeness<-as.numeric(myDF$Closeness)
    myDF$Eigenvector<-as.numeric(myDF$Eigenvector)
    myDF$Hub<-as.numeric(myDF$Hub)
    myDF$Authority<-as.numeric(myDF$Authority)
    myDF<-round_df(myDF,4)
    return(myDF)
}
