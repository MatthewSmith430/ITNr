#' @title ITN Properties
#'
#' @description This function calculates network level properties for the ITN. These include:
#' -Size (number of nodes) -Density -Reciprocity -Diameter
#' -Average path length -Average node strength -Average Degree
#' -Betweenness Centralisation -Closeness Centralisation -Eigenvector Centralisation
#' -Out Degree Centralisation -In Degree Centralisation -All Degree Centralisation -Clustering coefficent (transitivity)
#' -Clustering Weighted -Region Homophily -Degree Assortativity
#' @param gs International Trade Network - igraph object
#' @export
#' @return Table of centrality results (dataframe)
#' @examples
#' ##Load the network
#' data(ELEnet16)
#'
#' ##Calculate the network properties
#' ITNPROP<-ITNproperties(ELEnet16)
#'
ITNproperties<-function(gs){
  net <- cbind(igraph::get.edgelist(gs, names=FALSE), igraph::E(gs)$weight)
  net <- tnet::as.tnet(net, type="weighted one-mode tnet")

  WeightDegAll<-tnet::degree_w(net,measure=c("degree","output"), type="all")
  WeightedClustering<-tnet::clustering_w(net)

  Wall<-WeightDegAll[,3]
  Dall<-WeightDegAll[,2]

  BetCen<-igraph::centr_betw(gs)
  closenesscen<-igraph::centr_clo(gs,mode="total")
  DegOutCen<-igraph::centr_degree(gs,mode="out")
  DegInCen<-igraph::centr_degree(gs,mode="in")
  DegAllCen<-igraph::centr_degree(gs,mode="all")
  EigCen<-igraph::centr_eigen(gs)

  AveragePathLength<-igraph::mean_distance(gs, directed=TRUE)

  AverageNodeStrengthAll<-mean(Wall)
  AverageDegreeAll<-mean(Dall)

  den<-igraph::graph.density(gs,loops=F)
  DIA<-igraph::diameter(gs, directed = T)
  size<-igraph::vcount(gs)
  RECIP<-igraph::reciprocity(gs)

  CC<-igraph::transitivity(gs,"global")

  RegionHomophily<-igraph::assortativity_nominal(gs, igraph::V(gs)$region, directed=T)
  DegAssort<-igraph::assortativity_degree(gs, directed=T)

  myDF<-data.frame(
    Size=size,
    Density=den,
    Reciprocity=RECIP,
    Diameter=DIA,
    Average.path.lenth=AveragePathLength,
    Average.node.stregnth=AverageNodeStrengthAll,
    Average.Degree=AverageDegreeAll,
    Betweenness.Centralisation=BetCen$centralization,
    Closeness.Centralisation=closenesscen$centralization,
    Eigenvector.Centralisation=EigCen$centralization,
    Out.Degree.Centralisation=DegOutCen$centralization,
    In.Degree.Centralisation=DegInCen$centralization,
    All.Degree.Centralisation=DegAllCen$centralization,
    clustering.coefficient.transitivity=CC,
    Clustering.Weighted=WeightedClustering,
    Region.Homophily=RegionHomophily,
    Degree.Assortativity=DegAssort
  )
  myDF<-round_df(myDF,4)
  myDF<-t(myDF)
  myDF<-as.data.frame(myDF)
  rownames(myDF)<-c(
    "Size",
    "Density",
    "Reciprocity",
    "Diameter",
    "Average.path.length",
    "Average.node.strength",
    "Average.Degree",
    "Betweenness.Centralisation",
    "Closeness.Centralisation",
    "Eigenvector.Centralisation",
    "Out.Degree.Centralisation",
    "In.Degree.Centralisation",
    "All.Degree.Centralisation",
    "Clustering.coefficient.transitivity",
    "Clustering.Weighted",
    "Region.Homophily",
    "Degree.Assortativity")
  colnames(myDF)<-"Network.Properties"
  return(myDF)
}

