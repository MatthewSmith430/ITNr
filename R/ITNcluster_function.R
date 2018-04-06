#' @title ITN Cluster
#'
#' @description This function calculates cluster membership for ITN and compares with regional groupings
#' @param gs International Trade Network - igraph object (with region attribute)
#' @export
#' @return Cluster object containing various cluster memebrship and correlation results
#' @examples\donttest{
#' ##Load ITN
#' data(ELEnet16)
#'
#' ##Cluster Analysis
#' CLU<-ITNcluster(ELEnet16)
#'}

ITNcluster<-function(gs){
  cfg <- igraph::cluster_fast_greedy(igraph::as.undirected(gs))
  mem<-as.vector(igraph::membership(cfg))
  cluster.fast.greedy.mem<-mem
  igraph::V(gs)$cluster.fast.greedy.mem<-mem

  CSC<-igraph::cluster_spinglass(gs)
  CSCmem<-CSC$membership
  spinglass.com<-CSCmem
  igraph::V(gs)$spinglass.com<-CSCmem

  infomap<-igraph::cluster_infomap(gs, e.weights = igraph::E(gs)$weight,
                     v.weights = NULL, nb.trials = 10,
                     modularity = FALSE)
  igraph::V(gs)$infomap<-infomap$membership

  TAB<-igraph::get.data.frame(gs, what = "vertices")

  NAME<-TAB$name
  REG<-TAB$region
  REG<-as.numeric(REG)
  SPIN<-TAB$spinglass.com
  INFOmap<-TAB$infomap
  GREED<-TAB$cluster.fast.greedy.mem
  NG2<-TAB$NewmanGirvan
  mat<-matrix(0, nrow=1,ncol = 3)
  #dfTEST<-data.frame()
  mat[,1]<-GoodmanKruskal::GKtau(REG,SPIN)$tauyx
  mat[,2]<-GoodmanKruskal::GKtau(REG,INFOmap)$tauyx
  mat[,3]<-GoodmanKruskal::GKtau(REG,GREED)$tauyx
  colnames(mat)<-c("RegSpin","RegInfoMap","RegFastGreedy")
  rownames(mat)<-"GoodmanKruskal"
  spinglass.com<-cbind(NAME,SPIN)
  spinglass.com<-as.data.frame(spinglass.com)
  infomap<-cbind(NAME,INFOmap)
  infomap<-as.data.frame(infomap)
  cluster.fast.greedy.mem<-cbind(NAME,GREED)
  cluster.fast.greedy.mem<-as.data.frame(cluster.fast.greedy.mem)
  mat<-as.data.frame(mat)
  RESULTSclu<-list(
    "spinglass.com"=spinglass.com,
    "infomap"=infomap,
    "cluster.fast.greedy.mem"=cluster.fast.greedy.mem,
    "Region.Cluster.Correlation.Matrix"=mat)
  return(RESULTSclu)



}
