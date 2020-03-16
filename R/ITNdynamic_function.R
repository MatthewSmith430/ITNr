#' @title Dynamic ITN
#'
#' @description This function produces a dynamic network object for ITNs. It cleans and adjusts the individual networks, so they are the same size. This dynamic network object can then be used to create animations, mapping changes over time and to calculate temporal network statistics
#' @param NETlist A list of International Trade Networks (igraph objects)
#' @export
#' @return It returns the Dynamic Network Object
#' @examples\donttest{
#' require(igraph)
#'
#' ##Create a set of random International Trade Networks (igraph objects)
#' ##and add vertex names
#' ITN1<-erdos.renyi.game(75,0.05,directed = TRUE)
#' V(ITN1)$name<-1:vcount(ITN1)
#' ITN2<-erdos.renyi.game(100,0.01,directed = TRUE)
#' V(ITN2)$name<-1:vcount(ITN2)
#' ITN3<-erdos.renyi.game(55,0.1,directed = TRUE)
#' V(ITN3)$name<-1:vcount(ITN3)
#'
#'##Create network list
#'NETlist<-list(ITN1,ITN2,ITN3)
#'
#'##Create Dynamic Network Object
#'
#'ITNdyn<-ITNdynamic(NETlist)
#'}

ITNdynamic<-function(NETlist){
  MATlist<-list()
  for (i in 1:length(NETlist)){
    gs<-NETlist[[i]]
    mat1<-igraph::as_adjacency_matrix(gs)
    mat2<-as.matrix(mat1)
    NAME<-igraph::V(gs)$name
    rownames(mat2)<-NAME
    colnames(mat2)<-NAME
    MATlist[[i]]<-mat2
  }
  NETadjust<-list()
  for (p in 1:length(MATlist)){
    NETadjust[[p]]<-ITNadjust(MATlist,p)
  }

  #need to create list of networks
  SNAlist<-list()
  for (i in 1:length(NETadjust)){
    MAT<-NETadjust[[i]]
    NET<-network::as.network(MAT)
    SNAlist[[i]]<-NET
  }

  DynamicNetList<-networkDynamic::networkDynamic(network.list=SNAlist)

  network::set.network.attribute(DynamicNetList,'net.obs.period',
                        list(
                          observations=list(c(0,length(SNAlist))),
                          mode="discrete",
                          time.increment=1,
                          time.unit="year"))

  return(DynamicNetList)

}
