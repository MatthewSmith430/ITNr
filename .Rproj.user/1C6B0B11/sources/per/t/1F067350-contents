#' @title Core-Periphery for Weighted Networks
#'
#' @description This function implements rich club core-periphery algorithm
#' (Ma & Mondragón, 2015) to identify members of the core and periphery in weighted networks
#' @param gs International Trade Network - igraph object.
#' Note for networks not produced using ITNr there needs to be a vertex attribute "name" and edge attribute "weight"
#' @param type directed/undirected
#' @export
#' @return List - 1.)igraph object with core-periphery results added as a node attribute.
#' 2.) Dataframe of core-periphery results.
#' @references Ma A, Mondragón RJ (2015) Rich-Cores in Networks. PLoS ONE 10(3): e0119678. https://doi.org/10.1371/journal.pone.0119678
#' @examples
#' require(igraph)
#' ##Create random International Trade Network (igraph object)
#' ITN<-erdos.renyi.game(50,0.05,directed = TRUE)
#'
#' ##Add edge weights
#' E(ITN)$weight<-runif(ecount(ITN), 0, 1)
#'
#' ##Add vertex names
#' V(ITN)$name<-1:vcount(ITN)
#'
#' ##Implement core-periphery algorithm
#' ITNcp<-core_periphery_weighted(ITN,"directed")
core_periphery_weighted<-function(gs,type){
  if (type=="directed"){
    VERT<-igraph::get.data.frame(gs,"vertices")
    MINEL<-min(igraph::E(gs)$weight)
    igraph::E(gs)$weight<-igraph::E(gs)$weight/MINEL
    CENT<-ITNcentrality(gs)
    DF<-merge(VERT,CENT,by.x="name",by.y="NAMES")
    DF<-dplyr::filter(DF,DF$Binary.Degree.All!=0)
    df1<-igraph::get.data.frame(gs)
    RESlist<-list()
    for (i in 1:length(DF$name)){
      ego_node<-DF$name[[i]]
      ego_node<-as.character(ego_node)

      df1a<-dplyr::filter(df1,df1$from==ego_node)
      df1b<-dplyr::filter(df1,df1$to==ego_node)
      df2<-rbind(df1a,df1b)

      neigh<-igraph::graph.data.frame(df2)

      INel<-igraph::incident_edges(neigh, ego_node, mode = "in")
      OUTel<-igraph::incident_edges(neigh, ego_node, mode = "out")
      neighOUT<-igraph::delete.edges(neigh,INel[[1]])
      neighIN<-igraph::delete.edges(neigh,OUTel[[1]])

      igraph::V(neighOUT)$degree<-igraph::degree(neighOUT)
      igraph::V(neighIN)$degree<-igraph::degree(neighIN)

      neighOUT<-igraph::delete.vertices(neighOUT,
                                        igraph::V(neighOUT)$name[
                                          igraph::V(neighOUT)$degree == 0
                                          ])

      neighIN<-igraph::delete.vertices(neighIN,
                                        igraph::V(neighIN)$name[
                                          igraph::V(neighIN)$degree == 0
                                          ])

      STRENGTHnode<-dplyr::filter(DF,DF$name==DF$name[[i]])

      STRENGTHneighbour<-DF[DF$name%in%igraph::V(neigh)$name,]
      STRENGTHneighbourOUT<-DF[DF$name%in%igraph::V(neighOUT)$name,]
      STRENGTHneighbourIN<-DF[DF$name%in%igraph::V(neighIN)$name,]

      OUTstrength<-dplyr::filter(STRENGTHneighbourOUT,
                                 STRENGTHneighbourOUT$Weighted.Out.Degree>=
                                   STRENGTHnode$Weighted.Out.Degree)
      OUTlist<-unique(c(OUTstrength$name,ego_node))
      OUTlist<-as.character(OUTlist)
      if (igraph::vcount(neighOUT)>0){
        filter_out<-igraph::induced_subgraph(neighOUT,vids=OUTlist)
        sigma_out<-sum(igraph::E(filter_out)$weight)
      } else sigma_out<-0

      INstrength<-dplyr::filter(STRENGTHneighbourIN,
                                STRENGTHneighbourIN$Weighted.In.Degree>=
                                  STRENGTHnode$Weighted.In.Degree)
      INlist<-unique(c(INstrength$name,ego_node))
      INlist<-as.character(INlist)
      if (igraph::vcount(neighIN)>0){
        filter_in<-igraph::induced_subgraph(neighIN,vids=INlist)
        sigma_in<-sum(igraph::E(filter_in)$weight)
      } else sigma_in<-0


      sigma_all<-sigma_in+sigma_out

      RESdf<-data.frame(name=ego_node,
                        sigma_in=sigma_in,
                        sigma_out=sigma_out,
                        sigma_all=sigma_all)
      RESlist[[i]]<-RESdf
    }
    RESULT<-plyr::ldply(RESlist,data.frame)
    RESULT<-dplyr::as_data_frame(RESULT)
    RANK<-igraph::vcount(gs)+1-rank(DF$Weighted.Degree.All)
    RANKout<-igraph::vcount(gs)+1-rank(DF$Weighted.Out.Degree)
    RANKin<-igraph::vcount(gs)+1-rank(DF$Weighted.In.Degree)
    RESULT_rank<-cbind(RESULT,RANK,RANKout,RANKin)

    in_thres<-dplyr::filter(RESULT_rank,
                            RESULT_rank$sigma_in==max(RESULT_rank$sigma_in))[,7]

    out_thres<-dplyr::filter(RESULT_rank,
                             RESULT_rank$sigma_out==max(RESULT_rank$sigma_out))[,6]

    all_thres<-dplyr::filter(RESULT_rank,
                             RESULT_rank$sigma_all==max(RESULT_rank$sigma_all))[,5]
    in_cp<- RANKin
    in_cp[in_cp <= in_thres] <- 1
    in_cp[in_cp > in_thres] <- 0

    out_cp<- RANKout
    out_cp[out_cp <= out_thres] <- 1
    out_cp[out_cp > out_thres] <- 0

    all_cp<- RANK
    all_cp[all_cp <= all_thres] <- 1
    all_cp[all_cp > all_thres] <- 0

    CP_RESULTS_DF<-cbind(RESULT_rank,in_cp,out_cp,all_cp)

    NAMES_NODES<-igraph::V(gs)$name
    NAMES_NODES<-as.data.frame(NAMES_NODES,stringsAsFactors=FALSE)
    colnames(NAMES_NODES)<-"id"

    CP_RESULTS_DF2<-merge(NAMES_NODES,CP_RESULTS_DF,
                          by.x="id",by.y="name",all.x=TRUE)
    CP_RESULTS_DF2[is.na(CP_RESULTS_DF2)]<-0

    igraph::V(gs)$in_cp<-CP_RESULTS_DF2$in_cp
    igraph::V(gs)$out_cp<-CP_RESULTS_DF2$out_cp
    igraph::V(gs)$all_cp<-CP_RESULTS_DF2$all_cp

    CP_RESULTS<-list(gs,CP_RESULTS_DF2)
    return(CP_RESULTS)
  }
  else if (type=="undirected"){
    VERT<-igraph::get.data.frame(gs,"vertices")
    MINEL<-min(igraph::E(gs)$weight)
    igraph::E(gs)$weight<-igraph::E(gs)$weight/MINEL
    CENT<-ITNcentrality(gs)
    DF<-merge(VERT,CENT,by.x="name",by.y="NAMES")
    DF<-dplyr::filter(DF,DF$Binary.Degree.All!=0)
    df1<-igraph::get.data.frame(gs)
    RESlist<-list()
    for (i in 1:length(DF$name)){
      ego_node<-DF$name[[i]]
      ego_node<-as.character(ego_node)

      df1a<-dplyr::filter(df1,df1$from==ego_node)
      df1b<-dplyr::filter(df1,df1$to==ego_node)
      df2<-rbind(df1a,df1b)

      neigh<-igraph::graph.data.frame(df2)

      STRENGTHnode<-dplyr::filter(DF,DF$name==DF$name[[i]])

      STRENGTHneighbour<-DF[DF$name%in%igraph::V(neigh)$name,]

      ALLstrength<-dplyr::filter(STRENGTHneighbour,
                                 STRENGTHneighbour$Weighted.Degree.All>=
                                   STRENGTHnode$Weighted.Degree.All)

      ALLlist<-unique(c(ALLstrength$name,ego_node))

      filter_all<-igraph::induced_subgraph(neigh,vids=ALLlist)
      sigma_all<-sum(igraph::E(filter_all)$weight)

      RESdf<-data.frame(name=ego_node,
                        sigma_all=sigma_all)
      RESlist[[i]]<-RESdf
    }
    RESULT<-plyr::ldply(RESlist,data.frame)
    RESULT<-dplyr::as_data_frame(RESULT)
    RANK<-igraph::vcount(gs)+1-rank(DF$Weighted.Degree.All)
    RESULT_rank<-cbind(RESULT,RANK)

    all_thres<-dplyr::filter(RESULT_rank,
                             RESULT_rank$sigma_all==max(RESULT_rank$sigma_all))[,3]

    all_cp<- RANK
    all_cp[all_cp <= all_thres] <- 1
    all_cp[all_cp > all_thres] <- 0

    CP_RESULTS_DF<-cbind(RESULT_rank,all_cp)

    NAMES_NODES<-igraph::V(gs)$name
    NAMES_NODES<-as.data.frame(NAMES_NODES,stringsAsFactors=FALSE)
    colnames(NAMES_NODES)<-"id"

    CP_RESULTS_DF2<-merge(NAMES_NODES,CP_RESULTS_DF,
                          by.x="id",by.y="name",all.x=TRUE)
    CP_RESULTS_DF2[is.na(CP_RESULTS_DF2)]<-0

    igraph::V(gs)$all_cp<-CP_RESULTS_DF2$all_cp

    CP_RESULTS<-list(gs,CP_RESULTS_DF2)

    return(CP_RESULTS)
  }
  else print("Enter a valid network type")
}
