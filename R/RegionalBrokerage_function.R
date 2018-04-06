#' @title ITN Gould-Fernandez Brokerage Analysis
#'
#' @description This function conducts Gould-Fernandez Brokerage Analysis for the regional partition
#' @param gs International Trade Network - igraph object
#' @param filename Filename for the Brokerage Analysis
#' @export
#' @return Results Dataframe
#' @references Gould, R. V. and Fernandez, R. M. (1989) Structures of mediation: A formal approach to brokerage in transaction networks, Sociological methodology, 19(1989), pp. 89–126.
#' @examples \dontrun{
#' ##Load graph
#' data("ELEnet16")
#'
#' ##Otherwise download data from WITS and create an
#' ##International Trade Network using WITSclean()
#'
#' ##Calculate Regional Brokerage
#' REGkey<-RegionalBrokerage(ELEnet16,"ELE_REG_BROKERAGE")
#'
#' }
RegionalBrokerage<-function(gs,filename){
  RegBroker<-igraph::V(gs)$region
  exNetwork<-intergraph::asNetwork(gs)
  BrokerageAnalysis<-sna::brokerage(exNetwork,RegBroker)
  BrokerNameSNA<-paste0("Brokerage Analysis_",filename,".txt")
  sink(BrokerNameSNA)
  print(summary(BrokerageAnalysis))
  sink()
  BROKER.raw.nli<-BrokerageAnalysis$raw.nli
  RawColNames<-colnames(BROKER.raw.nli)
  colnames(BROKER.raw.nli)<-c("Coordinator role","Itinerant broker role",
                              "Gatekeeper role","Representative role",
                              "Liaison role","Total")
  BROKERkey<-cbind(RawColNames,c("Coordinator role","Itinerant broker role",
                                 "Gatekeeper role","Representative role",
                                 "Liaison role","Total"))
  colnames(BROKERkey)<-c("R Brokerage Name","Role")
  return(BROKERkey)
}
