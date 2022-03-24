#' @title make_trade_network
#'
#' @description This function takes (import) trade data and cleans it and transforms it into a network.
#' This function can be applied to trade data downloaded from UN Comtrade (download csv and read into R as a dataframe), or any other trade data. You just make sure it has the following column names:
#' reporter_iso, partner_iso and edge_weight. Some dataformats may have different names. Also - it is important to note that this function is for import data.
#' @param DF Dataframe of trade data downloaded (potentially using the comtradr package)
#' @param threshold Apply a threshold - TRUE, Extract the backbone - FALSE
#' @param cutoff Threshold - cutoff level, Backbone - significance level
#' @export
#' @return International Trade Network - igraph object

make_trade_network<-function(DF,threshold,cutoff){

  H<-stats::aggregate(edge_weight~reporter_iso+partner_iso, DF, sum)

  Sender<-as.vector(H[,"partner_iso"])
  Sender<- gsub('SER', 'SRB', Sender)
  Sender<- gsub('TMP', 'TLS', Sender)
  Sender<- gsub('ZAR', 'COD', Sender)
  Sender<- gsub('ROM', 'ROU', Sender)
  Sender<- gsub('SUD', 'SDN', Sender)
  Sender<- gsub('MNT', 'MNE', Sender)

  Receiver<-as.vector(H[,"reporter_iso"])
  Receiver<- gsub('SER', 'SRB', Receiver)
  Receiver<- gsub('TMP', 'TLS', Receiver)
  Receiver<- gsub('ZAR', 'COD', Receiver)
  Receiver<- gsub('ROM', 'ROU', Receiver)
  Receiver<- gsub('SUD', 'SDN', Receiver)
  Receiver<- gsub('MNT', 'MNE', Receiver)

  VAL<-H[,"edge_weight"]
  VAL<-as.numeric(VAL)
  FULLel<-as.data.frame(cbind(Sender,Receiver,VAL),stringsAsFactors = FALSE)
  FULLel$VAL<-as.numeric(FULLel$VAL)

  WDIDataSeries<-WDI::WDI_data
  WDICountryInfo<-WDIDataSeries$country
  WD<-as.data.frame(WDICountryInfo)


  COUNTRYlist<-WDICountryInfo[,"iso3c"]
  REGIONlist<-WDICountryInfo[,"region"]
  INCOMElist<-WDICountryInfo[,"income"]
  CountryRegion<-cbind(COUNTRYlist,REGIONlist)
  CountryIncome<-cbind(COUNTRYlist,INCOMElist)
  AggReg<-c("All","EUN","UNS","OAS","FRE",
            "SPE","VAT","UMI","ATA","PCN","AIA","COK",
            "SHN","MSR","NIU",
            "BES","BLM","BUN","BVT","CCK","CXR","FLK",#Small regions
            "HMD","IOT","NFK","SGS","TKL", #small regions
            "ESH","SPM","ATF"
  )
  AggRegMat<-matrix("Aggregates",length(AggReg),2)
  AggRegMat[,1]<-AggReg
  CountryRegion<-rbind(CountryRegion,AggRegMat)
  CountryIncome<-rbind(CountryIncome,AggRegMat)

  AggregatesList<-subset(CountryRegion, REGIONlist %in% "Aggregates")
  TotalCountryExports<-subset(FULLel,Receiver %in% "All")
  AllAllTotal<-as.matrix(subset(TotalCountryExports,Sender %in% "All"))
  GrandTotal<-as.numeric(AllAllTotal[,3])
  if (is.numeric(isEmpty(GrandTotal))==FALSE){
    GrandTotal<-sum(VAL)
  }else GrandTotal<-GrandTotal
  Share<-list()
  for (i in 1:length(VAL)){
    Share[[i]]<-(VAL[i]/GrandTotal)*100
  }
  Share <-plyr::ldply(Share, data.frame)
  Share<-dplyr::as_tibble(Share)
  colnames(Share)<-"Share"
  FULLel<-cbind(FULLel,Share)

  G1<-igraph::graph_from_data_frame(FULLel,direct=TRUE)
  igraph::E(G1)$weight<-FULLel[,4]
  igraph::V(G1)$id<-igraph::V(G1)$name

  CountryNames<-igraph::V(G1)$name
  NotCovered<-subset(CountryNames,!(CountryNames %in% CountryRegion[,1]))

  mm<-matrix("NA",length(NotCovered),2)
  mm[,1]<-NotCovered
  CountryRegion2<-rbind(CountryRegion,mm)
  CountryIncome2<-rbind(CountryIncome,mm)


  RegionListAttr<-list()
  IncomeListAttr<-list()

  for (i in 1:length(CountryNames)){
    RegionListAttr[[i]]<-subset(CountryRegion2,COUNTRYlist %in% CountryNames[i])
    IncomeListAttr[[i]]<-subset(CountryIncome2,COUNTRYlist %in% CountryNames[i])

  }
  dfREG<-plyr::ldply(RegionListAttr, data.frame)
  dfREG<-dplyr::as_tibble(dfREG)
  dfINC<-plyr::ldply(IncomeListAttr,data.frame)
  dfINC<-dplyr::as_tibble(dfINC)

  target<-CountryNames
  dfREG<-dfREG[match(target,dfREG$COUNTRYlist),]
  #
  RR<-unlist(dfREG[,2])
  RR2 <- as.factor(RR)
  H<-as.character(dfREG$REGIONlist)

  dfINC<-dfINC[match(target,dfINC$COUNTRYlist),]
  KK<-unlist(dfINC[,2])
  KK2<-as.factor(KK)
  U<-as.character(dfINC$INCOMElist)

  A<-levels(RR2)
  B<-1:length(A)
  KEY<-cbind(A,B)
  Ainc<-levels(KK2)
  Binc<-1:length(Ainc)

  IH<-fastmatch::fmatch(KK,Ainc)
  CH<-fastmatch::fmatch(RR, A)
  igraph::V(G1)$regionNAME<-H
  igraph::V(G1)$region<-CH
  igraph::V(G1)$income<-IH

  #Delete Aggregated Vertices etc
  KEY2<-as.data.frame(KEY)
  NAcheck<-"NA" %in% KEY2$A
  Aggrow<-as.data.frame(KEY2[KEY2$A=="Aggregates", ])
  AggNumber<-as.numeric(as.vector(Aggrow$B))
  if (NAcheck==TRUE){
    NArow<-as.data.frame(KEY2[KEY2$A=="NA", ])
    NANumber<-as.numeric(as.vector(NArow$B))
    G2<-igraph::delete.vertices(G1, which(igraph::V(G1)$region==NANumber))
    G3<-igraph::delete.vertices(G2, which(igraph::V(G2)$region==AggNumber))

  } else G3<-G1

  #Apply the threshold/backbone
  if(threshold==TRUE){
    G4<-igraph::delete.edges(G3,which(igraph::E(G3)$weight<cutoff))
  }  else {
    G4<-get.backbone(G3,cutoff,TRUE)
  }
  G5<-igraph::delete.vertices(G4, which(igraph::degree(G4)==0))
  return(G5)
}

