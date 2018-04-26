#' @title WITS data clean
#'
#' @description This function takes (import) trade data downloaded from WITS, cleans it and transforms it into a network.
#' Adding a number of country level attributes to nodes in the network, including: regional partition, GDP, GDP per capita, GDP growth and FDI.
#' @param CSVfile WITS csv file
#' @param YEAR Year
#' @param threshold Apply a threshold - TRUE, Extract the backbone - FALSE
#' @param cutoff Threshold - cutoff level, Backbone - significance level
#' @export
#' @return International Trade Network - igraph object
#' @examples\donttest{
#' ##Create an igraph object from international
#' ##trade data downloaded from WITS
#'
#' ##Applies a threhold
#' ##only retains ties that are at least
#' ## 0.01% of total trade
#'
#' ITN<-WITSclean("WITS_CSV_FILE_NAME.csv",2015,TRUE,0.01)
#'
#' }
WITSclean<-function(CSVfile,YEAR,threshold,cutoff){
  DATAV1<-utils::read.csv(CSVfile)
  DATA<-subset(DATAV1,DATAV1$Year==YEAR)
  Sender<-as.vector(DATA[,"PartnerISO3"])
  Receiver<-as.vector(DATA[,"ReporterISO3"])
  VAL<-DATA[,"TradeValue.in.1000.USD"]
  FULLel<-as.data.frame(cbind(Sender,Receiver,VAL))
  WDIDataSeries<-WDI::WDI_data
  WDICountryInfo<-WDIDataSeries$country
  WD<-as.data.frame(WDICountryInfo)

  COUNTRYlist<-WDICountryInfo[,"iso3c"]
  REGIONlist<-WDICountryInfo[,"region"]
  INCOMElist<-WDICountryInfo[,"income"]
  CountryRegion<-cbind(COUNTRYlist,REGIONlist)
  CountryIncome<-cbind(COUNTRYlist,INCOMElist)
  AggReg<-c("All","EUN","UNS","OAS","FRE","SPE")
  AggRegMat<-matrix("Aggregates",length(AggReg),2)
  AggRegMat[,1]<-AggReg
  CountryRegion<-rbind(CountryRegion,AggRegMat)
  CountryIncome<-rbind(CountryIncome,AggRegMat)

  WDIgdp1<-WDI::WDI(country="all",indicator = "NY.GDP.MKTP.CD", start = YEAR, end=YEAR )
  WDIgdp1<-as.data.frame(WDIgdp1)
  WDIgdp1$iso3<-WD$iso3c[match(WDIgdp1$iso2c,WD$iso2c)]
  WDIgdp2<-cbind(as.vector(WDIgdp1$iso3),as.vector(WDIgdp1$NY.GDP.MKTP.CD))
  colnames(WDIgdp2)<-c("iso3","GDP")
  WDIgdp2<-as.data.frame(WDIgdp2)

  WDIGDPgrowth1<-WDI::WDI(country="all",indicator = "NY.GDP.MKTP.KD.ZG", start = YEAR, end=YEAR )
  WDIGDPgrowth1<-as.data.frame(WDIGDPgrowth1)
  WDIGDPgrowth1$iso3<-WD$iso3c[match(WDIGDPgrowth1$iso2c,WD$iso2c)]
  WDIGDPgrowth2<-cbind(as.vector(WDIGDPgrowth1$iso3),as.vector(WDIGDPgrowth1$NY.GDP.MKTP.KD.ZG))
  colnames(WDIGDPgrowth2)<-c("iso3","GDPgrowth")
  WDIGDPgrowth2<-as.data.frame(WDIGDPgrowth2)

  WDIGDPPC1<-WDI::WDI(country="all",indicator = "NY.GDP.MKTP.PP.CD", start = YEAR, end=YEAR )
  WDIGDPPC1<-as.data.frame(WDIGDPPC1)
  WDIGDPPC1$iso3<-WD$iso3c[match(WDIGDPPC1$iso2c,WD$iso2c)]
  WDIGDPPC2<-cbind(as.vector(WDIGDPPC1$iso3),as.vector(WDIGDPPC1$NY.GDP.MKTP.PP.CD))
  colnames(WDIGDPPC2)<-c("iso3","GDPPC")
  WDIGDPPC2<-as.data.frame(WDIGDPPC2)

  WDIFDI1<-WDI::WDI(country="all",indicator = "BN.KLT.DINV.CD", start = YEAR, end=YEAR )
  WDIFDI1<-as.data.frame(WDIFDI1)
  WDIFDI1$iso3<-WD$iso3c[match(WDIFDI1$iso2c,WD$iso2c)]
  WDIFDI2<-cbind(as.vector(WDIFDI1$iso3),as.vector(WDIFDI1$BN.KLT.DINV.CD))
  colnames(WDIFDI2)<-c("iso3","FDI")
  WDIFDI2<-as.data.frame(WDIFDI2)

  AggregatesList<-subset(CountryRegion, REGIONlist %in% "Aggregates")
  TotalCountryExports<-subset(FULLel,Receiver %in% "All")
  AllAllTotal<-as.matrix(subset(TotalCountryExports,Sender %in% "All"))
  GrandTotal<-as.numeric(AllAllTotal[,3])
  Share<-list()
  for (i in 1:length(VAL)){
    Share[[i]]<-(VAL[i]/GrandTotal)*100
  }
  Share <-plyr::ldply(Share, data.frame)
  colnames(Share)<-"Share"
  FULLel<-cbind(FULLel,Share)

  G1<-igraph::graph_from_data_frame(FULLel,direct=TRUE)
  igraph::E(G1)$weight<-FULLel[,4]
  igraph::V(G1)$id<-igraph::V(G1)$name

  CountryNames<-igraph::V(G1)$name
  NotCovered<-subset(CountryNames,!(CountryNames %in% CountryRegion[,1]))
  NotCoveredWDI<-subset(CountryNames,!(CountryNames %in% WDIgdp2$iso3))
  mm<-matrix("NA",length(NotCovered),2)
  mm[,1]<-NotCovered
  CountryRegion2<-rbind(CountryRegion,mm)
  CountryIncome2<-rbind(CountryIncome,mm)
  mm2<-matrix("NA",length(NotCoveredWDI),2)
  mm2[,1]<-NotCoveredWDI

  colnames(mm2)<-colnames(WDIgdp2)
  WDIgdp3<-rbind(WDIgdp2,mm2)

  #GDPPC
  colnames(mm2)<-colnames(WDIGDPPC2)
  WDIGDPPC3<-rbind(WDIGDPPC2,mm2)

  #GDPgrowth
  colnames(mm2)<-colnames(WDIGDPgrowth2)
  WDIGDPgrowth3<-rbind(WDIGDPgrowth2,mm2)

  #FDI
  colnames(mm2)<-colnames(WDIFDI2)
  WDIFDI3<-rbind(WDIFDI2,mm2)

  RegionListAttr<-list()
  IncomeListAttr<-list()
  GDPListattr<-list()
  GDPPCListattr<-list()
  GDPgrowthListAttr<-list()
  FDIListAttr<-list()

  for (i in 1:length(CountryNames)){
    RegionListAttr[[i]]<-subset(CountryRegion2,COUNTRYlist %in% CountryNames[i])
    IncomeListAttr[[i]]<-subset(CountryIncome2,COUNTRYlist %in% CountryNames[i])
    GDPListattr[[i]]<-subset(WDIgdp3,WDIgdp3$iso3 %in% CountryNames[i])
    GDPPCListattr[[i]]<-subset(WDIGDPPC3,WDIGDPgrowth3$iso3 %in% CountryNames[i])
    GDPgrowthListAttr[[i]]<-subset(WDIGDPgrowth3,WDIGDPgrowth3$iso3 %in% CountryNames[i])
    FDIListAttr[[i]]<-subset(WDIFDI3,WDIFDI3$iso3 %in% CountryNames[i])
  }
  dfREG<-plyr::ldply(RegionListAttr, data.frame)
  dfINC<-plyr::ldply(IncomeListAttr,data.frame)
  dfGDP<-plyr::ldply(GDPListattr,data.frame)
  dfGDPPC<-plyr::ldply(GDPPCListattr, data.frame)
  dfGDPgrowth<-plyr::ldply(GDPgrowthListAttr, data.frame)
  dfFDI<-plyr::ldply(FDIListAttr, data.frame)

  target<-CountryNames
  dfREG<-dfREG[match(target,dfREG$COUNTRYlist),]
  RR<-as.vector(dfREG[,2])
  RR2<-as.factor(RR)
  H<-as.character(dfREG$REGIONlist)

  dfINC<-dfINC[match(target,dfINC$COUNTRYlist),]
  KK<-as.vector(dfINC[,2])
  KK2<-as.factor(KK)
  U<-as.character(dfINC$INCOMElist)

  A<-levels(RR2)
  B<-1:length(A)
  KEY<-cbind(A,B)
  Ainc<-levels(KK2)
  Binc<-1:length(Ainc)

  dfGDP<-dfGDP[match(target,dfGDP$iso3),]
  dfGDPPC<-dfGDPPC[match(target,dfGDPPC$iso3),]
  dfGDPgrowth<-dfGDPgrowth[match(target,dfGDPgrowth$iso3),]
  dfFDI<-dfFDI[match(target,dfFDI$iso3),]

  GGDP<-as.vector(dfGDP[,2])
  GGDP<-suppressWarnings(as.numeric(GGDP))
  GGDPPC<-as.vector(dfGDPPC[,2])
  GGDPPC<-suppressWarnings(as.numeric(GGDPPC))
  GGDPgrowth<-as.vector(dfGDPgrowth[,2])
  GFDI<-as.vector(dfFDI[,2])

  IH<-fastmatch::fmatch(KK,Ainc)
  CH<-fastmatch::fmatch(RR, A)
  igraph::V(G1)$regionNAME<-H
  igraph::V(G1)$region<-CH
  igraph::V(G1)$income<-IH
  igraph::V(G1)$GDP<-GGDP
  igraph::V(G1)$GDPPC<-GGDPPC
  igraph::V(G1)$logGDP<-log(GGDP)
  igraph::V(G1)$logGDPPC<-log(GGDPPC)
  igraph::V(G1)$GDPgrowth<-GGDPgrowth
  igraph::V(G1)$FDI<-GFDI

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
