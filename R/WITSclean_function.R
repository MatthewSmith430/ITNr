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
  DATA<-dplyr::filter(DATAV1,DATAV1$Year==YEAR)
  Sender<-as.vector(DATA[,"PartnerISO3"])
  Sender<- gsub('SER', 'SRB', Sender)
  Sender<- gsub('TMP', 'TLS', Sender)
  Sender<- gsub('ZAR', 'COD', Sender)
  Sender<- gsub('ROM', 'ROU', Sender)
  Sender<- gsub('SUD', 'SDN', Sender)
  Sender<- gsub('MNT', 'MNE', Sender)
  Receiver<-as.vector(DATA[,"ReporterISO3"])
  Receiver<- gsub('SER', 'SRB', Receiver)
  Receiver<- gsub('TMP', 'TLS', Receiver)
  Receiver<- gsub('ZAR', 'COD', Receiver)
  Receiver<- gsub('ROM', 'ROU', Receiver)
  Receiver<- gsub('SUD', 'SDN', Receiver)
  Receiver<- gsub('MNT', 'MNE', Receiver)
  VAL<-DATA[,"TradeValue.in.1000.USD"]

  FULLel<-data.frame(Sender=Sender,
                     Receiver=Receiver,
                     VAL=VAL,stringsAsFactors = FALSE)
  WDIDataSeries<-WDI::WDI_data
  WDICountryInfo<-WDIDataSeries$country
  WD<-as.data.frame(WDICountryInfo,stringsAsFactors = FALSE)

  COUNTRYlist<-WDICountryInfo[,"iso3c"]
  REGIONlist<-WDICountryInfo[,"region"]
  INCOMElist<-WDICountryInfo[,"income"]
  CountryRegion<-cbind(COUNTRYlist,REGIONlist)
  CountryIncome<-cbind(COUNTRYlist,INCOMElist)

  ##List all of the aggregate entities.
  AggReg<-c("All","EUN","UNS","OAS","FRE",
            "SPE","VAT","UMI","ATA","PCN","AIA","COK",
            "SHN","MSR","NIU",
            "BES","BLM","BUN","BVT","CCK","CXR","FLK",#Small regions
            "HMD","IOT","NFK","SGS","TKL",#small regions
            "ESH","SPM","ATF"
            )
  AggRegMat<-matrix("Aggregates",length(AggReg),2)

  AggRegMat[,1]<-AggReg

  CountryRegion<-rbind(CountryRegion,AggRegMat)
  CountryIncome<-rbind(CountryIncome,AggRegMat)

  CR<-as.data.frame(CountryRegion,stringsAsFactors = FALSE)
  ALL_AGG<-dplyr::filter(CR,REGIONlist=="Aggregates")
  ALL_AGG<-ALL_AGG$COUNTRYlist
  ALL_AGG<-as.vector(ALL_AGG)

  WDIgdp1<-WDI::WDI(country="all",indicator = "NY.GDP.PCAP.KD", start = YEAR, end=YEAR )
  WDIgdp1<-as.data.frame(WDIgdp1,stringsAsFactors=FALSE)
  #WDIgdp1<-merge(WD,WDIgdp1,by="iso2c")
  WDIgdp1$iso3<-WD$iso3c[match(WDIgdp1$iso2c,WD$iso2c)]
  WDIgdp2<-cbind(as.vector(WDIgdp1$iso3),
                 as.vector(WDIgdp1$NY.GDP.PCAP.KD))
  colnames(WDIgdp2)<-c("iso3","GDP")
  WDIgdp2<-as.data.frame(WDIgdp2,stringsAsFactors=FALSE)

  WDIGDPgrowth1<-WDI::WDI(country="all",indicator = "NY.GDP.MKTP.KD.ZG", start = YEAR, end=YEAR )
  WDIGDPgrowth1<-as.data.frame(WDIGDPgrowth1,stringsAsFactors=FALSE)
  WDIGDPgrowth1$iso3<-WD$iso3c[match(WDIGDPgrowth1$iso2c,WD$iso2c)]
  WDIGDPgrowth2<-cbind(as.vector(WDIGDPgrowth1$iso3),
                       as.vector(WDIGDPgrowth1$NY.GDP.MKTP.KD.ZG))
  colnames(WDIGDPgrowth2)<-c("iso3","GDPgrowth")
  WDIGDPgrowth2<-as.data.frame(WDIGDPgrowth2,stringsAsFactors=FALSE)

  WDIGDPPC1<-WDI::WDI(country="all",indicator = "NY.GDP.PCAP.PP.KD", start = YEAR, end=YEAR )
  WDIGDPPC1<-as.data.frame(WDIGDPPC1,stringsAsFactors=FALSE)
  WDIGDPPC1$iso3<-WD$iso3c[match(WDIGDPPC1$iso2c,WD$iso2c)]
  WDIGDPPC2<-cbind(as.vector(WDIGDPPC1$iso3),as.vector(WDIGDPPC1$NY.GDP.PCAP.PP.KD))
  colnames(WDIGDPPC2)<-c("iso3","GDPPC")
  WDIGDPPC2<-as.data.frame(WDIGDPPC2,stringsAsFactors=FALSE)

  WDIFDI1<-WDI::WDI(country="all",indicator = "BN.KLT.DINV.CD", start = YEAR, end=YEAR )
  WDIFDI1<-as.data.frame(WDIFDI1,stringsAsFactors=FALSE)
  WDIFDI1$iso3<-WD$iso3c[match(WDIFDI1$iso2c,WD$iso2c)]
  WDIFDI2<-cbind(as.vector(WDIFDI1$iso3),as.vector(WDIFDI1$BN.KLT.DINV.CD))
  colnames(WDIFDI2)<-c("iso3","FDI")
  WDIFDI2<-as.data.frame(WDIFDI2,stringsAsFactors=FALSE)

  ##Country Region is all countries and their regions
  # This gives a full list of countries that are aggregates
  #AggregatesList<-subset(CountryRegion, REGIONlist %in% "Aggregates")


  ##Total Exports per country
  TotalCountryExports<-subset(FULLel,Receiver %in% "All")

  #Grand Total Exports
  AllAllTotal<-as.matrix(subset(TotalCountryExports,Sender %in% "All"))
  GrandTotal<-as.numeric(AllAllTotal[,3])

  Share<-list()
  for (i in 1:length(VAL)){
    Share[[i]]<-(VAL[i]/GrandTotal)*100
  }

  Share <-plyr::ldply(Share, data.frame)
  #Share<-dplyr::as_data_frame(Share)
  colnames(Share)<-"Share"
  FULLel<-cbind(FULLel,Share)

  G1<-igraph::graph_from_data_frame(FULLel,direct=TRUE)
  igraph::E(G1)$weight<-FULLel[,4]
  igraph::V(G1)$id<-igraph::V(G1)$name

  CountryNames<-igraph::V(G1)$name

  ## Adds an NA to any countries not cover in WDI data
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
  #dfREG<-dplyr::as_data_frame(dfREG)
  dfINC<-plyr::ldply(IncomeListAttr,data.frame)
  #dfINC<-dplyr::as_data_frame(dfINC)
  dfGDP<-plyr::ldply(GDPListattr,data.frame)
  #dfGDP<-dplyr::as_data_frame(dfGDP)
  dfGDPPC<-plyr::ldply(GDPPCListattr, data.frame)
  #dfGDPPC<-dplyr::as_data_frame(dfGDPPC)
  dfGDPgrowth<-plyr::ldply(GDPgrowthListAttr, data.frame)
  #dfGDPgrowth<-dplyr::as_data_frame(dfGDPgrowth)
  dfFDI<-plyr::ldply(FDIListAttr, data.frame)
  #dfFDI<-dplyr::as_data_frame(dfFDI)

  target<-CountryNames

  dfREG<-dfREG[match(target,dfREG$COUNTRYlist),]
  RR<-as.vector(dfREG[,2])
  RR1<-c(RR)
  RR1<-unlist(RR1)
  RR2<-as.factor(RR1)
  H<-as.character(dfREG$REGIONlist)

  dfINC<-dfINC[match(target,dfINC$COUNTRYlist),]
  KK<-as.vector(dfINC[,2])
  KK1<-c(KK)
  KK1<-unlist(KK1)
  KK2<-as.factor(KK1)
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
  #KEY2<-as.data.frame(KEY,stringsAsFactors=FALSE)
  #NAcheck<-"NA" %in% KEY2$A
  #Aggrow<-as.data.frame(KEY2[KEY2$A=="Aggregates", ])
  #AggNumber<-as.numeric(as.vector(Aggrow$B))

  DEL_LIST<-subset(igraph::V(G1)$name,
                   igraph::V(G1)$name %in% ALL_AGG)


  G1<-igraph::delete_vertices(G1,c(DEL_LIST,"All"))
  G3<-igraph::delete.vertices(G1, which(is.na(igraph::V(G1)$region)))
  #if (NAcheck==TRUE){
  #  NArow<-as.data.frame(KEY2[KEY2$A=="NA", ])
  #  NANumber<-as.numeric(as.vector(NArow$B))
  #  G2<-igraph::delete.vertices(G1, which(igraph::V(G1)$region==NANumber))
  #  G3<-igraph::delete.vertices(G2, which(igraph::V(G2)$region==AggNumber))
  #  G3<-igraph::delete.vertices(G2, which(is.na(igraph::V(G2)$region)))
  #} else G3<-igraph::delete.vertices(G1, which(is.na(igraph::V(G1)$region)))

  #G3<-igraph::delete.vertices(G2, which(igraph::V(G2)$regionNAME=="Aggregates"))

  BACK_BONE_PREP<-igraph::get.data.frame(G3,what="vertices")

  #Apply the threshold/backbone
  if(threshold==TRUE){
    G4<-igraph::delete.edges(G3,which(igraph::E(G3)$weight<cutoff))
  }  else {
    G4<-get.backbone(G3,cutoff,TRUE)

    BB_ID<-igraph::get.data.frame(G4,what="vertices")
    BB_ATTR<-merge(BB_ID,BACK_BONE_PREP,by.all="name",all.x=TRUE,
                   all.y=FALSE)

    BB_ATTR<-BB_ATTR[order(match(BB_ATTR[, "name"],igraph::V(G4)$name)),]
    igraph::V(G4)$region<-BB_ATTR$region
    igraph::V(G4)$regionNAME<-BB_ATTR$regionNAME
    igraph::V(G4)$income<-BB_ATTR$income
    igraph::V(G4)$GDP<-BB_ATTR$GDP
    igraph::V(G4)$GDPPC<-BB_ATTR$GDPPC
    igraph::V(G4)$logGDP<-BB_ATTR$logGDP
    igraph::V(G4)$logGDPPC<-BB_ATTR$logGDPPC
    igraph::V(G4)$GDPgrowth<-BB_ATTR$GDPgrowth
    igraph::V(G4)$FDI<-BB_ATTR$FDI
    igraph::V(G4)$id<-BB_ATTR$id

  }
  G5<-igraph::delete.vertices(G4, which(igraph::degree(G4)==0))
  EW<-igraph::E(G5)$weight
  EW2<-as.numeric(EW)
  G6<-igraph::delete_edge_attr(G5,"weight")
  igraph::E(G6)$weight<-EW2

  return(G6)
}
