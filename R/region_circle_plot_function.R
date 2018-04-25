#' @title region_circle_plot
#'
#' @description This function creates a chord diagram/circle plot for levels of trade between regional partitions
#' @param gs igraph ITN object (with attributes added)
#' @export
#' @return Circle Plot
#' @examples\donttest{
#' ##Load graph
#' data("ELEnet16")
#'
#' ##Create region circle plot
#' region_circle_plot(ELEnet16)
#' }

region_circle_plot<-function(gs){
  DF<-igraph::get.data.frame(gs)

  DFregion<-igraph::get.data.frame(gs,what="vertices")
  DFmerge<-merge(DF,DFregion,by.x="from",by.y="id")
  DFmerge2<-cbind(DFmerge$from,DFmerge$to,DFmerge$regionNAME,DFmerge$region,DFmerge$Share)
  DFmerge2<-as.data.frame(DFmerge2,stringsAsFactors=FALSE)
  colnames(DFmerge2)<-c("from","to","from_regname","from_reg","Share")
  DFmerge3<-merge(DFmerge2,DFregion,by.x="to",by.y="id")

  DFfin<-cbind(DFmerge3$from,DFmerge3$to,DFmerge3$from_regname,DFmerge3$from_reg,
               DFmerge$regionNAME,DFmerge3$region,DFmerge3$Share)
  DFfin<-as.data.frame(DFfin,stringsAsFactors=FALSE)
  colnames(DFfin)<-c("from","to","from_regname","from_reg",
                     "to_regname","to_reg","Share")
  DFfin$Share<-as.numeric(DFfin$Share)

  fromCOL<-as.vector(DFfin$from_regname)
  regionFROM<-gsub("all income levels", "", fromCOL)
  regionFROM2<-gsub("\\(|\\)", "", regionFROM)

  toCOL<-as.vector(DFfin$to_regname)
  regionTO<-gsub("all income levels", "", toCOL)
  regionTO2<-gsub("\\(|\\)", "", regionTO)

  DFfin$from_regname<-regionFROM2
  DFfin$to_regname<-regionTO2
  BIN<-rep(1,length(DFfin$from))
  DFfin$Binary<-BIN

  aggWEIGHT<-stats::aggregate(DFfin$Share~DFfin$from_regname+DFfin$to_regname, DFfin, sum)
  colnames(aggWEIGHT)<-c("SENDER_REGION","RECEIVER_REGION","SHARE")
  aggBINARY<-stats::aggregate(DFfin$Binary~DFfin$from_regname+DFfin$to_regname, DFfin, sum)
  colnames(aggBINARY)<-c("SENDER_REGION","RECEIVER_REGION","TIE.COUNT")

  GW<-igraph::graph_from_data_frame(aggWEIGHT)
  igraph::E(GW)$weight<-aggWEIGHT$SHARE
  WM<-igraph::as_adjacency_matrix(GW,attr="weight")
  mat<-as.matrix(WM)

  rn <- rownames(mat)

  grid.col<-grDevices::rainbow(length(rn))[rank(rn)]


  circlize::circos.par(cell.padding=c(0,0,0,0), #track.margin=c(0,0.05),
             start.degree = 90, gap.degree =5)

  circlize::chordDiagram(mat, annotationTrack = "grid",
                         transparency = 0.5,
                         grid.col=grid.col,
                         preAllocateTracks = list(track.height = 0.2))

  circlize::circos.trackPlotRegion(track.index = 1,
                                   panel.fun = function(x, y) {
    xlim = circlize::get.cell.meta.data("xlim")
    xplot = circlize::get.cell.meta.data("xplot")
    ylim = circlize::get.cell.meta.data("ylim")
    sector.name = circlize::get.cell.meta.data("sector.index")
    if(abs(xplot[2] - xplot[1]) < 10) {
      circlize::circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                  niceFacing = TRUE, adj = c(0, 0.5),cex=0.55)
    } else {
      circlize::circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                  niceFacing = TRUE, adj = c(0.5, 0),cex=0.6)
    }

  }, bg.border = NA)

  circlize::circos.clear()

}

