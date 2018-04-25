#' @title Adjust ITN
#'
#' @description This function adjusts ITN matrices so they are the same size
#' @param MATlist A list of ITN matrices
#' @param j Element of matrix list to compare with others
#' @export
#' @return Matrix
#' @examples\donttest{
#' ##Create a list of random matrices (of different sizes)
#' ##Labels - letters of alphabet (can represent actor names)
#' mat1<- matrix(round(runif(10*10)), 10, 10)
#' rownames(mat1)<-LETTERS[1:10]
#' colnames(mat1)<-LETTERS[1:10]
#'
#' mat2<- matrix(round(runif(10*10)), 10, 10)
#' rownames(mat2)<-LETTERS[10:19]
#' colnames(mat2)<-LETTERS[10:19]
#'
#' mat3<- matrix(round(runif(12*12)), 12, 12)
#' rownames(mat3)<-LETTERS[15:26]
#' colnames(mat3)<-LETTERS[15:26]
#'
#' ##Create matrix list
#' MATlist<-list(mat1,mat2,mat3)
#'
#' ##Adjust matrix 1 so that it has additional rows/actors not
#' ##in the original matrix
#'
#' mat1adjust<-ITNadjust(MATlist,1)
#' }


ITNadjust<-function(MATlist,j){
  files2<-MATlist #matrix list
  files2[[j]]<-NULL
  NetMAT<-MATlist[[j]]
  for (z in 1:length(files2)){
    HMAT<-files2[z]
    NetMAT<- xergm.common::adjust(NetMAT, HMAT, remove = FALSE,add=TRUE,value=0,returnlabels = FALSE)
  }
  NetMat<-NetMAT[,sort(colnames(NetMAT))]
  NetMAT<-NetMAT[sort(rownames(NetMAT)),]
  return(NetMAT)
}
