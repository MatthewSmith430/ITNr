N<-igraph::vcount(gs)
m<-igraph::ecount(gs)
cl<-vertex_attr(gs,attrname)
classes<-unique(cl)
icl<-match(cl,classes)
br<-select(TOTAL_DF,Coordinator,Consultant,
           Representative,Gatekeeper,
           Liaison,Total)
br<-as.matrix(br)
gbr<-apply(br,2,sum)

d<-m/(N*(N-1))
clid<-unique(cl)         #Count the class memberships
n<-vector()
for(i in clid)
  n<-c(n,sum(cl==i))
n<-as.double(n)          #This shouldn't be needed, but R will generate
N<-as.double(N)          #integer overflows unless we coerce to double!
ebr<-matrix(0,length(clid),6)
vbr<-matrix(0,length(clid),6)
for(i in 1:length(clid)){  #Compute moments by broker's class
  #Type 1: Within-group (wI)
  ebr[i,1]<-d^2*(1-d)*(n[i]-1)*(n[i]-2)
  vbr[i,1]<-ebr[i,1]*(1-d^2*(1-d))+2*(n[i]-1)*(n[i]-2)*(n[i]-3)*d^3*(1-d)^3
  #Type 2: Itinerant (WO)
  ebr[i,2]<-d^2*(1-d)*sum(n[-i]*(n[-i]-1))
  vbr[i,2]<-ebr[i,2]*(1-d^2*(1-d))+ 2*sum(n[-i]*(n[-i]-1)*(n[-i]-2))*d^3*(1-d)^3
  #Type 3: Representative (bIO)
  ebr[i,3]<-d^2*(1-d)*(N-n[i])*(n[i]-1)
  vbr[i,3]<-ebr[i,3]*(1-d^2*(1-d))+ 2*((n[i]-1)*choose(N-n[i],2)+(N-n[i])*choose(n[i]-1,2))*d^3*(1-d)^3
  #Type 4: Gatekeeping (bOI)
  ebr[i,4]<-ebr[i,3]
  vbr[i,4]<-vbr[i,3]
  #Type 5: Liason (bO)
  ebr[i,5]<-d^2*(1-d)*(sum((n[-i])%o%(n[-i]))-sum(n[-i]^2))
  vbr[i,5]<-ebr[i,5]*(1-d^2*(1-d))+ 4*sum(n[-i]*choose(N-n[-i]-n[i],2)*d^3*(1-d)^3)
  #Total
  ebr[i,6]<-d^2*(1-d)*(N-1)*(N-2)
  vbr[i,6]<-ebr[i,6]*(1-d^2*(1-d))+2*(N-1)*(N-2)*(N-3)*d^3*(1-d)^3
}
br.exp<-vector()
br.sd<-vector()
for(i in 1:N){
  temp<-match(cl[i],clid)
  br.exp<-rbind(br.exp,ebr[temp,])
  br.sd<-rbind(br.sd,sqrt(vbr[temp,]))
}
br.z<-(br-br.exp)/br.sd
egbr<-vector()                     #Global expections/variances
vgbr<-vector()
#Type 1: Within-group (wI)
egbr[1]<-d^2*(1-d)*sum(n*(n-1)*(n-2))
vgbr[1]<-egbr[1]*(1-d^2*(1-d))+
  sum(n*(n-1)*(n-2)*(((4*n-10)*d^3*(1-d)^3)-(4*(n-3)*d^4*(1-d)^2)+
                       ((n-3)*d^5*(1-d))))
#Type 2: Itinerant (WO)
egbr[2]<-d^2*(1-d)*sum(n*(N-n)*(n-1))
vgbr[2]<-egbr[2]*(1-d^2*(1-d))+
  (sum(outer(n,n,function(x,y){
    x*y*(x-1)*(((2*x+2*y-6)*d^3*(1-d)^3)+
                 ((N-x-1)*d^5*(1-d)))}
    )) -
     sum(n*n*(n-1)*(((4*n-6)*d^3*(1-d)^3)+((N-n-1)*d^5*(1-d)))))
#Type 3: Representative (bIO)
egbr[3]<-d^2*(1-d)*sum(n*(N-n)*(n-1))
vgbr[3]<-egbr[3]*(1-d^2*(1-d))+
  sum(n*(N-n)*(n-1)*(((N-3)*d^3*(1-d)^3)+((n-2)*d^5*(1-d))))
#Type 4: Gatekeeping (bOI)
egbr[4]<-egbr[3]
vgbr[4]<-vgbr[3]
#Type 5: Liason (bO)
egbr[5]<- d^2*(1-d)*(sum(outer(n,n,function(x,y){
  x*y*(N-x-y)}
  ))-sum(n*n*(N-2*n)))
vgbr[5]<-egbr[5]*(1-d^2*(1-d))
for(i in 1:length(n))
  for(j in 1:length(n))
    for(k in 1:length(n))
      if((i!=j)&&(j!=k)&&(i!=k))
        vgbr[5]<-vgbr[5] + n[i]*n[j]*n[k] * ((4*(N-n[j])-2*(n[i]+n[k]+1))*d^3*(1-d)^3-(4*(N-n[k])-2*(n[i]+n[j]+1))*d^4*(1-d)^2+(N-(n[i]+n[k]+1))*d^5*(1-d))
#Total
egbr[6]<-d^2*(1-d)*N*(N-1)*(N-2)
vgbr[6]<-egbr[6]*(1-d^2*(1-d))+ N*(N-1)*(N-2)*
  (((4*N-10)*d^3*(1-d)^3)-(4*(N-3)*d^4*(1-d)^2)+((N-3)*d^5*(1-d)))

#Return the results
br.nam<-c("w_I",#coordinator
          "w_O",#Consultant
          "b_IO",#Representative
          "b_OI",#Gatekeeper
          "b_O", #Liaison
          "t")
colnames(br)<-br.nam
rownames(br)<-igraph::vertex_attr(gs,"name")
colnames(br.exp)<-br.nam
rownames(br.exp)<-igraph::vertex_attr(gs,"name")
colnames(br.sd)<-br.nam
rownames(br.sd)<-igraph::vertex_attr(gs,"name")
colnames(br.z)<-br.nam
rownames(br.z)<-igraph::vertex_attr(gs,"name")
names(gbr)<-br.nam
names(egbr)<-br.nam
names(vgbr)<-br.nam
colnames(ebr)<-br.nam
rownames(ebr)<-clid
colnames(vbr)<-br.nam
rownames(vbr)<-clid
out<-list(raw.nli=br,
          exp.nli=br.exp,
          sd.nli=br.sd,
          z.nli=br.z,
          raw.gli=gbr,
          exp.gli=egbr,
          sd.gli=sqrt(vgbr),
          z.gli=(gbr-egbr)/sqrt(vgbr),
          exp.grp=ebr,
          sd.grp=sqrt(vbr),
          cl=cl,
          clid=clid,n=n,N=N)



class(out)<-"brokerage"
return(out)
