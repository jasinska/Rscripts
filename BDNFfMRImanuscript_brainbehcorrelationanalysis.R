BDNFcordata<-read.csv("/Volumes/pid/BDNF/ROI/BDNFROInew.csv")

str(BDNFcordata)

x<-data.frame(cbind(BDNFcordata[,8:43]))
y<-data.frame(cbind(BDNFcordata[,46:57]))


parcorperm<-function(x,y,z,NR){
  PC = rep(NA, NR)
  for(i in 1:NR) {
    a = sample(x, replace = FALSE)
    b = sample(y, replace = FALSE)
    PC1 = pcor.test(a, b, z)
    PC[i]=PC1$estimate
  }
  R<-pcor.test(x,y,z)
  R<-R$estimate
  sum(PC >= R) / NR
}


cors <- matrix(NA, nrow=NCOL(x), ncol=NCOL(y))
for(iii in 1:ncol(x)) for(jjj in 1:ncol(y)) {
  k<-pcor.test(x[,iii], y[,jjj],BDNFcordata$Age_1)
  cors[iii,jjj]<-k$estimate
}
rownames(cors)<-colnames(x)
colnames(cors) <- colnames(y)
print(cors)


pvals <- matrix(NA, nrow=NCOL(x), ncol=NCOL(y))
for(iii in 1:ncol(x)) for(jjj in 1:ncol(y)) pvals[iii,jjj] <- parcorperm(x[,iii], y[,jjj], z=BDNFcordata$Age_1, NR=1000)
rownames(pvals)<-colnames(x)
colnames(pvals) <- colnames(y)
print(pvals) #prints new p values for correlation between each variable in x and y.


