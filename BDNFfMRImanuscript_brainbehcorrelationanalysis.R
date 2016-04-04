library(ppcor)
library(corrplot)
library(Hmisc)
library(ggplot2)

BDNFcordata<-read.csv("/Volumes/pid/BDNF/ROI/BDNFROInew.csv")

str(BDNFcordata)

x<-data.frame(cbind(BDNFcordata$SS_OralComprehension_1,BDNFcordata$SS_OralLanguage_1,	BDNFcordata$SS_PassageComp_1	,BDNFcordata$SS_WordAttack_1	,BDNFcordata$SS_LetterWord,	BDNFcordata$SS_StoryRecall,	BDNFcordata$SS_Spelling,	BDNFcordata$CTOPP_PhonoAwareness_1,	BDNFcordata$CTOPP_PhonoMemory_1	,BDNFcordata$WASI_RS_Vocabulary	,BDNFcordata$WASI_RS_BlockDesign,	BDNFcordata$WASI_RS_Similarities,	BDNFcordata$WASI_RS_Matrix))

colnames(x)<-cbind("Oral Comp.","Oral Lang.", "Passage Comp.","Pseudoword Read.","Letter-Word Decode.",	"Story Recall",	"Spelling",	"Phono. Awareness",	"Phono. Memory"	,"Verbal IQ: Vocab.","Perform. IQ: Block Design","Verbal IQ: Similarities",	"Performance IQ: Matrix Reason.")

y=data.frame(cbind(BDNFcordata$CC.CTCluster1Mean,BDNFcordata$CC.CTCluster2Mean,  BDNFcordata$CC.CTCluster3Mean,BDNFcordata$CC.CTCluster4Mean,BDNFcordata$CC.CTCluster5Mean,BDNFcordata$CC.CTCluster6Mean))

colnames(y)=cbind("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5","Cluster 6")

xx<-as.matrix(x)
behcor<-rcorr(xx)

impute <- function (a, a.impute){ 
  ifelse (is.na(a), a.impute, a)
}

#Impute missing values based on group and age
for (xx in 1:13){
  lm.imp.1 <- lm (x[,xx] ~ BDNFcordata$Age_1 +BDNFcordata$rs6265_2, data=BDNFcordata)
  pred.1 <- predict(lm.imp.1, x)
  x[,xx] <- impute(x[,xx], pred.1)
}

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

col<- colorRampPalette(c("black", "gret")
corrplot(cors, method="color",p.mat = pvals, sig.level = 0.05, insig = "blank",tl.col="black",cl.lim=c(-0.1,0.35),is.corr=FALSE)

behcor$P[is.na(behcor$P)] <- 1
corrplot(behcor$r, method="color",p.mat = behcor$P, sig.level = 0.004, insig = "blank",tl.col="black", t1.cex=1, order="hclust")
corrplot(behcor$r, method="color",p.mat = behcor$P, sig.level = 0.01, insig = "blank",tl.col="black")


ggplot(x, aes(x=x$`Oral Comp.`, fill=BDNFcordata$rs6265_2)) + geom_histogram(position="identity",binwidth = 0.25, alpha=0.75)

+ geom_vline(data=data, aes(xintercept=mean(subset(x, data$rs6265_2=="CC")), color="grey"), linetype="dashed") +
  geom_vline(data=data, aes(xintercept=mean(subset(data$MRI_Age, data$rs6265_2=="CT")), color="black"), linetype="dashed") +
  geom_histogram(position="identity",binwidth = 0.25, alpha=0.75) + scale_color_grey() + theme_classic() +
  scale_fill_manual(values=c("#999999", "black")) + labs(x="Age (years)", y = "Number of Participants") 
