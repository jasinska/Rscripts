library(Rmisc)
library(effects)
library(outliers)
library(combinat)
library(pwr)
library(boot)
library(ggm)
library(Hmisc)
library(polycor)
library(ppcor)
library(car)
library(lsmeans)
library(xlsx)

data<-read.xlsx("/Users/kajajasinka/Google Drive/Manuscripts/BDNF Structural/FINALDATA/FINALDATAsept19.xlsx",1)

#divide the data into two groups
dataCC<-subset(data, rs6265_2=="CC")
dataCT<-subset(data, rs6265_2=="CT")

#Replace outliers with NA based on outlier package
for (x in 54:469){
outlier_tf = outlier(dataCT[,x],logical=TRUE)
  find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
  dataCT[,x][find_outlier]<- mean(dataCT[,x])
}
for (x in 54:469){
  outlier_tf = outlier(dataCC[,x],logical=TRUE)
  find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
  dataCC[,x][find_outlier]<- mean(dataCC[,x])
}
data_new<-rbind(dataCC,dataCT)

#permutation testing to get significant P values
lmpermutation<-function(y,x,cov1,cov2,dataset,NR){
  LM_SHUFFLE = rep(NA, NR)
  for(i in 1:NR) {
    xSHUFFLE = sample(x, replace = FALSE)
    LM_SHUFFLE1 = lm(y~cov1+cov2+xSHUFFLE, data=dataset)
    LM_SHUFFLE[i]=LM_SHUFFLE1$coefficients[4]
  }
  LM<-lm(y~cov1+cov2+x, data=dataset)
  LMcoef<-LM$coefficients[4]
  pval<-sum(LM_SHUFFLE >= LMcoef) / NR
  LMsum<-summary(LM)
  AdjMod<-summary(lsmeans(LM, "x"))
  listadjmod<-c(AdjMod)[c("rs6265_2","lsmean", "SE")]
  print(pval)
  print(LMsum)
  print(AdjMod)
  return(rbind(pval,listadjmod$rs6265_2[1],listadjmod$lsmean[1], listadjmod$SE[1],listadjmod$rs6265_2[2],listadjmod$lsmean[2], listadjmod$SE[2]))
}

###THIS DOES ALL SUBCORTICAL LM PERMUTATIONS
lmpermutation(y=data_new$Right_Hippocampus, x=data_new$rs6265_2, cov1=data_new$IntraCranialVol, cov2=data_new$Age_1, dataset=data_new, NR=1000)
lmpermutation(y=data_new$Left_Hippocampus, x=data_new$rs6265_2, cov1=data_new$IntraCranialVol, cov2=data_new$Age_1, dataset=data_new, NR=1000)
lmpermutation(y=data_new$Right_Amygdala, x=data_new$rs6265_2, cov1=data_new$IntraCranialVol, cov2=data_new$Age_1, dataset=data_new, NR=1000)
lmpermutation(y=data_new$Left_Amygdala, x=data_new$rs6265_2, cov1=data_new$IntraCranialVol, cov2=data_new$Age_1, dataset=data_new, NR=1000)
lmpermutation(y=data_new$Right_Caudate, x=data_new$rs6265_2, cov1=data_new$IntraCranialVol, cov2=data_new$Age_1, dataset=data_new, NR=1000)
lmpermutation(y=data_new$Left_Caudate, x=data_new$rs6265_2, cov1=data_new$IntraCranialVol, cov2=data_new$Age_1, dataset=data_new, NR=1000)
lmpermutation(y=data_new$Right_Putamen, x=data_new$rs6265_2, cov1=data_new$IntraCranialVol, cov2=data_new$Age_1, dataset=data_new, NR=1000)
lmpermutation(y=data_new$Left_Putamen, x=data_new$rs6265_2, cov1=data_new$IntraCranialVol, cov2=data_new$Age_1, dataset=data_new, NR=1000)
lmpermutation(y=data_new$Right_Thalamus_Proper, x=data_new$rs6265_2, cov1=data_new$IntraCranialVol, cov2=data_new$Age_1, dataset=data_new, NR=1000)
lmpermutation(y=data_new$Left_Thalamus_Proper, x=data_new$rs6265_2, cov1=data_new$IntraCranialVol, cov2=data_new$Age_1, dataset=data_new, NR=1000)
lmpermutation(y=data_new$Right_Pallidum, x=data_new$rs6265_2, cov1=data_new$IntraCranialVol, cov2=data_new$Age_1, dataset=data_new, NR=1000)
lmpermutation(y=data_new$Left_Pallidum, x=data_new$rs6265_2, cov1=data_new$IntraCranialVol, cov2=data_new$Age_1, dataset=data_new, NR=1000)

##now use only columns that correspodnto wm rh lh and gm rh lh (to report) and thickness (just to check it out)
whitematter <-matrix(data=NA, ncol=(325-257), nrow=1)
for (aa in 258:325){
  aaa <-lmpermutation(y=data_new[,aa], x=data_new$rs6265_2, cov1=data_new$IntraCranialVol, cov2=data_new$Age_1, dataset=data_new, NR=1000)
  whitematter[,aa-257]<-aaa
}
colnames(whitematter)<-names(data_new[258:325])

lhvolume <-matrix(data=NA, ncol=(365-331), nrow=1)
for (aa in 332:365){
  aaa <-lmpermutation(y=data_new[,aa], x=data_new$rs6265_2, cov1=data_new$IntraCranialVol, cov2=data_new$Age_1, dataset=data_new, NR=1000)
  lhvolume[,aa-331]<-aaa
}
colnames(lhvolume)<-names(data_new[332:365])

rhvolume <-matrix(data=NA, ncol=(434-400), nrow=1)
for (aa in 401:434){
  aaa <-lmpermutation(y=data_new[,aa], x=data_new$rs6265_2, cov1=data_new$IntraCranialVol, cov2=data_new$Age_1, dataset=data_new, NR=1000)
  rhvolume[,aa-400]<-aaa
}
colnames(rhvolume)<-names(data_new[401:434])

lhthickness <-matrix(data=NA, ncol=(400-365), nrow=1)
for (aa in 366:400){
  aaa <-lmpermutation(y=data_new[,aa], x=data_new$rs6265_2, cov1=data_new$IntraCranialVol, cov2=data_new$Age_1, dataset=data_new, NR=1000)
  lhthickness[,aa-365]<-aaa
}
colnames(lhthickness)<-names(data_new[366:400])

rhthickness <-matrix(data=NA, ncol=(469-434), nrow=1)
for (aa in 435:469){
  aaa <-lmpermutation(y=data_new[,aa], x=data_new$rs6265_2, cov1=data_new$IntraCranialVol, cov2=data_new$Age_1, dataset=data_new, NR=1000)
  rhthickness[,aa-434]<-aaa
}
colnames(rhthickness)<-names(data_new[435:469])

#THESE ARE THE SIGN VOLUMES SO NOW FOLLOW-UP

sigvolumes<-cbind(data_new$wm_lh_paracentral,data_new$wm_lh_supramarginal,data_new$wm_rh_cuneus,data_new$wm_rh_middletemporal,data_new$wm_rh_postcentral,data_new$wm_rh_precuneus,data_new$wm_rh_rostralmiddlefrontal,data_new$lh_supramarginal_volume, data_new$Right_Hippocampus)

for (ii in 1:ncol(sigvolumes)){
  LM<-lm(sigvolumes[,ii]~Age_1+IntraCranialVol+rs6265_2, data=data_new)
  AdjMod<-summary(lsmeans(LM, "rs6265_2"))
  print(AdjMod)
}

##power calculations, for each effect size (in excel calculated as M1-M1/pooled sd) - TABLE 3
pwr.t2n.test(n1=49, n2=29, d=0.275, sig.level=.05)
pwr.t2n.test(n1=49, n2=29, d=0.336, sig.level=.05)
pwr.t2n.test(n1=49, n2=29, d=0.303, sig.level=.05)
pwr.t2n.test(n1=49, n2=29, d=0.266, sig.level=.05)
pwr.t2n.test(n1=49, n2=29, d=0.276, sig.level=.05)
pwr.t2n.test(n1=49, n2=29, d=0.267, sig.level=.05)

#power calculations for TABLE4
pwr.t2n.test(n1=49, n2=29, d=0.427, sig.level=.05)
pwr.t2n.test(n1=49, n2=29, d=0.591, sig.level=.05)
pwr.t2n.test(n1=49, n2=29, d=0.525, sig.level=.05)
pwr.t2n.test(n1=49, n2=29, d=0.399, sig.level=.05)
pwr.t2n.test(n1=49, n2=29, d=0.461, sig.level=.05)
pwr.t2n.test(n1=49, n2=29, d=0.430, sig.level=.05)
pwr.t2n.test(n1=49, n2=29, d=0.579, sig.level=.05)
pwr.t2n.test(n1=49, n2=29, d=0.597, sig.level=.05)
pwr.t2n.test(n1=49, n2=29, d=0.504, sig.level=.05)
##END OF LM

#Just a quick look and see if any beh group differences...and there are none.
behdat<-data.frame(cbind(data_new$AppliedProb,data_new$BasicReading,data_new$Calculation,data_new$Direction,data_new$LetterWord,data_new$MathFluency,data_new$OralComp,data_new$PassComp,data_new$PicVocab,data_new$ReadFluency,data_new$Spelling,data_new$StoryRecall,data_new$WordAttack,data_new$CTOPP_PhonoAwareness_1,data_new$CTOPP_PhonoMemory_1,data_new$WASI_IQPerformance_1,data_new$WASI_IQVerbal_1))
behlmpermutation<-function(y,x,cov1,cov2,dataset,NR){
  LM_SHUFFLE = rep(NA, NR)
  for(i in 1:NR) {
    xSHUFFLE = sample(x, replace = FALSE)
    LM_SHUFFLE1 = lm(y~cov1+xSHUFFLE, data=dataset)
    LM_SHUFFLE[i]=LM_SHUFFLE1$coefficients[3]
  }
  LM<-lm(y~cov1+x, data=dataset)
  LMcoef<-LM$coefficients[3]
  pval<-sum(LM_SHUFFLE >= LMcoef) / NR
  LMsum<-summary(LM)
  AdjMod<-summary(lsmeans(LM, "x"))
  listadjmod<-c(AdjMod)[c("rs6265_2","lsmean", "SE")]
  print(pval)
  print(LMsum)
  print(AdjMod)
}
beh <-matrix(data=NA, ncol=ncol(behdat), nrow=1)
for (aa in 1:ncol(behdat)){
  behlmpermutation(y=behdat[,aa], x=data_new$rs6265_2, cov1=data_new$Age_1, cov2=data_new$Gender, dataset=data_new, NR=1000)
}
#end of beh test

##START OF CORRELATION
###these are the regions to use, and the beh scores to use, and the partial correlation item
x<-data.frame(cbind(data_new$AppliedProb,data_new$BasicReading,data_new$Calculation,data_new$Direction,data_new$LetterWord,data_new$MathFluency,data_new$OralComp,data_new$PassComp,data_new$PicVocab,data_new$ReadFluency,data_new$Spelling,data_new$StoryRecall,data_new$WordAttack,data_new$CTOPP_PhonoAwareness_1,data_new$CTOPP_PhonoMemory_1,data_new$WASI_IQPerformance_1,data_new$WASI_IQVerbal_1))
y<-data.frame(cbind(data_new$wm_lh_paracentral,data_new$wm_lh_supramarginal,data_new$wm_rh_cuneus,data_new$wm_rh_middletemporal,data_new$wm_rh_postcentral,data_new$wm_rh_precuneus,data_new$wm_rh_rostralmiddlefrontal,data_new$lh_supramarginal_volume, data_new$rh_superiorparietal_area, data_new$rh_rostralmiddlefrontal_area, data_new$lh_parsopercularis_area, data_new$rh_lateraloccipital_thickness, data_new$lh_lateraloccipital_thickness, data_new$lh_paracentral_thickness, data_new$lh_superiortemporal_thickness, data_new$Right_Hippocampus))
z<-cbind(data_new$Right_UnsegmentedWhiteMatter,data_new$Left_UnsegmentedWhiteMatter,data_new$Right_UnsegmentedWhiteMatter,data_new$Right_UnsegmentedWhiteMatter,data_new$Right_UnsegmentedWhiteMatter,data_new$Right_UnsegmentedWhiteMatter,data_new$Right_UnsegmentedWhiteMatter,data_new$lhCortexVol, data_new$rh_WhiteSurfArea_area, data_new$rh_WhiteSurfArea_area, data_new$lh_WhiteSurfArea_area, data_new$rh_MeanThickness_thickness, data_new$lh_MeanThickness_thickness, data_new$lh_MeanThickness_thickness, data_new$lh_MeanThickness_thickness, data_new$IntraCranialVol)

colnames(y)<-c("L. Paracentral WM","L. Supramarginal WM","R. Cuneus WM","R. Middle Temporal WM","R. Postcentral WM","R. Precuneus WM","R. Rostral Middle Frontal WM","L. Supramarginal GM", "R. Superior Parietal Area", "R. Rostral Middle Frontal Area", "L. Pars Opercularis Area", "R. Lateral Occipital Thick.", "L. Lateral Occipital Thick.", "L. Paracentral Thick.", "L. Superior Temporal Thick.", "R. Hippocampus")
colnames(x)<-c("Problem Solving", "Basic Reading", "Calculation", 'Following Directions', "Letter-Word Id.", "Math Fluency", "Oral Comp.", "Passage Comp.", "Picture Vocab.", "Reading Fluency", "Spelling", "Story Recall", "Nonsense Word Read", "Phono. Awareness", "Phono. Memory", "Performance IQ", "Verbal IQ")

#permutation test for partial correlation between matrix x and y, while controlling for z. NR is number of reps.

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

#x and y are matrices for correlating - but z is always ICV
pvals <- matrix(NA, nrow=NCOL(x), ncol=NCOL(y))
for(iii in 1:ncol(x)) for(jjj in 1:ncol(y)) pvals[iii,jjj] <- parcorperm(x[,iii], y[,jjj], z=data_new$IntraCranialVol, NR=1000)
rownames(pvals)<-colnames(x)
colnames(pvals) <- colnames(y)
print(pvals) #prints new p values for correlation between each variable in x and y.

#x and y are matrices for correlating, but z yis flexible
pvals <- matrix(NA, nrow=ncol(x), ncol=ncol(y))
for(iii in 1:ncol(x)) for(jjj in 1:ncol(y)) pvals[iii,jjj] <- parcorperm(x[,iii], y[,jjj], z[,jjj], NR=1000)
rownames(pvals)<-colnames(x)
colnames(pvals) <- colnames(y)
print(pvals) #prints new p values for correlation between each variable in x and y.

#now make a matrix of partial correlations
cors <- matrix(NA, nrow=NCOL(x), ncol=NCOL(y))
for(iii in 1:ncol(x)) for(jjj in 1:ncol(y)) {
  k<-pcor.test(x[,iii], y[,jjj],data_new$IntraCranialVol)
  cors[iii,jjj]<-k$estimate
}
rownames(cors)<-colnames(x)
colnames(cors) <- colnames(y)
print(cors)

#now make a matrix of partial correlations, but z is flexible
cors <- matrix(NA, nrow=NCOL(x), ncol=NCOL(y))
for(iii in 1:ncol(x)) for(jjj in 1:ncol(y)) {
  k<-pcor.test(x[,iii], y[,jjj],z[,jjj])
  cors[iii,jjj]<-k$estimate
}
rownames(cors)<-colnames(x)
colnames(cors) <- colnames(y)
print(cors)

write.csv(cors,"/Users/kajajasinka/Google Drive/Manuscripts/BDNF Structural/FINALBRAINBEHANALYSIS/finalcorrelationsSEPT20.csv" )
write.csv(pvals,"/Users/kajajasinka/Google Drive/Manuscripts/BDNF Structural/FINALBRAINBEHANALYSIS/finalcorrelationpvaluesSEPT20.csv" )

corrplot(cors, method='color', tl.srt=45, tl.col="black", tl.cex=0.7, p.mat=pvals, sig.level=.05, insig="blank")
