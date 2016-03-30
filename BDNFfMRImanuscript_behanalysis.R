library(Rmisc)
library(effects)
library(outliers)


http://stackoverflow.com/questions/23496230/r-how-to-graphically-plot-adjusted-means-se-ci-ancova

data<-read.csv("/Users/kajajasinska/Google Drive/Manuscripts/BDNF/FINAL BEHAVIOURAL/Aug2015FINAL/data.csv")

dataFINAL<-read.csv("/Users/kajajasinka/Google Drive/Manuscripts/BDNF/FINAL BEHAVIOURAL/JULY2015check/drafts/JULY202015.csv")



data_new<-dataFINAL

impute <- function (a, a.impute){ 
  ifelse (is.na(a), a.impute, a)
}

#Replace outliers with NA based on outlier package
for (x in 8:34){
  outlier_tf = outlier(data_new[,x],logical=TRUE)
  find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
  data_new[,x][find_outlier]<- NA
}

#Impute missing values based on group and age
for (x in 8:34){
  lm.imp.1 <- lm (data_new[,x] ~ Age_1 +rs6265_2, data=data_new)
  pred.1 <- predict(lm.imp.1, data_new)
  data_new[,x] <- impute(data_new[,x], pred.1)
}

data_newIQ<-data_new

#Impute missing IQ values based on other IQ metrics
for (x in 35:38){
  lm.imp.1 <- lm(data_newIQ[,x] ~ WASI_IQPerformance_1+WASI_IQVerbal_1, data=data_newIQ)
  pred.1 <- predict(lm.imp.1, data_newIQ)
  data_newIQ[,x] <- impute(data_newIQ[,x], pred.1)
}

for (x in 39:41){
  lm.imp.1 <- lm(data_newIQ[,x] ~ WASI_RS_Vocabulary+WASI_RS_BlockDesign+WASI_RS_Similarities+WASI_RS_Matrix, data=data_newIQ)
  pred.1 <- predict(lm.imp.1, data_newIQ)
  data_newIQ[,x] <- impute(data_newIQ[,x], pred.1)
}

manovamodelFINAL<-manova(cbind(SS_StoryRecall,SS_Spelling,SS_OralLanguage_1,SS_LetterWord,SS_WordAttack_1,SS_PassageComp_1,SS_OralComprehension_1,CTOPP_PhonoAwareness_1,CTOPP_PhonoMemory_1,WASI_RS_Vocabulary,WASI_RS_BlockDesign,WASI_RS_Similarities,WASI_RS_Matrix)~Age_1*SNAP_Parent_Hyperactivity_1*rs6265_2,data=data_newIQ)

summary(manovamodelFINAL, test="Wilks")

summary.aov(manovamodelFINAL)

write.csv(data_newIQ, file = '/Users/kajajasinka/Google Drive/Manuscripts/BDNF/FINAL BEHAVIOURAL/Aug2015FINAL/data.csv')


library(lsmeans)
library(compute.es)


#change x to correspond to columns that are variables in the manova
x=12
a<-lm(data[,x]~Age_1+SNAP_Parent_Hyperactivity_1+rs6265_2,data=data)
lsmeans(a, "rs6265_2")
b<-summary(lm(data[,x]~data$Age_1+data$SNAP_Parent_Hyperactivity_1))
c<-summary(a)
a.fes(c$fstatistic[1], nrow(subset(data, data$rs6265_2=="CC")), nrow(subset(data, data$rs6265_2=="CT")), sqrt(b$r.squared), 2, level=95, cer = 0.2, dig = 2, verbose = T, id=NULL, data=NULL)

#ultimately did no use a.fes, just calculated effect size in excel using http://www.uccs.edu/lbecker/effect-size.html
#adjusted means and adjusted SE were used to compute mean difference and pooled sd for effect size calculation


aggregate(data$AGEYEAR, by=list(data$rs6265_2,data$AGEYEAR), FUN=length)

data$Group <- factor(data$rs6265_2, levels=c("CC","CT"), labels=c("Val/Val", "Met carrier"))


ggplot(data, aes(x=AGEYEAR, fill=Group)) + geom_vline(data=data, aes(xintercept=mean(subset(data$MRI_Age, data$rs6265_2=="CC")), color="grey"), linetype="dashed") +
  geom_vline(data=data, aes(xintercept=mean(subset(data$MRI_Age, data$rs6265_2=="CT")), color="black"), linetype="dashed") +
  geom_histogram(position="identity",binwidth = 0.25, alpha=0.75) + scale_color_grey() + theme_classic() +
scale_fill_manual(values=c("#999999", "black")) + labs(x="Age (years)", y = "Number of Participants") 
