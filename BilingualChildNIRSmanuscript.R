library(outliers)
library(predictmeans)
library(ggplot2)
library(nlme)

sfedata<-read.csv("~/Desktop/sfedata.csv")
grouproidiff<-read.csv("~/Desktop/groupsconditiondiffroi.csv")
groupexproidiff<-read.csv("~/Desktop/groupsconditiondiffexproi.csv")

#reordering the levels of each factor (.e are already effect coded)
sfedata$group<-factor(sfedata$group, levels=c("Monolingual English", "Bilingual English-French", "Bilingual English-Spanish"))
sfedata$group.e<-factor(sfedata$group.e, levels=c("Monolingual English", "Bilingual English-French", "Bilingual English-Spanish"))
sfedata$lang<-factor(sfedata$lang, levels=c("English", "French", "Spanish"))
sfedata$lang.e<-factor(sfedata$lang.e, levels=c("English", "French", "Spanish"))
sfedata$cond<-factor(sfedata$cond, levels=c("Regular", "Irregular", "Nonsense"))
sfedata$cond.e<-factor(sfedata$cond.e, levels=c("Regular", "Irregular", "Nonsense"))
#channel and id should be factorized
sfedata$ch<-factor(sfedata$ch)
sfedata$id<-factor(sfedata$id)

#Subset datasets for just english (all groups), and then just french bilinguals (both langauges), and spanish (both languages)
engsfedata<-subset(sfedata, lang=='English')
spgroupdata<-subset(sfedata, group=="Bilingual English-Spanish")
fregroupdata<-subset(sfedata, group=="Bilingual English-French")


#function for imputing missing values
impute <- function (a, a.impute){ 
  ifelse (is.na(a), a.impute, a)
}

#Replace outliers with NA based on outlier package
engsfedataNO<-engsfedata
outlier_tf = outlier(engsfedataNO$nirs,logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
engsfedataNO$nirs[find_outlier]<- NA



#Impute missing values based on id ch cond and group
lm.imp.1 <- lm (nirs~ id*ch*cond*group, data=engsfedataNO)
pred.1 <- predict(lm.imp.1, engsfedataNO)
engsfedataNO$nirs <- impute(engsfedataNO$nirs, pred.1)

#now for spanish and for french data
#Replace outliers with NA based on outlier package
outlier_tf = outlier(spgroupdata$nirs,logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
spgroupdata$nirs[find_outlier]<- NA
#Impute missing values
lm.imp.1 <- lm (nirs~ id*ch*cond, data=spgroupdata)
pred.1 <- predict(lm.imp.1, spgroupdata)
spgroupdata$nirs <- impute(spgroupdata$nirs, pred.1)
#Replace outliers with NA based on outlier package
outlier_tf = outlier(fregroupdata$nirs,logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
fregroupdata$nirs[find_outlier]<- NA
#Impute missing values
lm.imp.1 <- lm (nirs~ id*ch*cond, data=fregroupdata)
pred.1 <- predict(lm.imp.1, fregroupdata)
fregroupdata$nirs <- impute(fregroupdata$nirs, pred.1)
#note that outliers and lm and pred variables change

engsfedataNO$group<-factor(engsfedataNO$group, levels=c("Monolingual English", "Bilingual English-French", "Bilingual English-Spanish"))
engsfedataNO$group.e<-factor(engsfedataNO$group.e, levels=c("Monolingual English", "Bilingual English-French", "Bilingual English-Spanish"))
engsfedataNO$lang<-factor(engsfedataNO$lang, levels=c("English", "French", "Spanish"))
engsfedataNO$lang.e<-factor(engsfedataNO$lang.e, levels=c("English", "French", "Spanish"))
engsfedataNO$cond<-factor(engsfedataNO$cond, levels=c("Regular", "Irregular", "Nonsense"))
engsfedataNO$cond.e<-factor(engsfedataNO$cond.e, levels=c("Regular", "Irregular", "Nonsense"))

##below is to increase the ROI area and only look at left hemi
engsfedataNO$roiNEW<-engsfedataNO$roi
engsfedataNO$roiNEW[(engsfedataNO$group!="Bilingual English-French"&(engsfedataNO$ch==5|engsfedataNO$ch==6|engsfedataNO$ch==14|engsfedataNO$ch==10|engsfedataNO$ch==27|engsfedataNO$ch==32|engsfedataNO$ch==36|engsfedataNO$ch==37))|(engsfedataNO$group=="Bilingual English-French"&(engsfedataNO$ch==2|engsfedataNO$ch==5|engsfedataNO$ch==4|engsfedataNO$ch==13|engsfedataNO$ch==15|engsfedataNO$ch==16))]<-"BA"
BA1engsfedataNO<-subset(engsfedataNO, engsfedataNO$roiNEW=="BA")
engsfedataNO$roiNEW<-engsfedataNO$roi
engsfedataNO$roiNEW[(engsfedataNO$group!="Bilingual English-French"&engsfedataNO$ch==3|engsfedataNO$ch==4|engsfedataNO$ch==8|engsfedataNO$ch==9|engsfedataNO$ch==39|engsfedataNO$ch==40|engsfedataNO$ch==43|engsfedataNO$ch==44)|(engsfedataNO$group=="Bilingual English-French"&engsfedataNO$ch==8|engsfedataNO$ch==11|engsfedataNO$ch==12|engsfedataNO$ch==22|engsfedataNO$ch==23|engsfedataNO$ch==24)]<-"STG"
STG1engsfedataNO<-subset(engsfedataNO, roiNEW=='STG')
BA1engsfedataNO_lh<-subset(BA1engsfedataNO, hemi=='left')
STG1engsfedataNO_lh<-subset(STG1engsfedataNO, hemi=='left')

BA1nirsmodelNO_lh<-lme(nirs ~ cond*group, data=BA1engsfedataNO_lh, random=(~1|id/ch), na.action=na.omit, control=lmeControl(returnObject=TRUE))
STG1nirsmodelNO_lh<-lme(nirs ~ cond*group, data=STG1engsfedataNO_lh, random=(~1|id/ch), na.action=na.omit, control=lmeControl(returnObject=TRUE))

#just doing IFG and STG rois
#comparing models to null model
BA1nirsmodelNOa<-lme(nirs ~ 1, data=BA1engsfedataNO_lh, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
BA1nirsmodelNOb1<-lme(nirs ~ cond, data=BA1engsfedataNO_lh, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
BA1nirsmodelNOb2<-lme(nirs ~ group, data=BA1engsfedataNO_lh, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
BA1nirsmodelNOc<-lme(nirs ~ cond+group, data=BA1engsfedataNO_lh, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
BA1nirsmodelNOd<-lme(nirs ~ cond*group, data=BA1engsfedataNO_lh, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))

anova(BA1nirsmodelNOa,BA1nirsmodelNOb1,BA1nirsmodelNOb2,BA1nirsmodelNOc,BA1nirsmodelNOd)

STG1nirsmodelNOa<-lme(nirs ~ 1, data=STG1engsfedataNO_lh, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
STG1nirsmodelNOb1<-lme(nirs ~ cond, data=STG1engsfedataNO_lh, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
STG1nirsmodelNOb2<-lme(nirs ~ group, data=STG1engsfedataNO_lh, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
STG1nirsmodelNOc<-lme(nirs ~ cond+group, data=STG1engsfedataNO_lh, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
STG1nirsmodelNOd<-lme(nirs ~ cond*group, data=STG1engsfedataNO_lh, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))

anova(STG1nirsmodelNOa,STG1nirsmodelNOb1,STG1nirsmodelNOb2,STG1nirsmodelNOc,STG1nirsmodelNOd)

sumBA1nirsmodelNO<-summary(BA1nirsmodelNO)
sumSTG1nirsmodelNO<-summary(STG1nirsmodelNO)

BA1groupresults<-predictmeans(BA1nirsmodelNO, "cond:group",pair=TRUE, atvar="group", adj="bonferroni")
BA1condresults<-predictmeans(BA1nirsmodelNO, "cond:group",pair=TRUE, atvar="cond", adj="bonferroni")

STG1groupresults<-predictmeans(STG1nirsmodelNO, "cond:group",pair=TRUE, atvar="group", adj="bonferroni")
STG1condresults<-predictmeans(STG1nirsmodelNO, "cond:group",pair=TRUE, atvar="cond", adj="bonferroni")

#conditiondiffplot
ann_text<-data.frame(Condition=groupexproidiff$Condition,ROI=groupexproidiff$ROI,Activation=((groupexproidiff$Activation)+groupexproidiff$SE1)+.1,lab="*",Group=groupexproidiff$Group)
ann_text$Activation[5]=groupexproidiff$Activation[5]-groupexproidiff$SE1[5]-.2
ann_text$Activation[6]=groupexproidiff$Activation[6]-groupexproidiff$SE1[6]-.2
xpos=c(0.78,1.78,1.23,2.23)
Eann_text<-subset(ann_text, ann_text$Group=="English Monolingual")
Fann_text<-subset(ann_text, ann_text$Group=="French-English Bilingual")
Sann_text<-subset(ann_text, ann_text$Group=="Spanish-English Bilingual")
xposE=xpos[2:4]
xposF=xpos[c(2,4)]
xposS=xpos
ggplot(groupexproidiff, aes(x=Condition, y=Activation, fill=ROI), colour="black")+ scale_fill_manual(values=c("black","gray"))  + geom_errorbar(aes(ymin=Activation-SE1, ymax=Activation+SE1), width=.2, position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black") + facet_grid( ~ Group, scales="free") + theme_bw() + ylab("HbO")+ labs(title = "Inferior Frontal Gyrus") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  geom_text(data=Eann_text[2:4,],x=xposE,label="*") +geom_text(data=Fann_text[c(2,4),],x=xposF,label="*")+geom_text(data=Sann_text,x=xposS,label="*")



##resume with original ROIs and both hemispheres
#subset the data to ROIs
BAengsfedataNO<-subset(engsfedataNO, roi=='BA')
STGengsfedataNO<-subset(engsfedataNO, roi=='STG')

BAengsfedataNO<-subset(BAengsfedataNO, hemi=='left')
STGengsfedataNO<-subset(STGengsfedataNO, hemi=='left')

#Replace outliers with NA based on outlier package
BAengsfedataNO$nirs[BAengsfedataNO$nirs>(3)]<- NA
BAengsfedataNO$nirs[BAengsfedataNO$nirs<(-3)]<- NA
BAengsfedataNO$nirs<-na.aggregate(BAengsfedataNO$nirs, by=list(BAengsfedataNO$id,BAengsfedataNO$ch,BAengsfedataNO$cond,BAengsfedataNO$group), FUN=mean)

STGengsfedataNO$nirs[STGengsfedataNO$nirs>(3)]<- NA
STGengsfedataNO$nirs[STGengsfedataNO$nirs<(-3)]<- NA
STGengsfedataNO$nirs<-na.aggregate(STGengsfedataNO$nirs, by=list(STGengsfedataNO$id,STGengsfedataNO$ch,STGengsfedataNO$cond,STGengsfedataNO$group), FUN=mean)

BAnirsmodelNO<-lme(nirs ~ cond*group*hemi, data=BAengsfedataNO, random=(~1|id/ch), na.action=na.omit, control=lmeControl(returnObject=TRUE))
STGnirsmodelNO<-lme(nirs ~ cond*group*hemi, data=STGengsfedataNO, random=(~1|id/ch), na.action=na.omit, control=lmeControl(returnObject=TRUE))

#just doing IFG and STG rois
#comparing models to null model
BAnirsmodelNOa<-lme(nirs ~ 1, data=BAengsfedataNO, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
BAnirsmodelNOb<-lme(nirs ~ cond, data=BAengsfedataNO, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
BAnirsmodelNOc<-lme(nirs ~ cond+group, data=BAengsfedataNO, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
BAnirsmodelNOd<-lme(nirs ~ cond+group+hemi, data=BAengsfedataNO, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
BAnirsmodelNOe<-lme(nirs ~ cond*group*hemi, data=BAengsfedataNO, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))

anova(BAnirsmodelNOa,BAnirsmodelNOb,BAnirsmodelNOd,BAnirsmodelNOe)

STGnirsmodelNOa<-lme(nirs ~ 1, data=STGengsfedataNO, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
STGnirsmodelNOb<-lme(nirs ~ cond, data=STGengsfedataNO, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
STGnirsmodelNOb1<-lme(nirs ~ group, data=STGengsfedataNO, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
STGnirsmodelNOb2<-lme(nirs ~ hemi, data=STGengsfedataNO, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
STGnirsmodelNOc<-lme(nirs ~ cond+group, data=STGengsfedataNO, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
STGnirsmodelNOd<-lme(nirs ~ cond+group+hemi, data=STGengsfedataNO, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
STGnirsmodelNOe<-lme(nirs ~ cond*group*hemi, data=STGengsfedataNO, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))

anova(STGnirsmodelNOa,STGnirsmodelNOb,STGnirsmodelNOc,STGnirsmodelNOd,STGnirsmodelNOe)

sumBAnirsmodelNO<-summary(BAnirsmodelNO)
sumSTGnirsmodelNO<-summary(STGnirsmodelNO)
sumDLPFCnirsmodelNO<-summary(DLPFCnirsmodelNO)

#since there are sig interactions between group and cond and hemisphere:
#use predit means package to look at pairwise comparisions

BAgroupresults<-predictmeans(BAnirsmodelNO, "cond:group:hemi",pair=TRUE, atvar="group", adj="bonferroni")
BAcondresults<-predictmeans(BAnirsmodelNO, "cond:group:hemi",pair=TRUE, atvar="cond", adj="bonferroni")

BAgroupresults$`Predicted Means`
BAgroupresults$`Standard Error of Means`

STGgroupresults<-predictmeans(STGnirsmodelNO, "cond:group:hemi",pair=TRUE, atvar="group", adj="bonferroni")
STGcondresults<-predictmeans(STGnirsmodelNO, "cond:group:hemi",pair=TRUE, atvar="cond", adj="bonferroni")

DLPFCgroupresults<-predictmeans(DLPFCnirsmodelNO, "cond:group:hemi",pair=TRUE, atvar="group", adj="bonferroni")
DLPFCcondresults<-predictmeans(DLPFCnirsmodelNO, "cond:group:hemi",pair=TRUE, atvar="cond", adj="bonferroni")

#just setting up a table that we can plot from "plotting data"
conditions<-rep(c("Regular", "Irregular", "Pseudowords"),6)
hemi<-rep(c(rep("Left",3), rep("Right",3)),6)
group<-c(rep("Monolingual English",6), rep("Bilingual English-French",6), rep("Bilingual English-Spanish",6))
plottingdata<-cbind(hemi,group,conditions,BAgroupresults$`Predicted Means`[1:18], BAgroupresults$`Standard Error of Means`[1:18], STGgroupresults$`Predicted Means`[1:18],STGgroupresults$`Standard Error of Means`[1:18])
plottingdata<-data.frame(plottingdata)
plottingdata$conditions<-relevel(plottingdata$conditions, "Regular")
plottingdata$group<-relevel(plottingdata$group, "Monolingual English")
plottingdata$hemi<-relevel(plottingdata$hemi, "Left")
plottingdata$V4<-as.numeric(levels(plottingdata$V4))[plottingdata$V4]
plottingdata$V5<-as.numeric(levels(plottingdata$V5))[plottingdata$V5]
plottingdata$V6<-as.numeric(levels(plottingdata$V6))[plottingdata$V6]
plottingdata$V7<-as.numeric(levels(plottingdata$V7))[plottingdata$V7]
colnames(plottingdata)<-c("Hemisphere", "Group", "Word Type", "V4", "V5", "V6", "V7")

#IFG PLOT
ifgplot<-ggplot(plottingdata, aes(x=`Word Type`, y=V4, fill=Hemisphere), colour="black") + scale_fill_manual(values=c("black","gray")) + geom_errorbar(aes(ymin=V4-V5, ymax=V4+V5), width=.2, position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black") + facet_grid( ~ Group, scales="free") + theme_bw() + ylab("HbO")+ labs(title = "Inferior Frontal Gyrus") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

#STG PLOT
stgplot<-ggplot(plottingdata, aes(x=`Word Type`, y=V6, fill=Hemisphere), colour="black") + scale_fill_manual(values=c("black","gray")) + geom_errorbar(aes(ymin=V6-V7, ymax=V6+V7), width=.2, position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black") + facet_grid( ~ Group, scales="free") + theme_bw() + ylab("HbO")+ labs(title = "Superior Temporal Gyrus") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))


multiplot(ifgplot,stgplot, cols=2)

#left plotting data
leftplottingdata=subset(plottingdata, plottingdata$hemi=='left')
lifgplot<-ggplot(leftplottingdata, aes(x=`Word Type`, y=V4), colour="black") + geom_errorbar(aes(ymin=V4-V5, ymax=V4+V5), width=.2, position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black") + facet_grid( ~ Group, scales="free") + theme_bw() + ylab("HbO")+ labs(title = "Inferior Frontal Gyrus") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
lstgplot<-ggplot(leftplottingdata, aes(x=`Word Type`, y=V6), colour="black") + scale_fill_manual(values=c("black","gray")) + geom_errorbar(aes(ymin=V6-V7, ymax=V6+V7), width=.2, position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black") + facet_grid( ~ Group, scales="free") + theme_bw() + ylab("HbO")+ labs(title = "Superior Temporal Gyrus") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

#conditiondiffplot
ann_text<-data.frame(Condition=grouproidiff$Condition,ROI=grouproidiff$ROI,Activation=abs((grouproidiff$Activation)+grouproidiff$SE1)+.1,lab="*",Group=grouproidiff$Group)
ann_text$Activation[11]=(-1.2469-1.193692)-.15
xpos=c(0.78,1.78,1.23,2.23)
Eann_text<-subset(ann_text, ann_text$Group=="English Monolingual")
Fann_text<-subset(ann_text, ann_text$Group=="French-English Bilingual")
Sann_text<-subset(ann_text, ann_text$Group=="Spanish-English Bilingual")
xposE=xpos[4]
xposF=xpos[2:4]
xposS=xpos
ggplot(grouproidiff, aes(x=Condition, y=Activation, fill=ROI), colour="black")+ scale_fill_manual(values=c("black","gray"))  + geom_errorbar(aes(ymin=Activation-SE1, ymax=Activation+SE1), width=.2, position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black") + facet_grid( ~ Group, scales="free") + theme_bw() + ylab("HbO")+ labs(title = "Inferior Frontal Gyrus") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  geom_text(data=Eann_text[4,],x=xposE,label="*") +geom_text(data=Fann_text[2:4,],x=xposF,label="*")+geom_text(data=Sann_text,x=xposS,label="*")

#####NOW FOR SPANISH AND FRENCH

spgroupdata<-subset(spgroupdata, cond!="Irregular")
spgroupdata$lang<-factor(spgroupdata$lang)
spgroupdata$cond<-factor(spgroupdata$cond)
spgroupdata$group<-factor(spgroupdata$group)
fregroupdata$lang<-factor(fregroupdata$lang)
fregroupdata$cond<-factor(fregroupdata$cond)
fregroupdata$group<-factor(fregroupdata$group)

fregroupdata$lang<-factor(fregroupdata$lang, levels=c("English", "French"))
fregroupdata$lang.e<-factor(fregroupdata$lang.e, levels=c("English", "French"))
fregroupdata$cond<-factor(fregroupdata$cond, levels=c("Regular", "Irregular", "Nonsense"))
fregroupdata$cond.e<-factor(fregroupdata$cond.e, levels=c("Regular", "Irregular", "Nonsense"))

spgroupdata$lang<-factor(spgroupdata$lang, levels=c("English", "Spanish"))
spgroupdata$lang.e<-factor(spgroupdata$lang.e, levels=c("English", "Spanish"))
spgroupdata$cond<-factor(spgroupdata$cond, levels=c("Regular", "Nonsense"))
spgroupdata$cond.e<-factor(spgroupdata$cond.e, levels=c("Regular", "Nonsense"))


#subset the data to ROIs
BAspgroupdata<-subset(spgroupdata, roi=='BA')
STGspgroupdata<-subset(spgroupdata, roi=='STG')
DLPFCspgroupdata<-subset(spgroupdata, roi=='DLPFC')

#subset the data to ROIs
BAfregroupdata<-subset(fregroupdata, roi=='BA')
STGfregroupdata<-subset(fregroupdata, roi=='STG')
DLPFCfregroupdata<-subset(fregroupdata, roi=='DLPFC')

SP_BAnirsmodelNO<-lme(nirs ~ cond*lang*hemi, data=BAspgroupdata, random=(~1|id/ch), na.action=na.omit,control=lmeControl(returnObject=TRUE))
SP_STGnirsmodelNO<-lme(nirs ~ cond*lang*hemi, data=STGspgroupdata, random=(~1|id/ch), na.action=na.omit, control=lmeControl(returnObject=TRUE))
SP_DLPFCnirsmodelNO<-lme(nirs ~ cond*lang*hemi, data=DLPFCspgroupdata, random=(~1|id/ch), na.action=na.omit, control=lmeControl(returnObject=TRUE))

FR_BAnirsmodelNO<-lme(nirs ~ cond*lang*hemi, data=BAfregroupdata, random=(~1|id/ch), na.action=na.omit,control=lmeControl(returnObject=TRUE))
FR_STGnirsmodelNO<-lme(nirs ~ cond*lang*hemi, data=STGfregroupdata, random=(~1|id/ch), na.action=na.omit, control=lmeControl(returnObject=TRUE))
FR_DLPFCnirsmodelNO<-lme(nirs ~ cond*lang*hemi, data=DLPFCfregroupdata, random=(~1|id/ch), na.action=na.omit, control=lmeControl(returnObject=TRUE))


#just doing IFG and STG rois
#comparing models to null model
SP_BAnirsmodelNOa<-lme(nirs ~ 1, data=BAspgroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
SP_BAnirsmodelNOb<-lme(nirs ~ lang, data=BAspgroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
SP_BAnirsmodelNOb1<-lme(nirs ~ cond, data=BAspgroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
SP_BAnirsmodelNOc<-lme(nirs ~ lang+cond, data=BAspgroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
SP_BAnirsmodelNOd<-lme(nirs ~ lang+cond+hemi, data=BAspgroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
SP_BAnirsmodelNOe<-lme(nirs ~ lang*cond*hemi, data=BAspgroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))

anova(SP_BAnirsmodelNOa,SP_BAnirsmodelNOb,SP_BAnirsmodelNOc, SP_BAnirsmodelNOd,SP_BAnirsmodelNOe)
anova(SP_BAnirsmodelNOa,SP_BAnirsmodelNOb1)

SP_STGnirsmodelNOa<-lme(nirs ~ 1, data=STGspgroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
SP_STGnirsmodelNOb<-lme(nirs ~ lang, data=STGspgroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
SP_STGnirsmodelNOb1<-lme(nirs ~ cond, data=STGspgroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
SP_STGnirsmodelNOb2<-lme(nirs ~ hemi, data=STGspgroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
SP_STGnirsmodelNOc<-lme(nirs ~ lang+cond, data=STGspgroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
SP_STGnirsmodelNOd<-lme(nirs ~ lang+cond+hemi, data=STGspgroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
SP_STGnirsmodelNOe<-lme(nirs ~ lang*cond*hemi, data=STGspgroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))

anova(SP_STGnirsmodelNOa,SP_STGnirsmodelNOb,SP_STGnirsmodelNOc,SP_STGnirsmodelNOd,SP_STGnirsmodelNOe)
anova(SP_STGnirsmodelNOa,SP_STGnirsmodelNOb)
anova(SP_STGnirsmodelNOa,SP_STGnirsmodelNOb1)
anova(SP_STGnirsmodelNOa,SP_STGnirsmodelNOb2)


FR_BAnirsmodelNOa<-lme(nirs ~ 1, data=BAfregroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
FR_BAnirsmodelNOb<-lme(nirs ~ lang, data=BAfregroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
FR_BAnirsmodelNOb1<-lme(nirs ~ cond, data=BAfregroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
FR_BAnirsmodelNOb1<-lme(nirs ~ cond, data=BAfregroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
FR_BAnirsmodelNOc<-lme(nirs ~ lang+cond, data=BAfregroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
FR_BAnirsmodelNOd<-lme(nirs ~ lang+cond+hemi, data=BAfregroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
FR_BAnirsmodelNOe<-lme(nirs ~ lang*cond*hemi, data=BAfregroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))

anova(FR_BAnirsmodelNOa,FR_BAnirsmodelNOb,FR_BAnirsmodelNOc, FR_BAnirsmodelNOd,FR_BAnirsmodelNOe)
anova(FR_BAnirsmodelNOa,FR_BAnirsmodelNOb1)
anova(FR_BAnirsmodelNOa,FR_BAnirsmodelNOb)

FR_STGnirsmodelNOa<-lme(nirs ~ 1, data=STGfregroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
FR_STGnirsmodelNOb<-lme(nirs ~ lang, data=STGfregroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
FR_STGnirsmodelNOb1<-lme(nirs ~ cond, data=STGfregroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
FR_STGnirsmodelNOb2<-lme(nirs ~ hemi, data=STGfregroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
FR_STGnirsmodelNOc<-lme(nirs ~ lang+cond, data=STGfregroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
FR_STGnirsmodelNOd<-lme(nirs ~ lang+cond+hemi, data=STGfregroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))
FR_STGnirsmodelNOe<-lme(nirs ~ lang*cond*hemi, data=STGfregroupdata, random=(~1|id/ch), method="ML",na.action=na.omit, control=lmeControl(returnObject=TRUE))

anova(FR_STGnirsmodelNOa,FR_STGnirsmodelNOb,FR_STGnirsmodelNOc,FR_STGnirsmodelNOd,FR_STGnirsmodelNOe)
anova(FR_STGnirsmodelNOa,FR_STGnirsmodelNOb1)
anova(FR_STGnirsmodelNOa,FR_STGnirsmodelNOb)


SP_sumBAnirsmodelNO<-summary(SP_BAnirsmodelNO)
SP_sumSTGnirsmodelNO<-summary(SP_STGnirsmodelNO)
SP_sumDLPFCnirsmodelNO<-summary(SP_DLPFCnirsmodelNO)

#since there are sig interactions between group and cond and hemisphere:
#use predit means package to look at pairwise comparisions

SP_BAgroupresults<-predictmeans(SP_BAnirsmodelNO, "cond:lang:hemi", pair=TRUE, atvar="lang",adj="bonferroni")
SP_BAcondresults<-predictmeans(SP_BAnirsmodelNO, "cond:lang:hemi",pair=TRUE, atvar="cond", adj="bonferroni")

FR_BAgroupresults<-predictmeans(FR_BAnirsmodelNO, "cond:lang:hemi", pair=TRUE, atvar="lang",adj="bonferroni")
FR_BAcondresults<-predictmeans(FR_BAnirsmodelNO, "cond:lang:hemi",pair=TRUE, atvar="cond", adj="bonferroni")


SP_STGgroupresults<-predictmeans(SP_STGnirsmodelNO, "cond:lang:hemi", pair=TRUE, atvar="lang",adj="bonferroni")
SP_STGcondresults<-predictmeans(SP_STGnirsmodelNO, "cond:lang:hemi",pair=TRUE, atvar="cond", adj="bonferroni")

FR_STGgroupresults<-predictmeans(FR_STGnirsmodelNO, "cond:lang:hemi", pair=TRUE, atvar="lang",adj="bonferroni")
FR_STGcondresults<-predictmeans(FR_STGnirsmodelNO, "cond:lang:hemi",pair=TRUE, atvar="cond", adj="bonferroni")


##SPANISH
#just setting up a table that we can plot from "plotting data"
conditions<-rep(c("Pseuodwords", "Regular"),4)
hemi<-rep(c(rep("Left",2), rep("Right",2)),4)
lang<-c(rep("English",4), rep("Spanish",4))
SPplottingdata<-cbind(hemi,lang,conditions,SP_BAgroupresults$`Predicted Means`[1:8], SP_BAgroupresults$`Standard Error of Means`[1], SP_STGgroupresults$`Predicted Means`[1:8],SP_STGgroupresults$`Standard Error of Means`[1])
SPplottingdata<-data.frame(SPplottingdata)
SPplottingdata$conditions<-relevel(SPplottingdata$conditions, "Regular")
SPplottingdata$group<-relevel(SPplottingdata$lang, "English")
SPplottingdata$hemi<-relevel(SPplottingdata$hemi, "Left")
SPplottingdata$V4<-as.numeric(levels(SPplottingdata$V4))[SPplottingdata$V4]
SPplottingdata$V5<-as.numeric(levels(SPplottingdata$V5))[SPplottingdata$V5]
SPplottingdata$V6<-as.numeric(levels(SPplottingdata$V6))[SPplottingdata$V6]
SPplottingdata$V7<-as.numeric(levels(SPplottingdata$V7))[SPplottingdata$V7]
colnames(SPplottingdata)<-c("Hemisphere", "Language", "Word Type", "V4", "V5", "V6", "V7")

#IFG PLOT
ifgspplot<-ggplot(SPplottingdata, aes(x=`Word Type`, y=V4, fill=Hemisphere), colour="black") + scale_fill_manual(values=c("black","gray")) + geom_errorbar(aes(ymin=V4-V5, ymax=V4+V5), width=.2, position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black") + facet_grid( ~ Language, scales="free") + theme_bw() + ylab("HbO")+ labs(title = "Inferior Frontal Gyrus") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))


#STG PLOT
stgspplot<-ggplot(SPplottingdata, aes(x=`Word Type`, y=V6, fill=Hemisphere), colour="black") + scale_fill_manual(values=c("black","gray")) + geom_errorbar(aes(ymin=V6-V7, ymax=V6+V7), width=.2, position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black") + facet_grid( ~ Language, scales="free") + theme_bw() + ylab("HbO")+ labs(title = "Superior Temporal Gyrus")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

multiplot(ifgspplot, stgspplot, cols=2)



##FRENCH
#just setting up a table that we can plot from "plotting data"
conditions<-rep(c("Regular", "Irregular", "Pseudowords"),4)
hemi<-rep(c(rep("Left",3), rep("Right",3)),4)
lang<-c(rep("English",6), rep("French",6))
FRplottingdata<-cbind(hemi,lang,conditions,FR_BAgroupresults$`Predicted Means`[1:12], FR_BAgroupresults$`Standard Error of Means`[1:12], FR_STGgroupresults$`Predicted Means`[1:12],FR_STGgroupresults$`Standard Error of Means`[1:12])
FRplottingdata<-data.frame(FRplottingdata)
FRplottingdata$conditions<-relevel(FRplottingdata$conditions, "Regular")
FRplottingdata$group<-relevel(FRplottingdata$langg, "English")
FRplottingdata$hemi<-relevel(FRplottingdata$hemi, "Left")
FRplottingdata$V4<-as.numeric(levels(FRplottingdata$V4))[FRplottingdata$V4]
FRplottingdata$V5<-as.numeric(levels(FRplottingdata$V5))[FRplottingdata$V5]
FRplottingdata$V6<-as.numeric(levels(FRplottingdata$V6))[FRplottingdata$V6]
FRplottingdata$V7<-as.numeric(levels(FRplottingdata$V7))[FRplottingdata$V7]
colnames(FRplottingdata)<-c("Hemisphere", "Language", "Word Type", "V4", "V5", "V6", "V7")

#IFG PLOT
ifgfrplot<-ggplot(FRplottingdata, aes(x=`Word Type`, y=V4, fill=Hemisphere), colour="black") + scale_fill_manual(values=c("black","gray")) + geom_errorbar(aes(ymin=V4-V5, ymax=V4+V5), width=.2, position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black") + facet_grid( ~ Language, scales="free") + theme_bw() + ylab("HbO")+ labs(title = "Inferior Frontal Gyrus")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))


#STG PLOT
stgfrplot<-ggplot(FRplottingdata, aes(x=`Word Type`, y=V6, fill=Hemisphere), colour="black") + scale_fill_manual(values=c("black","gray")) + geom_errorbar(aes(ymin=V6-V7, ymax=V6+V7), width=.2, position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black") + facet_grid( ~ Language, scales="free") + theme_bw() + ylab("HbO")+ labs(title = "Superior Temporal Gyrus")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))


multiplot(ifgspplot,  ifgfrplot, stgspplot, stgfrplot,cols=2)



###some stuff to get average polhemus done

m1<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/10Apr07pb_origins.csv", header = T)[ ,1:4]
m2<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/11Apr07rl_origins.csv", header = T)[ ,1:4]
m3<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/16Apr07cp_origins.csv", header = T)[ ,1:4]
m4<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/16Apr07cw_origins.csv", header = T)[ ,1:4]
m5<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/16Apr07lp_origins.csv", header = T)[ ,1:4]
m6<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/17Apr07ef_origins.csv", header = T)[ ,1:4]
m7<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/17Apr07gs_origins.csv", header = T)[ ,1:4]
m8<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/17Apr07rm_origins.csv", header = T)[ ,1:4]
m9<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/22Mar07rb_origins.csv", header = T)[ ,1:4]
m10<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/23Mar07nr_origins.csv", header = T)[ ,1:4]
m11<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/29Mar07bh_origins.csv", header = T)[ ,1:4]

b1<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/23Apr07aj_origins.csv", header = T)[ ,1:4]
b2<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/23Apr07ar_origins.csv", header = T)[ ,1:4]
b3<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/23Apr07er_origins.csv", header = T)[ ,1:4]
b4<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/24Apr07im_origins.csv", header = T)[ ,1:4]
b5<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/24Apr07jc_origins.csv", header = T)[ ,1:4]
b6<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/24Apr07vp_origins.csv", header = T)[ ,1:4]
b7<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/25Apr07gvc_origins.csv", header = T)[ ,1:4]

m1<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/10Apr07pb_others.csv", header = F)[ ,1:4]
m2<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/11Apr07rl_others.csv", header = F)[ ,1:4]
m3<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/16Apr07cp_others.csv", header = F)[ ,1:4]
m4<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/16Apr07cw_others.csv", header = F)[ ,1:4]
m5<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/16Apr07lp_others.csv", header = F)[ ,1:4]
m6<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/17Apr07ef_others.csv", header = F)[ ,1:4]
m7<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/17Apr07gs_others.csv", header = F)[ ,1:4]
m8<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/17Apr07rm_others.csv", header = F)[ ,1:4]
m9<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/22Mar07rb_others.csv", header = F)[ ,1:4]
m10<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/23Mar07nr_others.csv", header = F)[ ,1:4]
m11<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/29Mar07bh_others.csv", header = F)[ ,1:4]

b1<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/23Apr07aj_others.csv", header = F)[ ,1:4]
b2<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/23Apr07ar_others.csv", header = F)[ ,1:4]
b3<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/23Apr07er_others.csv", header = F)[ ,1:4]
b4<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/24Apr07im_others.csv", header = F)[ ,1:4]
b5<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/24Apr07jc_others.csv", header = F)[ ,1:4]
b6<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/24Apr07vp_others.csv", header = F)[ ,1:4]
b7<-read.csv("/Users/kajajasinska/Documents/Studies/Spanish_English_Bilingual_Study/grandpolhemus/25Apr07gvc_others.csv", header = F)[ ,1:4]


avg_origins<-(m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+b1+b2+b3+b4+b5+b6+b7)/18
avg_others<-(m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+b1+b2+b3+b4+b5+b6+b7)/18

