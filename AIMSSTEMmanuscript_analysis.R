setwd("/Users/kajajasinska/Google Drive/Manuscripts/AIMS Study/AIMS DATA/")
data<-read.csv("finaldataSENCAM.csv")
pcadata<-read.csv("pcadataSENCAM.csv")
library(psych)
cortest.bartlett(pcadata)
KMO(pcadata)
pcadatamatrix<-cor(pcadata)
det(pcadatamatrix)
pcModel<-principal(pcadatamatrix, nfactors=19, rotate="none")
pcModelRot<-principal(pcadatamatrix, nfactors = 5, rotate = "varimax")
pcModelRotscores<-principal(pcadata, nfactors = 5, rotate = "varimax", scores = TRUE)
pcModelRotObscores<-principal(pcadata, nfactors = 19, rotate = "oblimin", scores = TRUE)
write.csv(pcModelRotscores$scores, "pcascoresothrrot.csv")
write.csv(pcModelRotObscores$scores, "pcascoresobliquerot.csv")
write.csv(pcModelRotscores$loadings, "pcaloadingsothrrot.csv")
write.csv(pcModelRotObscores$oadings, "pcaloadingsobliquerot.csv")
data<-read.csv("finaldata.csv")

nrow(subset(data, data$Gender=="F"))
nrow(subset(data, data$Gender=="M"))
summary(data)


modelmanova<-manova(cbind(RC1,RC2,RC3,RC4,RC5)~Gender*Language*Education*Field,data=data)
summary(modelmanova, test="Wilks")

modelRC1<-Anova(aov(RC1~Gender*Language*Education*Field,data=data), Type="II")
modelRC1
modelRC2<-Anova(aov(RC2~Gender*Language*Education*Field,data=data), Type="II")
modelRC2
modelRC3<-Anova(aov(RC3~Gender*Language*Education*Field,data=data), Type="II")
modelRC3
modelRC4<-Anova(aov(RC4~Gender*Language*Education*Field,data=data), Type="II")
modelRC4
modelRC5<-Anova(aov(RC5~Gender*Age*Language*Education*Field,data=data), Type="II")
modelRC5

modelmanova<-manova(cbind(RC1,RC2,RC3,RC4,RC5)~Gender+Language+Education+FieldGeneral,data=data)
summary(modelmanova, test="Wilks")
modelRC1a<-Anova(aov(RC1~Gender+Language+Education+FieldGeneral,data=data), Type="II")
modelRC1a
modelRC2a<-Anova(aov(RC2~Gender+Language+Education+FieldGeneral,data=data), Type="II")
modelRC2a
modelRC3a<-Anova(aov(RC3~Gender+Language+Education+FieldGeneral,data=data), Type="II")
modelRC3a
modelRC4a<-Anova(aov(RC4~Gender+Language+Education+FieldGeneral,data=data), Type="II")
modelRC4a
modelRC5a<-Anova(aov(RC5~Gender+Age+Language+Education+FieldGeneral,data=data), Type="II")
modelRC5a


modelmanova<-manova(cbind(RC1,RC2,RC3,RC4,RC5)~Gender+Language+Education+FieldGeneral,data=data)
summary(modelmanova, test="Wilks")

modelPerDem<-Anova(aov(GradePercent~RC1+RC4+Gender,data=data), Type="II")
modelPerDem

modelPerDem<-aov(GradePercent~RC4+RC1+Gender,data=data)
summary(modelPerDem)

ggplot(data=data, aes(x=1-data$RC4, y=data$GradePercent))+geom_point()+stat_smooth(method="lm") + theme(panel.background=element_rect(fill="white")) + xlab("Perceived Course Difficulty") + ylab("% Grade") + ggtitle("Effect of Perceived Course Difficulty on Course Grade") + annotate("text", x = 3.5, y= 95, label = "p < .05")
ggplot(data=data, aes(x=data$RC1, y=data$GradePercent))+geom_point()+stat_smooth(method="lm") + theme(panel.background=element_rect(fill="white")) + xlab("Interest and Confidence in Using Statistics") + ylab("% Grade") + ggtitle("Effect of Interest and Confidence in Using Statistics on Course Grade")+ annotate("text", x = 1.5, y= 95, label = "p < .05")

modelPerAtt<-lm(GradePercent~RC4+GenderLM,data=data)
summary(modelPerAtt)


aggregate(GradePercent~Gender,data,mean)
aggregate(RC1~Gender,data,mean)
aggregate(RC3~Education,data,mean)
aggregate(RC3~FieldGeneral,data,mean)
aggregate(RC4~Education,data,mean)

summary(data$Age)

G1<-ggplot() + geom_bar(aes(data$Nationality)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

chisq.test(data$Gender,data$Education)
fisher.test(data$Gender,data$Education)
fisher.test(data$Gender,data$FieldGeneral)
fisher.test(data$Gender,data$Language)

aggregate(Comp1Avg~Gender,data,mean)
aggregate(Comp1Avg~Gender,data,sd)
aggregate(Comp4Avg~Gender,data,mean)
aggregate(Comp4Avg~Gender,data,sd)
aggregate(Comp2Avg~FieldGenearl,data,mean)
aggregate(Comp2Avg~FieldGeneral,data,sd)
aggregate(Comp5Avg~Language,data,mean)
aggregate(Comp5Avg~Language,data,sd)


modelRC1<-anova(aov(RC1~Gender+Age+Language+Education+FieldGeneral,data=data))
modelRC1
modelRC2<-anova(aov(RC2~Gender+Age+Language+Education+FieldGeneral,data=data))
modelRC2
modelRC3<-anova(aov(RC3~Gender+Age+Language+Education+FieldGeneral,data=data))
modelRC3
modelRC4<-anova(aov(RC4~Gender+Age+Language+Education+FieldGeneral,data=data))
modelRC4
modelRC5<-anova(aov(RC5~Gender+Age+Language+Education+FieldGeneral,data=data))
modelRC5
modelmanova<-manova(cbind(RC1,RC2,RC3,RC4,RC5)~Gender+Age+Language+Education+FieldGeneral,data=data)
