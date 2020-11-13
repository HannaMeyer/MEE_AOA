# This script visualizes the results of several case study settings
# It is based on the results (data/resultsTable.RData) created with "CaseStudyScenarios.R"

rm(list=ls())
library(ggplot2)
library(reshape2)
library(hydroGOF)
dat <- get(load("../data/resultsTable.RData"))

means <- as.data.frame(matrix(unlist(dat$meansPCA),ncol=2,byrow = T))
sds <- as.data.frame(matrix(unlist(dat$sdPCA),ncol=2,byrow = T))

# these are the settings of the specific case study shown in the paper:
caseStudy <- dat[dat$npoints==50&dat$seed==10&
                         sds[,1]==2&sds[,2]==2&
                         means[,1]==3&means[,2]==-1,]

### Find ideal threshold:
pdf("../figures/threshold_box.pdf",width=6,height=5)
par(mar = c(5.1, 5.2, 2.1, 0.1))
boxplot(dat$model_RMSE-dat$`PredErrorAOA_RMSE_25%`,
        dat$model_RMSE-dat$`PredErrorAOA_RMSE_50%`,
        dat$model_RMSE-dat$`PredErrorAOA_RMSE_75%`,
        dat$model_RMSE-dat$`PredErrorAOA_RMSE_90%`,
        dat$model_RMSE-dat$`PredErrorAOA_RMSE_95%`,
        dat$model_RMSE-dat$`PredErrorAOA_RMSE_99%`,
        dat$model_RMSE-dat$`PredErrorAOA_RMSE_100%`,
        dat$model_RMSE-dat$PredErrorAOA_RMSE_default,
        names=c("0.25","0.50","0.75","0.90","0.95","0.99","1.00","default"),
        notch=T,ylab="ME",xlab="threshold")

points(colMeans(data.frame(dat$model_RMSE-dat$`PredErrorAOA_RMSE_25%`,
                           dat$model_RMSE-dat$`PredErrorAOA_RMSE_50%`,
                           dat$model_RMSE-dat$`PredErrorAOA_RMSE_75%`,
                           dat$model_RMSE-dat$`PredErrorAOA_RMSE_90%`,
                           dat$model_RMSE-dat$`PredErrorAOA_RMSE_95%`,
                           dat$model_RMSE-dat$`PredErrorAOA_RMSE_99%`,
                           dat$model_RMSE-dat$`PredErrorAOA_RMSE_100%`,
                           dat$model_RMSE-dat$PredErrorAOA_RMSE_default),
                na.rm = TRUE),pch=16)
abline(0,0,lwd=2)
dev.off()


performanceAOA <- c()
performanceNOTAOA <- c()

potentialthres <-unique(substr(names(dat)[grep("PredErrorAOA_R2",names(dat))],17,24))
potentialthres <- potentialthres[1:9]

for (thres in potentialthres){
performanceAOA <- rbind(performanceAOA,data.frame("thres"=thres,
                                                  "ME"=me(dat$model_RMSE,dat[,paste0("PredErrorAOA_RMSE_",thres)])))
performanceNOTAOA <- rbind(performanceNOTAOA,data.frame("thres"=thres,
                                                        "ME"=me(dat$model_RMSE,dat[,paste0("PredErrorNOTAOA_RMSE_",thres)])))
}
# ideal threshold:
bestthres <- performanceAOA$thres[which(abs(performanceAOA$ME)==min(abs(0-performanceAOA$ME)))]
print(bestthres)

#### How well does the model CV error represents the prediction error inside and outside the AOA?

for (thres in potentialthres){
pdfname<-sub("%","",paste0("../figures/comparison_modelRuns_scatter_",thres,".pdf"))
pdf(pdfname,width=9,height=5)
par(mfrow=c(1,2))

lim <- c(min(c(dat$model_RMSE,dat[,paste0("PredErrorAOA_RMSE_",thres)])),
         max(c(dat$model_RMSE,dat[,paste0("PredErrorAOA_RMSE_",thres)])))
plot(dat$model_RMSE~dat[,paste0("PredErrorAOA_RMSE_",thres)],
     xlim=lim,ylim=lim,
     xlab="RMSE prediction (AOA)",ylab="RMSE model CV")
points(caseStudy$model_RMSE~caseStudy[,paste0("PredErrorAOA_RMSE_",thres)],col="red",pch=16)
legend("topleft",legend="a",bty="n")
abline(0,1)
lim <- c(min(c(dat$model_RMSE,dat[,paste0("PredErrorNOTAOA_RMSE_",thres)]),na.rm=T),
         max(c(dat$model_RMSE,dat[,paste0("PredErrorNOTAOA_RMSE_",thres)]),na.rm=T))
plot(dat$model_RMSE~dat[,paste0("PredErrorNOTAOA_RMSE_",thres)],
     xlim=lim,ylim=lim,
     xlab="RMSE prediction (outside AOA)",ylab="RMSE model CV")
points(caseStudy$model_RMSE~caseStudy[,paste0("PredErrorNOTAOA_RMSE_",thres)],col="red",pch=16)
legend("topleft",legend="b",bty="n")
abline(0,1)
dev.off()
}


boxplot(dat[,c(which(names(dat)%in%c("model_RMSE")),
               which(grepl("PredErrorAOA_RMSE_",names(dat))))],
        names = c("CV",potentialthres),
        main="True prediction error inside AOA",ylim=c(0,1),
        col=c("blue",rep("grey",length(grepl("PredErrorAOA_RMSE_",names(dat))))))
par(new=TRUE)
boxplot(dat[,c(which(names(dat)%in%c("model_RMSE")),which(grepl("PredErrorNOTAOA_RMSE_",names(dat))))],
        names = c("CV",potentialthres),main="True prediction error outside AOA",ylim=c(0,1),
        col=c("blue",rep("orange",length(grepl("PredErrorAOA_RMSE_",names(dat))))))
#abline(h=1.5,v=1.5)                  

abline(mean(dat$model_RMSE),0,col="blue")      
legend("topleft",
       col=c("blue","grey","orange"),
pch=15,legend=c("CV","AOA","outside AOA"),bty="n")
