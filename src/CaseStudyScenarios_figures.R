# This script visualizes the results of several case study settings
# It is based on the results (data/resultsTable.RData) created with "CaseStudyScenarios.R"

rm(list=ls())
library(ggplot2)
library(reshape2)
dat <- get(load("../data/resultsTable.RData"))

means <- as.data.frame(matrix(unlist(dat$meansPCA),ncol=2,byrow = T))
sds <- as.data.frame(matrix(unlist(dat$sdPCA),ncol=2,byrow = T))

# these are the settings of the specific case study shown in the paper:
caseStudy <- dat[dat$npoints==50&dat$seed==10&
                         sds[,1]==2&sds[,2]==2&
                         means[,1]==3&means[,2]==-1,]


#### How well does the model CV error represents the prediction error inside and outside the AOA?

pdf("../figures/comparison_modelRuns_scatter.pdf",width=9,height=5)
par(mfrow=c(1,2))

lim <- c(min(c(dat$model_RMSE_mcv,dat$PredErrorAOA_RMSE,dat$model_RMSE_mcv,dat$PredErrorNOTAOA_RMSE),na.rm=T),
         max(c(dat$model_RMSE_mcv,dat$PredErrorAOA_RMSE,dat$model_RMSE_mcv,dat$PredErrorNOTAOA_RMSE),na.rm=T))
plot(dat$model_RMSE_mcv~dat$PredErrorAOA_RMSE,
     xlim=lim,ylim=lim,
     xlab="RMSE prediction (AOA)",ylab="RMSE model CV")
points(caseStudy$model_RMSE_mcv~caseStudy$PredErrorAOA_RMSE,col="red",pch=16)
legend("topleft",legend="a",bty="n")
abline(0,1)
plot(dat$model_RMSE_mcv~dat$PredErrorNOTAOA_RMSE,
     xlim=lim,ylim=lim,
     xlab="RMSE prediction (outside AOA)",ylab="RMSE model CV")
points(caseStudy$model_RMSE_mcv~caseStudy$PredErrorNOTAOA_RMSE,col="red",pch=16)
legend("topleft",legend="b",bty="n")
abline(0,1)
dev.off()

## R2 of the calibrated AOA
summary(dat$calibAOA_R2)

