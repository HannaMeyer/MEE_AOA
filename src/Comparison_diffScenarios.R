rm(list=ls())
library(ggplot2)
library(reshape2)
dat <- get(load("../data/resultsTable_small.RData"))

means=as.data.frame(matrix(unlist(dat$meansPCA),ncol=2,byrow = T))
sds=as.data.frame(matrix(unlist(dat$sdPCA),ncol=2,byrow = T))

caseStudy <- dat[dat$npoints==50&dat$seed==10&
                         sds[,1]==2&sds[,2]==2&
                         means[,1]==3&means[,2]==-1,]



pdf("../figures/comparison_modelRuns_box.pdf",width=12,height=7)
par(mfrow=c(1,2),mar=c(7,3,1,1))
boxplot(dat$model_RMSE,dat$PredError_RMSE,
        dat$`PredErrorAOA_RMSE`,
        dat$`PredErrorNOTAOA_RMSE`,
        names=c("CV RMSE","RMSE all", "RMSE AOA",
                 "RMSE !AOA"),las=2,notch=T)

boxplot(dat$model_R2,dat$PredError_R2,
        dat$`PredErrorAOA_R2`,
        dat$`PredErrorNOTAOA_R2`,
        names=c("CV R2","R2 all", "R2 AOA",
                "R2 !AOA"),las=2,notch=T)
dev.off()






pdf("../figures/comparison_modelRuns_scatter.pdf",width=9,height=5)
par(mfrow=c(1,2))
#lim <- c(min(c(dat$model_RMSE,dat$PredError_RMSE)),
#  max(c(dat$model_RMSE,dat$PredError_RMSE)))
#plot(dat$model_RMSE~dat$PredError_RMSE,
#     xlim=lim,ylim=lim,
#     xlab="RMSE prediction (entire study area)",ylab="RMSE model CV")
#points(caseStudy$model_RMSE~caseStudy$PredError_RMSE,col="red",pch=16)
#abline(0,1)
#legend("topleft",legend="a",bty="n")

lim <- c(min(c(dat$model_RMSE,dat$`PredErrorAOA_RMSE`)),
         max(c(dat$model_RMSE,dat$`PredErrorAOA_RMSE`)))
plot(dat$model_RMSE~dat$`PredErrorAOA_RMSE`,
     xlim=lim,ylim=lim,
     xlab="RMSE prediction (AOA)",ylab="RMSE model CV"
     #,main="AOA"
     )
points(caseStudy$model_RMSE~caseStudy$`PredErrorAOA_RMSE`,col="red",pch=16)
legend("topleft",legend="a",bty="n")
abline(0,1)
lim <- c(min(c(dat$model_RMSE,dat$`PredErrorNOTAOA_RMSE`),na.rm=T),
         max(c(dat$model_RMSE,dat$`PredErrorNOTAOA_RMSE`),na.rm=T))
plot(dat$model_RMSE~dat$`PredErrorNOTAOA_RMSE`,
     xlim=lim,ylim=lim,
     xlab="RMSE prediction (outside AOA)",ylab="RMSE model CV"
     #,main="Outside AOA"
     )
points(caseStudy$model_RMSE~caseStudy$`PredErrorNOTAOA_RMSE`,col="red",pch=16)

legend("topleft",legend="b",bty="n")
abline(0,1)
#mtext("How well does the model CV error reflect the true error within and outside of the AOA?",
#      side = 3, line = -1.5, outer = TRUE)
dev.off()
