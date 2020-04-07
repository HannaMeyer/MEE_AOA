## This script creates figures used to visualize the methodology
rm(list=ls())
#install_github("HannaMeyer/CAST")
library(CAST)
library(knitr)
library(scatterplot3d)
library(ggplot2)

################################################################################
# Figure 3d scatter
################################################################################
set.seed(15)
samples <- data.frame("a"=sample(1:50,30,replace=T), #1:50,30
                      "b"=sample(11:20,30,replace=T), #11:20,30
                      "c"=sample(12:16,30,replace=T)) #12:16,30
samples <- scale(samples[,1:3])


scaleparam <- attributes(samples)
samples<- data.frame(rbind(samples,
                           scale(data.frame("a"=-28,"b"=12,"c"=14),
                                 center=scaleparam$`scaled:center`,
                                 scale=scaleparam$`scaled:scale`)
))


samples$type=as.character(c(rep("training",30),"new"))

minvalues <- apply(samples[1:30,1:3],2,function(x){min(x,na.rm=TRUE)}) 
maxvalues <- apply(samples[1:30,1:3],2,function(x){max(x,na.rm=TRUE)}) 
maxdist <- dist(rbind(maxvalues,minvalues))


tmp <- NA
for (i in 1:nrow(samples[1:30,1:3])){
  mindist <- dist(rbind(samples[31,1:3],samples[i,1:3]))
  mindist <- pmin(mindist,tmp,na.rm=T)
  if(i>1&&mindist<tmp){
    whichIsMin <- i
  }
  tmp <- mindist
}

colors <- c("grey", "black")
colors <- colors[as.numeric(factor(samples$type))]
pchs <- c(16,4)
pchs <- pchs[as.numeric(factor(samples$type))]


cairo_pdf("../figures/3dscatter.pdf",width=7,height = 7)
plt <- scatterplot3d(samples[,1:3], pch = pchs,
                     grid=TRUE,lwd=2,cex.symbols = 1.1)
tmp <- lapply(1:30,FUN=function(t){
  plt$points3d(x=c(samples[t,1],samples[31,1]),
               y=c(samples[t,2],samples[31,2]),
               z=c(samples[t,3],samples[31,3]),
               type="l", col="grey", lwd=1,lty=1)
})


plt$points3d(x=c(samples[whichIsMin,1],samples[31,1]),
             y=c(samples[whichIsMin,2],samples[31,2]),
             z=c(samples[whichIsMin,3],samples[31,3]),
             type="l", col="black", lwd=2,lty=1)
attributes(mindist)$Labels[2]


plt$points3d(samples[,1:3], pch = pchs,
             lwd=2)
plt$box3d()

legend("topleft",pch=c(4,16,NA,NA),lty=c(NA,NA,1,1),
       lwd=c(NA,NA,2,1),
       col=c("black","black","black","grey"),
       legend=c("training","new","minimum distance","distances"),bty="n",cex=0.9)
dev.off()

### Return corresponding AOA statistics:

uncert <- aoa(samples[1:30,],samples[31,],variables=c("a","b","c"))

print(attributes(uncert))
cat(paste0("average mean distance in the training data = ",round(attributes(uncert)$aoa_stats$AvrgMean_train,3),
           ", \ndAverage min distance in the training data = ",round(attributes(uncert)$aoa_stats$AvrgMin_train,3),
           ", \ndSD of min distance in the training data = ",round(attributes(uncert)$aoa_stats$SdMin_train,3),
           ", \nthreshold = ",round(attributes(uncert)$aoa_stats$threshold,3),
           ", \ndistance to nearest training point = ",round(mindist,3),
           ", \nAOAI for the new data point = ",round(unlist(uncert$AOAI),3)))


################################################################################
# Relationship predictor, response, uncertainty
################################################################################

reference  <- c(1:7, 7.8, 8.3, 8.7, 8.9, 9,rep(9.1,8))
train <- data.frame("x"=c(1:6),"y"=reference[1:6])
newdat <- data.frame("x"=1:20)
tmp <- NA
for (i in 1:nrow(train)){
  mindist <- apply(newdat,1,function(x){dist(rbind(x,train[i,1]))})
  mindist <- pmin(mindist,tmp,na.rm=T)
  tmp <- mindist
}
trainDist <- as.matrix(dist(train$x))
diag(trainDist) <- NA
trainDist_min <- apply(trainDist,1,FUN=function(x){min(x,na.rm=T)})
trainDist_mean <- apply(trainDist,1,FUN=function(x){mean(x,na.rm=T)})
trainDist_avrgmean <- mean(trainDist_mean)
mindist <- -mindist/trainDist_avrgmean
predictions <- reference
predictions[nrow(train):length(predictions)]<- train$y[nrow(train)]

###Plot:
pdf("../figures/concept_relationship.pdf",width=10,height=5)
par(mar=c(4,4,1,1),mfrow=c(1,2))
plot(newdat$x,reference,type="l",lty=1,
     xlab="Predictor",ylab="Response",lwd=2)
lines(predictions,lty=2,lwd=2,col="red")
points(train$x,train$y)
legend("bottomright",lty=c(1,2,NA),pch=c(NA,NA,1),lwd=c(2,2,NA),
       col=c("black","red","black","black"),
       legend=c("Reference","Prediction","Sample"),bty="n",cex=0.8)
legend("topleft",legend="a",bty="n")
plot(newdat$x, mindist,type="l",xlab="Predictor",ylab="Applicability index",lwd=2)
legend("topright",legend="b",bty="n")
dev.off()

