## This script creates figures used to visualize the methodology
rm(list=ls())
#install_github("HannaMeyer/CAST")
library(CAST)
library(knitr)
library(scatterplot3d)
library(ggplot2)
library(randomForest)
library(quantregForest)

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

uncert <- aoa(samples[31,],train=samples[1:30,],variables=c("a","b","c"))

print(attributes(uncert))
cat(paste0("average mean distance in the training data = ",round(attributes(uncert)$aoa_stats$Mean_train,3),
            ", \nthreshold = ",round(attributes(uncert)$aoa_stats$threshold,3),
           ", \ndistance to nearest training point = ",round(mindist,3),
           ", \nDI for the new data point = ",round(unlist(uncert$DI),3)))



################################################################################
# Relationship predictor, response, prediction intervals
################################################################################

f = function(x, z = 0.8, b = 0, e = 0.3) {
  z * x+ b + rnorm(length(x)) * e
}
set.seed(131)
x = sort(runif(30, 0.7, 1.2))
y = f(x, z = 1)
m = lm(y~x)
x_new = seq(0,2.2,length.out=100)
newdata = data.frame(x = x_new, true = f(x_new, z = 1, e = 0))
p = predict(m, newdata, interval = "prediction")
ylim = range(p)
ylim = c(min(newdata$true),max(newdata$true))
xlim = c(0.1,2.1)
xlab = "predictor"
ylab = "response"

pdf("../figures/fig1.pdf",width=10,height=5)
par(mfrow = c(1,2 ))
par(mar = c(5.1, 4.1, 2.1, 0.1))
plot(y~x, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,pch=16)
newdata = cbind(newdata, p)
lines(true~x, newdata, col = 'green')
lines(fit~x, newdata, col = 'black')
lines(lwr~x, newdata, lty = 2, col = 'black')
lines(upr~x, newdata, lty = 2, col = 'black')
legend("topleft",legend="a",bty="n")
legend("bottomright",lty=c(1,2,1,NA),pch=c(NA,NA,NA,16),
       col=c("black","black","green","black"),
       legend=c("prediction","interval","truth","training"),bty="n")


f <- function(x){
  wave.1 <- sin(3*x)+1
  wave.2 <- sin(12*x)+1
  wave.3 <- 0.9 * wave.1 + 0.25 * wave.2
}

x <- seq(0,2.2,0.001)
newdata <- data.frame("x"=x,"true" = f(x))
set.seed(1)
x <- c(runif(10,0,0.4),runif(18,0.8,1.5),runif(11,1.5,1.7))
set.seed(10)
y <- f(x)+runif(length(x),-0.1,0.1)


ylim <- c(min(newdata$true),max(newdata$true))
xlim <- c(0.1,2.1)

#### Random Forest with predictions from individual trees
set.seed(10)
rf = randomForest(y~x, data.frame(x=x,y=y))
pr = predict(rf, newdata, predict.all = TRUE)
par(mar = c(5.1, 1.1, 2.1, 2.1))
plot(y~x,xlim=xlim,ylim=ylim,xlab = xlab, ylab = NA,yaxt="n",pch=16)
lines(true~x, newdata, col = 'green')
lines(pr$aggregate~newdata$x, col = 'black')
lwr = apply(pr$individual, 1, quantile, 0.025)
upr = apply(pr$individual, 1, quantile, 0.975)
lines(lwr~newdata$x, col = 'black', lty = 2)
lines(upr~newdata$x, col = 'black', lty = 2)
legend("topleft",legend="b",bty="n")

### Quantile regression forest
#set.seed(10)
#qrf =  quantregForest(data.frame(x),y)
#pr  <- predict(qrf,  newdata)
#par(mar = c(5.1, 1.1, 2.1, 2.1))
#plot(y~x,xlim=xlim,ylim=ylim,xlab = xlab, ylab = NA,yaxt="n",pch=16)
#lines(true~x, newdata, col = 'green')
#lines(pr[,2]~newdata$x, col = 'black')
#lines(pr[,1]~newdata$x, col = 'black', lty = 2)
#lines(pr[,3]~newdata$x, col = 'black', lty = 2)
#legend("topleft",legend="b",bty="n")

dev.off()

################################################################################
# Relationship predictor, response, AOA, DI
################################################################################

AOA <- aoa(newdata, train=data.frame("x"=x))

pdf("../figures/fig_last.pdf",width=6,height=8)
par(mfrow=c(2,1))
par(mar = c(0.1, 5.1, 2.1, 0.1))
plot(y~x,xlim=xlim,ylim=ylim,xlab = NA, xaxt='n',
     ylab = ylab,pch=16)
lines(true~x, newdata, col = 'green')
lines(pr$aggregate~newdata$x, col = 'black')
lwr = apply(pr$individual, 1, quantile, 0.025)
upr = apply(pr$individual, 1, quantile, 0.975)
lines(lwr~newdata$x, col = 'black', lty = 2)
lines(upr~newdata$x, col = 'black', lty = 2)



bp <-which(c(FALSE, tail(AOA$AOA,-1) != head(AOA$AOA,-1)))
polygon(x=c(newdata$x[bp[1]],newdata$x[bp[2]],newdata$x[bp[2]],newdata$x[bp[1]]),
        y=c(0,0,2.3,2.3),col=rgb(0.6,0.6,0.6,alpha=0.5),border = NA)

polygon(x=c(newdata$x[bp[3]],max(newdata$x)-0.022,max(newdata$x)-0.022,newdata$x[bp[3]]),
        y=c(0,0,2.3,2.3),col=rgb(0.6,0.6,0.6,alpha=0.5),
        border = NA)

legend("bottomleft",lty=c(1,2,1,NA,NA),pch=c(NA,NA,NA,16,15),
       col=c("black","black","green","black","grey"),
       legend=c("prediction","interval","truth","training","not in AOA")
       ,bty="n",bg="white")
legend("topleft",legend="a",bty="n")

par(mar = c(6, 5.1, 0.7, 0.1))
plot(AOA$DI~newdata$x,xlim=xlim,xlab = xlab, ylab = "DI",
     type="l",lwd=2)
abline(attributes(AOA)$aoa_stats$threshold,0,lty=2)
legend("topleft",legend="b",bty="n")
dev.off()

