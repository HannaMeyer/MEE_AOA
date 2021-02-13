rm(list=ls())
setwd("/scratch/tmp/hmeyer1/MappingAOA/")

install.packages("/home/h/hmeyer1/R/CAST_0.4.3.tar.gz", repos = NULL,
                 lib="/home/h/hmeyer1/R/")



library(parallel)
library(hydroGOF,lib.loc="/home/h/hmeyer1/R/")
library(virtualspecies,lib.loc="/home/h/hmeyer1/R/")
library(caret,lib.loc="/home/h/hmeyer1/R/")
library(CAST,lib.loc="/home/h/hmeyer1/R/")
library(doParallel)
library(rgeos,lib.loc="/home/h/hmeyer1/R/")
#library(rgeos)
library(scam,lib.loc="/home/h/hmeyer1/R/")
library(raster)
ncores=1



countries <- c("Germany","Ireland","France", "Sweden","Netherlands", "Norway", "United Kingdom","Switzerland",
               "Austria","Finland","Belgium")


countriesOutlier <- c("Turkmenistan","Azerbaijan","Georgia","Syria","Lebanon") #if design==biasedWithOutlier A single point is set here
#nclusters <- 10 #number of clusters if design==clustered
maxdist <- 0.6 #maxdist for clustered samples if

design <- c("random")#, biasedWithOutlier

npoints <- c(25,50,75,100) # number of training samples
#npoints <- c(500)
seed <- c(10,20,30)
#seed <- c(10)
meansPCA <- as.list(as.data.frame(t(expand.grid(c(1,2,3),c(-1,0,1)))))
sdPCA <-  as.list(as.data.frame(t(expand.grid(c(1,2,3),c(1,2,3)))))
#meansPCA <- as.list(as.data.frame(t(expand.grid(c(3),c(1)))))
#sdPCA <-  as.list(as.data.frame(t(expand.grid(c(3),c(1)))))



simulateResponse <- c("bio2","bio5","bio10", "bio13","bio14","bio19") # variables used to simulate the response
studyarea <- c(-15, 65, 30, 75) # extent of study area. Default: Europe

predictors_global <- raster::getData('worldclim', var='bio', res=10)
wp <- extent(studyarea)
predictors <- crop(predictors_global,wp)

mask <- predictors[[1]]
values(mask)[!is.na(values(mask))] <- 1
mask <- rasterToPolygons(mask,dissolve=TRUE)

## Start running trhough each setting
settings <- expand.grid("npoints"=npoints,
                        "meansPCA"=meansPCA,
                        "sdPCA"=sdPCA,"seed"=seed,
                        "design"=design)
resultsTable <- settings

for (setting in 1:nrow(settings)){
  seed <- resultsTable$seed[setting]
  npoints <- resultsTable$npoints[setting]
  sdPCA <- resultsTable$sdPCA[[setting]]
  meansPCA <- resultsTable$meansPCA[[setting]]
  design <- resultsTable$design[[setting]]

  if (design=="clustered"){
    if (npoints==25){
      nclusters <- 5 #number of clusters if design==clustered
    }
    if (npoints==50){
      nclusters <- 10 #number of clusters if design==clustered
    }
    if (npoints==75){
      nclusters <- 10 #number of clusters if design==clustered
    }
    if (npoints>75){
      nclusters <- round(npoints/10,0)
    }
    resultsTable$nclusters[setting] <- nclusters
  }else{
    resultsTable$nclusters[setting] <- NA
  }
  ## Generate Predictors and Response
  response <- generateSpFromPCA(predictors[[simulateResponse]],
                                means = meansPCA,
                                sds = sdPCA,
                                plot=F)$suitab.raster

  ### for clustered sampling
  csample <- function(x,n,nclusters,maxdist,seed){
    set.seed(seed)
    cpoints <- sp::spsample(x, n = nclusters, type="random")
    result <- cpoints
    result$clstrID <- 1:length(cpoints)
    for (i in 1:length(cpoints)){
      ext <- rgeos::gBuffer(cpoints[i,], width = maxdist)
      newsamples <- sp::spsample(ext, n = (n-nclusters)/nclusters,
                                 type="random")
      newsamples$clstrID <- rep(i,length(newsamples))
      result <- rbind(result,newsamples)

    }
    result$ID <- 1:nrow(result)
    return(result)
  }
  ###
  set.seed(seed)
  if (design=="clustered"){
    samplepoints <- csample(mask,npoints,nclusters,maxdist=maxdist,seed=seed)
  }
  if (design=="biased"){
    countryboundaries <- readRDS("countries_gadm36_sp.rds")
    countriesselected <- sample(countries,4)
    countryboundaries <- countryboundaries[countryboundaries$NAME_ENGLISH%in%c(countriesselected),]
    samplepoints <- spsample(countryboundaries,npoints,"random")
  }
  if (design=="biasedWithOutlier"){
    countryboundaries <- readRDS("countries_gadm36_sp.rds")
    countriesselected <- sample(countries,4)
    countriesOutlier_selected <- sample(countriesOutlier,1)
    countryboundariesOut <- countryboundaries[countryboundaries$NAME_ENGLISH%in%c(countriesOutlier_selected),]
    countryboundaries <- countryboundaries[countryboundaries$NAME_ENGLISH%in%c(countriesselected),]
    samplepoints <- spsample(countryboundaries,npoints,"random")
    samplepoints <- rbind(samplepoints,spsample(countryboundariesOut,1,"random"))
  }

  if (design=="random"){
    samplepoints <- spsample(mask,npoints,"random")
  }

  # Model training and prediction

  trainDat <- extract(predictors,samplepoints,df=TRUE)

  if (design=="clustered"){
    trainDat <- merge(trainDat,samplepoints,by.x="ID",by.y="ID")
  }

  trainDat$response <- extract (response,samplepoints)
  trainDat <- trainDat[complete.cases(trainDat),]
  set.seed(seed)


  set.seed(seed)
  if(design!="clustered"){
    model <- train(trainDat[,names(predictors)],
                   trainDat$response,
                   method="rf",
                   importance=TRUE,
                   tuneGrid = expand.grid(mtry = c(2:length(names(predictors)))),
                   trControl = trainControl(method="cv",savePredictions = TRUE))


  }
  #if data are clustered, clustered CV is used:
  if(design=="clustered"){
    folds <- CreateSpacetimeFolds(trainDat, spacevar="clstrID",k=nclusters)
    model <- train(trainDat[,names(predictors)],
                   trainDat$response,
                   method="rf",
                   importance=TRUE,
                   tuneGrid = expand.grid(mtry = c(2:length(names(predictors)))),
                   trControl = trainControl(method="cv",index=folds$index,savePredictions = TRUE))

  }



  ## Prediction and error calculation

  prediction <- predict(predictors,model)
  truediff <- abs(prediction-response)

  ### AOA estimation
  AOA <- aoa(predictors,model)


  ### AOA calib
  AOA_calib <- calibrate_aoa(AOA,model,window.size = 10,multiCV = TRUE,length.out = 10,showPlot = FALSE)
  resultsTable$calibAOA_R2[setting] <- summary(lm(values(AOA_calib[[3]])~values(truediff)))$r.squared



  #Standard deviation from individual trees for comparison
  RFsd <- function(predictors,model){
    prep <- as.data.frame(predictors)
    prep[is.na(prep)] <- -9999
    pred_all <- predict(model$finalModel,prep,predict.all=TRUE)
    sds <-  apply(pred_all$individual,1,sd)
    predsd <- predictors[[1]]
    values(predsd)<-sds
    values(predsd)[prep[,1]==-9999] <- NA
    return(predsd)
  }
  predsd <- RFsd(predictors,model)

  ## Relationship with the true error
  resultsTable$DI_R2[setting] <- summary(lm(values(truediff)~values(AOA$DI)))$r.squared
  resultsTable$RFSD_R2[setting] <- summary(lm(values(truediff)~values(predsd)))$r.squared
  resultsTable$PredError_R2[setting] <- summary(lm(values(response)~values(prediction)))$r.squared
  resultsTable$PredError_RMSE[setting] <- rmse(values(response),values(prediction))



  preds <- model$pred[model$pred$mtry==model$bestTune$mtry,]

  resultsTable$model_R2[setting] <- summary(lm(preds$pred~preds$obs))$r.squared
  resultsTable$model_RMSE[setting] <- rmse(preds$pred,preds$obs)

  resultsTable$model_R2_mcv[setting] <- model$results$Rsquared[model$results$mtry==model$bestTune$mtry]
  resultsTable$model_RMSE_mcv[setting] <- model$results$RMSE[model$results$mtry==model$bestTune$mtry]


  ################################################################################
  # compare for different thresholds
  ################################################################################
  potentialthresholds <- c(attributes(AOA)$aoa_stats$threshold, attributes(AOA)$aoa_stats$threshold_stats)
  names(potentialthresholds)<- c("default",names(attributes(AOA)$aoa_stats$threshold_stats))

  for (th in 1:length(potentialthresholds)){
    thres <- potentialthresholds[th]
    thres_name <- names(potentialthresholds)[th]
    predictionAOI <- prediction
    values(predictionAOI)[values(AOA$DI)>thres] <- NA

    resultsTable[setting,paste0("ncell_NOTAOA_",thres_name)] <- sum(values(AOA$DI)>thres,na.rm=T)
    resultsTable[setting,paste0("ncell_AOA_",thres_name)] <- sum(values(AOA$DI)<=thres,na.rm=T)

    resultsTable[setting,paste0("PredErrorAOA_R2_",thres_name)] <- summary(lm(values(response)~values(predictionAOI)))$r.squared
    resultsTable[setting,paste0("PredErrorAOA_RMSE_",thres_name)] <- rmse(values(response),values(predictionAOI))


    resultsTable[setting,paste0("RFSD_R2_AOA_",thres_name)] <- summary(lm(values(truediff)[values(AOA$AOA)==1]~values(predsd)[values(AOA$AOA)==1]))$r.squared
    resultsTable[setting,paste0("RFSD_RMSE_AOA_",thres_name)] <- rmse(values(truediff)[values(AOA$AOA)==1],values(predsd)[values(AOA$AOA)==1])

    resultsTable[setting,paste0("DI_R2_AOA_",thres_name)] <- summary(lm(values(truediff)[values(AOA$DI)<=thres]~values(AOA$DI)[values(AOA$DI)<=thres]))$r.squared
    resultsTable[setting,paste0("DI_RMSE_AOA_",thres_name)] <- rmse(values(truediff)[values(AOA$DI)<=thres],values(AOA$DI)[values(AOA$DI)<=thres])


    predictionNOTAOI <- prediction
    values(predictionNOTAOI)[values(AOA$DI)<=thres] <- NA


    if(sum(!is.na(values(predictionNOTAOI)))<2){
      resultsTable[setting,paste0("PredErrorNOTAOA_R2_",thres_name)] <- NA
      resultsTable[setting,paste0("PredErrorNOTAOA_RMSE_",thres_name)] <- NA
      resultsTable[setting,paste0("RFSD_R2_NOTAOA_",thres_name)] <- NA
      resultsTable[setting,paste0("RFSD_RMSE_NOTAOA_",thres_name)] <- NA
      resultsTable[setting,paste0("DI_RMSE_NOTAOA_",thres_name)] <- NA
      resultsTable[setting,paste0("DI_R2_NOTAOA_",thres_name)] <- NA

    }else{
      resultsTable[setting,paste0("PredErrorNOTAOA_R2_",thres_name)] <- summary(lm(values(response)~values(predictionNOTAOI)))$r.squared
      resultsTable[setting,paste0("PredErrorNOTAOA_RMSE_",thres_name)] <- rmse(values(response),values(predictionNOTAOI))
      resultsTable[setting,paste0("RFSD_R2_NOTAOA_",thres_name)] <- summary(lm(values(truediff)[values(AOA$DI)>thres]~values(predsd)[values(AOA$DI)>thres]))$r.squared
      resultsTable[setting,paste0("RFSD_RMSE_NOTAOA_",thres_name)] <- rmse(values(truediff)[values(AOA$DI)>thres],values(predsd)[values(AOA$DI)>thres])


      resultsTable[setting,paste0("DI_R2_NOTAOA_",thres_name)] <- summary(lm(values(truediff)[values(AOA$DI)>thres]~values(AOA$DI)[values(AOA$DI)>thres]))$r.squared
      resultsTable[setting,paste0("DI_RMSE_NOTAOA_",thres_name)] <- rmse(values(truediff)[values(AOA$DI)>thres],values(AOA$DI)[values(AOA$DI)>thres])


    }
  }

  print(paste0(setting," of ",nrow(settings)," done..."))
}

save(resultsTable,file="resultsTable.RData")
