rm(list=ls())
setwd("/scratch/tmp/hmeyer1/MappingAOA/")

install.packages("/home/h/hmeyer1/R/CAST_0.4.1.tar.gz", repos = NULL,
                 lib="/home/h/hmeyer1/R/")

library(parallel)
library(hydroGOF,lib.loc="/home/h/hmeyer1/R/")
library(virtualspecies,lib.loc="/home/h/hmeyer1/R/")
library(caret,lib.loc="/home/h/hmeyer1/R/")
library(CAST,lib.loc="/home/h/hmeyer1/R/")

npoints <- c(25,50,75,100) # number of training samples
seed <- c(10,20,30)
meansPCA <- as.list(as.data.frame(t(expand.grid(c(1,2,3),c(-1,0,1)))))
sdPCA <-  as.list(as.data.frame(t(expand.grid(c(1,2,3),c(1,2,3)))))

simulateResponse <- c("bio2","bio5","bio10", "bio13","bio14","bio19") # variables used to simulate the response
studyarea <- c(-15, 65, 30, 75) # extent of study area. Default: Europe

predictors_global <- getData('worldclim', var='bio', res=10)
wp <- extent(studyarea)
predictors <- crop(predictors_global,wp)

mask <- predictors[[1]]
values(mask)[!is.na(values(mask))] <- 1
mask <- rasterToPolygons(mask,dissolve=TRUE)

## Start running trhough each setting
settings <- expand.grid("npoints"=npoints,
                        "meansPCA"=meansPCA,
                        "sdPCA"=sdPCA,"seed"=seed)
resultsTable <- settings

for (setting in 1:nrow(settings)){
  seed <- resultsTable$seed[setting]
  npoints <- resultsTable$npoints[setting]
  sdPCA <- resultsTable$sdPCA[[setting]]
  meansPCA <- resultsTable$meansPCA[[setting]]

  ## Generate Predictors and Response
  response <- generateSpFromPCA(predictors[[simulateResponse]],
                                means = meansPCA,
                                sds = sdPCA,
                                plot=F)$suitab.raster
  set.seed(seed)
  samplepoints <- spsample(mask,npoints,"random")

  # Model training and prediction

  trainDat <- extract(predictors,samplepoints,df=TRUE)
  trainDat$response <- extract (response,samplepoints)
  trainDat <- trainDat[complete.cases(trainDat),]
  set.seed(seed)
  model <- train(trainDat[,names(predictors)],trainDat$response,
                 method="rf",importance=TRUE,tuneGrid = expand.grid(mtry = c(2:length(names(predictors)))),
                 trControl = trainControl(method="cv"))

  ## Prediction and error calculation

  prediction <- predict(predictors,model)
  truediff <- abs(prediction-response)

  ### AOA estimation
  uncert <- aoa(trainDat,predictors, variables = names(predictors),model=model)


  # Standard deviation from individual trees for comparison
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
  resultsTable$DI_R2[setting] <- summary(lm(values(truediff)~values(uncert$DI)))$r.squared
  resultsTable$RFSD_R2[setting] <- summary(lm(values(truediff)~values(predsd)))$r.squared
  resultsTable$PredError_R2[setting] <- summary(lm(values(response)~values(prediction)))$r.squared
  resultsTable$PredError_RMSE[setting] <- rmse(values(response),values(prediction))
  resultsTable$model_R2[setting] <- model$results$Rsquared[model$results$mtry==model$bestTune$mtry]
  resultsTable$model_RMSE[setting] <- model$results$RMSE[model$results$mtry==model$bestTune$mtry]


  ################################################################################
  # compare for different thresholds
  ################################################################################

  for (th in 1:length(attributes(uncert)$aoa_stats$threshold_stats)){
    thres <- attributes(uncert)$aoa_stats$threshold_stats[th]
    thres_name <- names(attributes(uncert)$aoa_stats$threshold_stats)[th]
    predictionAOI <- prediction
    values(predictionAOI)[values(uncert$DI)>thres] <- NA
    resultsTable[setting,paste0("PredErrorAOA_R2_",thres_name)] <- summary(lm(values(response)~values(predictionAOI)))$r.squared
    resultsTable[setting,paste0("PredErrorAOA_RMSE_",thres_name)] <- rmse(values(response),values(predictionAOI))
    predictionNOTAOI <- prediction
    values(predictionNOTAOI)[values(uncert$DI)<=thres] <- NA
    if(sum(!is.na(values(predictionNOTAOI)))<2){
      resultsTable[setting,paste0("PredErrorNOTAOA_R2_",thres_name)] <- NA
      resultsTable[setting,paste0("PredErrorNOTAOA_RMSE_",thres_name)] <- NA
    }else{
      resultsTable[setting,paste0("PredErrorNOTAOA_R2_",thres_name)] <- summary(lm(values(response)~values(predictionNOTAOI)))$r.squared
      resultsTable[setting,paste0("PredErrorNOTAOA_RMSE_",thres_name)] <- rmse(values(response),values(predictionNOTAOI))
    }
  }

  print(paste0(setting," of ",nrow(settings)," done..."))
}

save(resultsTable,file="resultsTable.RData")
