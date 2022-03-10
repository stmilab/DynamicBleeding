source('calcDeciles.R')
#First load datasets of Labels
load("Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/labels_v8_7JUN2016.RData")

load("Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/fulldata_raopeople_glm_five_fold_data_V8_7JUN2016.RData") #Rao+People GLM
rao.preds <- list()
rao.preds[[1]] <- list.crossval.results[[1]][[2]]
rao.preds[[2]] <- list.crossval.results[[2]][[2]]
rao.preds[[3]] <- list.crossval.results[[3]][[2]]
rao.preds[[4]] <- list.crossval.results[[4]][[2]]
rao.preds[[5]] <- list.crossval.results[[5]][[2]]

rao.thresholds <- c(list.crossval.results[[1]][[4]][[1]]$v,list.crossval.results[[2]][[4]][[1]]$v,
                              list.crossval.results[[3]][[4]][[1]]$v,list.crossval.results[[4]][[4]][[1]]$v,
                              list.crossval.results[[5]][[4]][[1]]$v)

labels.all <- c(test1.labels, test2.labels, test3.labels, test4.labels, test5.labels)
rao.probs <- c(rao.preds[[1]],rao.preds[[2]],
                         rao.preds[[3]],rao.preds[[4]],
                         rao.preds[[5]])
# calib.results.clinical <- calcDeciles(full.clinical.probs, labels.all)
# calib.results.all <- calcDeciles(full.all.probs, labels.all)
dec.vals.clinical <- decPoints(rao.preds, 5)
#dec.vals.all <- decPoints(full.all.preds, 5)

mean.deciles.clinical <- vector()

for(idx in 1:length(dec.vals.clinical)) {
  mean.deciles.clinical[idx] <- mean(dec.vals.clinical[[idx]])
}

calib.results.clinical <- sortDeciles(rao.probs, labels.all, mean.deciles.clinical)
#calib.results.all <- sortDeciles(full.all.probs, labels.all, mean.deciles.all)




#FULL XGB CLINICAL
load("Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/XGB_AUC_clinical_five_fold_data_V8_7JUN2016.RData")

full.clinical.preds <- list()
full.clinical.preds[[1]] <- xgboost.list.responses[[1]]
full.clinical.preds[[2]] <- xgboost.list.responses[[2]]
full.clinical.preds[[3]] <- xgboost.list.responses[[3]]
full.clinical.preds[[4]] <- xgboost.list.responses[[4]]
full.clinical.preds[[5]] <- xgboost.list.responses[[5]]

full.clinical.thresholds <- c(xgboost.list.f[[1]][[1]]$v,xgboost.list.f[[2]][[1]]$v,
                              xgboost.list.f[[3]][[1]]$v,xgboost.list.f[[4]][[1]]$v,
                              xgboost.list.f[[5]][[1]]$v)

#Full All
load("Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/full_XGB_AUC_all_five_fold_data_V8_7JUN2016.RData")
full.all.preds <- list()
full.all.preds[[1]] <- xgboost.list.responses[[1]]
full.all.preds[[2]] <- xgboost.list.responses[[2]]
full.all.preds[[3]] <- xgboost.list.responses[[3]]
full.all.preds[[4]] <- xgboost.list.responses[[4]]
full.all.preds[[5]] <- xgboost.list.responses[[5]]

labels.all <- c(test1.labels, test2.labels, test3.labels, test4.labels, test5.labels)
full.clinical.probs <- c(full.clinical.preds[[1]],full.clinical.preds[[2]],
                         full.clinical.preds[[3]],full.clinical.preds[[4]],
                         full.clinical.preds[[5]])

full.all.probs <- c(full.all.preds[[1]],full.all.preds[[2]],
                         full.all.preds[[3]],full.all.preds[[4]],
                         full.all.preds[[5]])
# calib.results.clinical <- calcDeciles(full.clinical.probs, labels.all)
# calib.results.all <- calcDeciles(full.all.probs, labels.all)
dec.vals.clinical <- decPoints(full.clinical.preds, 5)
dec.vals.all <- decPoints(full.all.preds, 5)

mean.deciles.clinical <- vector()
mean.deciles.all <- vector()

for(idx in 1:length(dec.vals.clinical)) {
  mean.deciles.clinical[idx] <- mean(dec.vals.clinical[[idx]])
  mean.deciles.all[idx] <- mean(dec.vals.all[[idx]])
}

calib.results.clinical <- sortDeciles(full.clinical.probs, labels.all, mean.deciles.clinical)
calib.results.all <- sortDeciles(full.all.probs, labels.all, mean.deciles.all)

#FULL XGB CLINICAL
load("Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/femoral_and_radial_XGB_AUC_clinical_five_fold_data_V8_7JUN2016.RData")
load("Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/femoral_and_radial_labels_v8_7JUN2016.RData")
fem.radial.labels.all <- c(test1.labels, test2.labels, test3.labels, test4.labels, test5.labels)
fem.radial.preds <- list()
fem.radial.preds[[1]] <- xgboost.list.responses[[1]]
fem.radial.preds[[2]] <- xgboost.list.responses[[2]]
fem.radial.preds[[3]] <- xgboost.list.responses[[3]]
fem.radial.preds[[4]] <- xgboost.list.responses[[4]]
fem.radial.preds[[5]] <- xgboost.list.responses[[5]]

fem.radial.thresholds <- c(xgboost.list.f[[1]][[1]]$v,xgboost.list.f[[2]][[1]]$v,
                              xgboost.list.f[[3]][[1]]$v,xgboost.list.f[[4]][[1]]$v,
                              xgboost.list.f[[5]][[1]]$v)

#FULL XGB CLINICAL
load("Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/femoral_XGB_AUC_clinical_five_fold_data_V8_7JUN2016.RData")
load("Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/femoral_labels_v8_7JUN2016.RData")
fem.labels.all <- c(test1.labels, test2.labels, test3.labels, test4.labels, test5.labels)
fem.preds <- list()
fem.preds[[1]] <- xgboost.list.responses[[1]]
fem.preds[[2]] <- xgboost.list.responses[[2]]
fem.preds[[3]] <- xgboost.list.responses[[3]]
fem.preds[[4]] <- xgboost.list.responses[[4]]
fem.preds[[5]] <- xgboost.list.responses[[5]]

fem.thresholds <- c(xgboost.list.f[[1]][[1]]$v,xgboost.list.f[[2]][[1]]$v,
                           xgboost.list.f[[3]][[1]]$v,xgboost.list.f[[4]][[1]]$v,
                           xgboost.list.f[[5]][[1]]$v)

#FULL XGB CLINICAL
load("Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/radial_XGB_AUC_clinical_five_fold_data_V8_7JUN2016.RData")
load("Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/radial_labels_v8_7JUN2016.RData")
radial.labels.all <- c(test1.labels, test2.labels, test3.labels, test4.labels, test5.labels)
radial.preds <- list()
radial.preds[[1]] <- xgboost.list.responses[[1]]
radial.preds[[2]] <- xgboost.list.responses[[2]]
radial.preds[[3]] <- xgboost.list.responses[[3]]
radial.preds[[4]] <- xgboost.list.responses[[4]]
radial.preds[[5]] <- xgboost.list.responses[[5]]

radial.thresholds <- c(xgboost.list.f[[1]][[1]]$v,xgboost.list.f[[2]][[1]]$v,
                           xgboost.list.f[[3]][[1]]$v,xgboost.list.f[[4]][[1]]$v,
                           xgboost.list.f[[5]][[1]]$v)


fem.radial.probs <- c(fem.radial.preds[[1]],fem.radial.preds[[2]],
                      fem.radial.preds[[3]],fem.radial.preds[[4]],
                      fem.radial.preds[[5]])
fem.probs <- c(fem.preds[[1]],fem.preds[[2]],
                      fem.preds[[3]],fem.preds[[4]],
                      fem.preds[[5]])
radial.probs <- c(radial.preds[[1]],radial.preds[[2]],
                  radial.preds[[3]],radial.preds[[4]],
                  radial.preds[[5]])


dec.vals.fem.radial <- decPoints(fem.radial.preds, 5)
dec.vals.fem <- decPoints(fem.preds, 5)
dec.vals.radial <- decPoints(radial.preds, 5)

mean.deciles.fem.radial <- vector()
mean.deciles.fem <- vector()
mean.deciles.radial <- vector()

for(idx in 1:length(dec.vals.fem.radial)) {
  mean.deciles.fem.radial[idx] <- mean(dec.vals.fem.radial[[idx]])
  mean.deciles.fem[idx] <- mean(dec.vals.fem[[idx]])
  mean.deciles.radial[idx] <- mean(dec.vals.radial[[idx]])
}

calib.results.fem.radial <- sortDeciles(fem.radial.probs, fem.radial.labels.all, mean.deciles.fem.radial)
calib.results.fem <- sortDeciles(fem.probs, fem.labels.all, mean.deciles.fem)
calib.results.radial <- sortDeciles(radial.probs, radial.labels.all, mean.deciles.radial)

save.image(file='Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/model_calibration.RData')


risk.diff <- full.all.probs - rao.probs
risk.diff.sorted <- risk.diff[order(risk.diff)]
risk.diff.labels <- labels.all[order(risk.diff)]
risk.diff.labels.factor <- risk.diff.labels
risk.diff.labels.factor[risk.diff.labels.factor == 0] <- 'No Bleed'
risk.diff.labels.factor[risk.diff.labels.factor == 1] <- 'Bleed'
risk.diff.labels.factor <- as.factor(risk.diff.labels.factor)
#Now plot difference
library(ggplot2)
df.risk <- data.frame(risk.diff.sorted, risk.diff.labels.factor, seq(1, length(risk.diff.sorted), 1))
colnames(df.risk) <- c('Risk', 'Labels', 'Index')

p <- ggplot(df.risk)
p <- p + theme_bw() + xlab('Sorted Index of Patients') + ylab('Difference in Calculated Risk')
p <- p + geom_point(aes(x=Index, y=Risk, color=Labels))
p
