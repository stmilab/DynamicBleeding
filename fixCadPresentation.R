#Fix CADPresentation
source('./catToBin.R')
for(f in c(1,2,3,4,5)) {
  train.fold <- ncdr.data.crossval[[1]][[f]]
  test.fold <- ncdr.data.crossval[[2]][[f]]
  
  train.fold$CADPresentation[which(is.na(train.fold$CADPresentation))] <- 1
  test.fold$CADPresentation[which(is.na(test.fold$CADPresentation))] <- 1
  cadCatTrain <- catToBin(as.factor(train.fold$CADPresentation))
  cadCatTest <- catToBin(as.factor(test.fold$CADPresentation))
  colnames(cadCatTrain) <- c('CADPresentation_NoneOrMissing', 'CADPresentation_SxUnlikelyIschemic',
                             'CADPresentation_StableAngina', 'CADPresentation_UnstableAngina', 'CADPresentation_NSTEMI', 'CADPresentation_STEMI')
  colnames(cadCatTest) <- c('CADPresentation_NoneOrMissing', 'CADPresentation_SxUnlikelyIschemic',
                            'CADPresentation_StableAngina', 'CADPresentation_UnstableAngina', 'CADPresentation_NSTEMI', 'CADPresentation_STEMI')
  train.fold <- cbind(train.fold, cadCatTrain)
  test.fold <- cbind(test.fold , cadCatTest)
  
  train.fold$CADPresentation <- NULL
  test.fold$CADPresentation <- NULL
  
  ncdr.data.crossval[[1]][[f]] <- train.fold 
  ncdr.data.crossval[[2]][[f]] <- test.fold
}


save('ncdr.data.crossval', file='~/NCDR-ACC/NCDR/BJMDATA/CPBLEED/DynamicModel/NCDR_NoCabg_Bleed_Crossval_v2_09JUN2017.RData')