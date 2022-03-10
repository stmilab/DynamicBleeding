#Fix CADPresentation
source('./catToBin.R')
load(file='~/NCDR-ACC/NCDR/BJMDATA/CPBLEED/DynamicModel/NCDR_NoCabg_Bleed_Crossval_v2_09JUN2017.RData')
# for(f in c(1,2,3,4,5)) {
#   train.fold <- ncdr.data.crossval[[1]][[f]]
#   test.fold <- ncdr.data.crossval[[2]][[f]]
#   
#   train.fold$CADPresentation[which(is.na(train.fold$CADPresentation))] <- 1
#   test.fold$CADPresentation[which(is.na(test.fold$CADPresentation))] <- 1
#   cadCatTrain <- catToBin(as.factor(train.fold$CADPresentation))
#   cadCatTest <- catToBin(as.factor(test.fold$CADPresentation))
#   colnames(cadCatTrain) <- c('CADPresentation_NoneOrMissing', 'CADPresentation_SxUnlikelyIschemic',
#                              'CADPresentation_StableAngina', 'CADPresentation_UnstableAngina', 'CADPresentation_NSTEMI', 'CADPresentation_STEMI')
#   colnames(cadCatTest) <- c('CADPresentation_NoneOrMissing', 'CADPresentation_SxUnlikelyIschemic',
#                             'CADPresentation_StableAngina', 'CADPresentation_UnstableAngina', 'CADPresentation_NSTEMI', 'CADPresentation_STEMI')
#   train.fold <- cbind(train.fold, cadCatTrain)
#   test.fold <- cbind(test.fold , cadCatTest)
#   
#   train.fold$CADPresentation <- NULL
#   test.fold$CADPresentation <- NULL
#   
#   ncdr.data.crossval[[1]][[f]] <- train.fold 
#   ncdr.data.crossval[[2]][[f]] <- test.fold
# }

for(f in c(1,2,3,4,5)) {
  cat(paste('Fold:',f,'\n'))
  train.fold <- ncdr.data.crossval[[1]][[f]]
  test.fold <- ncdr.data.crossval[[2]][[f]]
  #IABP Timing
  iabpTrain <- catToBin(as.factor(train.fold$IABPTiming))
  colnames(iabpTrain) <- c('IABPTiming_Inplaceatstart', 'IABPTiming_DuringorPriortoPCI', 'IABPTiming_AfterPCIStart', 'IABPTiming_NoneOrMissing')
  iabpTest <- catToBin(as.factor(test.fold$IABPTiming))
  colnames(iabpTest) <- c('IABPTiming_Inplaceatstart', 'IABPTiming_DuringorPriortoPCI', 'IABPTiming_AfterPCIStart', 'IABPTiming_NoneOrMissing')
  # length(which(is.na(train.fold$IABPTiming) & (train.fold$IABP == 1)))
  # [1] 38
  train.fold$IABPTiming <- NULL
  test.fold$IABPTiming <- NULL
  
  train.fold <- cbind(train.fold, iabpTrain)
  test.fold <- cbind(test.fold, iabpTest)
  #OMVS Timing
  mvTrain <- catToBin(as.factor(train.fold$MVSupportTiming))
  colnames(mvTrain) <- c('MVSupportTiming_Inplaceatstart', 'MVSupportTiming_DuringorPriortoPCI', 'MVSupportTiming_AfterPCIStart', 'MVSupportTiming_NoneOrMissing')
  mvTest <- catToBin(as.factor(test.fold$MVSupportTiming))
  colnames(mvTest) <- c('MVSupportTiming_Inplaceatstart', 'MVSupportTiming_DuringorPriortoPCI', 'MVSupportTiming_AfterPCIStart', 'MVSupportTiming_NoneOrMissing')
  
  train.fold$MVSupportTiming <- NULL
  test.fold$MVSupportTiming <- NULL
  
  train.fold <- cbind(train.fold, mvTrain)
  test.fold <- cbind(test.fold, mvTest)
  # length(which(is.na(train.fold$MVSupportTiming) &(train.fold$MVSupport == 1)))
  # [1] 4
  #Card Transplant Type
  # length(which(is.na(train.fold$CardiacTransType) & (train.fold$CardiacTransplant == 1)))
  # [1] 9
  cardTransTrain <- catToBin(as.factor(train.fold$CardiacTransType))
  colnames(cardTransTrain) <- c('CardTransType_Donor','CardTransType_Candidate', 'CardTransType_PostFollowUp', 'CardTransType_NoneOrMissing')
  cardTransTest <- catToBin(as.factor(test.fold$CardiacTransType))
  colnames(cardTransTest) <- c('CardTransType_Donor','CardTransType_Candidate', 'CardTransType_PostFollowUp', 'CardTransType_NoneOrMissing')
  
  train.fold$CardiacTransType <- NULL
  test.fold$CardiacTransType <- NULL
  
  train.fold <- cbind(train.fold, cardTransTrain)
  test.fold <- cbind(test.fold, cardTransTest)
  #RxRecommendation
  #OtherORMissing
  # > summary(train.fold$DCathTreatment)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #  1.00    3.00    3.00    2.99    3.00    5.00  273428 
  train.fold$DCathTreatment[which(is.na(train.fold$DCathTreatment))] <- 5
  test.fold$DCathTreatment[which(is.na(test.fold$DCathTreatment))] <- 5
  
  cathTreatTrain <- catToBin(as.factor(train.fold$DCathTreatment))
  colnames(cathTreatTrain) <- c('DCathTreatment_None','DCathTreatment_MedTherapy','DCathTreatment_PCI','DCathTreatment_CABG','DCathTreatment_OtherOrMissing')
  cathTreatTest <- catToBin(as.factor(test.fold$DCathTreatment))
  colnames(cathTreatTest) <- c('DCathTreatment_None','DCathTreatment_MedTherapy','DCathTreatment_PCI','DCathTreatment_CABG','DCathTreatment_OtherOrMissing')
  
  train.fold$DCathTreatment <- NULL
  test.fold$DCathTreatment <- NULL
  
  train.fold <- cbind(train.fold, cathTreatTrain)
  test.fold <- cbind(test.fold, cathTreatTest)
  
  #Dominance
  dominanceTrain <- catToBin(as.factor(train.fold$Dominance))
  colnames(dominanceTrain) <- c('Dominance_Left', 'Dominance_Right' ,'Dominance_Co')
  
  dominanceTest <- catToBin(as.factor(test.fold$Dominance))
  colnames(dominanceTest) <- c('Dominance_Left', 'Dominance_Right' ,'Dominance_Co')
  
  train.fold$Dominance <- NULL
  test.fold$Dominance <- NULL
  
  train.fold <- cbind(train.fold, dominanceTrain)
  test.fold <- cbind(test.fold, dominanceTest)
  
  #PCI Indication
  # > summary(train.fold$PCIndication)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  # 1.000   6.000   6.000   5.778   8.000   8.000     824 
  train.fold$PCIndication[which(is.na(train.fold$PCIndication))] <- 8
  test.fold$PCIndication[which(is.na(test.fold$PCIndication))] <- 8
  
  pcindicationTrain <- catToBin(as.factor(train.fold$PCIndication))
  colnames(pcindicationTrain) <- c('PCIIndication_ImmediateForSTEMI','PCIIndication_STEMIUnstable','PCIIndication_STEMIStable','PCIIndication_STEMIStableWThrombolysis',
                                   'PCIIndication_RescuePCIForSTEMI','PCIIndication_HighRiskNSTEMIorUnstableAngina','PCIIndication_StagedPCI','PCIIndication_OtherOrMissing')
  
  pcindicationTest <- catToBin(as.factor(test.fold$PCIndication))
  colnames(pcindicationTest) <- c('PCIIndication_ImmediateForSTEMI','PCIIndication_STEMIUnstable','PCIIndication_STEMIStable','PCIIndication_STEMIStableWThrombolysis',
                                   'PCIIndication_RescuePCIForSTEMI','PCIIndication_HighRiskNSTEMIorUnstableAngina','PCIIndication_StagedPCI','PCIIndication_OtherOrMissing')
  
  train.fold$PCIndication <- NULL
  test.fold$PCIndication <- NULL
  
  train.fold <- c(train.fold, pcindicationTrain)
  test.fold <- c(test.fold, pcindicationTest)
  # #PCIDelayReason
  # > summary(train.fold$PCIDelayReason)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #   1.0     6.0     6.0     5.6     6.0     6.0 2257925 
  # #> length(which((train.fold$PCIIndication_ImmediateForSTEMI == 1) & (is.na(train.fold$PCIDelayReason))))
  # [1] 669
  train.fold$PCIDelayReason[which((train.fold$PCIIndication_ImmediateForSTEMI == 1) & (is.na(train.fold$PCIDelayReason)))] <- 6
  train.fold$PCIDelayReason[which((is.na(train.fold$PCIDelayReason)))] <- 5
  
  test.fold$PCIDelayReason[which((test.fold$PCIIndication_ImmediateForSTEMI == 1) & (is.na(test.fold$PCIDelayReason)))] <- 6
  test.fold$PCIDelayReason[which((is.na(test.fold$PCIDelayReason)))] <- 5
  
  PCIDelayReasonTrain <- catToBin(as.factor(train.fold$PCIDelayReason))
  colnames(PCIDelayReasonTrain) <- c('PCIDelayReason_DifficultVascularAccess','PCIDelayReason_CardiacArrestOrIntubationBeforePCI','PCIDelayReason_PatientDelayConsent',
                                     'PCIDelayReason_DifficultyCrossingCulpritLesion','PCIDelayReason_OtherOrMissing','PCIDelayReason_None')
  
  PCIDelayReasonTest <- catToBin(as.factor(test.fold$PCIDelayReason))
  colnames(PCIDelayReasonTest) <- c('PCIDelayReason_DifficultVascularAccess','PCIDelayReason_CardiacArrestOrIntubationBeforePCI','PCIDelayReason_PatientDelayConsent',
                                    'PCIDelayReason_DifficultyCrossingCulpritLesion','PCIDelayReason_OtherOrMissing','PCIDelayReason_None')
  
  train.fold$PCIDelayReason <- NULL
  test.fold$PCIDelayReason <- NULL
  
  train.fold <- cbind(train.fold, PCIDelayReasonTrain)
  test.fold <- cbind(test.fold, PCIDelayReasonTest)
  
  ncdr.data.crossval[[1]][[f]] <- train.fold 
  ncdr.data.crossval[[2]][[f]] <- test.fold
}
save('ncdr.data.crossval', file='~/NCDR-ACC/NCDR/BJMDATA/CPBLEED/DynamicModel/NCDR_NoCabg_Bleed_Crossval_v3_09JUN2017.RData')
