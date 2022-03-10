load(file='Y:/NCDR/BJMDATA/CPBLEED/FullCohort_v3_07MAR2016.RData')

#Remove Certain Columns
numeric.IDs <- c('HospitalID','VisitKey', 'EpisodeKey', 'ZipCode')
numeric.removes <- c('SubmissionKey', 'SubmissionPatientKey',
                     'DeathCause', 'DeathLab','ZipCodeNA', 'HospStatus', 'ArrivalDate',
                     'PriorPCIDate', 'PriorCABGDate', 'CABGDate', 'DCDate', 'ArrivalTime', 'CABGTime',
                     'NCDRPatientID', 'DOB', 'TimeframeCode', 'PopulationCode', 'BenchmarkCode', 
                     'STATE','MPN', 'ClientName.x', 'H_NPI', 'H_AHA', 'Address1.x',
                     'Address2.x', 'City.x', 'Zip.x', 'CPMPN', 'ICDMPN', 'ACTMPN', 'mpn2', 
                     'PROV_NUM', 'MDNDUP', 'PCI', 'OnsetDate', 'ThromDate', 'ProcedureDate', 
                     'SubECGDate', 'FirstDevActiDate', 'EDPresentDate', 'OnsetTime', 'ThromTime',
                     'ProcedureTime', 'SubECGTime','FirstDevActiTime',
                     'EDPresentTime', 'PrevLesionTime','HID', 'PID', 'EID', 'ClientName.y', 
                     'ParticipantClassificationDesc', 'ParticipantClassOtherDesc', 'Address1.y', 
                     'Address2.y', 'City.y', 'Zip.y', 'CountryCode', 'CountryName', 'ISOCode2', 
                     'ISONumber','StateProvinceCode', 'StateProvinceName', 'MasterContractDate', 
                     'MasterContractEffectiveDate', 'ContractDate', 'ContractEffectiveDate',
                     'PaidThroughDate', 'DateLastModified', 'iMISID', 'Hosp_ClientName', 'Hosp_ H_AHA',
                     'Hosp_ H_NPI', 'Hops_ CITY', 'Hosp_ STATE', 'Hosp_ MPN')
numeric.exc <- c('exc1', 'exc2','exc3', 'exc4', 'exc5', 'exc6')
numeric.other <- c('DCathOperatorKey' ,'PCIOperatorKey')
numeric.insurance <- c('InsPrivate', 'InsMedicare' ,'InsMedicaid', 'InsMilitary','InsState', 
                       'InsIHS', 'InsNonUS', 'InsNone')

STEMI <- rep(0, dim(final.data)[1])
STEMI[which(final.data$CADPresentation == 6)] <- 1

#Fix Insurance - 
#All Insurance Fields with NA == When InsNone == 1 so -> Impute 0 for those field
idx <- which(colnames(final.data) %in% numeric.insurance)
for(i in idx) {
  final.data[,i][which(is.na(final.data[,i]))] <- 0
}
final.data$STEMI <- STEMI
final.input.data <- final.data[,which(!(colnames(final.data) %in% c(numeric.removes, numeric.exc, numeric.other)))]
final.exclusions <- final.data[,which(colnames(final.data) %in% numeric.exc)]

#Fix Blanks in Columns
#numeric.blanks <- c('CABGLocation', 'CABGIndication', 'CABGStatus')
#Blanks should -> 0
#Blanks should correlate with CABG == 0
final.input.data$CABGLocation[which(final.input.data$CABGLocation == "")] <- NA
final.input.data$CABGLocation <- as.numeric(droplevels(final.input.data$CABGLocation))
final.input.data$CABGIndication[which(final.input.data$CABGIndication == "")] <- NA
final.input.data$CABGIndication <- as.numeric(droplevels(final.input.data$CABGIndication))
final.input.data$CABGStatus[which(final.input.data$CABGStatus == "")] <- NA
final.input.data$CABGStatus <- as.numeric(droplevels(final.input.data$CABGStatus))

final.input.data$CTO[which(final.input.data$CTO == -Inf)] <- NA
final.input.data$PreTIMI[which(final.input.data$PreTIMI == Inf)] <- NA
final.input.data$NEWSEQ[which(final.input.data$NEWSEQ == -Inf)] <- NA

# 
# numeric.inf <- c('CTO', 'PreTIMI', 'NEWSEQ')


#'SegmentID' - Convert from categorical to 1 hot binary
temp.segdata <- final.input.data$SegmentID

source('./catToBin.R')
temp.bin.segdata <- catToBin(temp.segdata)
colnames(temp.bin.segdata) <- paste('SegID',colnames(temp.bin.segdata), sep='')

final.input.data <- cbind(final.input.data, temp.bin.segdata)

#Admit Source - Other or Unknown
final.input.data$AdmtSource[which(is.na(final.input.data$AdmtSource))] <- 3
temp.segdata <- as.factor(final.input.data$AdmtSource)
temp.bin.segdata <- catToBin(temp.segdata)
colnames(temp.bin.segdata) <- paste('AdmtSrc_',colnames(temp.bin.segdata), sep='')
final.input.data <- cbind(final.input.data, temp.bin.segdata)


categorical.var <- c('SegmentID', 'NPINumber' ,'AHANumber', 'CommunityDesc', 'ProfitTypeDesc', 
                     'HemoDynamicEquipment1','HemoDynamicEquipment2',
                     'HemoDynamicEquipment3', 'HemoDynamicEquipment4', 'AdmtSource')

final.input.data <- final.input.data[,-which(colnames(final.input.data) %in% categorical.var)]

#Fix NA in columns
numeric.NA <- c('DCStatus', 'Smoker', 'Hypertension', 'Dyslipidemia', 'FamilyHxCAD', 'PriorMI', 
                'PriorHF', 'ValveSurgery', 'PriorPCI', 'PriorCABG', 'Height', 'Weight', 
                'CurrentDialysis', 'PriorCVD', 'PriorPAD', 'ChronicLungDisease', 'Diabetes', 
                'AdmtSource', 'DCLocation', 'DC_CardRehab', 'CABG', 'OtherMajorSurgery' ,
                'DCLVEF', 'HispOrig', 'AntiAnginalMed', 'AA_BetaBlockers' ,'AA_CaChannel', 
                'AA_LongActingNitrates', 'AA_Ranolazine', 'AA_OtherAgent', 'Prior2weeksHF', 
                'CardioLVSD', 'PeriopEval', 'PriorCardioShock', 'PriorCardiacArrest', 'StressImaging', 
                'LMStenosis', 'LMNA', 'ProxLADStenosis', 'MidDistalLADStenosis', 'CIRCStenosis', 
                'RCAStenosis', 'CADPresentation', 'AnginalClass', 'Dominance',
                'OtherProcedure', 'FluroTime', 'ContrastVol', 'IABP', 'MVSupport', 'AcessSite',
                'PostMI', 'PostCardiogenicShock','PostHF', 'PostCVA', 'PostTamponade', 'PostDialysis',
                'PostOtherVasComp', 'PostTransfusion', 'PostBleed', 'DiagCorAngio', 'LeftHeartCath', 
                'CardiacTransplant', 'DCathStatus' ,'DCathTreatment' ,'PCIStatus', 'PrePCILVEF', 
                'PCICardioShock', 'DissectionSeg', 'PerfSeg', 'PCIndication',
                'PreProcCreat', 'PreProcHgb', 'PostProcCreat', 'PostProcHgb', 'CulpritArtery', 
                'StenosisPriorTreat', 'PreProcTIMI', 'PrevTreatedLesion',
                'LesionLength', 'Thrombus', 'BifurcationLesion', 'GuidewireLesion', 'StensosiPostProc',
                'PostProcTIMI', 'DeviceDeployed', 'LesionGraft','LesonComplexty', 'DoesMonitorVolume', 
                'CEACount', 'FTLabOther', 'UpperNormCKMBFemale', 'UpperNormCKMBMale',
                'DoesTnT', 'TnDecisionLimit', 'DischargeMed10', 'DischargeMed11', 'DischargeMed12', 
                'DischargeMed13', 'DischargeMed14', 'DischargeMed15', 'DischargeMed15', 'DischargeMed17',
                'DischargeMed19', 'DischargeMed21', 'PreOpMed6', 'PreOpMed7', 'PreOpMed3', 'PreOpMed4', 
                'PreOpMed5', 'PreOpMed1', 'PreOpMed8', 'PreOpMed2', 'PreOpMed9', 'PreOpMed18', 'PreOpMed20',
                'ICDEV_Drug Eluting Stent', 'ICDEV_Balloon', 'ICDEV_Bare Metal Stent', 'ICDEV_Cutting Balloon', 'ICDEV_Thrombectomy', 
                'ICDEV_Extraction Catheter' ,'ICDEV_Embolic Protection', 'ICDEV_Atherectomy', 'ICDEV_Laser', 'ICDEV_Other', 'ICDEV_Coated Stent',
                'ICDEV_Chronic Total Occlusion','ICDEV_Brachy Therapy', 'ICDEV_Covered Stent', 'LesionCounter.y', 
                'LSDEV_Drug Eluting Stent', 'LSDEV_Balloon', 'LSDEV_Bare Metal Stent', 'LSDEV_Cutting Balloon',
                'LSDEV_Thrombectomy', 'LSDEV_Extraction Catheter', 'LSDEV_Embolic Protection', 
                'LSDEV_Atherectomy', 'LSDEV_Laser', 'LSDEV_Other', 'LSDEV_Coated Stent', 
                'LSDEV_Chronic Total Occlusion', 'LSDEV_Brachy Therapy', 'LSDEV_Covered Stent',
                "ClosID1"      ,                 "ClosID10"       ,               "ClosID11"       ,               "ClosID12"    ,                 
                "ClosID13"      ,                "ClosID14"       ,               "ClosID15"      ,                "ClosID16"   ,                   "ClosID17",                     
                "ClosID18"       ,               "ClosID19"       ,               "ClosID2"       ,                "ClosID20"   ,                   "ClosID21" ,                    
               "ClosID22"         ,             "ClosID23"        ,              "ClosID24"       ,               "ClosID25"    ,                  "ClosID26" ,                    
                "ClosID27"         ,             "ClosID28"       ,               "ClosID29"      ,                "ClosID3"    ,                   "ClosID30",                     
                "ClosID31"          ,            "ClosID32"       ,               "ClosID33"      ,                "ClosID34"   ,                   "ClosID35",                     
                "ClosID36"            ,          "ClosID37"       ,               "ClosID38"      ,                "ClosID39"   ,                   "ClosID4" ,                     
                "ClosID40"          ,            "ClosID41"       ,               "ClosID42"      ,                "ClosID43"    ,                  "ClosID44",                     
                "ClosID45"           ,           "ClosID46"       ,               "ClosID47"      ,                "ClosID48"   ,                   "ClosID49",                     
                "ClosID5"           ,            "ClosID50"       ,               "ClosID51"      ,                "ClosID52"   ,                   "ClosID53",                     
                "ClosID54"          ,            "ClosID55"       ,               "ClosID56"      ,                "ClosID57"   ,                   "ClosID58",                     
                "ClosID59"          ,            "ClosID6"        ,               "ClosID60"      ,                "ClosID61"   ,                   "ClosID62",                     
                "ClosID63"          ,            "ClosID64"       ,               "ClosID65"      ,                "ClosID66"   ,                   "ClosID68",                     
                "ClosID69"          ,            "ClosID7"        ,               "ClosID70"      ,                "ClosID71"   ,                   "ClosID72",                     
                "ClosID8"           ,            "ClosID9"        ,               "ClosID952"     ,                "ClosID953"  ,                   "ClosID954",                    
                "ClosID955")

#Can I keep 'PostTransfusion' <- or would that be highly indicative of a bleed??
numeric.large.NA <- c('OnsetTimeEst', 'OnsetTimeNA', 'FluroDose',  
                      'Prior2weekNYHA',
                      'PostProcCKMB', 'PostprocCKMBNM', 'PostProcTnl', 'PostProcTnT', 'ClosureCounter',
                      'LesionCounter.x')


#Remove Post Proc, Discharge, and Death Information?
outcome <- c('PostTransfusion' ,'PostBleed', 'PostBleedAccessSite', 'PostHemStroke', 
             'HgbPriorTransfusion', 'PostBleedAccessSite',
             'PostBleedHematoma', 'PostBleedHemaSize', 'PostRetroBleed', 'PostGIBleed', 
             'PostGUBleed', 'PostOtherBleed', 'StensosiPostProc', 'PostProcTIMI',
             'Bleed', 
             
             'PostMI', 'PostCardiogenicShock','PostHF', 'PostCVA', 'PostTamponade', 'PostDialysis',
             'PostOtherVasComp', 'PostTransfusion', 'PostBleed','PostProcTIMI')

post.meds <- c('DischargeMed13', 'DischargeMed14', 'DischargeMed10', 'DischargeMed11', 'DischargeMed12', 
               'DischargeMed15', 'DischargeMed15', 'DischargeMed17', 'DischargeMed19', 'DischargeMed21','DischargeMed16',
               "PostBleedHematoma" ,"PostProcCKMBND","PostProcCreat",                
               "PostProcCreatND","PostProcHgb", "PostProcHgbND" ,'DCLocation', 'DCLVEF', 'DCLVEFNA', 'DCStatus', 
               "PostProcTnlND","PostProcTnTND",'DC_CardRehab')

removes.extra <- c('AHANumber', 
                   'Hosp_ CITY','NPINumber', 'MPNDUP' )
# 
# "FTLabOther","FTLabStaffAdminSuper","FTLabStaffAnalytics","FTLabStaffClerical", "FTLabStaffCVT",                
# "FTLabStaffNurse","FTLabStaffRT", 
# sorted <- final.data[,sort(colnames(final.data))]
# test.sorted <- summary(sorted)
# print(test.sorted)


#Remove Large NAs
final.complete.input <- final.input.data[,which(!(colnames(final.input.data)) %in% c(numeric.other, removes.extra))]

# sorted <- final.complete.input[,sort(colnames(final.complete.input))]
# test.sorted <- summary(sorted)
# print(test.sorted)                                                                       


#final.complete.input$NPINumber <- as.numeric(final.complete.input$NPINumber)
#final.complete.input$AHANumber <- as.numeric(final.complete.input$AHANumber)

# #final.complete.input$CommunityDesc <- as.numeric(levels(final.complete.input$CommunityDesc))
# lvls <- levels(final.complete.input$CommunityDesc)
# cmtdesc <- rep(0, length(final.complete.input$CommunityDesc))
# for(i in 1:length(cmtdesc)) {
#   cmtdesc[i] <- which(lvls == final.complete.input$CommunityDesc[i])
# }
# 
# lvls.profit <- levels(final.complete.input$ProfitTypeDesc)
# desc.profit <- rep(0, length(final.complete.input$ProfitTypeDesc))
# 
# for(i in 1:length(desc.profit)) {
#   desc.profit[i] <- which(lvls.profit == final.complete.input$ProfitTypeDesc[i])
# 
# }

final.numeric.input <- final.complete.input
# final.numeric.input$CommunityDesc <- cmtdesc
# final.numeric.input$ProfitTypeDesc <- desc.profit



#numeric.inf <- c('CTO', 'PreTIMI', 'NEWSEQ')
# final.numeric.input$CTO[which(final.numeric.input$CTO < 0)] <- NA
# final.numeric.input$PreTIMI[which(final.numeric.input$PreTIMI == Inf)] <- NA
# final.numeric.input$NEWSEQ[which(final.numeric.input$NEWSEQ < 0)] <- 0

#final.numeric.input <- final.numeric.input[,which(!(colnames(final.numeric.input)) %in% c(numeric.blanks))]
#Impute or Leave as own Category and create a catagorical Variable? and an NA variable?
#numeric.NA
#numeric.missing <- numeric.NA[which(!(numeric.NA %in% outcome))]
# [1] "DCStatus"                      "Smoker"                        "Hypertension"                  "Dyslipidemia"                  "FamilyHxCAD"                  
# [6] "PriorMI"                       "PriorHF"                       "ValveSurgery"                  "PriorPCI"                      "PriorCABG"                    
# [11] "Height"                        "Weight"                        "CurrentDialysis"               "PriorCVD"                      "PriorPAD"                     
# [16] "ChronicLungDisease"            "Diabetes"                      "AdmtSource"                    "DCLocation"                    "DC_CardRehab"                 
# [21] "CABG"                          "OtherMajorSurgery"             "DCLVEF"                        "HispOrig"                      "AntiAnginalMed"               
# [26] "AA_BetaBlockers"               "AA_CaChannel"                  "AA_LongActingNitrates"         "AA_Ranolazine"                 "AA_OtherAgent"                
# [31] "Prior2weeksHF"                 "CardioLVSD"                    "PeriopEval"                    "PriorCardioShock"              "PriorCardiacArrest"           
# [36] "StressImaging"                 "LMStenosis"                    "LMNA"                          "ProxLADStenosis"               "MidDistalLADStenosis"         
# [41] "CIRCStenosis"                  "RCAStenosis"                   "CADPresentation"               "AnginalClass"                  "Dominance"                    
# [46] "OtherProcedure"                "FluroTime"                     "ContrastVol"                   "IABP"                          "MVSupport"                    
# [51] "AcessSite"                     "PostMI"                        "PostCardiogenicShock"          "PostHF"                        "PostCVA"                      
# [56] "PostTamponade"                 "PostDialysis"                  "PostOtherVasComp"              "DiagCorAngio"                  "LeftHeartCath"                
# [61] "CardiacTransplant"             "DCathStatus"                   "DCathTreatment"                "PCIStatus"                     "PrePCILVEF"                   
# [66] "PCICardioShock"                "DissectionSeg"                 "PerfSeg"                       "PCIndication"                  "PreProcCreat"                 
# [71] "PreProcHgb"                    "PostProcCreat"                 "PostProcHgb"                   "CulpritArtery"                 "StenosisPriorTreat"           
# [76] "PreProcTIMI"                   "PrevTreatedLesion"             "LesionLength"                  "Thrombus"                      "BifurcationLesion"            
# [81] "GuidewireLesion"               "DeviceDeployed"                "LesionGraft"                   "LesonComplexty"                "DoesMonitorVolume"            
# [86] "CEACount"                      "FTLabOther"                    "UpperNormCKMBFemale"           "UpperNormCKMBMale"             "DoesTnT"                      
# [91] "TnDecisionLimit"               "PreOpMed6"                     "PreOpMed7"                     "PreOpMed3"                     "PreOpMed4"                    
# [96] "PreOpMed5"                     "PreOpMed1"                     "PreOpMed8"                     "PreOpMed2"                     "PreOpMed9"                    
# [101] "PreOpMed18"                    "PreOpMed20"                    "ICDEV_Drug Eluting Stent"      "ICDEV_Balloon"                 "ICDEV_Bare Metal Stent"       
# [106] "ICDEV_Cutting Balloon"         "ICDEV_Thrombectomy"            "ICDEV_Extraction Catheter"     "ICDEV_Embolic Protection"      "ICDEV_Atherectomy"            
# [111] "ICDEV_Laser"                   "ICDEV_Other"                   "ICDEV_Coated Stent"            "ICDEV_Chronic Total Occlusion" "ICDEV_Brachy Therapy"         
# [116] "ICDEV_Covered Stent"           "LesionCounter.y"               "LSDEV_Drug Eluting Stent"      "LSDEV_Balloon"                 "LSDEV_Bare Metal Stent"       
# [121] "LSDEV_Cutting Balloon"         "LSDEV_Thrombectomy"            "LSDEV_Extraction Catheter"     "LSDEV_Embolic Protection"      "LSDEV_Atherectomy"            
# [126] "LSDEV_Laser"                   "LSDEV_Other"                   "LSDEV_Coated Stent"            "LSDEV_Chronic Total Occlusion" "LSDEV_Brachy Therapy"         
# [131] "LSDEV_Covered Stent"           "ClosID45"                      "ClosID1"                       "ClosID36"                      "ClosID36"                     
# [136] "ClosID35"                      "ClosID23"                      "ClosID19"                      "ClosID4"                       "ClosID2"                      
# [141] "ClosID31"                      "ClosID15"                      "ClosID27"                      "ClosID12"                      "ClosID5"                      
# [146] "ClosID44"                      "ClosID"                        "ClosID.1"                      "ClosID.2"                      "ClosID.3"                     
# [151] "ClosID.4"                      "ClosID.5"                      "ClosID.6"                      "ClosID.7"                      "ClosID.8"                     
# [156] "ClosID.9"                      "ClosID.10"                     "ClosID.11"                     "ClosID.12"                     "ClosID.13"                    
# [161] "ClosID.14"                     "ClosID.15"                     "ClosID.16"                     "ClosID.17"                     "ClosID.18"                    
# [166] "ClosID.19"                     "ClosID.20"                     "ClosID.21"                     "ClosID.22"                     "ClosID.23"                    
# [171] "ClosID.24"                     "ClosID.25"                     "ClosID.26"                     "ClosID.27"                     "ClosID.28"                    
# [176] "ClosID.29"                     "ClosID.30"                     "ClosID.31"                     "ClosID.32"                     "ClosID.33"                    
# [181] "ClosID.34"                     "ClosID.35"                     "ClosID.36"                     "ClosID.37"                     "ClosID.38"                    
# [186] "ClosID.39"                     "ClosID.40"                     "ClosID.41"                     "ClosID.42"                     "ClosID.43"                    
# [191] "ClosID.44"                     "ClosID.45"                     "ClosID.46"                     "ClosID.47"                     "ClosID.48"                    
# [196] "ClosID.49"                     "ClosID.50"                     "ClosID.51"                     "ClosID.52"                     "ClosID.53"                    
# [201] "ClosID.54"                     "ClosID.55"                     "ClosID.56"                     "ClosID.57"                     "ClosID.58"                    
# [206] "ClosID.59"                     "ClosID.60"  

#Closure, LSDEV, ICDEV, and PreOpMed NA => 0
med.vars <- c("PreOpMed6"  ,                   "PreOpMed7"     ,                "PreOpMed3"                 ,    "PreOpMed4"                   , 
              "PreOpMed5"   ,                  "PreOpMed1"    ,                 "PreOpMed8"                 ,    "PreOpMed2"                    , "PreOpMed9"      ,              
              "PreOpMed18"   ,                 "PreOpMed20"   )

for(var in med.vars) {
  idx <- which(colnames(final.numeric.input) == var)
  final.numeric.input[,idx][which(final.numeric.input[,idx] == 3)] <- NA
  final.numeric.input[,idx][which(final.numeric.input[,idx] == 2)] <- 5
  final.numeric.input[,idx][which(final.numeric.input[,idx] == 1)] <- 2
  final.numeric.input[,idx][which(final.numeric.input[,idx] == 5)] <- 1
}



dev.na <- c(          "ICDEV_Drug Eluting Stent"  ,    "ICDEV_Balloon"                , "ICDEV_Bare Metal Stent"  ,     
            "ICDEV_Cutting Balloon" ,        "ICDEV_Thrombectomy" ,           "ICDEV_Extraction Catheter" ,    "ICDEV_Embolic Protection"     , "ICDEV_Atherectomy"        ,    
            "ICDEV_Laser"            ,       "ICDEV_Other"         ,          "ICDEV_Coated Stent"        ,    "ICDEV_Chronic Total Occlusion", "ICDEV_Brachy Therapy"      ,   
            "ICDEV_Covered Stent"     ,      "LesionCounter.y"      ,         "LSDEV_Drug Eluting Stent"  ,    "LSDEV_Balloon"                , "LSDEV_Bare Metal Stent"    ,   
            "LSDEV_Cutting Balloon"    ,     "LSDEV_Thrombectomy"    ,        "LSDEV_Extraction Catheter" ,    "LSDEV_Embolic Protection"     , "LSDEV_Atherectomy"         ,   
            "LSDEV_Laser"               ,    "LSDEV_Other"            ,       "LSDEV_Coated Stent"        ,    "LSDEV_Chronic Total Occlusion", "LSDEV_Brachy Therapy"      ,   
            "LSDEV_Covered Stent"       ,    
            "ClosID1"      ,                 "ClosID10"       ,               "ClosID11"       ,               "ClosID12"    ,                 
            "ClosID13"      ,                "ClosID14"       ,               "ClosID15"      ,                "ClosID16"   ,                   "ClosID17",                     
            "ClosID18"       ,               "ClosID19"       ,               "ClosID2"       ,                "ClosID20"   ,                   "ClosID21" ,                    
            "ClosID22"         ,             "ClosID23"        ,              "ClosID24"       ,               "ClosID25"    ,                  "ClosID26" ,                    
            "ClosID27"         ,             "ClosID28"       ,               "ClosID29"      ,                "ClosID3"    ,                   "ClosID30",                     
            "ClosID31"          ,            "ClosID32"       ,               "ClosID33"      ,                "ClosID34"   ,                   "ClosID35",                     
            "ClosID36"            ,          "ClosID37"       ,               "ClosID38"      ,                "ClosID39"   ,                   "ClosID4" ,                     
            "ClosID40"          ,            "ClosID41"       ,               "ClosID42"      ,                "ClosID43"    ,                  "ClosID44",                     
            "ClosID45"           ,           "ClosID46"       ,               "ClosID47"      ,                "ClosID48"   ,                   "ClosID49",                     
            "ClosID5"           ,            "ClosID50"       ,               "ClosID51"      ,                "ClosID52"   ,                   "ClosID53",                     
            "ClosID54"          ,            "ClosID55"       ,               "ClosID56"      ,                "ClosID57"   ,                   "ClosID58",                     
            "ClosID59"          ,            "ClosID6"        ,               "ClosID60"      ,                "ClosID61"   ,                   "ClosID62",                     
            "ClosID63"          ,            "ClosID64"       ,               "ClosID65"      ,                "ClosID66"   ,                   "ClosID68",                     
            "ClosID69"          ,            "ClosID7"        ,               "ClosID70"      ,                "ClosID71"   ,                   "ClosID72",                     
            "ClosID8"           ,            "ClosID9"        ,               "ClosID952"     ,                "ClosID953"  ,                   "ClosID954",                    
            "ClosID955")

for(j in 1:dim(final.numeric.input)[2]) {
  col <- colnames(final.numeric.input)[j]
  #if(col %in% numeric.missing) {
    if((col %in% med.vars) | (col %in% dev.na)) {
      final.numeric.input[,j][which(is.na(final.numeric.input[,j]))] <- 0
    }
#     } else {
#       #Use Median
#       final.numeric.input[,j][which(is.na(final.numeric.input[,j]))] <- median(final.numeric.input[,j], na.rm=TRUE)
#       #library(matrixStats)
#       #Use Mean
#       #final.numeric.input[,j][which(is.na(final.numeric.input[,j]))] <- colMeans(final.numeric.input[,j], na.rm=TRUE)
#     }
  #}
}


# 
# misc <- c('Hosp_ CITY', 'MPNDUP', 'PostMI', 'PostCardiogenicShock', 'PostHF','PostCVA', 'PostTamponade', 'PostDialysis', 'PostOtherVasComp', 'PostBleedHematoma',
#           'PostProcCKMBND', 'PostProcTnlND', 'PostProcTnTND', 'PostProcCreat', 'PostProcCreatND', 'PostProcHgb', 'PostProcHgbND', 'StenosisPostProc',
#           'DischargeMed16')

idx <- which((final.numeric.input$Diabetes == 0) & is.na(final.numeric.input$DiabetesControl))
final.numeric.input$DiabetesControl[idx] <- 0#None because diabetes = No
#Remaining missing gets put to "other"
final.numeric.input$DiabetesControl[which(is.na(final.numeric.input$DiabetesControl))] <- 5

temp.segdata <- as.factor(final.numeric.input$DiabetesControl)
temp.bin.segdata <- catToBin(temp.segdata)
colnames(temp.bin.segdata) <- c('DiabetesControlNone','DiabetesControlDiet','DiabetesControlOral', 'DiabetesControlInsulin', 'DiabetesControlOtherOrMissing')

final.numeric.input <- cbind(final.numeric.input, temp.bin.segdata)

final.numeric.input$DiabetesControl <- NULL

ncdr.data.full <- final.numeric.input

# 
# 
# 
# #final.data.imputed <- final.numeric.input[,which(!(colnames(final.numeric.input) %in% misc))]
# #imput CTO, PreTIMI
# final.data.imputed$CTO[which(is.na(final.data.imputed$CTO))] <- median(final.data.imputed$CTO, na.rm=TRUE)
# final.data.imputed$PreTIMI[which(is.na(final.data.imputed$PreTIMI))] <- median(final.data.imputed$PreTIMI, na.rm=TRUE)
# ncdr.data.full <- final.data.imputed
# ncdr.data.full$Bleed <- final.data$Bleed


ncdr.data.full$DQRULD <- NULL
ncdr.data.full$DQRULT <- NULL

#Create RAO Variables
#Have STEMI
AGELE70 <- rep(0, dim(ncdr.data.full)[1])
AGELE70[which(ncdr.data.full$Age <= 70)] <- 1

ncdr.data.full$AGELE70 <- AGELE70

AGEGT70 <- rep(0, dim(ncdr.data.full)[1])
AGEGT70[which(ncdr.data.full$Age > 70)] <- 1

ncdr.data.full$AGEGT70 <- AGEGT70

BMI <- (ncdr.data.full$Weight * 10000)/(ncdr.data.full$Height*ncdr.data.full$Height) #Direct from Yongfei's code
BMI[which(BMI < 5)] <- NA
BMI[which(BMI > 100)] <- NA
#BMI[which(is.na(BMI))] <- median(BMI, na.rm=TRUE)

ncdr.data.full$BMI <- BMI

BMILE30 <- as.numeric((BMI <= 30))

ncdr.data.full$BMILE30 <- BMILE30

NEWDIAB <- rep(0, dim(ncdr.data.full)[1])
NEWDIAB[which((final.complete.input$Diabetes == 1) & (final.complete.input$DiabetesControl > 1))] <- 1
NEWDIAB[which((final.complete.input$Diabetes == 1) & (final.complete.input$DiabetesControl == 4))] <- 2

ncdr.data.full$NEWDIAB <- NEWDIAB

NEWDIAB1 <- as.numeric(NEWDIAB == 1)
NEWDIAB2 <- as.numeric(NEWDIAB == 2)

ncdr.data.full$NEWDIAB1 <- NEWDIAB1
ncdr.data.full$NEWDIAB2 <- NEWDIAB2

#From Yongfei's Code
HDEF <- ncdr.data.full$PrePCILVEF
#Removed the imputation aspects for consistency
HDEF[which(HDEF > 60)] <- 60
HDEF <- HDEF/5

ncdr.data.full$HDEF <- HDEF

FEMALE <- as.numeric(ncdr.data.full$Sex == 2)

ncdr.data.full$FEMALE <- FEMALE

GENDMULT <- rep(1, dim(ncdr.data.full)[1])
GENDMULT[which(FEMALE == 1)] <- 0.742

ncdr.data.full$GENDMULT <- GENDMULT

RACEMULT <- rep(1, dim(ncdr.data.full)[1])
RACEMULT[which(ncdr.data.full$RaceBlack == 1)] <- 1.21

ncdr.data.full$RACEMULT <- RACEMULT

GFR <- rep(NA, dim(ncdr.data.full)[1])
idx <- which(ncdr.data.full$PreProcCreat > 0)
GFR[idx] <- (186 * (ncdr.data.full$PreProcCreat[idx] ^ (-1.154))) * (ncdr.data.full$Age[idx] ^ (-0.203)) * GENDMULT[idx] * RACEMULT[idx]

RENFAIL <- as.numeric((GFR < 30) | (ncdr.data.full$CurrentDialysis == 1))

ncdr.data.full$RENFAIL <- RENFAIL

CKD <- rep(3, dim(ncdr.data.full)[1])
CKD[which((ncdr.data.full$CurrentDialysis == 1) | (GFR < 30))] <- 4
CKD[which(GFR >= 30)] <- 2
CKD[which(GFR >= 45)] <- 1
CKD[which(GFR >= 60)] <- 0

CKD1 <- as.numeric(CKD == 1)
CKD2 <- as.numeric(CKD == 2)
CKD3 <- as.numeric(CKD == 3)
CKD4 <- as.numeric(CKD == 4)

ncdr.data.full$CKD <- CKD
ncdr.data.full$CKD1 <- CKD1
ncdr.data.full$CKD2 <- CKD2
ncdr.data.full$CKD3 <- CKD3
ncdr.data.full$CKD4 <- CKD4

GFR[which(GFR > 90)] <- 90
GFR <- GFR/5

ncdr.data.full$GFR <- GFR

LYTICS <- as.numeric((ncdr.data.full$CADPresentation == 6) & (ncdr.data.full$ThromTherapy == 1))

ncdr.data.full$LYTICS <- LYTICS

CARSHOCK <- as.numeric((ncdr.data.full$PriorCardioShock == 1) | (ncdr.data.full$PCICardioShock == 1))

ncdr.data.full$CARSHOCK <- CARSHOCK

DCARSHOCK <- rep(0, dim(ncdr.data.full)[1])
DCARSHOCK[which(ncdr.data.full$PriorCardioShock == 1)] <- (1 + as.numeric(ncdr.data.full$PCICardioShock == 0))[which(ncdr.data.full$PriorCardioShock == 1)]
DCARSHOCK[which(ncdr.data.full$PriorCardioShock == 0)] <- (3 + as.numeric(ncdr.data.full$PCICardioShock == 0))[which(ncdr.data.full$PriorCardioShock == 0)]

ncdr.data.full$DCARSHOCK <- DCARSHOCK

SHOCKPCIS <- rep(6, dim(ncdr.data.full)[1])
SHOCKPCIS[which(ncdr.data.full$PCIStatus == 2)] <- 5
SHOCKPCIS[which(ncdr.data.full$PCIStatus == 3)] <- 4
SHOCKPCIS[which((DCARSHOCK == 2) | (DCARSHOCK == 3))] <- 3
SHOCKPCIS[which((DCARSHOCK == 1) | (ncdr.data.full$PCIStatus == 4))] <- 2
SHOCKPCIS[which((DCARSHOCK == 1) & (ncdr.data.full$PCIStatus == 4))] <- 1

SHOCKPCIS1 <- as.numeric(SHOCKPCIS == 1)
SHOCKPCIS2 <- as.numeric(SHOCKPCIS == 2)
SHOCKPCIS3 <- as.numeric(SHOCKPCIS == 3)
SHOCKPCIS4 <- as.numeric(SHOCKPCIS == 4)
SHOCKPCIS5 <- as.numeric(SHOCKPCIS == 5)
SHOCKPCIS6 <- as.numeric(SHOCKPCIS == 6)

ncdr.data.full$SHOCKPCIS <- SHOCKPCIS
ncdr.data.full$SHOCKPCIS1 <- SHOCKPCIS1
ncdr.data.full$SHOCKPCIS2 <- SHOCKPCIS2
ncdr.data.full$SHOCKPCIS3 <- SHOCKPCIS3
ncdr.data.full$SHOCKPCIS4 <- SHOCKPCIS4
ncdr.data.full$SHOCKPCIS5 <- SHOCKPCIS5
ncdr.data.full$SHOCKPCIS6 <- SHOCKPCIS6


LESSCAI23 <- as.numeric((ncdr.data.full$LESSCAI == 2) | (ncdr.data.full$LESSCAI == 3))
LESSCAI4 <- as.numeric(ncdr.data.full$LESSCAI == 4)

ncdr.data.full$LESSCAI23 <- LESSCAI23
ncdr.data.full$LESSCAI4 <- LESSCAI4

NEWSEQ2 <- as.numeric(ncdr.data.full$NEWSEQ == 2)
NEWSEQ3 <- as.numeric(ncdr.data.full$NEWSEQ == 3)

ncdr.data.full$NEWSEQ2 <- NEWSEQ2
ncdr.data.full$NEWSEQ3 <- NEWSEQ3

NYHA123 <- as.numeric((ncdr.data.full$Prior2weekNYHA == 1) | (ncdr.data.full$Prior2weekNYHA == 2) | (ncdr.data.full$Prior2weekNYHA == 3))
NYHA4 <- as.numeric(ncdr.data.full$Prior2weekNYHA == 4)

ncdr.data.full$NYHA123 <- NYHA123
ncdr.data.full$NYHA4 <- NYHA4

PRETIMINO <- as.numeric(ncdr.data.full$PreTIMI == 0)

ncdr.data.full$PRETIMINO <- PRETIMINO

NVD <- rep(0, dim(ncdr.data.full)[1])
NVD[which(ncdr.data.full$LMStenosis <= 50)] <- (as.numeric(ncdr.data.full$RCAStenosis > 70) + 
                                                  as.numeric((ncdr.data.full$ProxLADStenosis > 70) | (ncdr.data.full$MidDistalLADStenosis > 70)) +
                                                  as.numeric((ncdr.data.full$CIRCStenosis > 70) | (ncdr.data.full$RamusStenosis > 70)))[which(ncdr.data.full$LMStenosis <= 50)]
NVD[which((ncdr.data.full$LMStenosis > 50) & ((ncdr.data.full$Dominance == 1) | (ncdr.data.full$RCAStenosis > 70)))] <- 3
NVD[which((ncdr.data.full$LMStenosis > 50) & !((ncdr.data.full$Dominance == 1) | (ncdr.data.full$RCAStenosis > 70)))] <- 2

NVD23 <- as.numeric((NVD ==2) | (NVD == 3))

ncdr.data.full$NVD <- NVD
ncdr.data.full$NVD23 <- NVD23

PREHGBLE13 <- as.numeric(ncdr.data.full$PreProcHgb <= 13)
PREHGBGT13 <- as.numeric(ncdr.data.full$PreProcHgb > 13)

ncdr.data.full$PREHGBLE13 <- PREHGBLE13
ncdr.data.full$PREHGBGT13 <- PREHGBGT13



path = 'Y:/NCDR/BJMDATA/CATHPCI/'
source('./maxfunc.R')
files <- list.files(path=path, pattern="\\.RData$")
load(file=paste(path,files[1],sep=''))
data.sas <- data.sas[with(data.sas, order(VisitKey)),]

closure.IDs <- colnames(ncdr.data.full)[which(grepl('ClosID',colnames(ncdr.data.full)))]
closure.types <- rep(NA, length(closure.IDs))

for(i in 1:length(closure.IDs)) {
  clos.id <- closure.IDs[i]
  id.num <- as.numeric(strsplit(clos.id, 'ID')[[1]][2])
  id.type <- as.character(data.sas[which(data.sas$ClosureDevID == id.num),]$DeviceTypeName[1])
  closure.types[i] <- id.type
}

clos.info <- data.frame(closure.IDs, closure.types)

closure.data <- ncdr.data.full[,which(colnames(ncdr.data.full) %in% closure.IDs)]
clos.vars <- matrix(0, dim(ncdr.data.full)[1], length(unique(closure.types)))


library(doParallel)
library(foreach)
registerDoParallel()
clos.data <- foreach(i=1:dim(ncdr.data.full)[1], .combine='rbind') %dopar% {
#for(i in 1:dim(ncdr.data.full)[1]) {
  #patch
  row <- ncdr.data.full[i,]
  clos.vec <- vector()
  IDs <- unique(clos.info[which(clos.info$closure.types == 'Patch'),][,1])
  clos.vec <- c(clos.vec,sum(row[,which(colnames(row) %in% IDs)]))
  #clos.vars$Clos_Patch[i] <- sum(row[,which(colnames(row) %in% IDs)])
  #Manual com
  IDs <- unique(clos.info[which(clos.info$closure.types == 'Manual com'),][,1])
  clos.vec <- c(clos.vec,sum(row[,which(colnames(row) %in% IDs)]))
  #clos.vars$`Clos_Manual com`[1] <- sum(row[,which(colnames(row) %in% IDs)])
  #Sealant
  IDs <- unique(clos.info[which(clos.info$closure.types == 'Sealant'),][,1])
  clos.vec <- c(clos.vec,sum(row[,which(colnames(row) %in% IDs)]))
  #clos.vars$Clos_Sealant[i] <- sum(row[,which(colnames(row) %in% IDs)])
  #Other
  IDs <- unique(clos.info[which(clos.info$closure.types == 'Other'),][,1])
  clos.vec <- c(clos.vec,sum(row[,which(colnames(row) %in% IDs)]))
  #clos.vars$Clos_Other[i] <- sum(row[,which(colnames(row) %in% IDs)])
  #Mechanical
  IDs <- unique(clos.info[which(clos.info$closure.types == 'Mechanical'),][,1])
  clos.vec <- c(clos.vec,sum(row[,which(colnames(row) %in% IDs)]))
  #clos.vars$Clos_Mechanical[i] <- sum(row[,which(colnames(row) %in% IDs)])
  #Suture
  IDs <- unique(clos.info[which(clos.info$closure.types == 'Suture'),][,1])
  clos.vec <- c(clos.vec,sum(row[,which(colnames(row) %in% IDs)]))
  #clos.vars$Clos_Suture[i] <- sum(row[,which(colnames(row) %in% IDs)])
  #Staple
  IDs <- unique(clos.info[which(clos.info$closure.types == 'Staple'),][,1])
  clos.vec <- c(clos.vec,sum(row[,which(colnames(row) %in% IDs)]))
  #clos.vars$Clos_Staple[i] <- sum(row[,which(colnames(row) %in% IDs)])
  
  as.numeric(clos.vec)
}
stopImplicitCluster()

clos.vars <- data.frame(clos.data)
colnames(clos.vars) <- paste('Clos_',unique(closure.types), sep='')

ncdr.data.full <- ncdr.data.full[,-which(colnames(ncdr.data.full) %in% closure.IDs)]
ncdr.data.full <- cbind(ncdr.data.full, clos.vars)


rem.extras <- c('HospitalID','VisitKey', 'EpisodeKey', 'ZipCode')
ncdr.data.full <- ncdr.data.full[,-which(colnames(ncdr.data.full) %in% rem.extras)]

# 
# 
save('ncdr.data.full', file='Y:/NCDR/BJMDATA/CPBLEED/DynamicModel/NCDR_FullData_v1_11MAY2017.RData')

# #Remove NA/ND columns
# idx.na <- which((grepl('ND', colnames(ncdr.data.full)) | grepl('NA', colnames(ncdr.data.full))) & !(grepl('GENDMULT', colnames(ncdr.data.full))))
# 
#Remove CABG Patients
cabg.pats <- which(ncdr.data.full$CABG == 1)
ncdr.data.full <- ncdr.data.full[-cabg.pats,]
#Remove CABG Columns
idx.cabg <- which((grepl('CABG', colnames(ncdr.data.full))) & !(grepl('PriorCABG', colnames(ncdr.data.full))))
ncdr.data.full <- ncdr.data.full[, -idx.cabg]
#Remove other major surgery patients
surg.pats <- which(ncdr.data.full$OtherMajorSurgery == 1)
ncdr.data.full <- ncdr.data.full[-surg.pats,]
#Remove other major surgery columns
ncdr.data.full$OtherMajorSurgery <- NULL

#Fix Closure + Access Site variables

clos.cols <- which(grepl('Clos', colnames(ncdr.data.full)))
clos.names <- colnames(ncdr.data.full)[clos.cols]
access.names <- c('Femoral', 'Brachial','Radial', 'Other')
femoral <- rep(0, dim(ncdr.data.full)[1])
brachial <- rep(0, dim(ncdr.data.full)[1])
radial <- rep(0, dim(ncdr.data.full)[1])
other <- rep(0, dim(ncdr.data.full)[1])

femoral[which(ncdr.data.full$AcessSite == 1)] <- 1
brachial[which(ncdr.data.full$AcessSite == 2)] <- 1
radial[which(ncdr.data.full$AcessSite == 3)] <- 1
other[which(ncdr.data.full$AcessSite == 4)] <- 1

access.closure.names <- vector()
for(i in 1:length(access.names)) {
  for(j in 1:length(clos.names)) {
    access.closure.names <- c(access.closure.names, paste(access.names[i], '_', clos.names[j], sep=''))
  }
}

access.closure <- matrix(0, dim(ncdr.data.full)[1], length(access.closure.names))
for(i in 1:length(access.closure.names)) {
  closure.name <- access.closure.names[i]
  print(i)
  if(grepl('Femoral_', closure.name)) {
    closure.val <- strsplit(closure.name, 'Femoral_')[[1]][2]
    clos.index <- which(clos.names == closure.val)
    clos.idx <- clos.cols[which(clos.names == closure.val)]
    mat.idx <- clos.index

    access.closure[,mat.idx][which(femoral == 1)] <- ncdr.data.full[,clos.idx][which(femoral == 1)]
    
  } else if(grepl('Brachial_', closure.name)) {
    closure.val <- strsplit(closure.name, 'Brachial_')[[1]][2]
    clos.index <- which(clos.names == closure.val)
    clos.idx <- clos.cols[which(clos.names == closure.val)]
    mat.idx <- 7 + clos.index
    
    access.closure[,mat.idx][which(brachial == 1)] <- ncdr.data.full[,clos.idx][which(brachial == 1)]
  } else if(grepl('Radial_', closure.name)) {
    closure.val <- strsplit(closure.name, 'Radial_')[[1]][2]
    clos.index <- which(clos.names == closure.val)
    clos.idx <- clos.cols[which(clos.names == closure.val)]
    mat.idx <- 14 + clos.index
    
    access.closure[,mat.idx][which(radial == 1)] <- ncdr.data.full[,clos.idx][which(radial == 1)]
  } else {
    closure.val <- strsplit(closure.name, 'Other_')[[1]][2]
    clos.index <- which(clos.names == closure.val)
    clos.idx <- clos.cols[which(clos.names == closure.val)]
    mat.idx <- 21 + clos.index
    
    access.closure[,mat.idx][which(other == 1)] <- ncdr.data.full[,clos.idx][which(other == 1)]
  }
}
access.closure <- data.frame(access.closure)
colnames(access.closure) <- access.closure.names

clos.na <- which(rowSums(access.closure) == 0)

fem.noclos <- rep(0, dim(ncdr.data.full)[1])
brachial.noclos <- rep(0, dim(ncdr.data.full)[1])
radial.noclos <- rep(0, dim(ncdr.data.full)[1])
other.noclos <- rep(0, dim(ncdr.data.full)[1])

fem.idx <- which(femoral == 1)
brachial.idx <- which(brachial == 1)
radial.idx <- which(radial == 1)
other.idx <- which(other == 1)

fem.noclos[fem.idx[which(fem.idx %in% clos.na)]] <- 1
brachial.noclos[brachial.idx[which(brachial.idx %in% clos.na)]] <- 1
radial.noclos[radial.idx[which(radial.idx %in% clos.na)]] <- 1
other.noclos[other.idx[which(other.idx %in% clos.na)]] <- 1


noclos.df <- data.frame(fem.noclos, brachial.noclos, radial.noclos, other.noclos)
colnames(noclos.df) <- c('Femoral_Clos_None', 'Brachial_Clos_None', 'Radial_Clos_None', 'Other_Clos_None')

c

site.df <- data.frame(femoral, brachial, radial, other)
colnames(site.df) <- c('Femoral_site', 'Brachial_site', 'Radial_site', 'Other_site')

access.closure <- cbind(access.closure, site.df)

ncdr.data.full <- cbind(ncdr.data.full, access.closure)
remove.cols <- c(clos.names, 'AcessSite')

ncdr.data.full <- ncdr.data.full[,-which(colnames(ncdr.data.full) %in% remove.cols)]
save('ncdr.data.full',  file='Y:/NCDR/BJMDATA/CPBLEED/DynamicModel/NCDR_NoCabg_v1_11MAY2017.RData')

#Create Five Fold
source('setup_crossval.R')
ncdr.data.crossval <- crossval.list(ncdr.data.full, ncdr.data.full$Bleed, 5)
save('ncdr.data.crossval', file='Y:/NCDR/BJMDATA/CPBLEED/DynamicModel/NCDR_NoCabg_Bleed_Crossval_v1_11MAY2017.RData')
