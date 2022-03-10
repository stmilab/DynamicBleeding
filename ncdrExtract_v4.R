# library(sas7bdat)
# path='Y:/NCDR/CATHPCI/CPV4/CPV4_04212015/'
# outpath = 'Y:/NCDR/BJMDATA/CATHPCI/'
# files <- list.files(path=path, pattern = "\\.sas7bdat$")
# 
# #Convert sas7bdat to RData
# for(f in files) {
#   cat(paste('reading file: ', f, '\n'))
#   data.sas <- read.sas7bdat(paste(path, f, sep=''))
#   cat(paste('saving file: ', f, '\n'))
#   save('data.sas', file=paste(outpath,f, '.RData', sep=''))
# }
# 
# #Start by reading patient stay table
# #data.pepsd <- read.sas7bdat(paste(path, 'v4pepsd.sas7bdat', sep=''))

path = '/data/Projects/ACC_NCDR/NCDR/BJMDATA/CATHPCI/'
source('./maxfunc.R')
files <- list.files(path=path, pattern="\\.RData$")

# full.sas.data <- read.csv(file='Y:/NCDR/BJMDATA/CPBLEED/FullData/temp1.csv')
# 
# list.sites <- list()
# 
# for(i in 1:length(fids)) {
#   f <- fids[i]
#   idx <- which(full.sas.data$FID == f)
#   site.data <- full.sas.data[idx,4:59]
#   vals.sums <- colSums(site.data, na.rm=TRUE)
#   vals.means <- colMeans(site.data, na.rm=TRUE)
#   vals <- c(length(idx), vals.sums, vals.means)
#   list.sites[[i]]  <- vals
# }
# 
# df <- data.frame(matrix(unlist(list.sites), nrow=length(list.sites), byrow=T))
# 
# temp <- colnames(site.data)
# 
# sum.names <- vector()
# mean.names <- vector()
# 
# for(i in 1:length(temp)) {
#   sum.names[i] <- paste(temp[i], 'sum', sep='_')
#   mean.names[i] <- paste(temp[i], 'mean', sep='_')
#   
# }
# 
# df.names <- c('Size', sum.names, mean.names)
# colnames(df) <- df.names
# 
# save('df', file='Y:/NCDR/BJMDATA/CPBLEED/FullData/site_specific_vars_existingvars.RData')
# write.table(df, file='Y:/NCDR/BJMDATA/CPBLEED/FullData/site_specific_vars_existingvars.csv', sep=',')


#
#Load pepsd first

load(file=paste(path,files[10],sep=''))

full.data <- data.sas
library(lubridate)
#Create Exclusion 1 and Exclusion 4
dc.date <- full.data$DCDate
dc.convert <- as.Date(dc.date, origin='1960-01-01')
exc1 <- rep(0, length(dc.convert))
exc4 <- rep(0, length(dc.convert))

dc.year <- year(dc.convert)
dc.month <- month(dc.convert)
exc1[which(dc.year < 2009)] <- 1
exc1[which( (dc.year == 2009) & (dc.month < 7))] <- 1
exc1[which( (dc.year == 2015) & (dc.month > 6))] <- 1

arr.date <- full.data$ArrivalDate
arr.convert <- as.Date(arr.date, origin='1960-01-01')

exc4[which((arr.convert == dc.convert) & (full.data$DCStatus == 2))] <- 1
full.data$exc1 <- exc1
full.data$exc4 <- exc4

full.data <- full.data[with(full.data, order(SubmissionPatientKey)),] #sort by patient key
pep.data <- full.data
#read psubm - replaces data.sas
load(file=paste(path,files[13],sep=''))
sub.data <- data.sas
sub.data <- sub.data[with(sub.data, order(SubmissionPatientKey)),] #sort by patient key
#checked that there are no duplicate keys in sub.data

full.data <- merge(full.data, sub.data, by='SubmissionPatientKey')

#
full.data <- full.data[with(full.data, order(SubmissionKey)),] #sort by Submission Key

#Read HSUBM
load(file=paste(path,files[5],sep=''))
hsub.data <- data.sas
#Yongfei keeps only ClientID and Sumission Key
#ClientID is the hospital ID
#Rename ClientID to HospitalID
colnames(hsub.data)[5] <- 'HospitalID'

load(file=paste(path,files[4],sep=''))
hosp.data <- data.sas
#Rename HID to HospitalID
colnames(hosp.data)[2] <- 'HospitalID'
#Remove all rows where MPN == ""
hosp.data <- hosp.data[-which(hosp.data$MPN == ""),]

#sort hosp.data by MPN
hosp.data <- hosp.data[with(hosp.data, order(MPN)),]
#Count MPN keep those with count > 1
MPN <- unique(hosp.data$MPN)
counts <- rep(NA, length(MPN))
for(i in 1:length(MPN)) {
  counts[i] <- length(which(hosp.data$MPN == MPN[i]))
}
Keep.MPN <- MPN[which(counts > 1)]
mdndup <- rep(0, length(hosp.data$MPN))
mdndup[which(hosp.data$MPN %in% Keep.MPN)] <- 1
hosp.data$MPNDUP <- mdndup
#Merge the two hospital data sets
hosp.merge <- merge(hsub.data, hosp.data, by='HospitalID', all.x=TRUE) #all.x=TRUE matches the (IN=IN2) - CHECK THIS

#Sort by SubmissionKey
hosp.merge <- hosp.merge[with(hosp.merge, order(SubmissionKey)),]

full.data <- merge(full.data, hosp.merge, by='SubmissionKey', all.x=TRUE)

#Sort by Episode Key
full.data <- full.data[with(full.data, order(EpisodeKey)),]

#Load Visit
load(file=paste(path,files[15],sep=''))
visit.data <- data.sas

#Sort Visit by Episode Key then Procedure Date Then Procedure Time
#CHECK THIS STEP DUE TO THE DATE/TIME issue
visit.data <- visit.data[with(visit.data, order(EpisodeKey, ProcedureDate, ProcedureTime)),]

#visit.data <- visit.data[which(visit.data$PCI == 1),]
exc2 <- rep(0, dim(visit.data)[1])
exc2[which(visit.data$PCI != 1)] <- 1
exc3 <- rep(0, dim(visit.data)[1])
exc3[duplicated(visit.data$EpisodeKey)] <- 1
#visit.data <- visit.data[!duplicated(visit.data$EpisodeKey),]
visit.data$exc2 <- exc2
visit.data$exc3 <- exc3
visit.data <- visit.data[with(visit.data, order(VisitKey)),] #Filters on the exclusions up front vs. SAS code that does it at end

#Load Lesion Data
load(file=paste(path,files[7],sep=''))
lesion.data <- data.sas
lesion.data$Culprit <- rep(0, dim(lesion.data)[1])
lesion.data$Culprit[which(lesion.data$CulpritArtery == 1)] <- 1
lesion.data$CTO <- as.numeric((lesion.data$StenosisPriorTreat == 100) & (lesion.data$ChronicOcclusion==1))
lesion.data$THROM <- as.numeric((lesion.data$PrevLesionTime== 1) & (lesion.data$InThrombosis == 1))
lesion.data$PreTIMI <- lesion.data$PreProcTIMI - 1
LESSCAI <- rep(NA, dim(lesion.data)[1])
LESSCAI[which((lesion.data$LesionComplexity == 1)&(lesion.data$StenosisPriorTreat >= 0) & (lesion.data$StenosisPriorTreat <= 100))] <- 1 + 2*(lesion.data$StenosisPriorTreat==100)
LESSCAI[which((lesion.data$LesionComplexity == 2)&(lesion.data$StenosisPriorTreat >= 0) & (lesion.data$StenosisPriorTreat <= 100))] <- 2 + 2*(lesion.data$StenosisPriorTreat==100)
LESSCAI[which(is.na(LESSCAI) & (lesion.data$StenosisPriorTreat == 100))] <- 3
LESSCAI[which(is.na(LESSCAI) & (lesion.data$LesonComplexty == 2))] <- 2
LESSCAI[which(is.na(LESSCAI))] <- 1
lesion.data$LESSCAI <- LESSCAI

load(file=paste(path,files[9],sep=''))
lsegm.data <- data.sas
NEWSEQ <- rep(0, dim(lsegm.data)[1])
NEWSEQ[which(substr(lsegm.data$SegmentID,1,2) == 11)] <- 3 #Left Main
NEWSEQ[which(substr(lsegm.data$SegmentID,1,2) == 12)] <- 2 #pLad
NEWSEQ[which(substr(lsegm.data$SegmentID,1,2) %in% c(1,13,18))] <- 1
lsegm.data$NEWSEQ <- NEWSEQ

lesion.data <- lesion.data[with(lesion.data, order(VisitKey, LesionCounter)),]

colnames(lsegm.data)[2] <- 'VisitKey'
lsegm.data <- lsegm.data[with(lsegm.data, order(VisitKey, LesionCounter)),]

#VisitKey has a lot of repeats
#Lesion Counter has even more repeats
#For each repetition, take the max NEWSEQ

# maxNEWSEQ <- NEWSEQ
# uni.visit <- unique(lsegm.data$VisitKey)
# uni.lesion <- unique(lsegm.data$LesionCounter)
# 
# for(i in 1:length(uni.visit)) {
#   v <- uni.visit[i]
#   if(i %% 100000 == 1) {
#     cat(paste('i:', i,'\n'))
#   }
#   for(l in uni.lesion) {
#     idx <- which((lsegm.data$VisitKey == v) & (lsegm.data$LesionCounter == l))
#     SEQ <- maxNEWSEQ[idx]
#     maxNEWSEQ[idx] <- max(SEQ)
#   }
# }
# 
# 
# templsegm=as.matrix(lsegm.data[,c(2:4)])
# temptemp=templsegm
# temptemp=data.frame(temptemp)
# tempsplit=split(temptemp,list(temptemp[,1],temptemp[,2]))
# for (j in 1:length(tempsplit)) {
#   v=tempsplit[[j]]
#   if (nrow(v)>1) {
#     v[,3]=max(v[,3])
#   }
#   tempsplit[[j]]=v
# }
# aa=unsplit(tempsplit,list(temptemp[,1],temptemp[,2])) ##this will hold the corrected matrix
# ####run the above code. below is not useful
# 
# lsegm=lsegm.data
# lsplitonvisit=split(lsegm,lsegm[,c(1,2)])
# for (j in 1:length(lsplitonvisit)) {
#   ll=lsplitonvisit[[j]]
#   splitonlesion=split(ll,ll[,3])
#   for (k in 1:length(splitonlesion)) {
#     aa=splitonlesion[[k]]
#     aa[,4]=max(aa[,4])
#     splitonlesion[[k]]=aa
#   }
#   lsplitonvisit[[j]]=unsplit(splitonlesion,ll[,3])  
# }
# blah=unsplit(lsplitonvisit,lsegm[,2])
# lsegm=templsegm
# 
# vv=split(lsegm,lsegm[,c(2,3)])
# for (j in 1:length(vv)) {
#   if(nrow(vv[[j]])>0) {
#     vv[[j]][,4]=max(vv[[j]][,4])
#   }
# }
# vv=unsplit(vv,lsegm[,c(2,3)])

templsegm <- lsegm.data[,2:4]
templsegm <- maxfunc(templsegm)
lsegm.data[,2:4] <- templsegm
lsegm.data <- lsegm.data[!duplicated(lsegm.data[,2:4]),]
lesion.merge <- merge(lesion.data, lsegm.data, by=c('VisitKey', 'LesionCounter'), all.x=TRUE)
lesion.temp <- data.frame(lesion.merge$VisitKey, lesion.merge$Culprit)
# vis.keys <- unique(lesion.merge$VisitKey)
# cul.data <- lesion.merge$Culprit
# someca <- cul.data
# for(v in vis.keys) {
#   idx <- which(lesion.merge$VisitKey == v)
#   vals <- cul.data[idx]
#   someca[idx] <- max(vals)
# }
lesion.res <- maxfunc_alt(lesion.temp)
lesion.res <- unique(lesion.res)
colnames(lesion.res)[1] <- 'VisitKey'
colnames(lesion.res)[2] <- 'SOMECA'
lesion.merge <- merge(lesion.merge, lesion.res, by=c('VisitKey'), all.x=TRUE)
#lesion.merge <- lesion.merge[!duplicated(lesion.merge[,c(1,33)]),]

temp4.data <- lesion.merge
temp4.data <- temp4.data[which((temp4.data$Culprit == 1) & (temp4.data$SOMECA == 1) ),]
temp4.data.proc <- temp4.data[,c('VisitKey', 'NEWSEQ', 'CTO', 'THROM', 'LESSCAI', 'PreTIMI')]

temp4.data.proc <- newmaxfunc(temp4.data.proc)
temp4.data.proc <- as.data.frame(temp4.data.proc)
#Visit Key for temp4 and temp4 proc are the same so everything is already ordered
temp4.data$NEWSEQ <- temp4.data.proc$NEWSEQ
temp4.data$CTO <- temp4.data.proc$CTO
temp4.data$THROM <- temp4.data.proc$THROM
temp4.data$LESSCAI <- temp4.data.proc$LESSCAI
temp4.data$PreTIMI <- temp4.data.proc$PreTIMI
#temp4.data <- unique(temp4.data)
temp4.data <- temp4.data[!duplicated(temp4.data[,c('VisitKey', 'NEWSEQ', 'CTO', 'THROM', 'LESSCAI', 'PreTIMI')]),]

#lesion.merge <- merge(lesion.merge, temp4.data, by=c('VisitKey'), all.x=TRUE)

temp5.data <- lesion.merge
temp5.data <- temp5.data[which(temp5.data$SOMECA == 0),]
temp5.data.proc <- temp5.data[,c('VisitKey', 'NEWSEQ', 'CTO', 'THROM', 'LESSCAI', 'PreTIMI')]

temp5.data.proc <- newmaxfunc(temp5.data.proc)
temp5.data.proc <- as.data.frame(temp5.data.proc)
temp5.data$NEWSEQ <- temp5.data.proc$NEWSEQ
temp5.data$CTO <- temp5.data.proc$CTO
temp5.data$THROM <- temp5.data.proc$THROM
temp5.data$LESSCAI <- temp5.data.proc$LESSCAI
temp5.data$PreTIMI <- temp5.data.proc$PreTIMI
temp5.data <- temp5.data[!duplicated(temp5.data[,c('VisitKey', 'NEWSEQ', 'CTO', 'THROM', 'LESSCAI', 'PreTIMI')]),]
#temp5.data <- unique(temp5.data)
##################
#Verified Dimensions up to this point
################
#This is my best attempt at following Yongfei's code AND keeping the extra columns in lesion.merge
lesion.merge <- rbind(temp4.data, temp5.data)

#Merge with visit data
visit.data <- merge(visit.data, lesion.merge, by='VisitKey', all.x=TRUE)

#merge with Full Data and Create Bleed Variable
final.data <- merge(full.data, visit.data, by='EpisodeKey')

#Create Bleed
Bleed <- rep(NA, dim(final.data)[1])

idx <-which(!is.na(final.data$PostBleed) | !is.na(final.data$PostTamponade) | !is.na(final.data$PostHemStroke) | 
              (!is.na(final.data$PostTransfusion) & !is.na(final.data$PreProcHgb) & !is.na(final.data$CABG)) | 
              (!is.na(final.data$PreProcHgb) & !is.na(final.data$PostProcHgb))  ) 

Bleed[which(!is.na(final.data$PostBleed) | !is.na(final.data$PostTamponade) | !is.na(final.data$PostHemStroke) | 
              (!is.na(final.data$PostTransfusion) & !is.na(final.data$PreProcHgb) & !is.na(final.data$CABG)) | 
              (!is.na(final.data$PreProcHgb) & !is.na(final.data$PostProcHgb))  )] <-
  ifelse(any( c(final.data$PostBleed==1, final.data$PostTamponade == 1, final.data$PostHemStroke == 1,
                ((final.data$PostTransfusion == 1) & (final.data$PreProcHgb > 8) & (final.data$CABG == 1)), 
                (!is.na(final.data$PreProcHgb) & (final.data$PreProcHgb - final.data$PostProcHgb >= 3) &
                   (final.data$PreProcHgb <= 16)  )       ), na.rm=TRUE), 1, 0)
#Bleed[ which( !(is.na(final.data$PostBleed)) |  !(is.na(final.data$PostTamponade)) | !(is.na(final.data$PostHemStroke))
#              ( !(is.na(final.data$PostTransfusion)) & !(is.na(final.data$PreProcHgb)) & !(is.na(final.data$CABG))) |
#              ( !(is.na(final.data$PreProcHgb)) & !(is.na(final.data$PostProcHgb))     )
#              )] <- (   ((final.data$PostBleed == 1) | (final.data$PostTamponade == 1) | (final.data$PostHemStroke == 1)) |
#                        ( (final.data$PostTransfusion == 1) & (final.data$PreProcHgb > 8) & (final.data$CABG != 1))    |
#                          ((final.data$PreProcHgb - final.data$PostProcHgb >= 3) & (!(is.na(final.data$PreProcHgb))) & (final.data$PreProcHgb <= 16))
#                     )
Bleed <- rep(NA, dim(final.data)[1])
for(i in 1:dim(final.data)[1]) {
  if(i %% 100000 == 1) {
    print(i)
  }
  
  row <- final.data[i,]
  b.vec <- vector()
  if (!is.na(row$PostBleed) | !is.na(row$PostTamponade) | !is.na(row$PostHemStroke) | 
      (!is.na(row$PostTransfusion) & !is.na(row$PreProcHgb) & !is.na(row$CABG)) | 
      (!is.na(row$PreProcHgb) & !is.na(row$PostProcHgb))  )   {
    
    b.vec <- c(b.vec, (row$PostBleed == 1))
    b.vec <- c(b.vec, (row$PostTamponade == 1))
    b.vec <- c(b.vec, (row$PostHemStroke == 1))
    b.vec <- c(b.vec, ( (row$PostTransfusion == 1) & (row$PreProcHgb > 8) & (row$CABG != 1)    ) )
    b.vec <- c(b.vec, ( !is.na(row$PreProcHgb) & (row$PreProcHgb - row$PostProcHgb >= 3) &  (row$PreProcHgb <= 16) ))
    if(any(b.vec, na.rm=TRUE)) {
      Bleed[i] <- 1
    } else {
      Bleed[i] <- 0
    }
  }
}


final.data$Bleed <- Bleed

#Create exc5
exc5 <- rep(0, length(Bleed))
exc5[which(is.na(Bleed))] <- 1 #Missing data related to bleeding

final.data$exc5 <- exc5

final.data <- final.data[with(final.data, order(HospitalID)),] #Renamed ClientID to HospitalID
Bleed.data <- data.frame(final.data$HospitalID, final.data$Bleed)
colnames(Bleed.data)[1] <- 'HospitalID'
colnames(Bleed.data)[2] <- 'Bleed'
Bleed.data <- maxfunc_sum(Bleed.data)
Bleed.data.alt <- data.frame(final.data$HospitalID, final.data$Bleed)

exc6 <- rep(0, dim(Bleed.data)[1])
exc6[which(Bleed.data[,2] <= 0)] <- 1

final.data$exc6 <- exc6

final.data <- final.data[with(final.data, order(HospitalID, NCDRPatientID,EpisodeKey, VisitKey)),]

#Create FID, SID, PID as 0 to whatever
uniq.hospid <- unique(final.data$HospitalID)
uniq.patid <- unique(final.data$NCDRPatientID)
uniq.epikey <- unique(final.data$EpisodeKey)

uniq.hid <- seq(1, length(uniq.hospid),1)
uniq.pid <- seq(1, length(uniq.patid), 1)
uniq.eid <- seq(1, length(uniq.epikey), 1)

hid <- rep(NA, dim(final.data)[1])
pid <- rep(NA, dim(final.data)[1])
eid <- rep(NA, dim(final.data)[1])

h=matrix(0,max(final.data$HospitalID),1)
i=0
for (j in (uniq.hospid)) {
  i=i+1
  if (i %% 10000 == 0) print(i)
  if(h[j]==0)
    h[j]=i
}

hid=h[final.data$HospitalID]


p=matrix(0,max(final.data$NCDRPatientID),1)
i=0
for (j in (uniq.patid)) {
  i=i+1
  if (i %% 10000 == 0) print(i)
  if(p[j]==0)
    p[j]=i
}

pid=p[final.data$NCDRPatientID]

# for(i in 1:length(uniq.hospid)) {
#   if(i %% 1000 == 1) {
#     print(i)
#   }
#   hosp <- uniq.hospid[i]
#   hid[which(final.data$HospitalID == hosp)] <- i#uniq.hid[i]
# }
# 
# for(i in 1:length(uniq.patid)) {
#   if(i %% 1000000 == 1) {
#     print(i)
#   }
#   pat <- uniq.patid[i]
#   pid[which(final.data$NCDRPatientID == pat)] <- uniq.pid[i]
# }
# 
# for(i in 1:length(uniq.epikey)) {
#   if(i %% 1000000 == 1) {
#     print(i)
#   }
#   epi <- uniq.epikey[i]
#   eid[which(final.data$EpisodeKey == epi)] <- uniq.eid[i]
# }

a=matrix(0,max(final.data$EpisodeKey),1)
i=0
for (j in (uniq.epikey)) {
  i=i+1
  if (i %% 10000 == 0) print(i)
  if(a[j]==0)
    a[j]=i
}

eid=a[final.data$EpisodeKey]

final.data$HID <- hid
final.data$PID <- pid
final.data$EID <- eid
save('final.data', file='Y:/NCDR/BJMDATA/CPBLEED/YongfeiExtraction_modified.RData')

rows <-vector()
episodes <- vector()
pats <- vector()
hosps <- vector()

rows <- c(rows, dim(final.data)[1])
episodes <- c(episodes, length(unique(final.data$EID)))
pats <- c(pats, length(unique(final.data$PID)))
hosps <- c(hosps, length(unique(final.data$HID)))

final.data <- final.data[which(final.data$exc1 == 0),]

rows <- c(rows, dim(final.data)[1])
episodes <- c(episodes, length(unique(final.data$EID)))
pats <- c(pats, length(unique(final.data$PID)))
hosps <- c(hosps, length(unique(final.data$HID)))

final.data <- final.data[which((final.data$exc1 == 0) & (final.data$exc2 == 0)),]

rows <- c(rows, dim(final.data)[1])
episodes <- c(episodes, length(unique(final.data$EID)))
pats <- c(pats, length(unique(final.data$PID)))
hosps <- c(hosps, length(unique(final.data$HID)))

final.data <- final.data[which((final.data$exc1 == 0) & (final.data$exc2 == 0) & (final.data$exc3 == 0)),]

rows <- c(rows, dim(final.data)[1])
episodes <- c(episodes, length(unique(final.data$EID)))
pats <- c(pats, length(unique(final.data$PID)))
hosps <- c(hosps, length(unique(final.data$HID)))

final.data <- final.data[which((final.data$exc1 == 0) & (final.data$exc2 == 0) & (final.data$exc3 == 0) & 
                                                                  (final.data$exc4 == 0)),]

rows <- c(rows, dim(final.data)[1])
episodes <- c(episodes, length(unique(final.data$EID)))
pats <- c(pats, length(unique(final.data$PID)))
hosps <- c(hosps, length(unique(final.data$HID)))

final.data <-  final.data[which((final.data$exc1 == 0) & (final.data$exc2 == 0) & (final.data$exc3 == 0) & 
                                                    (final.data$exc4 == 0)  & (final.data$exc5 == 0)),]
rows <- c(rows, dim(final.data)[1])
episodes <- c(episodes, length(unique(final.data$EID)))
pats <- c(pats, length(unique(final.data$PID)))
hosps <- c(hosps, length(unique(final.data$HID)))

final.data <- final.data[which((final.data$exc1 == 0) & (final.data$exc2 == 0) & (final.data$exc3 == 0) & 
                                         (final.data$exc4 == 0)  & (final.data$exc5 == 0) & (final.data$exc6 == 0)),]
rows <- c(rows, dim(final.data)[1])
episodes <- c(episodes, length(unique(final.data$EID)))
pats <- c(pats, length(unique(final.data$PID)))
hosps <- c(hosps, length(unique(final.data$HID)))

#Generate Counts
#print('Case | Total Number of Rows | Distinct Episodes | Distinct Patients | Distinct Hospitals\n')
#cat(paste('All |',dim(final.data)[1], '|', length(unique(final.data$EID)), '|', length(unique(final.data$PID)), '|' , length(unique(final.data$HID)), '\n'))
#Exc 1 - Out of date ranges

rows <-vector()
episodes <- vector()
pats <- vector()
hosps <- vector()

exc1.data <- final.data[which(final.data$exc1 == 0),]
#Exc 2 - Not PCI
exc2.data <- final.data[which((final.data$exc1 == 0) & (final.data$exc2 == 0)),]
#Exc 3 - Not first PCI during Stay
exc3.data <- final.data[which((final.data$exc1 == 0) & (final.data$exc2 == 0) & (final.data$exc3 == 0)),]
#Exc 4 - Discharge Date == Arrival Date and DC Status = 2
exc4.data <- final.data[which((final.data$exc1 == 0) & (final.data$exc2 == 0) & (final.data$exc3 == 0) & 
                                (final.data$exc4 == 0)),]
#Exc 5 - Missing data related to bleeding
exc5.data <- final.data[which((final.data$exc1 == 0) & (final.data$exc2 == 0) & (final.data$exc3 == 0) & 
                                (final.data$exc4 == 0)  & (final.data$exc5 == 0)),]
#Exc 6 - Bleed Sum <= 0
exc6.data <- final.data[which((final.data$exc1 == 0) & (final.data$exc2 == 0) & (final.data$exc3 == 0) & 
                                (final.data$exc4 == 0)  & (final.data$exc5 == 0) & (final.data$exc6 == 0)),]

rows <- c(dim(final.data)[1], dim(exc1.data)[1], dim(exc2.data)[1], dim(exc3.data)[1], 
          dim(exc4.data)[1], dim(exc5.data)[1], dim(exc6.data)[1])
episodes <- c(length(unique(final.data$EID)), length(unique(exc1.data$EID)),length(unique(exc2.data$EID)),
              length(unique(exc3.data$EID)),length(unique(exc4.data$EID)),length(unique(exc5.data$EID)),
              length(unique(exc6.data$EID)))
pats <- c(length(unique(final.data$PID)), length(unique(exc1.data$PID)),length(unique(exc2.data$PID)),
          length(unique(exc3.data$PID)),length(unique(exc4.data$PID)),length(unique(exc5.data$PID)),
          length(unique(exc6.data$PID)))
hosps <- c(length(unique(final.data$HID)), length(unique(exc1.data$HID)),length(unique(exc2.data$HID)),
           length(unique(exc3.data$HID)),length(unique(exc4.data$HID)),length(unique(exc5.data$HID)),
           length(unique(exc6.data$HID)))

results.df <- data.frame(rows, episodes, pats, hosps)
colnames(results.df) <- c('Total Rows', 'Distinct Episodes', 'Distinct Patients', 'Distinct Hospitals')
rownames(results.df) <- c('Total', 'Proper Dates', 'PCI', 'First PCI During Stay', 'Discharge Date after Arrival', 'All Data relating to Bleeds', 'Has Bleed Info')
save('final.data', file='Y:/NCDR/BJMDATA/CPBLEED/YongfeiExtraction_modified_Exclusion.RData')

#################
## Extract Additional Files
#################
path = 'Y:/NCDR/BJMDATA/CATHPCI/'
source('./maxfunc.R')
files <- list.files(path=path, pattern="\\.RData$")
#load
load(file='Y:/NCDR/BJMDATA/CPBLEED/YongfeiExtraction_modified_Exclusion.RData')
#Site
load(file=paste(path,files[14],sep=''))
colnames(data.sas)[28] <- 'HospitalID' #From ClientID
final.data <- merge(final.data, data.sas, by='HospitalID')

#dischargemeds
load(file=paste(path,files[2],sep=''))
#append discharge to colnames 2 and 3
colnames(data.sas)[2] <- 'DischargeMedID'
colnames(data.sas)[3] <- 'DischargeMedAdmin'

data.sas <- data.sas[with(data.sas, order(EpisodeKey)),]

curr.key <- NA
medKeys <- data.sas$EpisodeKey
keptKeys <- final.data$EpisodeKey
medIDs <- unique(data.sas$DischargeMedID)
medMatrix <- matrix(NA, nrow=length(unique(medKeys)), ncol=1+length(unique(data.sas$DischargeMedID)))
curr.row.idx <- 1
curr.row <- rep(NA, dim(medMatrix)[2])
for(i in 1:length(medKeys)) {
  #For each row
  medKey <- medKeys[i]
  if(i %% 1000000 == 1) {
    print(i)
  }
  if(!(is.na(curr.key)) & (medKey == curr.key)) {
    curr.ID <- 1 + which(data.sas$DischargeMedID[i] == medIDs)
    curr.row[curr.ID] <- data.sas$DischargeMedAdmin[i]
  } else {
    if(is.na(curr.key)) {
      #first row
      curr.key <- medKey
      curr.row[1] <- medKey
      curr.ID <- 1 + which(data.sas$DischargeMedID[i] == medIDs)
      curr.row[curr.ID] <- data.sas$DischargeMedAdmin[i]
    } else {
      #new row
      medMatrix[curr.row.idx,] <- curr.row
      curr.row.idx <- curr.row.idx + 1
      curr.row <- rep(NA, dim(medMatrix)[2])
      curr.key <- medKey
      curr.row[1] <- medKey
      curr.ID <- 1 + which(data.sas$DischargeMedID[i] == medIDs)
      curr.row[curr.ID] <- data.sas$DischargeMedAdmin[i]
    }
  } 
  
}
medMatrix[curr.row.idx,] <- curr.row



dmed.names <- rep('DischargeMed',10)
dmed.id <- unique(data.sas$DischargeMedID)
for(i in 1:length(dmed.id)) {
  dmed.names[i] <- paste(dmed.names[i], dmed.id[i],sep='')
}

dmed.names <- c('EpisodeKey', dmed.names)

medFrame <- data.frame(medMatrix)
colnames(medFrame) <- dmed.names

#keptRows <- which(medFrame$EpisodeKey %in% keptKeys)
#Impute NAs? - mark those with missing data?
#exc.dmed
#exc.dmed <- rep(1, length(keptKeys))
#exc.dmed[which(keptKeys %in% medFrame$EpisodeKey)] <- 0

#temp <- medFrame[keptRows,]
#temp <- cbind(medFrame, exc.dmed)

final.data <- merge(final.data, medFrame, by='EpisodeKey', all.x=TRUE) #Since not all episodes have discharge med information

#Pre-Procedure Meds
load(file=paste(path,files[12],sep=''))
colnames(data.sas)[1] <- 'VisitKey'
colnames(data.sas)[2] <- 'PreOpMedID'
colnames(data.sas)[3] <- 'PreOpMedAdmin'

data.sas <- data.sas[with(data.sas, order(VisitKey)),]

curr.key <- NA
medKeys <- data.sas$VisitKey
keptKeys <- final.data$VisitKey

newkeys <- 0
for(j in 1:length(medKeys)) {
  med = medKeys[j]
  if(is.na(curr.key)) {
    curr.key <- med
    newkeys <- newkeys + 1
  } else if (!(med == curr.key)) {
    curr.key <- med
    newkeys <- newkeys + 1
  }
}

curr.key <- NA
medKeys <- data.sas$VisitKey
keptKeys <- final.data$VisitKey

medIDs <- unique(data.sas$PreOpMedID)
medMatrix <- matrix(NA, nrow=length(unique(medKeys)), ncol=1+length(medIDs))
curr.row.idx <- 1
curr.row <- rep(NA, dim(medMatrix)[2])
i <- 1
for(i in 1:length(medKeys)) {
  #For each row
  medKey <- medKeys[i]
  if(i %% 1000000 == 1) {
    print(i)
  }
  
  if(is.na(curr.key)) {
    #first row
    curr.key <- medKey
    curr.row[1] <- medKey
    curr.ID <- 1 + which(data.sas$PreOpMedID[i] == medIDs)
    curr.row[curr.ID] <- data.sas$PreOpMedAdmin[i]
  } else if (medKey == curr.key) {
    curr.ID <- 1 + which(data.sas$PreOpMedID[i] == medIDs)
    curr.row[curr.ID] <- data.sas$PreOpMedAdmin[i]
  } else {
    medMatrix[curr.row.idx,] <- curr.row
    curr.row.idx <- curr.row.idx + 1
    curr.row <- rep(NA, dim(medMatrix)[2])
    curr.key <- medKey
    curr.row[1] <- medKey
    curr.ID <- 1 + which(data.sas$PreOpMedID[i] == medIDs)
    curr.row[curr.ID] <- data.sas$PreOpMedAdmin[i]
  }
  
#   if(!(is.na(curr.key)) & (medKey == curr.key)) {
#     curr.ID <- 1 + which(data.sas$PreOpMedID[i] == medIDs)
#     curr.row[curr.ID] <- data.sas$PreOpMedAdmin[i]
#   } else {
#     if(is.na(curr.key)) {
#       #first row
#       curr.key <- medKey
#       curr.row[1] <- medKey
#       curr.ID <- 1 + which(data.sas$PreOpMedID[i] == medIDs)
#       curr.row[curr.ID] <- data.sas$PreOpMedAdmin[i]
#     } else {
#       #new row
#       if(!(length(curr.row) == dim(medMatrix)[2])) {
#         print(i)
#         print(length(curr.row))
#         print(curr.row.idx)
#         print(dim(medMatrix))
#       }
#        medMatrix[curr.row.idx,] <- curr.row
#       curr.row.idx <- curr.row.idx + 1
#       curr.row <- rep(NA, dim(medMatrix)[2])
#       curr.key <- medKey
#       curr.row[1] <- medKey
#       curr.ID <- 1 + which(data.sas$PreOpMedID[i] == medIDs)
#       curr.row[curr.ID] <- data.sas$PreOpMedAdmin[i]
#     }
#   } 
  
}
medMatrix[curr.row.idx,] <- curr.row



dmed.names <- rep('PreOpMed',11)
dmed.id <- unique(data.sas$PreOpMedID)
for(i in 1:length(dmed.id)) {
  dmed.names[i] <- paste(dmed.names[i], dmed.id[i],sep='')
}

dmed.names <- c('VisitKey', dmed.names)

medFrame <- data.frame(medMatrix)
colnames(medFrame) <- dmed.names

keptRows <- which(medFrame$VisitKey %in% keptKeys)

final.data <- merge(final.data, medFrame, by='VisitKey')

save('final.data', file='Y:/NCDR/BJMDATA/CPBLEED/Cohort_and_Med_05NOV2015.RData')
########################
## Unclear after here
path = 'Y:/NCDR/BJMDATA/CATHPCI/'
source('./maxfunc.R')
files <- list.files(path=path, pattern="\\.RData$")
load(file='Y:/NCDR/BJMDATA/CPBLEED/Cohort_and_Med_05NOV2015.RData')

#IC Dev
load(file=paste(path, files[6], sep=''))
#Multiple rows per visit key ~ 3 rows per key
data.sas <- data.sas[with(data.sas, order(VisitKey)),]
### ONLY KEEPING DEVICE TYPE FOR NOW
curr.key <- NA
devKeys <- data.sas$VisitKey
keptKeys <- final.data$VisitKey

DevType <- unique(data.sas$DeviceTypeName)
devMatrix <- matrix(NA, nrow=length(unique(devKeys)), ncol=1+length(DevType))
curr.row.idx <- 1
curr.row <- rep(0, dim(devMatrix)[2])
for(i in 1:length(devKeys)) {
  #For each row
  devKey <- devKeys[i]
  if(i %% 1000000 == 1) {
    print(i)
  }
  
  if(is.na(curr.key)) {
    #first row
    curr.key <- devKey
    curr.row[1] <- devKey
    curr.ID <- 1 + which(data.sas$DeviceTypeName[i] == DevType)
    curr.row[curr.ID] <- 1 + curr.row[curr.ID]
  } else if (devKey == curr.key) {
    curr.ID <- 1 + which(data.sas$DeviceTypeName[i] == DevType)
    curr.row[curr.ID] <- 1 + curr.row[curr.ID]
  } else {
    devMatrix[curr.row.idx,] <- curr.row
    curr.row.idx <- curr.row.idx + 1
    curr.row <- rep(0, dim(devMatrix)[2])
    curr.key <- devKey
    curr.row[1] <- devKey
    curr.ID <- 1 + which(data.sas$DeviceTypeName[i] == DevType)
    curr.row[curr.ID] <- 1 + curr.row[curr.ID]
  }
  
}
devMatrix[curr.row.idx,] <- curr.row



dev.names <- rep('ICDEV_',length(DevType))
for(i in 1:length(DevType)) {
  dev.names[i] <- paste(dev.names[i], DevType[i],sep='')
}

dev.names <- c('VisitKey', dev.names)

devFrame <- data.frame(devMatrix)
colnames(devFrame) <- dev.names
devFrame[is.na(devFrame)] <- 0

keptRows <- which(devFrame$VisitKey %in% keptKeys)
final.data <- merge(final.data, devFrame, by='VisitKey', all.x=TRUE)

#LSDev - 
load(file=paste(path, files[8], sep=''))
#Multiple rows per visit key ~ 3 rows per key
data.sas <- data.sas[with(data.sas, order(VisitKey)),]
### ONLY KEEPING DEVICE TYPE FOR NOW
curr.key <- NA
devKeys <- data.sas$VisitKey
keptKeys <- final.data$VisitKey

DevType <- unique(data.sas$DeviceTypeName)
devMatrix <- matrix(NA, nrow=length(unique(devKeys)), ncol=2+length(DevType))
curr.row.idx <- 1
curr.row <- rep(0, dim(devMatrix)[2])
for(i in 1:length(devKeys)) {
  #For each row
  devKey <- devKeys[i]
  if(i %% 1000000 == 1) {
    print(i)
  }
  
  if(is.na(curr.key)) {
    #first row
    curr.key <- devKey
    curr.row[1] <- devKey
    curr.row[2] <- data.sas$LesionCounter[i]
    curr.ID <- 2 + which(data.sas$DeviceTypeName[i] == DevType)
    curr.row[curr.ID] <- 1 + curr.row[curr.ID]
  } else if (devKey == curr.key) {
    curr.ID <- 2 + which(data.sas$DeviceTypeName[i] == DevType)
    curr.row[curr.ID] <- 1 + curr.row[curr.ID]
  } else {
    devMatrix[curr.row.idx,] <- curr.row
    curr.row.idx <- curr.row.idx + 1
    curr.row <- rep(0, dim(devMatrix)[2])
    curr.key <- devKey
    curr.row[1] <- devKey
    curr.row[2] <- data.sas$LesionCounter[i]
    curr.ID <- 2 + which(data.sas$DeviceTypeName[i] == DevType)
    curr.row[curr.ID] <- 1 + curr.row[curr.ID]
  }
  
}
devMatrix[curr.row.idx,] <- curr.row



dev.names <- rep('LSDEV_',length(DevType))
for(i in 1:length(DevType)) {
  dev.names[i] <- paste(dev.names[i], DevType[i],sep='')
}

dev.names <- c('VisitKey', 'LesionCounter', dev.names)

devFrame <- data.frame(devMatrix)
colnames(devFrame) <- dev.names
devFrame[is.na(devFrame)] <- 0

keptRows <- which(devFrame$VisitKey %in% keptKeys)
final.data <- merge(final.data, devFrame, by='VisitKey', all.x=TRUE)

#closm
load(file=paste(path,files[1],sep=''))
data.sas <- data.sas[with(data.sas, order(VisitKey)),]
### ONLY KEEPING Closure ID and Counter FOR NOW
curr.key <- NA
closKeys <- data.sas$VisitKey
keptKeys <- final.data$VisitKey

ClosID <- unique(data.sas$ClosureDevID)
closMatrix <- matrix(NA, nrow=length(unique(closKeys)), ncol=2+length(ClosID))
curr.row.idx <- 1
curr.row <- rep(0, dim(closMatrix)[2])
for(i in 1:length(closKeys)) {
  #For each row
  closKey <- closKeys[i]
  if(i %% 1000000 == 1) {
    print(i)
  }
  
  if(is.na(curr.key)) {
    #first row
    curr.key <- closKey
    curr.row[1] <- closKey
    curr.row[2] <- data.sas$ClosureCounter[i]
    curr.ID <- 2 + which(data.sas$ClosureDevID[i] == ClosID)
    curr.row[curr.ID] <- 1 + curr.row[curr.ID]
  } else if (closKey == curr.key) {
    curr.ID <- 2 + which(data.sas$ClosureDevID[i] == ClosID)
    curr.row[curr.ID] <- 1 + curr.row[curr.ID]
  } else {
    closMatrix[curr.row.idx,] <- curr.row
    curr.row.idx <- curr.row.idx + 1
    curr.row <- rep(0, dim(closMatrix)[2])
    curr.key <- closKey
    curr.row[1] <- closKey
    curr.row[2] <- data.sas$ClosureCounter[i]
    curr.ID <- 2 + which(data.sas$ClosureDevID[i] == ClosID)
    curr.row[curr.ID] <- 1 + curr.row[curr.ID]
  }
  
}
closMatrix[curr.row.idx,] <- curr.row



clos.names <- rep('ClosID',length(ClosID))
for(i in 1:length(ClosID)) {
  clos.names[i] <- paste(clos.names[i], ClosID[i],sep='')
}

clos.names <- c('VisitKey', 'ClosureCounter', clos.names)

closFrame <- data.frame(closMatrix)
colnames(closFrame) <- clos.names
closFrame[is.na(closFrame)] <- 0

keptRows <- which(closFrame$VisitKey %in% keptKeys)
final.data <- merge(final.data, closFrame, by='VisitKey', all.x=TRUE)

#HMPN - state info and hospital name
load(file=paste(path, files[3],sep=''))
colnames(data.sas)[6] <- 'HospitalID'
colnames(data.sas)[1] <- 'Hosp_ClientName'
colnames(data.sas)[2] <- paste('Hosp_',colnames(data.sas)[2])
colnames(data.sas)[3] <- paste('Hosp_',colnames(data.sas)[3])
colnames(data.sas)[4] <- paste('Hosp_',colnames(data.sas)[4])
colnames(data.sas)[5] <- paste('Hosp_',colnames(data.sas)[5])
colnames(data.sas)[7] <- paste('Hosp_',colnames(data.sas)[7])
#data.merge <- data.sas[,c(5,6)]#State info

#temp <- merge(final.data, data.merge, by='HospitalID')

final.data <- merge(final.data, data.sas, by='HospitalID')

save('final.data', file='Y:/NCDR/BJMDATA/CPBLEED/FullCohort_v3_07MAR2016.RData')
##################################
#phys - don't need - have operator key - who cares about name?
load(file=paste(path,files[11],sep=''))
colnames(data.sas)[8] <- 'HospitalID'


##################################
## Create Rao + Variables and 
load(file='Y:/NCDR/BJMDATA/CPBLEED/ExistingModel/ExistingModel_SASInput.RData')
## Remainder + Variables
eids <- final.data$EID
pids <- final.data$PID
idx <- which((sas.data$SID %in% eids) | (sas.data$PID %in% pids))
rao.variables <- final.data[idx,]
remainder.variables <- final.data[-idx,]
save('rao.variables', 'remainder.variables', 'idx', file='Y:/NCDR/BJMDATA/CPBLEED/RaoVariables_v3_07MAR2016.RData')

