##
## Raw dataset transformation
##

#clean
rm(list = ls(all.names = TRUE))
cat("\014")

#load packages
require(data.table);require(opal);require(magrittr);require('Hmisc');require(foreign)

source('~/cptp/opal-login.r')

atlH <- '/home/yamadou/export/Atlantic_R2-ATL_Measurements-20170209102652/ATL_Measurements/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.) %>% data.table(.,key='entity_id')

######atl
id_map = fread('~/data/reorg/idmap/Participant-atl_r2.csv',
               colClasses = 'character',col.names = c("OpalID","SourceID")) %>% data.table ;

id_exclu = Filter(function(x) !is.na(x),stringr::str_extract(
  readr::read_lines('~/data/reorg/exclu/ATL1_exclusionJan2017.txt'),"[0-9]+"))

id_exclu

lid <- fread('~/data/reorg/exclu/AtlanticPATH_ParticipantDetails_Nov9_2016.csv',
             colClasses = 'character') %>% data.table;

names(lid)[1] <- 'SourceID' ; setkey(lid,'SourceID')

o <- opal.login('yamadou','Na96wal2','https://cptp-harmo.oicr.on.ca:8443/')

opal.assign.table(o,'cptp','Atlantic_R2.AtlanticPath_PhysicalMeasuresSet1_April18_2016',id.name = "entity_id")
ds1 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id')
opal.assign.table(o,'cptp','Atlantic_R2.AtlanticPATH_PhysicalMeasuresSet2_Jan27_2017_Final',id.name = "entity_id")
ds2 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id')
opal.assign.table(o,'cptp','Atlantic_R2.AtlanticPATH_SelfReportedPhysicalMeasures_July29_2016_old',id.name = "entity_id")
ds3 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id')
opal.assign.table(o,'cptp','Atlantic_R2.AtlanticPATH_SelfReportedPhysicalMeasures_April19_2016',id.name = "entity_id")
ds4 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id')
opal.assign.table(o,'cptp','Atlantic_R2.AtlanticPATH_PhysicalMeasures_Sept19_2016',id.name = "entity_id")
ds5 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id')

dsReduce(intersect,list(ds1$entity_id,id_map$SourceID)) %>% length == length(ds1$entity_id)
Reduce(intersect,list(ds2$entity_id,id_map$SourceID)) %>% length == length(ds2$entity_id)
Reduce(intersect,list(ds3$entity_id,id_map$SourceID)) %>% length == length(ds3$entity_id)
Reduce(intersect,list(ds4$entity_id,id_map$SourceID)) %>% length == length(ds4$entity_id)
Reduce(intersect,list(ds5$entity_id,id_map$OpalID)) %>% length == length(ds5$entity_id)

grep("SourceID|OpalID|WithdrawalStatus|RemovalStatus",names(ds1),ignore.case = T,value = T)
grep('SourceID|OpalID|WithdrawalStatus|RemovalStatus',names(ds2),ignore.case = T,value = T)
grep('SourceID|OpalID|WithdrawalStatus|RemovalStatus',names(ds3),ignore.case = T,value = T)
grep('SourceID|OpalID|WithdrawalStatus|RemovalStatus',names(ds4),ignore.case = T,value = T)
grep('SourceID|OpalID|WithdrawalStatus|RemovalStatus',names(ds5),ignore.case = T,value = T)

ds1 <- subset(ds1,select = which(!names(ds1) %in% c("SourceID","OpalID","WithdrawalStatus","RemovalStatus")))
ds2 <- subset(ds2,select = which(!names(ds2) %in% c("SourceID","OpalID","WithdrawalStatus","RemovalStatus")))
ds3 <- subset(ds3,select = which(!names(ds3) %in% c("SourceID","OpalID","WithdrawalStatus","RemovalStatus")))
ds4 <- subset(ds4,select = which(!names(ds4) %in% c("SourceID","OpalID","WithdrawalStatus","RemovalStatus")))
ds5 <- subset(ds5,select = which(!names(ds5) %in% c("SourceID","OpalID","WithdrawalStatus","RemovalStatus")))

names(ds1)[1] <- "SourceID" ; ##setkey(ds1,"SourceID")
names(ds2)[1] <- "SourceID" ;#setkey(ds2,"SourceID")
names(ds3)[1] <- "SourceID" ;#setkey(ds3,"SourceID")
names(ds4)[1] <- "SourceID" ;#setkey(ds4,"SourceID")
names(ds5)[1] <- "OpalID" ;#setkey(ds5,"OpalID")

id_map <- merge(id_map,lid,by='SourceID',all.x=T)

ds1_t = merge(ds1,id_map,by="SourceID",all.x=T)
ds1_t = ds1_t[,c('SourceID',names(ds1_t)),with=F]; 
sapply(ds1_t[,list(SourceID,OpalID)],anyNA); sapply(ds1_t[,list(SourceID,OpalID)],anyDuplicated); 


#sapply(ds2[,list(SourceID,OpalID)],function(x){return (ifelse(NA %in% x || "" %in% x || grepl("[NANULL]",x,ignore.case = T),TRUE,FALSE))})

ds2_t = merge(ds2,id_map,by="SourceID",all.x=T)
ds2_t = ds2_t[,c('SourceID',names(ds2_t)),with=F]; 
sapply(ds2_t[,list(SourceID,OpalID)],anyNA); sapply(ds2_t[,list(SourceID,OpalID)],anyDuplicated); 

ds3_t = merge(ds3,id_map,by="SourceID",all.x=T)
ds3_t = ds3_t[,c('SourceID',names(ds3_t)),with=F]; 
sapply(ds3_t[,list(SourceID,OpalID)],anyNA); sapply(ds3_t[,list(SourceID,OpalID)],anyDuplicated); 

ds4_t = merge(ds4,id_map,by="SourceID",all.x=T)
ds4_t = ds4_t[,c('SourceID',names(ds4_t)),with=F]; 
sapply(ds4_t[,list(SourceID,OpalID)],anyNA); sapply(ds4_t[,list(SourceID,OpalID)],anyDuplicated); 

ds5_t = merge(ds5,id_map,by="OpalID",all.x=T)
ds5_t = ds5_t[,c('SourceID',names(ds5_t)),with=F]; 
sapply(ds5_t[,list(SourceID,OpalID)],anyNA); sapply(ds5_t[,list(SourceID,OpalID)],anyDuplicated); 

ds1_t[,Exclusion := ifelse(SourceID %in% id_exclu,'1','0')]
ds2_t[,Exclusion := ifelse(SourceID %in% id_exclu,'1','0')]
ds3_t[,Exclusion := ifelse(SourceID %in% id_exclu,'1','0')]
ds4_t[,Exclusion := ifelse(SourceID %in% id_exclu,'1','0')]
ds5_t[,Exclusion := ifelse(SourceID %in% id_exclu,'1','0')]

ds1_t %>% write.csv(., '~/data/reorg/data/atl/AtlanticPath_PhysicalMeasuresSet1_April18_2016.csv',row.names = FALSE)
ds2_t %>% write.csv(., '~/data/reorg/data/atl/AtlanticPATH_PhysicalMeasuresSet2_Jan27_2017_Final.csv',row.names = FALSE)
ds3_t %>% write.csv(., '~/data/reorg/data/atl/AtlanticPATH_SelfReportedPhysicalMeasures_July29_2016.csv',row.names = FALSE)
ds4_t %>% write.csv(., '~/data/reorg/data/atl/AtlanticPATH_SelfReportedPhysicalMeasures_April19_2016.csv',row.names = FALSE)
ds5_t %>% write.csv(., '~/data/reorg/data/atl/AtlanticPATH_PhysicalMeasures_Sept19_2016.csv',row.names = FALSE)




ds1_t = merge(id_map,ds1,by="SourceID",all.y=T)
ds1_t = merge(ds1_t,ds5[,list(SourceID,AppoinmentTime)],by="SourceID",all.x=T)
ds2_t = merge(id_map[,list(SourceID,OpalID)],ds2[,list(SourceID,Exclusion)],by="SourceID",all.y=T)
ds3_t = merge(id_map[,list(SourceID,OpalID)],ds3,by="OpalID",all.y=T,all.x=F)
setcolorder(ds3_t, c("SourceID", "OpalID","Exclusion",names(ds3_t)[-c(1,2,14)]))
ds4_t = merge(id_map[,list(SourceID,OpalID)],ds4[,list(SourceID,Exclusion)],by="SourceID",all.y=T)


#Withdrawal status design 
#No recontact: Data and biosamples collected can be used. The participant does not wish to be recontacted.
#No linkage: Data and biosamples collected can be used. Data linkage is not allowed.
#No use: Data and biosamples collected can no more be used.
#N/A (no status): Data and biosamples collected can be used.

#Removal status design
#0: Participant is not excluded from CPTP Harminized dataset
#1: Participant is excluded from CPTP Harminized dataset

#atl withdrawal status mapping
'No further contact' : 'No recontact'
'Moved - New Address Unknown' : 'No recontact'
'No further contact regarding Assessment Centres' : 'No recontact'
'Deceased' : 'No recontact'
'NULL': 'N/A'

setkey(lid,'entity_id')
lid[,WithdrawalStatus := plyr::mapvalues(DO_NOT_CONTACT_STATUS,
                                         c('Deceased','Moved - New Address Unknown','No further contact',
                                           'No further contact regarding Assessment Centres','NULL'),
                                         c('No recontact','No recontact','No recontact','No recontact',NA))]

ds1_b = ds1[,.(WithdrawalStatus = lid[entity_id,WithdrawalStatus],
               RemovalStatus = ifelse(entity_id %in% id_exclu,'1','0')),entity_id]
ds2_b = ds2[,.(WithdrawalStatus = lid[entity_id,WithdrawalStatus],
               RemovalStatus = ifelse(entity_id %in% id_exclu,'1','0')),entity_id]
ds3_b = ds3[,.(WithdrawalStatus = lid[entity_id,WithdrawalStatus],
               RemovalStatus = ifelse(entity_id %in% id_exclu,'1','0')),entity_id]
ds4_b = ds4[,.(WithdrawalStatus = lid[entity_id,WithdrawalStatus],
               RemovalStatus = ifelse(entity_id %in% id_exclu,'1','0')),entity_id]

ds1 %>% write.csv(., '~/data/reorg/data/atl/AtlanticPath_PhysicalMeasuresSet1_April18_2016.csv',row.names = FALSE)
ds2 %>% write.csv(., '~/data/reorg/data/atl/AtlanticPATH_PhysicalMeasuresSet2_Jan27_2017_Final.csv',row.names = FALSE)
ds3 %>% write.csv(., '~/data/reorg/data/atl/AtlanticPATH_SelfReportedPhysicalMeasures_July29_2016.csv',row.names = FALSE)
ds4 %>% write.csv(., '~/data/reorg/data/atl/AtlanticPATH_SelfReportedPhysicalMeasures_April19_2016.csv',row.names = FALSE)

ds1_b %>% write.csv(., '~/data/reorg/data/atl/AtlanticPath_PhysicalMeasuresSet1_April18_2016_WDS_RMS.csv',row.names = FALSE,na = "")
ds2_b %>% write.csv(., '~/data/reorg/data/atl/AtlanticPATH_PhysicalMeasuresSet2_Jan27_2017_Final_WDS_RMS.csv',row.names = FALSE,na = "")
ds3_b %>% write.csv(., '~/data/reorg/data/atl/AtlanticPATH_SelfReportedPhysicalMeasures_July29_2016_WDS_RMS.csv',row.names = FALSE,na = "")
ds4_b %>% write.csv(., '~/data/reorg/data/atl/AtlanticPATH_SelfReportedPhysicalMeasures_April19_2016_WDS_RMS.csv',row.names = FALSE,na = "")


######bcgp
id_map = fread('~/data/reorg/idmap/Participant-bcgp_r2.csv',
               colClasses = 'character',col.names = c("OpalID","SourceID")) %>% data.table ;
#some ids with 'B' suffiixes ; need to remove it
id_map[grepl("BC[0-9]+B",SourceID), SourceID_b := stringr::str_extract(SourceID,"BC[0-9]+")]
id_map <- unique(id_map,by='OpalID')

id_map1 = read.csv('~/data/reorg/idmap/Participant-bcgp_onl_apr2016.csv',
                   colClasses = 'character',col.names = c("OpalID","SourceID")) %>% data.table ; 
id_map2 = read.csv('~/data/reorg/idmap/Participant-bcgp_telf_apr2016.csv',
                   colClasses = 'character',col.names = c("OpalID","SourceID")) %>% data.table 
id_map3 = read.csv('~/data/reorg/idmap/Participant-bcgp_opal_apr2016.csv',
                   colClasses = 'character',col.names = c("OpalID","SourceID")) %>% data.table ; 
id_map_full <- unique(Reduce(rbind,list(id_map1,id_map2,id_map3,id_map))[,lapply(.SD,toupper),.SDcols = 1:2]
                      ,by='OpalID')
id_map_full <- unique(Reduce(function(x,y) rbindlist(list(x,y),use.names = T,fill = T),
                             list(id_map1,id_map2,id_map3,id_map)),by='OpalID')

setkey(id_map_full,'SourceID')

id_exclu = Filter(function(x) !is.na(x),stringr::str_extract(
  readr::read_lines('~/data/reorg/exclu/BCGP_exclusionJan2017.txt'),"BC[0-9]+"))

id_exclu

id_map_full[,Exclusion:=ifelse(SourceID %in% id_exclu,'1','0')]

lid0 <- xlsx::read.xlsx('~/data/reorg/exclu/Participant and WD_List2MR.xlsx',sheetIndex = 3) %>% data.table
lid0 <- lid0[,lapply(.SD,as.character.factor),.SDcols=1:4]
lid0 <- unique(lid0[,lapply(.SD,toupper),.SDcols = 1:4],by='OTHER_BC') ; names(lid0)[2] <- 'SourceID'

id_map_full<-merge(id_map_full,lid0,by='SourceID',all.x=T,suffixes = c('','_Switch'))

id_map_full<-merge(id_map_full,lid1,by='SourceID',all.x=T,suffixes = c('','_WDList'))

id_map_full<-merge(id_map_full,lid2,by='SourceID',all.x=T,suffixes = c('','_NoConsent'))

o <- opal.login('yamadou','Na96wal2','https://cptp-harmo.oicr.on.ca:8443/')

opal.assign.table(o,'cptp','BCGP_R2.PM_N16618_old',id.name = "entity_id")
ds1 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id') ;
opal.assign.table(o,'cptp','BCGP_R2.CIPQ_N4889_old',id.name = "entity_id")
ds2 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id') ;
opal.assign.table(o,'cptp','BCGP_R2.SDQ_N25867_old',id.name = "entity_id")
ds3 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id') ;
opal.assign.table(o,'cptp','BCGP_R2.PM_ACCESS_Dates4MR_old',id.name = "entity_id")
ds4 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id');

names(ds1)[1] <- "OpalID"
names(ds2)[1] <- "OpalID"
names(ds3)[1] <- "OpalID"
names(ds4)[1] <- "OpalID"

##ids still not corrected
Filter(function(x) !x %in% id_map_full$SourceID,lid0$PT_BC)
lid0[PT_BC %in% Filter(function(x) !x %in% id_map_full$SourceID,lid0$PT_BC),SourceID] %in% id_map_full$SourceID
id_map_full[, SourceID := plyr::mapvalues(SourceID,
                                          c("BC204988", "BC205232", "BC013987", "BC987753"),
                                          c("BC977836", "BC999000", "BC103987", "BC987575"))]
##non correct ids in the list
Filter(function(x) x %in% id_map_full$SourceID,lid0$SourceID)
lid0[SourceID %in% Filter(function(x) x %in% id_map_full$SourceID,lid0$SourceID),PT_BC] %in% id_map_full$SourceID
#all their correction in the llist, so no correction required

##non correct ids not in the list
Filter(function(x) !x %in% id_map_full$SourceID,lid0$SourceID)
lid0[SourceID %in% Filter(function(x) !x %in% id_map_full$SourceID,lid0$SourceID),PT_BC] %in% id_map_full$SourceID
#all their correction in th list, so no correction required

Reduce(intersect,list(ds1$OpalID,id_map_full$OpalID)) %>% length == length(ds1$OpalID)
Reduce(intersect,list(ds2$OpalID,id_map_full$OpalID)) %>% length == length(ds2$OpalID)
Reduce(intersect,list(ds3$OpalID,id_map_full$OpalID)) %>% length == length(ds3$OpalID)
Reduce(intersect,list(ds4$OpalID,id_map_full$OpalID)) %>% length == length(ds4$OpalID)

ds1_t = merge(ds1,id_map_full,by="OpalID",all.x=T)
ds1_t = ds1_t[,c('SourceID',names(ds1_t)),with=F]; 
sapply(ds1_t[,list(SourceID,OpalID)],anyNA); sapply(ds1_t[,list(SourceID,OpalID)],anyDuplicated); 
ds2_t = merge(ds2,id_map_full,by="OpalID",all.x=T)
ds2_t = ds2_t[,c('SourceID',names(ds2_t)),with=F]; 
sapply(ds2_t[,list(SourceID,OpalID)],anyNA); sapply(ds2_t[,list(SourceID,OpalID)],anyDuplicated); 
ds3_t = merge(ds3,id_map_full,by="OpalID",all.x=T)
ds3_t = ds3_t[,c('SourceID',names(ds3_t)),with=F]; 
sapply(ds3_t[,list(SourceID,OpalID)],anyNA); sapply(ds3_t[,list(SourceID,OpalID)],anyDuplicated); 
ds4_t = merge(ds4,id_map_full,by="OpalID",all.x=T)
ds4_t = ds4_t[,c('SourceID',names(ds4_t)),with=F]; 
sapply(ds4_t[,list(SourceID,OpalID)],anyNA); sapply(ds4_t[,list(SourceID,OpalID)],anyDuplicated); 

ds1_t %>% write.csv(., '~/data/reorg/data/bcgp/PM_N16618.csv',row.names = FALSE,na = "")
ds2_t %>% write.csv(., '~/data/reorg/data/bcgp/CIPQ_N4889.csv',row.names = FALSE,na = "")
ds3_t %>% write.csv(., '~/data/reorg/data/bcgp/SDQ_N25867.csv',row.names = FALSE,na = "")
ds4_t %>% write.csv(., '~/data/reorg/data/bcgp/PM_ACCESS_Dates4MR.csv',row.names = FALSE,na = "")

#bcgp withdrawal status mapping
'No further contact' : 'No recontact'
'No further access' : 'No linkage'
'No further use' : 'No use'

lid1 <- xlsx::read.xlsx('~/data/reorg/exclu/Participant and WD_List2MR.xlsx',sheetIndex = 2,startRow = 8,
                        colClasses = 'character') %>% data.table ; names(lid1)[1] <- 'entity_id'
lid1 <- lid1[,lapply(.SD,as.character.factor),.SDcols=1:3,key='entity_id']

Filter(function(x) x %in% lid1$entity_id,lid0$SourceID) #ids are ok

lid1[,WithdrawalLevel := plyr::mapvalues(WDLevel,
                                         c('1','2','3','1-death','1-deceased','1-moved'),
                                         c('1','2','3','1','1','1'))]
lid1[,WithdrawalStatus := plyr::mapvalues(WithdrawalLevel,
                                          c('1','2','3'),
                                          c('No recontact','No linkage','No use'))]
names(lid1)[1]<- 'SourceID'

id_map_full <- merge(id_map_full,lid1[,list(SourceID,WDLevel,WDDate)],all=T)

id_map_full <- merge(id_map_full,lid2,all=T,suffixes = '_NO_CONSENT')


lid2 <- xlsx::read.xlsx('~/data/reorg/exclu/Participant and WD_List2MR.xlsx',sheetIndex = 1) %>% data.table
lid2 <- lid2[,lapply(.SD,as.character.factor),list(Type,index),.SDcols=2][,key='OTHER_BC'];
names(lid2)[3] <- 'SourceID_NO_CONSENT'
lid2 <- unique(lid2,by='entity_id')

id_map_full <- merge(id_map_full,lid2[,list(Type,index,SourceID)],all=T,suffixes = c('','_nocnsnt'))
id_map_full$SourceID <- sapply(id_map_full$SourceID,toupper) %>% array


t <- id_map_full[!is.na(PT_BC) | !is.na(Type_nocnsnt),] %>% 
  write.csv(., 't.csv',row.names = FALSE,na = "")

sapply(id_map_full,anyNA); 
sapply(id_map_full,anyDuplicated); 


id_map_full[duplicated(id_map_full$SourceID),][,by='SourceID']


Filter(function(x) x %in% lid2$entity_id,lid0$SourceID) #1 id is not ok
lid0[SourceID %in% Filter(function(x) x %in% lid2$entity_id,lid0$SourceID),] #correction not necessary

ds1_b = ds1_t[,.(WithdrawalStatus = lid1[entity_id,WithdrawalStatus],
                 RemovalStatus = ifelse(entity_id %in% id_exclu,'1','0')),entity_id]
ds1_b[entity_id %in% lid2$entity_id,WithdrawalStatus := 'No use']
ds2_b = ds2_t[,.(WithdrawalStatus = lid1[entity_id,WithdrawalStatus],
                 RemovalStatus = ifelse(entity_id %in% id_exclu,'1','0')),entity_id]
ds2_b[entity_id %in% lid2$entity_id,WithdrawalStatus := 'No use']
ds3_b = ds3_t[,.(WithdrawalStatus = lid1[entity_id,WithdrawalStatus],
                 RemovalStatus = ifelse(entity_id %in% id_exclu,'1','0')),entity_id]
ds3_b[entity_id %in% lid2$entity_id,WithdrawalStatus := 'No use']
ds4_b = ds4_t[,.(WithdrawalStatus = lid1[entity_id,WithdrawalStatus],
                 RemovalStatus = ifelse(entity_id %in% id_exclu,'1','0')),entity_id]
ds4_b[entity_id %in% lid2$entity_id,WithdrawalStatus := 'No use']

ds1_b %>% write.csv(., '~/data/reorg/data/bcgp/PM_N16618_WDS_RMS.csv',row.names = FALSE,na = "")
ds2_b %>% write.csv(., '~/data/reorg/data/bcgp/CIPQ_N4889_WDS_RMS.csv',row.names = FALSE,na = "")
ds3_b %>% write.csv(., '~/data/reorg/data/bcgp/SDQ_N25867_WDS_RMS.csv',row.names = FALSE,na = "")
ds4_b %>% write.csv(., '~/data/reorg/data/bcgp/PM_ACCESS_Dates4MR_WDS_RMS.csv',row.names = FALSE,na = "")





######cag

ds1 <- '/home/yamadou/export/CAG_R2-20170224101635/cag_final_phaseA/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.,colClasses = 'character') %>% data.table(.,key='entity_id')

ds2 <- '/home/yamadou/export/CAG_R2-20170224101648/cag_final_phaseA_blood_runtime/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.,colClasses = 'character') %>% data.table(.,key='entity_id')

names(ds1)[1] <- "OpalID"
names(ds2)[1] <- "OpalID"

id_map = fread('~/data/reorg/idmap/Participant-cag.csv',
               colClasses = 'character',col.names = c("OpalID","SourceID")) %>% data.table(.,key='OpalID') ;
sapply(id_map,anyNA); sapply(id_map,anyDuplicated); 

lid <- fread("~/data/reorg/exclu/cptp_recontact_472430.csv",colClasses = 'character')

id_map[,Exclusion:=0]

id_map <- merge(id_map,lid,by='SourceID',all.x=T)

#cag withdrawal status mapping
'sollicitation' : 'No linkage'
'recontact' : 'No recontact'
'deceased' : 'N/A'

lid[,WithdrawalStatus := plyr::mapvalues(sollicitation,c('0','1'),c(NA,'No linkage'))]
lid[,WithdrawalStatus := plyr::mapvalues(recontact,c('0','1'),c(NA,'No recontact'))]
setkey(lid,'entity_id')

Reduce(intersect,list(ds1$entity_id,id_map$OpalID)) %>% length == length(ds1$entity_id)
Reduce(intersect,list(ds2$entity_id,id_map$OpalID)) %>% length == length(ds2$entity_id)

ds1_t = merge(ds1,id_map,by="OpalID",all.x=T)
ds1_t = ds1_t[,c('SourceID',names(ds1_t)),with=F];
sapply(ds1_t[,list(SourceID,OpalID)],anyNA); sapply(ds1_t[,list(SourceID,OpalID)],anyDuplicated); 
ds2_t = merge(ds2,id_map,by="OpalID",all.x=T)
ds2_t = ds2_t[,c('SourceID',names(ds2_t)),with=F]; 
sapply(ds2_t[,list(SourceID,OpalID)],anyNA); sapply(ds2_t[,list(SourceID,OpalID)],anyDuplicated); 

names(ds1_t)[1] <- "entity_id"
names(ds2_t)[1] <- "entity_id"



ds1_t %>% write.csv(., '~/data/reorg/data/cag/cag_final_phaseA.csv',row.names = FALSE,na = "")
ds2_t %>% write.csv(., '~/data/reorg/data/cag/cag_final_phaseA_blood_runtime.csv',row.names = FALSE,na = "")


ds1_b = ds1_t[,.(WithdrawalStatus = lid[entity_id,WithdrawalStatus],
                 RemovalStatus = '0'),entity_id]
ds2_b = ds2_t[,.(WithdrawalStatus = lid[entity_id,WithdrawalStatus],
                 RemovalStatus = '0'),entity_id]

ds1_b %>% write.csv(., '~/data/reorg/data/cag/cag_final_phaseA_WDS_RMS.csv',row.names = FALSE,na = "")
ds2_b %>% write.csv(., '~/data/reorg/data/cag/cag_final_phaseA_blood_runtime_WDS_RMS.csv',row.names = FALSE,na = "")


######ohs

id_map = fread('~/data/reorg/idmap/Participant-ohs_r2.csv',
               colClasses = 'character',col.names = c("OpalID","SourceID")) %>% data.table(.,key='SourceID') ;
sapply(id_map,anyNA); sapply(id_map,anyDuplicated); 

id_exclu1 = Filter(function(x) !is.na(x),stringr::str_extract(
  readr::read_lines('~/data/reorg/exclu/OHS1_exclusionJan2017.txt'),"[0-9]+"))

id_exclu2 = Filter(function(x) !is.na(x),stringr::str_extract(
  readr::read_lines('~/data/reorg/exclu/OHS2_exclusionJan2017.txt'),"[0-9]+"))

lid1 <- fread("~/data/reorg/exclu/OHSDuplicates_Identified  Switch Reqd.csv",
             colClasses = 'character') %>% data.table 
names(lid1)[2] <- 'SourceID' ; #names(lid1)[5] <- 'Correction' ; lid1 = lid1[,list(SourceID,Correction)]
setkey(lid1,'SourceID')

id_map <- merge(id_map,lid1,by='SourceID',all.x=T)
id_map <- merge(id_map,lid2,by='SourceID',all.x=T,suffixes = c('','_NoSwitch'))
id_map[,ExclusionOHS1 :=ifelse(SourceID %in% id_exclu1,'1','0')]
id_map[,ExclusionOHS2 :=ifelse(SourceID %in% id_exclu2,'1','0')]


id_map[!is.na(Correction),SourceID:=Correction]

id_map[!is.na(Correction),]

lid2 <- fread("~/data/reorg/exclu/OHSDuplicates_Identified No Switch.csv",
              colClasses = 'character') %>% data.table 
names(lid2)[1] <- 'SourceID' ; setkey(lid2,'SourceID')


#AC

opal.assign.table(o,'cptp','OHS_R2.OHS_AC_PhysicalMeasures',id.name = "entity_id")
ds1 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id')
opal.assign.table(o,'cptp','OHS_R2.OHS_AC_PhysicalMeasures_160902_old',id.name = "entity_id")
ds2 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id')
opal.assign.table(o,'cptp','OHS_R2.CPTP_Biosample_AC_Participants_151222_old',id.name = "entity_id")
ds3 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id')

names(ds1)[1] <- "OpalID"
names(ds2)[1] <- "OpalID"
names(ds3)[1] <- "OpalID"

Reduce(intersect,list(ds1$OpalID,id_map$OpalID)) %>% length == length(ds1$OpalID)
Reduce(intersect,list(ds2$OpalID,id_map$OpalID)) %>% length == length(ds2$OpalID)
Reduce(intersect,list(ds3$OpalID,id_map$OpalID)) %>% length == length(ds3$OpalID)

l = id_map[!is.na(Correction),OpalID]





ds1 = merge(ds1,ds2,by="OpalID")

ds1_t = merge(ds1,id_map,by="OpalID",all.x=T)
ds1_t = ds1_t[,c('SourceID',names(ds1_t)),with=F]; 
sapply(ds1_t[,list(SourceID,OpalID)],anyNA); sapply(ds1_t[,list(SourceID,OpalID)],anyDuplicated); 
ds2_t = merge(ds2,id_map,by="OpalID",all.x=T)
ds2_t = ds2_t[,c('SourceID',names(ds2_t)),with=F]; 
sapply(ds2_t[,list(SourceID,OpalID)],anyNA); sapply(ds2_t[,list(SourceID,OpalID)],anyDuplicated); 
ds3_t = merge(ds3,id_map,by="OpalID",all.x=T)
ds3_t = ds3_t[,c('SourceID',names(ds3_t)),with=F]; 
sapply(ds3_t[,list(SourceID,OpalID)],anyNA); sapply(ds3_t[,list(SourceID,OpalID)],anyDuplicated); 



ds1_t %>% write.csv(., '~/data/reorg/data/ohs/ac/OHS_AC_PhysicalMeasures.csv',row.names = FALSE,na = "")
ds2_t %>% write.csv(., '~/data/reorg/data/ohs/ac/OHS_AC_PhysicalMeasures_160902.csv',row.names = FALSE,na = "")
ds2_t %>% write.csv(., '~/data/reorg/data/ohs/ac/CPTP_Biosample_AC_Participants_151222.csv',row.names = FALSE,na = "")

rm(list = ls(pattern = "ds[0-9a-z_]"))

#LSC
opal.assign.table(o,'cptp','OHS_R2.OHS_LSC_PhysicalMeasures_160902_old',id.name = "entity_id")
ds1 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id')
opal.assign.table(o,'cptp','OHS_R2.CPTP_Biosample_LSC_Participants_151222_old',id.name = "entity_id")
ds2 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id')

names(ds1)[1] <- "OpalID"
names(ds2)[1] <- "OpalID"

Reduce(intersect,list(ds1$OpalID,id_map$OpalID)) %>% length == length(ds1$OpalID)
Reduce(intersect,list(ds2$OpalID,id_map$OpalID)) %>% length == length(ds2$OpalID)

ds1_t = merge(ds1,id_map,by="OpalID")
ds1_t = ds1_t[,c('SourceID',names(ds1_t)),with=F]; 
sapply(ds1_t[,list(SourceID,OpalID)],anyNA); sapply(ds1_t[,list(SourceID,OpalID)],anyDuplicated); 
ds2_t = merge(ds2,id_map,by="OpalID")
ds2_t = ds2_t[,c('SourceID',names(ds2_t)),with=F]; 
sapply(ds2_t[,list(SourceID,OpalID)],anyNA); sapply(ds2_t[,list(SourceID,OpalID)],anyDuplicated); 

ds1_t %>% write.csv(., '~/data/reorg/data/ohs/lsc/OHS_LSC_PhysicalMeasures_160902.csv',row.names = FALSE,na = "")
ds2_t %>% write.csv(., '~/data/reorg/data/ohs/lsc/CPTP_Biosample_LSC_Participants_151222.csv',row.names = FALSE,na = "")

rm(list = ls(pattern = "ds[0-9a-z_]"))


#pilot
opal.assign.table(o,'cptp','OHS_R2.pilot_physicalmeasures_old',id.name = "entity_id")
ds1 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id')

opal.assign.table(o,'cptp','OHS_R2.pilot_physicalmeasures_20160920_old',id.name = "entity_id")
ds2 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id')


names(ds1)[1] <- "OpalID"
names(ds2)[1] <- "OpalID"

Reduce(intersect,list(ds1$OpalID,id_map$OpalID)) %>% length == length(ds1$OpalID)
Reduce(intersect,list(ds2$OpalID,id_map$OpalID)) %>% length == length(ds2$OpalID)

ds1 = merge(ds1,ds2,by='OpalID')
ds1_t = merge(ds1,id_map,by="OpalID")
ds1_t = ds1_t[,c('SourceID',names(ds1_t)),with=F]; 
sapply(ds1_t[,list(SourceID,OpalID)],anyNA); sapply(ds1_t[,list(SourceID,OpalID)],anyDuplicated); 

ds1_t %>% write.csv(., '~/data/reorg/data/ohs/pilot/pilot_physicalmeasures.csv',row.names = FALSE,na = "")
ds2_t %>% write.csv(., '~/data/reorg/data/ohs/pilot/pilot_physicalmeasures_20160920.csv',row.names = FALSE,na = "")

rm(list = ls(pattern = "ds[0-9a-z_]"))

#BC
opal.assign.table(o,'cptp','OHS_R2.CPTP_Biosample_BC_Participants_151222_old',id.name = "entity_id")
ds1 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id')
opal.assign.table(o,'cptp','OHS_R2.CPTP_Biosample_BC_Participants_151222_BloodCollection',id.name = "entity_id")
ds2 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id')

names(ds1)[1] <- "OpalID"
names(ds2)[1] <- "OpalID"

Reduce(intersect,list(ds1$OpalID,id_map$OpalID)) %>% length == length(ds1$OpalID)
Reduce(intersect,list(ds2$OpalID,id_map$OpalID)) %>% length == length(ds2$OpalID)

ds1 = merge(ds1,ds2,by='OpalID')
ds1_t = merge(ds1,id_map,by="OpalID")
ds1_t = ds1_t[,c('SourceID',names(ds1_t)),with=F]; 
sapply(ds1_t[,list(SourceID,OpalID)],anyNA); sapply(ds1_t[,list(SourceID,OpalID)],anyDuplicated); 

ds1_t %>% write.csv(., '~/data/reorg/data/ohs/bc/CPTP_Biosample_BC_Participants_151222.csv',row.names = FALSE,na = "")
ds2_t %>% write.csv(., '~/data/reorg/data/ohs/bc/CPTP_Biosample_BC_Participants_151222_BloodCollection.csv',row.names = FALSE,na = "")


rm(list = ls(pattern = "ds[0-9a-z_]"))


lid <- fread("~/data/reorg/exclu/cptp_recontact_472430.csv",colClasses = 'character')


#####ATP
id_map = fread('~/data/reorg/idmap/Participant-atp_r2.csv',
               colClasses = 'character',col.names = c("OpalID","SourceID")) %>% data.table(.,key='SourceID');
sapply(id_map,anyNA); sapply(id_map,anyDuplicated);

atp_id_exclu = Filter(function(x) !is.na(x),stringr::str_extract(
  readr::read_lines('~/data/reorg/exclu/ATP_exclusionJan2017.txt'),"[0-9]+"))


#paper
opal.assign.table(o,'cptp','TTP_R2.PMRB_CPAC_old',id.name = "entity_id")
ds1 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id')
opal.assign.table(o,'cptp','TTP_R2.CPAC_SCQ_old',id.name = "entity_id")
ds2 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id')

names(ds1)[1] <- "OpalID"
names(ds2)[1] <- "OpalID"

Reduce(intersect,list(ds1$OpalID,id_map$OpalID)) %>% length == length(ds1$OpalID)
Reduce(intersect,list(ds2$OpalID,id_map$OpalID)) %>% length == length(ds2$OpalID)

#remove Exclusion et Duplicata from lid0

##id fix

lid0 <- xlsx::read.xlsx('~/data/reorg/exclu/ATP_ID_fix.xlsx',sheetIndex = 1) %>% data.table(.,key='ID')
lid0 <- lid0[,lapply(.SD,as.character),.SDcols=1:4]
lid0 <- unique(lid0,by='ID') ; names(lid0)[1]<-'SourceID'


id_map[,Exclusion:=ifelse(SourceID %in% id_exclu,'1','0')]
id_map <- merge(id_map,lid0,by='SourceID',all.x =T)
id_map <- merge(id_map,lid1,by='SourceID',all.x =T)



#test ids not corrected
Filter(function(x) !x %in% id_map$SourceID,lid0$ID) #all are not corrected
id_map[lid0[Filter(function(x) x %in% id_map$SourceID,lid0$ID),Correction],] #except first,
#correction already exist

#ids to correct
#"14504812" "14519140" "14529226" "14559916" "14577828" "14587431" "14625498" "14627446" "14668382" "14684509"
#"14687370"

id_map = merge(id_map,lid0[Filter(function(x) x %in% id_map$SourceID,lid0$SourceID),list(SourceID,Correction)],by='SourceID',all.x=T)

id_map[lid0$SourceID,]


#duplicata
Reduce(rbind,lapply(na.exclude(id_map$Correction) %>% c,
       function(x) id_map[SourceID == x| Correction == x,])) #only 1st fix is not duplicated
dup = id_map[SourceID %in% na.exclude(id_map$Correction) %>% c,SourceID]

#it is not a problem that same source id has multiple opal id

id_map <- id_map[!is.na(Correction),SourceID := Correction]
setkey(id_map,'SourceID')

sapply(id_map,anyNA); sapply(id_map,anyDuplicated);

l = id_map[!is.na(Correction),OpalID]
#do we have opalids of duplicated sourceids in datasets?

l %in% ds1$OpalID
ds1_t = merge(ds1,id_map,by="OpalID",all.x=T)
ds1_t = ds1_t[,c('SourceID',names(ds1_t)),with=F]; 
sapply(ds1_t[,list(SourceID,OpalID)],anyNA); sapply(ds1_t[,list(SourceID,OpalID)],anyDuplicated); 

l %in% ds2$OpalID
ds2_t = merge(ds2,id_map,by="OpalID",all.x=T)
ds2_t = ds2_t[,c('SourceID',names(ds2_t)),with=F]; 
sapply(ds2_t[,list(SourceID,OpalID)],anyNA); sapply(ds2_t[,list(SourceID,OpalID)],anyDuplicated); 

ds1_t %>% write.csv(., '~/data/reorg/data/atp/paper/PMRB_CPAC.csv',row.names = FALSE,na = "")
ds2_t %>% write.csv(., '~/data/reorg/data/atp/paper/CPAC_SCQ.csv',row.names = FALSE,na = "")


names(ds1_t)[1] <- 'CPAC_KEY';setkey(ds1_t,'CPAC_KEY')
names(ds2_t)[1] <- 'CPAC_KEY';setkey(ds1_t,'CPAC_KEY')
ds1_b = merge(ds1_t,lid1,by='CPAC_KEY',all.x=T)[,Exclusion:=ifelse(CPAC_KEY %in% id_exclu,'1','0')]
ds2_b = merge(ds2_t,lid1,by='CPAC_KEY',all.x=T)[,Exclusion:=ifelse(CPAC_KEY %in% id_exclu,'1','0')]

ds1_b %>% write.csv(., '~/data/reorg/data/atp/paper/PMRB_CPAC.csv',row.names = FALSE,na = "")
ds2_b %>% write.csv(., '~/data/reorg/data/atp/paper/CPAC_SCQ.csv',row.names = FALSE,na = "")

lid1 = fread('~/data/reorg/exclu/ATP_Number_Respondant.csv',
             colClasses = 'character') %>% data.table(.,key='CPAC_KEY');
sapply(lid1,anyNA); sapply(lid1,anyDuplicated);

Filter(function(x) x %in% lid0$SourceID,lid1$CPAC_KEY) #all are OK

Filter(function(x) x %in% lid0$SourceID,id_exclu) #all exclusion need correction
id_exclu = plyr::mapvalues(id_exclu,lid0$SourceID,lid0$Correction)

ds1_b = merge(ds1_t,lid1,by='CPAC_KEY',all.x=T)[,Exclusion:=ifelse(SourceID %in% id_exclu,'1','0')]
ds2_b = merge(ds2_t,lid1,by='CPAC_KEY',all.x=T)[,Exclusion:=ifelse(SourceID %in% id_exclu,'1','0')]

rm(list = ls(pattern = "ds[0-9a-z_]"))


#onyx
ds1 <- '/home/yamadou/export/TTP_R2-20170306095034/ATP_CI/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.,colClasses = 'character') %>% data.table(.,key='entity_id')
ds2 <- '/home/yamadou/export/TTP_R2-20170306095034/ATP_Measurements/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.,colClasses = 'character') %>% data.table(.,key='entity_id')
ds3 <- '/home/yamadou/export/TTP_R2-20170306095034/ATP_onyx_blood_runtime/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.,colClasses = 'character') %>% data.table(.,key='entity_id')
ds4 <- '/home/yamadou/export/TTP_R2-20170306095034/ATP_onyx_instrument_runtime/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.,colClasses = 'character') %>% data.table(.,key='entity_id')

names(ds1)[1] <- "OpalID";names(ds2)[1] <- "OpalID";names(ds3)[1] <- "OpalID";names(ds4)[1] <- "OpalID"

Reduce(intersect,list(ds1$OpalID,id_map$OpalID)) %>% length == length(ds1$OpalID)
Reduce(intersect,list(ds2$OpalID,id_map$OpalID)) %>% length == length(ds2$OpalID)
Reduce(intersect,list(ds3$OpalID,id_map$OpalID)) %>% length == length(ds3$OpalID)
Reduce(intersect,list(ds4$OpalID,id_map$OpalID)) %>% length == length(ds4$OpalID)


l = id_map[!is.na(Correction),OpalID]

l %in% ds1$OpalID
ds1_t = merge(ds1,id_map[,list(SourceID,OpalID)],by="OpalID")
ds1_t = ds1_t[,c('SourceID',names(ds1_t)),with=F]; 
sapply(ds1_t[,list(SourceID,OpalID)],anyNA); sapply(ds1_t[,list(SourceID,OpalID)],anyDuplicated); 

l %in% ds2$OpalID
ds2_t = merge(ds2,id_map[,list(SourceID,OpalID)],by="OpalID")
ds2_t = ds2_t[,c('SourceID',names(ds2_t)),with=F]; 
sapply(ds2_t[,list(SourceID,OpalID)],anyNA); sapply(ds2_t[,list(SourceID,OpalID)],anyDuplicated); 

l %in% ds4$OpalID
ds4_t = merge(ds4,id_map,by="OpalID",all.x=T)
ds4_t = ds4_t[,c('SourceID',names(ds4_t)),with=F]; 
sapply(ds4_t[,list(SourceID,OpalID)],anyNA); sapply(ds4_t[,list(SourceID,OpalID)],anyDuplicated); 

l %in% ds3$OpalID

#SourceIDs '14519835', '14625223' are duplicated ; we keep the latest information
ds3_t = merge(ds3,id_map,by="OpalID",all.x=T)
ds3_t = ds3_t[,c('SourceID',names(ds3_t)),with=F]; 
sapply(ds3_t[,list(SourceID,OpalID)],anyNA); sapply(ds3_t[,list(SourceID,OpalID)],anyDuplicated); 

ds3_t[SourceID %in% c('14519835', '14625223'),]
#we keep the latest information to date, and remove OpalIDs c('2966081886','9057034457')
ds3_t = subset(ds3_t,subset = !OpalID %in% c('2966081886','9057034457'))
sapply(ds3_t[,list(SourceID,OpalID)],anyNA); sapply(ds3_t[,list(SourceID,OpalID)],anyDuplicated); 


ds1_t %>% write.csv(., '~/data/reorg/data/atp/onyx/ATP_CI.csv',row.names = FALSE,na = "")
ds2_t %>% write.csv(., '~/data/reorg/data/atp/onyx/ATP_Measurements.csv',row.names = FALSE,na = "")
ds3_t %>% write.csv(., '~/data/reorg/data/atp/onyx/ATP_onyx_blood_runtime.csv',row.names = FALSE,na = "")
ds4_t %>% write.csv(., '~/data/reorg/data/atp/onyx/ATP_onyx_instrument_runtime.csv',row.names = FALSE,na = "")


Filter(function(x) x %in% lid0$SourceID,id_exclu) #all exclusion need correction
id_exclu = plyr::mapvalues(id_exclu,lid0$SourceID,lid0$Correction)

Filter(function(x) x %in% lid0$SourceID,lid1$CPAC_KEY) #all are OK

names(ds1_t)[1] <- 'CPAC_KEY';setkey(ds1_t,'CPAC_KEY')
names(ds2_t)[1] <- 'CPAC_KEY';setkey(ds1_t,'CPAC_KEY')
names(ds3_t)[1] <- 'CPAC_KEY';setkey(ds1_t,'CPAC_KEY')
names(ds4_t)[1] <- 'CPAC_KEY';setkey(ds1_t,'CPAC_KEY')

ds1_b = merge(ds1_t,lid1,by='CPAC_KEY',all.x=T)[,Exclusion:=ifelse(CPAC_KEY %in% id_exclu,'1','0')]
ds2_b = merge(ds2_t,lid1,by='CPAC_KEY',all.x=T)[,Exclusion:=ifelse(CPAC_KEY %in% id_exclu,'1','0')]
ds3_b = merge(ds3_t,lid1,by='CPAC_KEY',all.x=T)[,Exclusion:=ifelse(CPAC_KEY %in% id_exclu,'1','0')]
ds4_b = merge(ds4_t,lid1,by='CPAC_KEY',all.x=T)[,Exclusion:=ifelse(CPAC_KEY %in% id_exclu,'1','0')]

ds1_b %>% write.csv(., '~/data/reorg/data/atp/onyx/ATP_CI.csv',row.names = FALSE,na = "")
ds2_b %>% write.csv(., '~/data/reorg/data/atp/onyx/ATP_Measurements.csv',row.names = FALSE,na = "")
ds3_b %>% write.csv(., '~/data/reorg/data/atp/onyx/ATP_onyx_blood_runtime.csv',row.names = FALSE,na = "")
ds4_b %>% write.csv(., '~/data/reorg/data/atp/onyx/ATP_onyx_instrument_runtime.csv',row.names = FALSE,na = "")

rm(list = ls(pattern = "ds[0-9a-z_]"))