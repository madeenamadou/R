# R Script
#
# Par Youssef de Madeen Amadou
#
# Retracer la presence des participants dans les differents
# dans les differentes tables de donnees de chaque cohorte. 
#



source('~/scripts/dev/startup.R')
source('~/scripts/dev/push_opal.R')
source('~/scripts/dev/pull_opal.R')


## ATL
atl_pm <-do.call(fl_opal,list('/home/yamadou/export/Views/ATL/ATL_Measurements.csv',c('SourceID','OpalID')))
atl_bs <-do.call(fl_opal,list('/home/yamadou/export/Views/ATL/ATL_Sample.csv',c('SAMPLE_ID','ADM_PART_ID')))
atl_core1 <-do.call(tbl_opal,list('Atlantic-online.ATL_PATH_nov2015_July2016_MERGED',c('SourceID','OpalID')))
atl_core3 <-do.call(tbl_opal,list('Atlantic3.ATL2_nov17_2015_MERGED',c('SourceID','OpalID')))

core1_push <- atl_core1[,.(entity_id = SourceID,
                             SourceID = SourceID,
                             OpalID = OpalID,
                             ADM_PART_CORE3 = ifelse(SourceID %in% atl_core3$SourceID,'1','0'),
                             ADM_PART_PM = ifelse(SourceID %in% atl_pm$SourceID,'1','0'),
                             ADM_PART_PRELIM = ifelse(SourceID %in% atl_pm$SourceID,'1','0'),
                             ADM_PART_BS = ifelse(SourceID %in% atl_bs$ADM_PART_ID,'1','0'))]

do.call(push_opal,list(list(core1_push,'ADM_PART_CPTP_YA'),
                       c('Atlantic-online','entity_id','Participant')))

core3_push <- atl_core3[,.(entity_id = SourceID,
                             SourceID = SourceID,
                             OpalID = OpalID,
                             ADM_PART_CORE1 = ifelse(SourceID %in% atl_core1$SourceID,'1','0'),
                             ADM_PART_PM = ifelse(SourceID %in% atl_pm$SourceID,'1','0'),
                             ADM_PART_PRELIM = ifelse(SourceID %in% atl_pm$SourceID,'1','0'),
                             ADM_PART_BS = ifelse(SourceID %in% atl_bs$ADM_PART_ID,'1','0'))]

do.call(push_opal,list(list(core3_push,'ADM_PART_CPTP_YA'),
                       c('Atlantic3','entity_id','Participant')))


pm_push <- atl_pm[,.(entity_id = SourceID,
                        SourceID = SourceID,
                        OpalID = as.character(OpalID),
                        ADM_PART_CORE1 = ifelse(SourceID %in% atl_core1$SourceID,'1','0'),
                        ADM_PART_CORE3 = ifelse(SourceID %in% atl_core3$SourceID,'1','0'),
                        ADM_PART_BS = ifelse(SourceID %in% unique(atl_bs$ADM_PART_ID),'1','0'))]

do.call(push_opal,list(list(pm_push,'ADM_PART_CPTP_YA'),
                         c('Atlantic_R2','entity_id','Participant')))

bs_push <- unique(atl_bs,by='ADM_PART_ID')[,.(ADM_PART_ID)][,.(entity_id = ADM_PART_ID,
                        SourceID = ADM_PART_ID,
                        ADM_PART_CORE1 = ifelse(ADM_PART_ID %in% atl_core1$SourceID,'1','0'),
                        ADM_PART_CORE3 = ifelse(ADM_PART_ID %in% atl_core3$SourceID,'1','0'),
                        ADM_PART_PM = ifelse(ADM_PART_ID %in% atl_pm$SourceID,'1','0'))]

do.call(push_opal,list(list(bs_push,'ADM_PART_CPTP_YA'),
                       c('Atlantic_R2_SAMPLE','entity_id','Participant')))



## BCGP
bc_meas <-do.call(fl_opal,list('/home/yamadou/pm/backup/BCGP/BCGP_Measurements.csv',c('SourceID','OpalID')))
bc_bs <-do.call(fl_opal,list('/home/yamadou/export/Views/BCGP/BCGP_Sample/data.csv',c('SAMPLE_ID','ADM_PART_ID')))
bc_prel <-do.call(fl_opal,list('/home/yamadou/pm/backup/BCGP/BCGP_R2-BCGP_Prelimqx-20170518160709/BCGP_Prelimqx/data.csv',c('SourceID','OpalID')))
bc_opal <-do.call(tbl_opal,list('BCGP-OPAL.OPAL_4CPTP_UPDATED2',c('BARCODE_FULL','Participant')))
bc_tlf <-do.call(tbl_opal,list('BCGP_TELEFORM.bcgp2_telf_MERGED',c('BARCODE_FULL','Participant')))
bc_on <-do.call(tbl_opal,list('BCGP_online.BCGP1_ONL_MERGED',c('BARCODE_FULL','Participant')))

bc_opal_push <- bc_opal[,.(entity_id = BARCODE_FULL,
                           SourceID = BARCODE_FULL,
                           OpalID = Participant,
                           ADM_PART_TELEFORM = ifelse(BARCODE_FULL %in% bc_tlf$BARCODE_FULL,'1','0'),
                           ADM_PART_ONLINE = ifelse(BARCODE_FULL %in% bc_on$BARCODE_FULL,'1','0'),
                           ADM_PART_MEASURE = ifelse(BARCODE_FULL %in% bc_meas$SourceID,'1','0'),
                           ADM_PART_PRELIM = ifelse(BARCODE_FULL %in% bc_prel$SourceID,'1','0'),
                           ADM_PART_BS = ifelse(BARCODE_FULL %in% bc_bs$ADM_PART_ID,'1','0')
                           )]

do.call(push_opal,list(list(bc_opal_push,'ADM_PART_CPTP_YA'),
                       c('BCGP-OPAL','entity_id','Participant')))








########
#attr(core_push,'label') <- list(list(name='label',value=''),
#     list(name='label',value='Participant Opal-generated ID'),
#     list(name='label',value='The participant has PM assessement measurements'),
#     list(name='label',value='The participant has Condraindication assessment'),
#     list(name='label',value='The participant has Blood samples data'))
#attr(core_push,'label') <- c("",
#  'Participant Cohort ID',
#  'Participant Opal-generated ID',
#  "The participant has PM assessement measurements",
#  "The participant has Condraindication assessment",
#  "The participant has Blood samples data")
