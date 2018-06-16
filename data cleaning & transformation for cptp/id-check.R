##
##
##

#clean the environment
rm(list = ls(all.names = TRUE))
cat("\014")

source('~/cptp/opal-login.r')

knitr::opts_chunk$set(echo = TRUE)
require(data.table);require(magrittr);require(data.table);require(opal);
require(ggplot2);require(lubridate);require(knitr);require(htmlTable);require(xlsx)


#ATL
opal.file_download(o,'/home/ndragieva/atl12_for_biosample_count/',
                   "~/cptp/id_check.zip")
unzip("~/cptp/id_check.zip",
      exdir = "~/cptp/id_check/atl/");unlink("~/cptp/id_check.zip")

atl_bsH_id <- fread('~/cptp/id_check/atl/atl12_for_biosample_count/Atlantic_R2_SAMPLE-ATL_Sample-20170426090634/ATL_Sample/data.csv',
      colClasses = 'character',select = 'ADM_PART_ID',col.names = 'id',data.table = TRUE) %>% unique
atl_cqx_id <- data.table(id=Reduce(union,list(
  Reduce(union,fread('~/cptp/id_check/atl/atl12_for_biosample_count/Atlantic-online-atl1_r2_harmo_merge_test_test_v2-20170428150641/atl1_r2_harmo_merge_test_test_v2/data.csv',
               colClasses = 'character',select = c('SourceID','entity_id','ADM_PART_ID'),data.table = TRUE) %>% as.list),
  Reduce(union,fread('~/cptp/id_check/atl/atl12_for_biosample_count/Atlantic3-atl3_harmo_view_merge_test_test_v2-20170428215736/atl3_harmo_view_merge_test_test_v2/data.csv',
               colClasses = 'character',select = c('SourceID'),data.table = TRUE) %>% as.list))))
id_check_atl = do.call(explr,list(c('atl_bsH_id','atl_cqx_id'),'id'))

table(id_map[SourceID %in% id_check_atl$atl_bsH_id_entities_notin_atl_cqx_id$id,OpalID] %in% 
        Reduce(union,list(fread('~/cptp/id_check/atl/atl12_for_biosample_count/Atlantic-online-atl1_r2_harmo_merge_test_test_v2-20170428150641/atl1_r2_harmo_merge_test_test_v2/data.csv',
                                colClasses = 'character',select = c('OpalID'),data.table = TRUE)$OpalID,
                          fread('~/cptp/id_check/atl/atl12_for_biosample_count/Atlantic3-atl3_harmo_view_merge_test_test_v2-20170428215736/atl3_harmo_view_merge_test_test_v2/data.csv',
                                colClasses = 'character',select = c('OpalID'),data.table = TRUE)$OpalID)))

rbind(atl_bsH[SAMPLE_ID=='100068133',],atl_bsH[ADM_PART_ID=='100068133',]) %>% View

intersect(atl_bsH$SAMPLE_ID,atl_bsH$ADM_PART_ID) %>% length

opal.assign.table(o,'cptp','Atlantic_R2_SAMPLE.AtlanticPATHBiosamples_October3_2016_noNAs')
atl_bs <- opal.execute(o,'cptp') %>% data.table

atl_bs_file <- '/home/jhicks/Biosamples/AtlanticPATHBiosamples_October3,2016.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.) %>% data.table

atl_bs_file[SAMPLE_RESERVE==4,tblBiosamples_HARMONIZATION_ID]

atl_bs_file[SAMPLE_RESERVE==4,.N] == atl_bs_file[!is.na(SAMPLE_ID),][is.na(SAMPLE_PRIMARY_TUBE_ID),.N]

table(id_check_atl$atl_bsH_id_entities_notin_atl_cqx_id$id %in% atl_bs_file[SAMPLE_RESERVE==4, tblBiosamples_HARMONIZATION_ID])

#BCGP

opal.file_download(o,'/home/ndragieva/bcgp123_for_biosample_count/',
                   "~/cptp/id_check.zip")
unzip("~/cptp/id_check.zip",
      exdir = "~/cptp/id_check/bcgp/");unlink("~/cptp/id_check.zip")


bc_bs <- '/home/yamadou/export/Views/BCGP/BCGP_Sample/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.) %>% data.table ; bc_bs_id  <- bc_bs[,list(ADM_PART_ID,SAMPLE_ID)]

bc_drv <- '/home/yamadou/export/Views/BCGP/BCGP_Deriv_Sample/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.) %>% data.table ; bc_drv_id <- bc_drv[,list(SAMPLE_DERIV_ID,SAMPLE_DERIV_ADM_PART_ID)]

bc_bsH_id <- data.table(id=Reduce(union,list(bc_bs$ADM_PART_ID,bc_drv$SAMPLE_DERIV_ADM_PART_ID)) %>% na.exclude)


bc_cqx_id <- data.table( id = Reduce(union,list(
  Reduce(union,fread('~/cptp/id_check/bcgp/bcgp123_for_biosample_count/BCGP_online-bcgp1_harmo_view_merge_test_v2-20170426165017/bcgp1_harmo_view_merge_test_v2/data.csv',
        colClasses = 'character',select = c('entity_id','bcgp_id',"ADM_PART_ID"),data.table = TRUE) %>% as.list),
  Reduce(union,fread('~/cptp/id_check/bcgp/bcgp123_for_biosample_count/BCGP_TELEFORM-bcgp2_harmo_view_merge_test_v2-20170426164942/bcgp2_harmo_view_merge_test_v2/data.csv',
        colClasses = 'character',select = c('entity_id','bcgp_id',"A_ADM_PART_ID"),data.table = TRUE)  %>% as.list),
  Reduce(union,fread('~/cptp/id_check/bcgp/bcgp123_for_biosample_count/BCGP-OPAL-20170427125333/bcgp3_harmo_view_merge_test_v2/data.csv',
        colClasses = 'character',select = c('entity_id','bcgp_id',"ADM_PART_ID"),data.table = TRUE)  %>% as.list))) %>% na.exclude)
id_check_bcgp = do.call(explr,list(c('bc_bsH_id','bc_cqx_id'),'id'))

id_check_bcgp$bc_bsH_id_entities_notin_bc_cqx_id$id[!id_check_bcgp$bc_bsH_id_entities_notin_bc_cqx_id$id %in% id_map$SourceID]


#CAG

opal.file_download(o,'/home/ndragieva/cagab_for_biosample_count/',
                   "~/cptp/id_check.zip")
unzip("~/cptp/id_check.zip",
      exdir = "~/cptp/id_check/cag/");unlink("~/cptp/id_check.zip")

cag_bs <- '/home/yamadou/export/Views/CaG/CAG_Sample/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.,colClasses = 'character') %>% data.table ; 

cag_drv <- '/home/yamadou/export/Views/CaG/CAG_Deriv_Sample/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.,colClasses = 'character') %>% data.table ; 

cag_bsH_id <- data.table(id=Reduce(union,list(cag_bs$ADM_PART_ID,cag_drv$SAMPLE_DERIV_ADM_PART_ID)) %>% na.exclude)

cag_cqx_id <- data.table(id = Reduce(union,list(
  Reduce(union,fread('~/cptp/id_check/cag/cagab_for_biosample_count/CaG_B-cag_b_harmo_view_merge_test_v2-20170426184048/cag_b_harmo_view_merge_test_v2/data.csv',
                     colClasses = 'character',select = c('SourceID','entity_id','ADM_PART_ID'),data.table = TRUE) %>% as.list),
  Reduce(union,fread('~/cptp/id_check/cag/cagab_for_biosample_count/CaG-cag_harmo_view_merge_test_v2-20170426173718/cag_harmo_view_merge_test_v2/data.csv',
                     colClasses = 'character',select = c('SourceID','entity_id','ADM_PART_ID'),data.table = TRUE)  %>% as.list))))
id_check_cag = do.call(explr,list(c('cag_bsH_id','cag_cqx_id'),'id'))


#ATP

opal.file_download(o,'/home/ndragieva/atp12_for_biosample_counts/',
                   "~/cptp/id_check.zip")
unzip("~/cptp/id_check.zip",
      exdir = "~/cptp/id_check/atp/");unlink("~/cptp/id_check.zip")

atp_bs <- '/home/yamadou/export/Views/ATP/ATP_Sample/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.,colClasses = 'character') %>% data.table ; 
atp_bsH_id <- data.table(id=unique(atp_bs$ADM_PART_ID %>% na.exclude))

atp_bsH_id_nd <- fread('~/cptp/id_check/atp/atp12_for_biosample_counts/TTP_R2_SAMPLE-ATP_Sample-20170427093105/ATP_Sample/data.csv',
                    colClasses = 'character',select = 'ADM_PART_ID',col.names = 'id',data.table = TRUE) %>% unique

atp_cqx_id <- data.table( id = Reduce(union,list(
  Reduce(union,fread('~/cptp/id_check/atp/atp12_for_biosample_counts/TTP-atp1_harmo_view_merge_test_v2-20170427113525/atp1_harmo_view_merge_test_v2/data.csv',
                     colClasses = 'character',select = c('SourceID'),data.table = TRUE) %>% as.list),
  Reduce(union,fread('~/cptp/id_check/atp/atp12_for_biosample_counts/TTP-coreQA-atp2_R2_harmo_view_merge_test_v2-20170428154058/atp2_R2_harmo_view_merge_test_v2/data.csv',
                     colClasses = 'character',select = c('SourceID'),data.table = TRUE)  %>% as.list))))
id_check_atp = do.call(explr,list(c('atp_bsH_id','atp_cqx_id'),'id'))


#OHS

opal.file_download(o,'/home/ndragieva/ohs123_for_biosample_counts/',
                   "~/cptp/id_check.zip")
unzip("~/cptp/id_check.zip",
      exdir = "~/cptp/id_check/ohs/");unlink("~/cptp/id_check.zip")

#ohs_bs <- '/home/yamadou/export/Views/ATP/ATP_Sample/data.csv' %>%  file.path(.) %>% 
#  opal.file(o,.) %>% read.csv(text=.,colClasses = 'character') %>% data.table ; 
#ohs_bsH_id <- data.table(id=unique(atp_bs$ADM_PART_ID) %>% na.exclude)

#ohs_base=  fread('~/cptp/id_check/ohs/ohs123_for_biosample_counts/OHS_R2_SAMPLE-20170427090308/OHS_Sample/data.csv',
#                 colClasses = 'character',select = 'ADM_PART_ID',col.names = 'id_cpac',data.table = TRUE)
#ohs_base[,id:=stringr::str_extract(id_cpac,"[0-9]+")]
ohs_base<-data.table(id=Reduce(union,list(ohs_samp_new$CPACID,ohs_samp_old$CPACID,ohs_base$id)))


ohs_bsH_id <- data.table(Reduce(rbind,list(
  ohs_base,
  fread('~/cptp/id_check/ohs/ohs123_for_biosample_counts/OHS_R2_SAMPLE-20170427090308/OHS_Pilot_Sample/data.csv',
        colClasses = 'character',select = 'ADM_PART_ID',col.names = 'id',data.table = TRUE))) %>% unique %>% na.exclude)

ohs_cqx_id <- data.table( id = Reduce(union,list(
  Reduce(union,fread('~/cptp/id_check/ohs/ohs123_for_biosample_counts/OHS-20170426202359/ohs1_harmo_merge_test_v2/data.csv',
                     colClasses = 'character',select = c('SourceID','entity_id','ADM_PART_ID'),data.table = TRUE) %>% as.list),
  Reduce(union,fread('~/cptp/id_check/ohs/ohs123_for_biosample_counts/OHS2-20170426201536/ohs2_r2_harmo_view_merge_test_v2/data.csv',
                     colClasses = 'character',select = c('SourceID','entity_id','ADM_PART_ID'),data.table = TRUE)  %>% as.list),
  Reduce(union,fread('~/cptp/id_check/ohs/ohs123_for_biosample_counts/OHS_pilot-20170426201412/ohs_pilot_harmo_merge_test_v2/data.csv',
                     colClasses = 'character',select = c('SourceID','entity_id','ADM_PART_ID'),data.table = TRUE)  %>% as.list))))
id_check_ohs = do.call(explr,list(c('ohs_bsH_id','ohs_cqx_id'),'id'))




table = dcast(atp_bs[,list(ADM_PART_ID,SAMPLE_TYPE)], ADM_PART_ID ~ SAMPLE_TYPE, value.var = c("SAMPLE_TYPE"))
colMeans(atp_bs_reshape[,c('10','3','4','5','7','9'),with=FALSE])



sapply(c('atp_bs','bc_bsH','atl_bsH','cag_bs'),function(x){
  a = eval(parse(text=x))[,list(ADM_PART_ID,SAMPLE_TYPE)]
  b = dcast(a, ADM_PART_ID ~ SAMPLE_TYPE, value.var = c("SAMPLE_TYPE"))
  c = colMeans(subset(b,select=colnames(b)[-1]))
  c = 
  return (c)
})










