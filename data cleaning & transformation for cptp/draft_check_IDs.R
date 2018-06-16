


#ATL-check


id_map = fread('~/data/reorg/idmap/Participant-atl_r2.csv',
               colClasses = 'character',col.names = c("OpalID","SourceID")) %>% data.table ;

tab_ND = fread('~/data/reorg/idmap/ND/AtlanticPATH_ParticipantDetails_Nov9_2016_ND.csv',
               colClasses = 'character') %>% data.table ;

opal.assign.table(o,'cptp','Atlantic_R2.AtlanticPath_PhysicalMeasuresSet1_April18_2016',id.name = "entity_id")
ds1 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id')
opal.assign.table(o,'cptp','Atlantic_R2.AtlanticPATH_PhysicalMeasuresSet2_Jan27_2017_Final',id.name = "entity_id")
ds2 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id')

atl1_nd <- '/home/ndragieva/coreqx_data_published/csv_sourceIDs/atl1_online/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.) %>% data.table(.,key='entity_id')
atl2_nd <- '/home/ndragieva/coreqx_data_published/csv_sourceIDs/atl2_computerized_paper/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.) %>% data.table(.,key='entity_id')

R_atl = lapply(c('ds1$entity_id','ds2$entity_id'),
  function(x){
    t = eval(parse(text=x))
    a = Filter(function(x){return (x %in% tab_ND$Participant_ID)},t)
    b = Filter(function(x){return (!x %in% tab_ND$Participant_ID)},t)
    d = intersect(tab_ND[Active==1,Participant_ID], t)
    e = intersect(tab_ND[Active==0,Participant_ID], t)
    f = d[which(d %in% union(atl1_nd$entity_id,atl2_nd$entity_id))]
    g = d[which(!d %in% union(atl1_nd$entity_id,atl2_nd$entity_id))]
    h = t[which(!t %in% union(atl1_nd$entity_id,atl2_nd$entity_id))]
    i = h[which(h %in% tab_ND[Active==0,Participant_ID])]
    j = h[which(!h %in% tab_ND[Active==0,Participant_ID])]

    c = paste0(
      '\n',x,'\n',
      '# total ids : ', t %>% length ,'\n',
      '# ids found in tab_ND : ', a %>% length,'\n',
      '# ids found active : ', d %>% length, '\n',
      '# ids found not active : ', e %>% length,'\n',
      '# ids found active in coreqx : ', f %>% length,'\n',
      '# ids found active not in coreqx : ', g %>% length,'\n',
      '# ids not coreqx not active : ', i %>% length,'\n',
      '# ids not coreqx not not active : ', j %>% length,'\n',
      '# ids not found in tab_ND : ', b %>% length ,'\n'
      )
    return (list(report=c,
                 ids_activ=d,
                 ids_noactive=e,
                 ids_notfound=b,
                 ids_nocore_notactiv=i,
                 ids_nocore_not_notactiv=j))
  })


lapply(ds1[SourceID %in% R_atl[[1]]$ids_nocore_notactiv,
           c('DO_NOT_CONTACT_STATUS','Exclusion'),with=F],
       function(x){ as.factor(x) %>% summary})

lapply(ds1[SourceID %in% R_atl[[1]]$ids_nocore_not_notactiv,
           c('DO_NOT_CONTACT_STATUS','Exclusion'),with=F],
       function(x){ as.factor(x) %>% summary})

lapply(ds2[SourceID %in% R_atl[[2]]$ids_nocore_notactiv,
           c('DO_NOT_CONTACT_STATUS','Exclusion'),with=F],
       function(x){ as.factor(x) %>% summary})

lapply(ds2[SourceID %in% R_atl[[2]]$ids_nocore_not_notactiv,
           c('DO_NOT_CONTACT_STATUS','Exclusion'),with=F],
       function(x){ as.factor(x) %>% summary})


#BCGP-check

id_map = fread('~/data/reorg/idmap/Participant-bcgp_r2.csv',
               colClasses = 'character',col.names = c("OpalID","SourceID")) %>% data.table ;

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


tab_ND = fread('~/data/reorg/idmap/ND/DC2BC_N28933_4MR_ND.csv',
               colClasses = 'character') %>% data.table ;

opal.assign.table(o,'cptp','BCGP_R2.PM_N16618',id.name = "entity_id")
ds1 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id') ;
opal.assign.table(o,'cptp','BCGP_R2.CIPQ_N4889',id.name = "entity_id")
ds2 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id') ;
opal.assign.table(o,'cptp','BCGP_R2.SDQ_N25867',id.name = "entity_id")
ds3 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id') ;
opal.assign.table(o,'cptp','BCGP_R2.PM_ACCESS_Dates4MR',id.name = "entity_id")
ds4 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id');

bcgp_onl_nd <- '/home/ndragieva/coreqx_data_published/csv_sourceIDs/bcgp1_onl/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.) %>% data.table(.,key='entity_id')
bcgp_telf_nd <- '/home/ndragieva/coreqx_data_published/csv_sourceIDs/bcgp2_telf/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.) %>% data.table(.,key='entity_id')
bcgp_opal_nd <- '/home/ndragieva/coreqx_data_published/csv_sourceIDs/bcgp3_opal/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.) %>% data.table(.,key='entity_id')

R_bcgp = lapply(c('ds1$entity_id','ds2$entity_id','ds3$entity_id','ds4$entity_id'),
               function(x){
                 t = eval(parse(text=x))
                 a = Filter(function(x){return (x %in% tab_ND$BARCODE)},t)
                 b = Filter(function(x){return (!x %in% tab_ND$BARCODE)},t)
                 d = intersect(tab_ND[Active==1,BARCODE], t)
                 e = intersect(tab_ND[Active==0,BARCODE], t)
                 f = d[which(d %in% Reduce(union,list(bcgp_onl_nd$entity_id,bcgp_telf_nd$entity_id,bcgp_opal_nd$entity_id)))]
                 g = d[which(!d %in% Reduce(union,list(bcgp_onl_nd$entity_id,bcgp_telf_nd$entity_id,bcgp_opal_nd$entity_id)))]
                 h = t[which(!t %in% Reduce(union,list(bcgp_onl_nd$entity_id,bcgp_telf_nd$entity_id,bcgp_opal_nd$entity_id)))]
                 i = h[which(h %in% tab_ND[Active==0,BARCODE])]
                 j = h[which(!h %in% tab_ND[Active==0,BARCODE])]
                 
                 c = paste0(
                   '\n',x,'\n',
                   '# total ids : ', t %>% length ,'\n',
                   '# ids found in tab_ND : ', a %>% length,'\n',
                   '# ids found active : ', d %>% length, '\n',
                   '# ids found not active : ', e %>% length,'\n',
                   '# ids found active in coreqx : ', f %>% length,'\n',
                   '# ids found active not in coreqx : ', g %>% length,'\n',
                   '# ids not coreqx not active : ', i %>% length,'\n',
                   '# ids not coreqx not not active : ', j %>% length,'\n',
                   '# ids not found in tab_ND : ', b %>% length ,'\n'
                 )
                 return (list(report=c,
                              ids_activ=d,
                              ids_noactive=e,
                              ids_notfound=b,
                              ids_nocore_notactiv=i,
                              ids_nocore_not_notactiv=j))
               })


lapply(ds1[SourceID %in% R_bcgp[[1]]$ids_nocore_not_notactiv,
           c('Type_NoConsent','Exclusion','WDLevel','Type'),with=F],
       function(x){ as.factor(x) %>% summary})


#BCGP-check

id_map = fread('~/data/reorg/idmap/Participant-bcgp_r2.csv',
               colClasses = 'character',col.names = c("OpalID","SourceID")) %>% data.table ;

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


tab_ND = fread('~/data/reorg/idmap/ND/DC2BC_N28933_4MR_ND.csv',
               colClasses = 'character') %>% data.table ;

opal.assign.table(o,'cptp','BCGP_R2.PM_N16618',id.name = "entity_id")
ds1 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id') ;
opal.assign.table(o,'cptp','BCGP_R2.CIPQ_N4889',id.name = "entity_id")
ds2 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id') ;
opal.assign.table(o,'cptp','BCGP_R2.SDQ_N25867',id.name = "entity_id")
ds3 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id') ;
opal.assign.table(o,'cptp','BCGP_R2.PM_ACCESS_Dates4MR',id.name = "entity_id")
ds4 <- opal.execute(o,'cptp') %>% data.table(.,key='entity_id');

bcgp_onl_nd <- '/home/ndragieva/coreqx_data_published/csv_sourceIDs/bcgp1_onl/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.) %>% data.table(.,key='entity_id')
bcgp_telf_nd <- '/home/ndragieva/coreqx_data_published/csv_sourceIDs/bcgp2_telf/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.) %>% data.table(.,key='entity_id')
bcgp_opal_nd <- '/home/ndragieva/coreqx_data_published/csv_sourceIDs/bcgp3_opal/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.) %>% data.table(.,key='entity_id')

R_bcgp = lapply(c('ds1$entity_id','ds2$entity_id','ds3$entity_id','ds4$entity_id'),
                function(x){
                  t = eval(parse(text=x))
                  a = Filter(function(x){return (x %in% tab_ND$BARCODE)},t)
                  b = Filter(function(x){return (!x %in% tab_ND$BARCODE)},t)
                  d = intersect(tab_ND[Active==1,BARCODE], t)
                  e = intersect(tab_ND[Active==0,BARCODE], t)
                  f = d[which(d %in% Reduce(union,list(bcgp_onl_nd$entity_id,bcgp_telf_nd$entity_id,bcgp_opal_nd$entity_id)))]
                  g = d[which(!d %in% Reduce(union,list(bcgp_onl_nd$entity_id,bcgp_telf_nd$entity_id,bcgp_opal_nd$entity_id)))]
                  h = t[which(!t %in% Reduce(union,list(bcgp_onl_nd$entity_id,bcgp_telf_nd$entity_id,bcgp_opal_nd$entity_id)))]
                  i = h[which(h %in% tab_ND[Active==0,BARCODE])]
                  j = h[which(!h %in% tab_ND[Active==0,BARCODE])]
                  
                  c = paste0(
                    '\n',x,'\n',
                    '# total ids : ', t %>% length ,'\n',
                    '# ids found in tab_ND : ', a %>% length,'\n',
                    '# ids found active : ', d %>% length, '\n',
                    '# ids found not active : ', e %>% length,'\n',
                    '# ids found active in coreqx : ', f %>% length,'\n',
                    '# ids found active not in coreqx : ', g %>% length,'\n',
                    '# ids not coreqx not active : ', i %>% length,'\n',
                    '# ids not coreqx not not active : ', j %>% length,'\n',
                    '# ids not found in tab_ND : ', b %>% length ,'\n'
                  )
                  return (list(report=c,
                               ids_activ=d,
                               ids_noactive=e,
                               ids_notfound=b,
                               ids_nocore_notactiv=i,
                               ids_nocore_not_notactiv=j))
                })


lapply(ds1[SourceID %in% R_bcgp[[1]]$ids_nocore_not_notactiv,
           c('Type_NoConsent','Exclusion','WDLevel','Type'),with=F],
       function(x){ as.factor(x) %>% summary})




shipid <- '/home/chenwei/Release 2/Biosample/Data up to May 31, 2016/CPTP_OHS_Biosample_Source_ShipID_20170331.csv' %>%  
  file.path(.) %>% opal.file(o,.) %>% read.csv(text=.) %>% data.table





#Fixing dual date issue
a = stringr::str_split_fixed(atl_bs$SAMPLE_COLLECT_DATE,"/",n = 3) %>% data.table

b = data.table(entity_id=atl_bs$entity_id,
               date0=atl_bs$SAMPLE_COLLECT_DATE,
               date1 = a[,(step1=Reduce(function(x,y){
  ifelse(as.integer(y)<10 & nchar(y)==1 |
           as.integer(x)<10 & nchar(x)==1 |
           as.integer(y) > 12,
         paste0("\'",fdt(y),"/",fdt(x),"/",V3,"\'"),
         ifelse(as.integer(x)<=12,
                NA,
                NA)
         )
},list(V1,V2)))])

b[!is.na(date1),]

b[,check:=lapply(date0,function(y){
  a = unlist(stringr::str_split(y,"/",n = 3))
  as.integer(a[1])>=10 & as.integer(a[1])<=12 & as.integer(a[2])>=10})]



fdt = function(x){ifelse(as.integer(x)<10 & nchar(x)==1,paste0("0",x),x)}











