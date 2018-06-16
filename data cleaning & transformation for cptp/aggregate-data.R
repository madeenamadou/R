##
## Variables aggregation & cross-tables
##

#clean
rm(list = ls(all.names = TRUE))
cat("\014")

#load packages
require(data.table);require(opal);require(magrittr);require('Hmisc');require(foreign)

save.image('/home/yamadou/data/nci/nci')

nci = read.csv('/home/yamadou/data/nci/data.csv') %>% data.table
nci[,ASIAN:=S_SDC_EB_S_ASIAN+S_SDC_EB_SE_ASIAN+S_SDC_EB_W_ASIAN+S_SDC_EB_E_ASIAN+S_SDC_EB_FILIPINO>=1]
nci[,OTHER:=S_SDC_EB_JEWISH+S_SDC_EB_ARAB+S_SDC_EB_OTHER>=1]
nci[,MLTP:=S_SDC_EB_CAT==13]
nci[,UNKW:=is.na(S_SDC_EB_CAT)]

###cancer count

ts = subset(nci,grepl("BCGP",nci$A_ADM_STUDY_DATASET,ignore.case = T))
summary(as.factor(ts$A_SDC_GENDER))

id_all = merge(nci_id,nci[,c('entity_id',"A_ADM_STUDY_ID","A_ADM_STUDY_DATASET"),with=F],by='entity_id')
id_sd = id_all[grepl("BCGP",A_ADM_STUDY_DATASET,ignore.case = T)]

var = list("A_DIS_CANCER1","A_DIS_CANCER2","A_DIS_CANCER3","S_DIS_CANCER4","S_DIS_CANCER5","S_DIS_CANCER6","A_SDC_GENDER")

#ohs
opal.assign.table(o,'cptp','CoreQx_Jan2016.Coreqx_final_feb16_OHS1_copymar23',variables = var,id.name = "entity_id")
ohs1 <- opal.execute(o,'cptp') %>% data.table
opal.assign.table(o,'cptp','CoreQx_Jan2016.Coreqx_final_feb16_OHS2_copymar23',var,id.name = "entity_id")
ohs2 <- opal.execute(o,'cptp') %>% data.table
opal.assign.table(o,'cptp','CoreQx_Jan2016.Coreqx_final_feb16_OHS3_copyfeb26',var,id.name = "entity_id")
ohs3 <- opal.execute(o,'cptp') %>% data.table
Reduce(intersect,list(ohs1$entity_id,ohs2$entity_id,ohs3$entity_id))
ohs_full = Reduce(rbind,list(ohs1,ohs2,ohs3))

id_sd = id_all[grepl("OHS",A_ADM_STUDY_DATASET,ignore.case = T)]
id_map = read.csv('/home/yamadou/data/cptpid/Participant-ohs_r2.csv') %>% data.table ; 
names(id_map)<-c("entity_id","Participant") ; class(id_map$entity_id) <- "character"
ohs_full = merge(ohs_full,id_map,by="entity_id",all.x=T)
ohs_full <- ohs_full[Participant %in% as.character.factor(id_sd$Participant)]

ohs_full[,A_DIS_CANCER2_b:=A_DIS_CANCER2];ohs_full[,A_DIS_CANCER3_b:=A_DIS_CANCER3];
ohs_full[,S_DIS_CANCER4_b:=S_DIS_CANCER4];ohs_full[,S_DIS_CANCER5_b:=S_DIS_CANCER5];
ohs_full[,S_DIS_CANCER6_b:=S_DIS_CANCER6]

ohs_full[A_DIS_CANCER2 == A_DIS_CANCER1,A_DIS_CANCER2_b:=NA]
ohs_full[A_DIS_CANCER3 == A_DIS_CANCER2|A_DIS_CANCER3 == A_DIS_CANCER1,A_DIS_CANCER3_b:=NA]
ohs_full[S_DIS_CANCER4 == A_DIS_CANCER3|S_DIS_CANCER4 == A_DIS_CANCER2|S_DIS_CANCER4 == A_DIS_CANCER1,S_DIS_CANCER4_b:=NA]
ohs_full[S_DIS_CANCER5 == S_DIS_CANCER4|S_DIS_CANCER5 == A_DIS_CANCER3|S_DIS_CANCER5 == A_DIS_CANCER2|
           S_DIS_CANCER5 == A_DIS_CANCER1,S_DIS_CANCER5_b:=NA]
ohs_full[S_DIS_CANCER6 == S_DIS_CANCER5|S_DIS_CANCER6 == S_DIS_CANCER4|S_DIS_CANCER6 == A_DIS_CANCER3|
           S_DIS_CANCER6 == A_DIS_CANCER2|S_DIS_CANCER6 == A_DIS_CANCER1,S_DIS_CANCER6_b:=NA]

#atp
opal.assign.table(o,'cptp','CoreQx_Jan2016.Coreqx_final_feb16_ATP1_copyfeb26',var,id.name = "entity_id")
atp1 <- opal.execute(o,'cptp') %>% data.table
opal.assign.table(o,'cptp','CoreQx_Jan2016.Coreqx_final_feb16_ATP2_copyMay16',var,id.name = "entity_id")
atp2 <- opal.execute(o,'cptp') %>% data.table
Reduce(intersect,list(atp1$entity_id,atp2$entity_id))
atp_full = Reduce(rbind,list(atp1,atp2))

id_sd = id_all[grepl("ATP",A_ADM_STUDY_DATASET,ignore.case = T)]
id_map = read.csv('/home/yamadou/data/cptpid/Participant-atp_r2.csv') %>% data.table ; 
names(id_map)<-c("entity_id","Participant") ; class(id_map$entity_id) <- "character"
atp_full = merge(atp_full,id_map,by="entity_id",all.x=T)
atp_full <- atp_full[Participant %in% as.character.factor(id_sd$Participant)]

atp_full[,A_DIS_CANCER2_b:=A_DIS_CANCER2];atp_full[,A_DIS_CANCER3_b:=A_DIS_CANCER3];
atp_full[,S_DIS_CANCER4_b:=S_DIS_CANCER4];atp_full[,S_DIS_CANCER5_b:=S_DIS_CANCER5];
atp_full[,S_DIS_CANCER6_b:=S_DIS_CANCER6]

atp_full[A_DIS_CANCER2 == A_DIS_CANCER1,A_DIS_CANCER2_b:=NA]
atp_full[A_DIS_CANCER3 == A_DIS_CANCER2|A_DIS_CANCER3 == A_DIS_CANCER1,A_DIS_CANCER3_b:=NA]
atp_full[S_DIS_CANCER4 == A_DIS_CANCER3|S_DIS_CANCER4 == A_DIS_CANCER2|S_DIS_CANCER4 == A_DIS_CANCER1,S_DIS_CANCER4_b:=NA]
atp_full[S_DIS_CANCER5 == S_DIS_CANCER4|S_DIS_CANCER5 == A_DIS_CANCER3|S_DIS_CANCER5 == A_DIS_CANCER2|
           S_DIS_CANCER5 == A_DIS_CANCER1,S_DIS_CANCER5_b:=NA]
atp_full[S_DIS_CANCER6 == S_DIS_CANCER5|S_DIS_CANCER6 == S_DIS_CANCER4|S_DIS_CANCER6 == A_DIS_CANCER3|
           S_DIS_CANCER6 == A_DIS_CANCER2|S_DIS_CANCER6 == A_DIS_CANCER1,S_DIS_CANCER6_b:=NA]

#atl
opal.assign.table(o,'cptp','CoreQx_Jan2016.Coreqx_final_feb16_ATL1_copyfeb26',var,id.name = "entity_id")
atl1 <- opal.execute(o,'cptp') %>% data.table
opal.assign.table(o,'cptp','CoreQx_Jan2016.Coreqx_final_feb16_ATL2_copymar23',var,id.name = "entity_id")
atl2 <- opal.execute(o,'cptp') %>% data.table
Reduce(intersect,list(atl1$entity_id,atl2$entity_id))
atl_full = Reduce(rbind,list(atl1,atl2))

id_sd = id_all[grepl("ATL",A_ADM_STUDY_DATASET,ignore.case = T)]
id_map = read.csv('/home/yamadou/data/cptpid/Participant-atl_r2.csv') %>% data.table ; 
names(id_map)<-c("entity_id","Participant") ; class(id_map$entity_id) <- "character"
atl_full = merge(atl_full,id_map,by="entity_id",all.x=T)
atl_full <- atl_full[Participant %in% as.character.factor(id_sd$Participant)]

atl_full[,A_DIS_CANCER2_b:=A_DIS_CANCER2];atl_full[,A_DIS_CANCER3_b:=A_DIS_CANCER3];
atl_full[,S_DIS_CANCER4_b:=S_DIS_CANCER4];atl_full[,S_DIS_CANCER5_b:=S_DIS_CANCER5];
atl_full[,S_DIS_CANCER6_b:=S_DIS_CANCER6]

atl_full[A_DIS_CANCER2 == A_DIS_CANCER1,A_DIS_CANCER2_b:=NA]
atl_full[A_DIS_CANCER3 == A_DIS_CANCER2|A_DIS_CANCER3 == A_DIS_CANCER1,A_DIS_CANCER3_b:=NA]
atl_full[S_DIS_CANCER4 == A_DIS_CANCER3|S_DIS_CANCER4 == A_DIS_CANCER2|S_DIS_CANCER4 == A_DIS_CANCER1,S_DIS_CANCER4_b:=NA]
atl_full[S_DIS_CANCER5 == S_DIS_CANCER4|S_DIS_CANCER5 == A_DIS_CANCER3|S_DIS_CANCER5 == A_DIS_CANCER2|
           S_DIS_CANCER5 == A_DIS_CANCER1,S_DIS_CANCER5_b:=NA]
atl_full[S_DIS_CANCER6 == S_DIS_CANCER5|S_DIS_CANCER6 == S_DIS_CANCER4|S_DIS_CANCER6 == A_DIS_CANCER3|
           S_DIS_CANCER6 == A_DIS_CANCER2|S_DIS_CANCER6 == A_DIS_CANCER1,S_DIS_CANCER6_b:=NA]

#cag
opal.assign.table(o,'cptp','CoreQx_Jan2016.Coreqx_final_feb16_CaG1_copyfeb26',var,id.name = "entity_id")
cag1 <- opal.execute(o,'cptp') %>% data.table
opal.assign.table(o,'cptp','CoreQx_Jan2016.Coreqx_final_feb16_CaG2_copymar23',var,id.name = "entity_id")
cag2 <- opal.execute(o,'cptp') %>% data.table
Reduce(intersect,list(cag1$entity_id,cag2$entity_id))
cag_full = Reduce(rbind,list(cag1,cag2))

id_sd = id_all[grepl("CAG",A_ADM_STUDY_DATASET,ignore.case = T)]
id_map = read.csv('/home/yamadou/data/cptpid/Participant-cag.csv') %>% data.table ; 
names(id_map)<-c("entity_id","Participant") ; class(id_map$entity_id) <- "character"
cag_full = merge(cag_full,id_map,by="entity_id",all.x=T)
cag_full <- cag_full[Participant %in% as.character.factor(id_sd$Participant)]

cag_full[,A_DIS_CANCER2_b:=A_DIS_CANCER2];cag_full[,A_DIS_CANCER3_b:=A_DIS_CANCER3];
cag_full[,S_DIS_CANCER4_b:=S_DIS_CANCER4];cag_full[,S_DIS_CANCER5_b:=S_DIS_CANCER5];
cag_full[,S_DIS_CANCER6_b:=S_DIS_CANCER6]

cag_full[A_DIS_CANCER2 == A_DIS_CANCER1,A_DIS_CANCER2_b:=NA]
cag_full[A_DIS_CANCER3 == A_DIS_CANCER2|A_DIS_CANCER3 == A_DIS_CANCER1,A_DIS_CANCER3_b:=NA]
cag_full[S_DIS_CANCER4 == A_DIS_CANCER3|S_DIS_CANCER4 == A_DIS_CANCER2|S_DIS_CANCER4 == A_DIS_CANCER1,S_DIS_CANCER4_b:=NA]
cag_full[S_DIS_CANCER5 == S_DIS_CANCER4|S_DIS_CANCER5 == A_DIS_CANCER3|S_DIS_CANCER5 == A_DIS_CANCER2|
           S_DIS_CANCER5 == A_DIS_CANCER1,S_DIS_CANCER5_b:=NA]
cag_full[S_DIS_CANCER6 == S_DIS_CANCER5|S_DIS_CANCER6 == S_DIS_CANCER4|S_DIS_CANCER6 == A_DIS_CANCER3|
           S_DIS_CANCER6 == A_DIS_CANCER2|S_DIS_CANCER6 == A_DIS_CANCER1,S_DIS_CANCER6_b:=NA]

#bcgp
opal.assign.table(o,'cptp','CoreQx_Jan2016.Coreqx_final_feb16_BCGP1_copyMay16',var,id.name = "entity_id")
bcgp1 <- opal.execute(o,'cptp') %>% data.table
id_map = read.csv('/home/yamadou/data/cptpid/Participant-bcgp_onl_apr2016.csv') %>% data.table ; 
names(id_map)<-c("entity_id","Participant") ; class(id_map$entity_id) <- "character"
bcgp1 = merge(bcgp1,id_map,by="entity_id",all.x=T)

opal.assign.table(o,'cptp','CoreQx_Jan2016.Coreqx_final_feb16_BCGP2_copyMay16',var,id.name = "entity_id")
bcgp2 <- opal.execute(o,'cptp') %>% data.table
id_map = read.csv('/home/yamadou/data/cptpid/Participant-bcgp_telf_apr2016.csv') %>% data.table ; 
names(id_map)<-c("entity_id","Participant") ; class(id_map$entity_id) <- "character"
bcgp2 = merge(bcgp2,id_map,by="entity_id",all.x=T)

opal.assign.table(o,'cptp','CoreQx_Jan2016.Coreqx_final_feb16_BCGP3_copyMay16',var,id.name = "entity_id")
bcgp3 <- opal.execute(o,'cptp') %>% data.table
id_map = read.csv('/home/yamadou/data/cptpid/Participant-bcgp_opal_apr2016.csv') %>% data.table ; 
names(id_map)<-c("entity_id","Participant") ; class(id_map$entity_id) <- "character"
bcgp3 = merge(bcgp3,id_map,by="entity_id",all.x=T)

Reduce(intersect,list(bcgp1$entity_id,bcgp2$entity_id,bcgp3$entity_id))
bcgp_full = Reduce(rbind,list(bcgp1,bcgp2,bcgp3))

id_sd = id_all[grepl("BCGP",A_ADM_STUDY_DATASET,ignore.case = T)]
bcgp_full <- bcgp_full[Participant %in% as.character.factor(id_sd$Participant)]

bcgp_full[,A_DIS_CANCER2_b:=A_DIS_CANCER2];bcgp_full[,A_DIS_CANCER3_b:=A_DIS_CANCER3];
bcgp_full[,S_DIS_CANCER4_b:=S_DIS_CANCER4];bcgp_full[,S_DIS_CANCER5_b:=S_DIS_CANCER5];
bcgp_full[,S_DIS_CANCER6_b:=S_DIS_CANCER6]

bcgp_full[A_DIS_CANCER2 == A_DIS_CANCER1,A_DIS_CANCER2_b:=NA]
bcgp_full[A_DIS_CANCER3 == A_DIS_CANCER2|A_DIS_CANCER3 == A_DIS_CANCER1,A_DIS_CANCER3_b:=NA]
bcgp_full[S_DIS_CANCER4 == A_DIS_CANCER3|S_DIS_CANCER4 == A_DIS_CANCER2|S_DIS_CANCER4 == A_DIS_CANCER1,S_DIS_CANCER4_b:=NA]
bcgp_full[S_DIS_CANCER5 == S_DIS_CANCER4|S_DIS_CANCER5 == A_DIS_CANCER3|S_DIS_CANCER5 == A_DIS_CANCER2|
           S_DIS_CANCER5 == A_DIS_CANCER1,S_DIS_CANCER5_b:=NA]
bcgp_full[S_DIS_CANCER6 == S_DIS_CANCER5|S_DIS_CANCER6 == S_DIS_CANCER4|S_DIS_CANCER6 == A_DIS_CANCER3|
           S_DIS_CANCER6 == A_DIS_CANCER2|S_DIS_CANCER6 == A_DIS_CANCER1,S_DIS_CANCER6_b:=NA]



ctype <- opal.variable(o,"CoreQx_Jan2016","Coreqx_final_feb16_BCGP1_copyMay16","A_DIS_CANCER1")
cancer <- do.call(function(x){
  tmp1 = x$categories
  tmp2 = tmp1  %>% unlist %>% matrix(ncol=length(tmp1)) %>% .[c(1,4),] %>% t
  tmp3 = array("",dim=list(length(tmp1)+1,2)) ; #tmp3[1,1] <- c(label) ; 
  tmp3[,2] <- c(tmp2[,1],"NA") ; tmp3[,1] <- c(tmp2[,2],"Missing") ; 
  colnames(tmp3) <- c("Category","Code") ; 
  t = data.table(tmp3) ; setkey(t,Category)
  return (t)
},list(ctype)) ; 
cancer$Category[c(10,13,14,20)] <- c('Leukemia','Non-Hodgkins Lymphoma','Not applicable','Skin')
setkey(cancer,Category)

cvar = c("A_DIS_CANCER1","A_DIS_CANCER2_b","A_DIS_CANCER3_b","S_DIS_CANCER4_b","S_DIS_CANCER5_b","S_DIS_CANCER6_b")

wb <- createWorkbook(type = "xlsx")
lapply(c('ohs_full','cag_full','bcgp_full','atp_full','atl_full'),function(i){
  
sheet = createSheet(wb, sheetName=i)
z = eval(parse(text=i))
p = lapply(z[,cvar,with=F],function(y){
a = lapply(cancer$Category,function(x){
    ifelse(cancer[x,Code] %in% levels(factor(y)),
         tmp <- cbind(c(x,x),c("Male","Female"),
                      t(t(table(y,z$A_SDC_GENDER,useNA ="ifany")[cancer[x,Code],]))),
         tmp <- NA)
  return (tmp)
}) ; b = Reduce(rbind,a[which(!is.na(a))]) ; ifelse(is.null(b),NA,colnames(b) <- c('Cancer','Gender',paste0("Count_",i)))
return (data.table(b))}) ; p <- Filter(function(x){return (length(x) !=0)},p) ; 
for (k in 1:length(p)) {names(p[[k]])[3] <- paste0(names(p[[k]])[3],"_",attributes(p)$names[k])}

cc <- Reduce(function(x,y){return (merge(x,y,by=c("Cancer","Gender"),all=T))},p)

#colSums(r[[1]][,3:ncol(r[[1]])],dims = 1)
#colSums(cc[,3:ncol(cc)])

addDataFrame(cc, sheet, col.names=T, row.names=F,
             showNA=T, characterNA="", byrow=FALSE)
})
saveWorkbook(wb, 't.xlsx')

#doing it one dataset at a time
p = lapply(atl_full[,c(cvar),with=F],function(y){
  a = lapply(cancer$Category,function(x){
    ifelse(cancer[x,Code] %in% levels(factor(y)),
           tmp <- cbind(c(x,x),c("Male","Female"),
                        t(t(table(y,atl_full$A_SDC_GENDER,useNA ="ifany")[cancer[x,Code],]))),
           tmp <- NA)
    return (tmp)
  }) ; b = Reduce(rbind,a[which(!is.na(a))]) ; ifelse(is.null(b),NA,colnames(b) <- c('Cancer','Gender',"Count"))
  return (data.table(b))}) ; p <- Filter(function(x){return (length(x) !=0)},p)

atl_full_cc <- Reduce(function(x,y){return (merge(x,y,by=c("Cancer","Gender"),all=T))},p)
atl_full_cc %>% write.csv(.,"atl_full.csv")



####enrollment count

ts = subset(nci,grepl("OHS",nci$A_ADM_STUDY_DATASET,ignore.case = T))
atl_ethn <- Reduce(rbind,list(
  do.call(function(x){
    
    tmp = Reduce(cbind,lapply(c("1","2"),function(g){
      tmp = c(
        with(ts[MLTP=="FALSE",],table(S_SDC_EB_LATIN,S_SDC_EB_ABORIGINAL,A_SDC_GENDER)[x,"1",g]),
        with(ts[MLTP=="FALSE",],table(S_SDC_EB_LATIN,ASIAN,A_SDC_GENDER)[x,"TRUE",g]),
        with(ts[MLTP=="FALSE",],table(S_SDC_EB_LATIN,S_SDC_EB_BLACK,A_SDC_GENDER)[x,"1",g]),
        with(ts[MLTP=="FALSE",],table(S_SDC_EB_LATIN,S_SDC_EB_WHITE,A_SDC_GENDER)[x,"1",g]),
        with(ts[MLTP=="FALSE",],table(S_SDC_EB_LATIN,S_SDC_EB_LATIN,A_SDC_GENDER)[x,"1",g]),
        with(ts[MLTP=="FALSE",],table(S_SDC_EB_LATIN,OTHER,A_SDC_GENDER)[x,"TRUE",g]),
        with(ts[MLTP=="TRUE",],table(S_SDC_EB_LATIN,A_SDC_GENDER)[x,g]))
      return (tmp)}))
    colnames(tmp) = c("Male","Female") ; 
    rownames(tmp) <- c('American Indian/Alaska Native','Asian','Black or African-American',
                 'White','Hispanic or Latino','Other ethnic background','Multiple ethnicity')
    return (tmp)},list("0")),
  
  do.call(function(x){
    
    tmp = Reduce(cbind,lapply(c("1","2"),function(g){
      tmp = c(
        with(ts[MLTP=="FALSE",],table(S_SDC_EB_LATIN,S_SDC_EB_ABORIGINAL,A_SDC_GENDER)[x,"1",g]),
        with(ts[MLTP=="FALSE",],table(S_SDC_EB_LATIN,ASIAN,A_SDC_GENDER)[x,"TRUE",g]),
        with(ts[MLTP=="FALSE",],table(S_SDC_EB_LATIN,S_SDC_EB_BLACK,A_SDC_GENDER)[x,"1",g]),
        with(ts[MLTP=="FALSE",],table(S_SDC_EB_LATIN,S_SDC_EB_WHITE,A_SDC_GENDER)[x,"1",g]),
        with(ts[MLTP=="FALSE",],table(S_SDC_EB_LATIN,S_SDC_EB_LATIN,A_SDC_GENDER)[x,"1",g]),
        with(ts[MLTP=="FALSE",],table(S_SDC_EB_LATIN,OTHER,A_SDC_GENDER)[x,"TRUE",g]),
        with(ts[MLTP=="TRUE",],table(S_SDC_EB_LATIN,A_SDC_GENDER)[x,g]))
      return (tmp)}))
    colnames(tmp) = c("Male","Female") ; 
    rownames(tmp) <- c('American Indian/Alaska Native','Asian','Black or African-American',
                       'White','Hispanic or Latino','Other ethnic background','Multiple ethnicity')
    return (tmp)},list("1")),
  do.call(function(x){
    tmp = with(ts,table(UNKW,A_SDC_GENDER)["TRUE",]) %>% data.frame %>% t
    colnames(tmp) <- c("Male","Female") ; rownames(tmp) <- x
    return (tmp)}
    ,list("Unknown"))
  ))

bcgp_ethn %>% write.csv(.,"bcgp.csv")
ohs_ethn %>%  write.csv(.,"ohs.csv")
atp_ethn %>%  write.csv(.,"atp.csv")
atl_ethn %>%  write.csv(.,"atl.csv")
cag_ethn %>%  write.csv(.,"cag.csv")