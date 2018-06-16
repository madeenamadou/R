##
## Get samples and participants counts for all studies
##

source('~/scripts/default.r')
require(data.table);require(opal);require(magrittr);require('Hmisc');require(foreign);require(lubridate)



fcatg = function(x){
  a = opal.variable(o,fd,ds,x)$categories
  b = a  %>% unlist %>% matrix(ncol=length(a)) %>% .[c(1,4),] %>% t
  c = array("",dim=list(length(a)+1,2)) ; #tmp3[1,1] <- c(label) ; 
  c[,2] <- c(b[,1],"NA") ; c[,1] <- c(b[,2],"Missing") ; 
  colnames(c) <- c("Category","Code") ; 
  d = data.table(c) ; setkey(d,Code)
  return (d)}

f <- function(table,var) {
  return (lapply(var,function(x) {
    tmp = eval(parse(text=paste0(table,'$',x)));
    tmp1 = table(tmp,useNA = "ifany") %>% data.table;
    tmp1[is.na(tmp),tmp:="NA"] ; names(tmp1)[1]<-'Code'
    tmp12 = do.call(fcatg,list('SAMPLE_TYPE'))
    tmp2 = Reduce(rbind,lapply(tmp1$Code,function(y){
      b = data.table(Code=y,
                     sample=eval(parse(text=paste0(table,'[',x,'==y,.N]'))),
                     participants=eval(parse(text=paste0(table,'[',x,'==y,key(t),with=F]'))) %>% unique %>% nrow)
      return (b)})) ; setkey(tmp2,'Code')
    tmp22 = merge(tmp2,tmp12,by='Code',all.x=TRUE);
    tmp22[is.na(Category),Category:=Code][,Code:=NULL]; setcolorder(tmp22,c('Category','sample','participants'))
    colnames(tmp22) <- c("Sample Type","# sample","# participants")
    return (tmp22)
  }))}


f <- function(table,var) {
  return (lapply(var,function(x) {
    a = eval(parse(text=paste0(table,'[!trimws(',x,')%in% c(\'9\',\'10\',\'2\'),]')))
    #a = eval(parse(text=paste0(table)))
    b = data.table(study = table,
                   participants = unique(a,by=key(a)) %>% nrow)
    colnames(b)[2] <- '# of participants w/ venous blood sample'
    return (b)
  }))}

Reduce(rbind,list(
  do.call(f,list(c('atl'),'SAMPLE_TYPE'))[[1]],
  do.call(f,list(c('bcgp'),'Sample_Type'))[[1]],
  do.call(f,list(c('cag'),'SAMPLE_TYPE'))[[1]],
  do.call(f,list(c('ohs_b'),'Sample_TYPE'))[[1]],
  do.call(f,list(c('ohs_p'),'SampleType'))[[1]],
  do.call(f,list(c('atp_blood'),'aliquot_type'))[[1]])) %>% 
  WriteXLS::WriteXLS(., ExcelFileName = "t.xls",SheetNames = 'Sheet1')







#ATL
opal.assign.table(o,'cptp','Atlantic_R2_SAMPLE.AtlanticPATHBiosamples_October3_2016_noNAs', 
                  variables = list("tblBiosamples_HARMONIZATION_ID","SAMPLE_TYPE"),
                  id.name = "entity_id")
t <- opal.execute(o,'cptp') %>% data.table(.,key="tblBiosamples_HARMONIZATION_ID")
stud = 't'
fd = 'Atlantic_R2_SAMPLE'
ds = 'ATL_Sample'
atl = do.call(f,list('t','SAMPLE_TYPE'))

t[SAMPLE_TYPE==5|SAMPLE_TYPE==6,.N]
t[SAMPLE_TYPE==5|SAMPLE_TYPE==6,unique(tblBiosamples_HARMONIZATION_ID)] %>% length

t[!SAMPLE_TYPE %in% c('2','9','10'),unique(tblBiosamples_HARMONIZATION_ID)] %>% length

Reduce(union,
       list(t[SAMPLE_TYPE %in% c('9'),unique(tblBiosamples_HARMONIZATION_ID)],
            t[SAMPLE_TYPE %in% c('2'),unique(tblBiosamples_HARMONIZATION_ID)],
            t[!SAMPLE_TYPE %in% c('2','9','10'),unique(tblBiosamples_HARMONIZATION_ID)])) %>% length

Reduce(union,
       list(t[SAMPLE_TYPE %in% c('2'),unique(tblBiosamples_HARMONIZATION_ID)],
            t[!SAMPLE_TYPE %in% c('2','9','10'),unique(tblBiosamples_HARMONIZATION_ID)])) %>% length



#BCGP
opal.assign.table(o,'cptp','BCGP_R2_SAMPLE.Sample4harmonization_N623203', 
                  variables = list("Subject","Sample_Type"),
                  id.name = "entity_id")
t <- opal.execute(o,'cptp') %>% data.table(.,key="Subject")

t[,Sample_Type_mapped := plyr::mapvalues(Sample_Type,
                                         c('4','5','6','10','7','11','8'),
                                         c('1','3','4','5','7','9','10'))]

stud = 't'
fd = 'BCGP_R2_SAMPLE'
ds = 'BCGP_Sample'
bcgp = do.call(f,list('t','Sample_Type_mapped'))

t[Sample_Type_mapped==5|Sample_Type_mapped==6,.N]
t[Sample_Type_mapped==5|Sample_Type_mapped==6,unique(Subject)] %>% length

t[!Sample_Type_mapped %in% c('2','9','10'),unique(Subject)] %>% length

Reduce(union,
       list(t[Sample_Type_mapped %in% c('9'),unique(Subject)],
            t[Sample_Type_mapped %in% c('2'),unique(Subject)],
            t[!Sample_Type_mapped %in% c('2','9','10'),unique(Subject)])) %>% length



#BCGP-deriv-saliva
opal.assign.table(o,'cptp','BCGP_R2_SAMPLE.T3_SampleDeriv_Saliva', 
                  variables = list("DERIV_TYPE","DERIV_SAMPLE_ID"),
                  id.name = "entity_id")
t <- opal.execute(o,'cptp') %>% data.table(.,key="DERIV_SAMPLE_ID")

stud = 't'
fd = 'BCGP_R2_SAMPLE'
ds = 'BCGP_Sample'
bcgp_deriv_sal = do.call(f,list('t','DERIV_TYPE'))


tsal[DERIV_TYPE==11,.N]
tsal[DERIV_TYPE==11,unique(Subject)] %>% length 

#74,38

#BCGP-deriv-buffy
opal.assign.table(o,'cptp','BCGP_R2_SAMPLE.T3_SampleDeriv_BuffyCoat', 
                  variables = list("DERIV_TYPE","DERIV_SAMPLE_ID"),
                  id.name = "entity_id")
t <- opal.execute(o,'cptp') %>% data.table(.,key="entity_id")

stud = 't'
fd = 'BCGP_R2_SAMPLE'
ds = 'BCGP_Sample'
bcgp_deriv_buffy = do.call(f,list('t','DERIV_TYPE'))

tbuff[DERIV_TYPE==11,.N]
tbuff[DERIV_TYPE==11,unique(Subject)] %>% length

#384, 384

#CAG
opal.assign.table(o,'cptp','CAG_R2_SAMPLE.cptp_sample_472430', 
                  variables = list("ADM_PART_ID","SAMPLE_TYPE"),
                  id.name = "entity_id")
t <- opal.execute(o,'cptp') %>% data.table(.,key="ADM_PART_ID")

stud = 't'
fd = 'CAG_R2_SAMPLE'
ds = 'CAG_Sample'
cag = do.call(f,list('t','SAMPLE_TYPE'))

t[SAMPLE_TYPE==5|SAMPLE_TYPE==6,.N]
t[SAMPLE_TYPE==5|SAMPLE_TYPE==6,unique(ADM_PART_ID)] %>% length

t[!SAMPLE_TYPE %in% c('2','9','10'),unique(ADM_PART_ID)] %>% length

Reduce(union,
       list(t[SAMPLE_TYPE %in% c('9'),unique(ADM_PART_ID)],
            t[SAMPLE_TYPE %in% c('2'),unique(ADM_PART_ID)],
            t[!SAMPLE_TYPE %in% c('2','9','10'),unique(ADM_PART_ID)])) %>% length


#OHS-BASELINE
opal.assign.table(o,'cptp','OHS_R2_SAMPLE.CPTP_OHS_Biosample_Primary_By_SampleID_20161018', 
                  variables = list("CPAC_PatientID","Sample_TYPE"),
                  id.name = "entity_id")
t <- opal.execute(o,'cptp') %>% data.table(.,key="CPAC_PatientID")

stud = 't'
fd = 'OHS_R2_SAMPLE'
ds = 'OHS_Sample'
ohs_b = do.call(f,list('t','Sample_TYPE'))

t[Sample_TYPE=='Plasma, single spun',.N] #43146
t[Sample_TYPE=='Plasma, single spun',unique(CPAC_PatientID)] %>% length #21585

t[!Sample_TYPE %in% c('Urine'),unique(CPAC_PatientID)] %>% length #21600

Reduce(union,
       list(t[!Sample_TYPE %in% c('Urine'),unique(CPAC_PatientID)])) %>% length #21600




#OHS-PILOT
opal.assign.table(o,'cptp','OHS_R2_SAMPLE.ohs_pilot_biosample_20170124_upd', 
                  variables = list("CPTPID","SampleType"),
                  id.name = "entity_id")
t <- opal.execute(o,'cptp') %>% data.table(.,key="CPTPID")

stud = 't'
fd = 'OHS_R2_SAMPLE'
ds = 'OHS_Pilot_Sample'
ohs_p = do.call(f,list('t','SampleType'))

t[SampleType=='Plasma',.N] #45718
t[SampleType=='Plasma',unique(CPTPID)] %>% length #7630

t[!SampleType %in% c('Saliva','Urine'),unique(CPTPID)] %>% length #7658

Reduce(union,
       list(t[SampleType %in% c('Saliva'),unique(CPTPID)],
            t[!SampleType %in% c('2','Saliva','Urine'),unique(CPTPID)])) %>% length #7824

#ATP-BLOOD
opal.assign.table(o,'cptp','TTP_R2_SAMPLE.ATP_LIMS_BLOOD', 
                  variables = list("cpac_key","aliquot_type"),
                  id.name = "entity_id")
t <- opal.execute(o,'cptp') %>% data.table(.,key="cpac_key")

stud = 't'
fd = 'TTP_R2_SAMPLE'
ds = 'ATP_Sample'
atp_blood = do.call(f,list('t',c('aliquot_type')))

t[aliquot_type=='PLASMA',.N]
t[aliquot_type=='PLASMA',unique(cpac_key)] %>% length

t[,unique(cpac_key)] %>% length

Reduce(union,
       list(tblood[,unique(cpac_key)],
            tsal[,unique(entity_id)])) %>% length


#ATP-URINE
opal.assign.table(o,'cptp','TTP_R2_SAMPLE.ATP_LIMS_urine_wo_lf', 
                  variables = list("cpac_key","aliquot_type"),
                  id.name = "entity_id")
t <- opal.execute(o,'cptp') %>% data.table(.,key="cpac_key")

atp_urine = do.call(f,list('t',c('aliquot_type')))

#ATP-SALIVA
opal.assign.table(o,'cptp','TTP_R2_SAMPLE.CPAC_SALIVA', 
                  variables = list("SAMPLE_TYPE"),
                  id.name = "entity_id")
t <- opal.execute(o,'cptp') %>% data.table(.,key="entity_id")

atp_saliva = do.call(f,list('t',c('SAMPLE_TYPE')))

c(atl,bcgp,cag,ohs_b,ohs_p,atp_blood,atp_urine,atp_saliva) %>% 
  WriteXLS::WriteXLS(., ExcelFileName = "t.xls", 
                     SheetNames = c('atl','bcgp','cag','ohs_b','ohs_p','atp_blood','atp_urine','atp_saliva'),na = "NA")




#---------------------------
