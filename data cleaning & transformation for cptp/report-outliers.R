##
## Report outliers
##

#clean
rm(list = ls(all.names = TRUE))
cat("\014")

#load packages
require(data.table);require(opal);require(magrittr);require('Hmisc');require(foreign)

##create objects required
load('~/data/cptp-PM-table')

atlid = fread('~/data/reorg/idmap/Participant-atl_r2.csv',
              colClasses = 'character',col.names = c("Participant","atl_r2")) %>% data.table ;

cagH <- "/home/yamadou/export/Views/CaG/CAG_Sample/data.csv" %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.) %>% data.table(.)
class(atl$entity_id) <- 'character'

ohs_samp_new <- "/home/chenwei/Release 2/Biosample/Data up to May 31, 2016/CPTP_OHS_Biosample_Primary_By_SampleID_20161116.csv" %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.) %>% data.table(.)




nrow(t)
unique(t$CPAC_PatientID) %>% length
unique(t$CPAC_Source_ID) %>% length
unique(t$CPAC_Sample_ID) %>% length
grep("ID",names(t),ignore.case = TRUE,value = TRUE)




##create the main function

f = function(y,z) {
  
  sheet = createSheet(wb, sheetName=toupper(y))
  c = createCell(createRow(sheet, rowIndex=1:5), colIndex=1:7)
  cs=CellStyle(wb) + Font(wb, isBold=TRUE)
  mapply(setCellStyle,c[1:3,1],list('1'=cs + Font(wb,color="red",underline = TRUE),'2'=cs,'3'=cs))
  
  mapply(setCellValue, c[c(1:3),1], c(paste("Cohort Dataset :",toupper(y)),"List of participants with outliers values",
                                      paste(months(Sys.Date()),year(now()))))
  
  mapply(setCellValue, c[5,], c("Variable Name","Variable label","Criteria","Source ID","Opal ID","Value","Correction"))
  sapply(c[5,1:7],setCellStyle,cs)
  createFreezePane(sheet, rowSplit = 6, colSplit = 2)
  
  r=6
  col = "cyan"
  
  
  for (j in 1:length(pm_vars$Names)) {
    temp00 = eval(parse(text=paste0(y,"[,list(entity_id,",pm_vars$Names[j],")]")))
    colnames(temp00) = c("Participant","V2")
    temp0 = merge(z,temp00,by="Participant",all.y=T,all.x=F)
    
    if (!is.element(pm_vars$Names[j],c('PM_BD_TSCORE','PM_BD_ZSCORE'))) {temp1 = temp0[V2!='0' & V2!='999',]} else
    {temp1 = temp0[V2!='999',]}
    
    min = pm_vars$Min[j]
    max = pm_vars$Max[j]
    
    temp2 = temp1[V2<min | V2>max,]
    colnames(temp2) = c("V0","V1","V2")
    
    
    if (nrow(temp2)==0) {t=array(dim=c(1,7))} else {t=array(dim=c(nrow(temp2),7))}
    if (is.na(min) & is.na(max)) {t[1,c(1:3)] = c(pm_vars$Names[j],
                                                  opal.attribute_values(opal.variable(o, "Atlantic_R2",'ATL_Measurements',pm_vars$Names[j])$attributes,name="label"),
                                                  paste0("No range"))} else 
                                                  {t[1,c(1:3)] = c(pm_vars$Names[j],
                                                                   opal.attribute_values(opal.variable(o, "Atlantic_R2",'ATL_Measurements',pm_vars$Names[j])$attributes,name="label"),
                                                                   paste0("'",min,"<x<",max,"'"))}
    
    if (nrow(temp2)==0) {t[,4:5]=NA;t[,6]="No outliers"} else 
    {t[,c(4:6)]=temp2[,c(V0,V1,V2)]}
    
    cs0 <- CellStyle(wb) + Fill(backgroundColor = col,foregroundColor = col)
    
    addDataFrame(t, sheet, col.names=FALSE, row.names=FALSE,
                 startRow=r, startColumn=1,
                 colStyle=list('1'=cs0 + Font(wb, isItalic=TRUE),
                               '2'=cs0,
                               '3'=cs0 + Alignment(h="ALIGN_CENTER"),
                               '4'=cs0 + Alignment(h="ALIGN_CENTER"),
                               '5'=cs0 + Alignment(h="ALIGN_CENTER"),
                               '6'=cs0 + Alignment(h="ALIGN_CENTER"),
                               '7'=cs0 + Alignment(h="ALIGN_CENTER")),
                 colnamesStyle=CellStyle(wb, font=Font(wb, isBold=TRUE)),
                 showNA=FALSE, characterNA="", byrow=FALSE)
    r = r+nrow(t)
    col = c("cyan","white")[j %% 2+1]
    
  }
  
  saveWorkbook(wb, 'RangeOverView.xlsx')
  
}

##create the xlsx workbook
wb <- createWorkbook(type = "xlsx")

##fill the workbook with the main function output
lapply(c('ohs_ac','ohs_lsc','ohs_p'),f,ohsid)
lapply(c('cag'),f,cagid)
lapply(c('bcgp'),f,bcgpid)
lapply(c('atl'),f,atlid)
lapply(c('ttp1','ttp2'),f,atpid)


##create some files to document the correction process

out = xlsx::read.xlsx("~/RangeOverView.xlsx",sheetIndex = 1,startRow = 5,colClasses = 'character') %>% data.table
out =  xlsx::read.xlsx("~/ATL_.xlsx",sheetIndex = 1,startRow = 5,colClasses = 'character') %>% data.table
out <- out[,lapply(.SD,function(x) as.character(x)),.SDcols = 1:ncol(out)]

fd = 'Atlantic_R2'; ds = 'ATL_Measurements'

a = Reduce(rbind,lapply(opal.variables(o,fd,ds),function(x){
  a = opal.attribute_values(x$attributes,name='script')
  b = stringr::str_extract_all(a,"'[0-9A-Z_.-]+'") %>% unlist %>% gsub("['/]",'',.)
  c = data.table(rep(x$name,length(b)),b)
  return (c)
})) ; names(a) <- c("HARMO","SOURCE") ; setkey(a,HARMO)

tab = c('AtlanticPath_PhysicalMeasuresSet1_April18_2016',
        'AtlanticPATH_PhysicalMeasuresSet2_Jan27_2017_Final')

opal.assign.table(o,'cptp','Atlantic_R2.AtlanticPath_PhysicalMeasuresSet1_April18_2016',id.name = "entity_id");
ds1 <- opal.execute(o,'cptp') %>% data.table
opal.assign.table(o,'cptp','Atlantic_R2.AtlanticPATH_PhysicalMeasuresSet2_Jan27_2017_Final',id.name = "entity_id")
ds2 <- opal.execute(o,'cptp') %>% data.table

ids = list(ds1 %>% .[,entity_id],ds2 %>% .[,entity_id])

var <- lapply(tab,function(x){
  sapply(opal.variables(o,'Atlantic_R2',x),function(x) x$name)})

a[,TAB := sapply(SOURCE,function(x){
  a = which(sapply(var,function(y) intersect(x,y) %>% length!=0))
  return (ifelse(length(a)!=0,tab[a],ifelse(x %in% HARMO,NA,'name error')))
})]; 
a[TAB=='name error',] %>% nrow ; 
a[TAB=='name error',SOURCE := c("RES_T.SCORE","RES_Z.SCORE")]
out = out[,list(Variable.Name,Source.ID,Opal.ID,Value)]

out[,dataset := sapply(Opal.ID,function(x){
  a = which(sapply(ids,function(y) intersect(x,y) %>% length!=0))
  return (ifelse(length(a)>1,'duplicata',tab[a]))
})]
names(a)[c(1,3)] <- c("Variable.Name","dataset")
setkeyv(a,c("Variable.Name","dataset")) ; setkeyv(out,c("Variable.Name","dataset"))
out = merge(out,a,by=c("Variable.Name","dataset"))

t = out[complete.cases(out),] ; setkey(t,'SOURCE')

tb = Reduce(rbind,lapply(unique(t$SOURCE),function(x){
  t[x,list(Variable.Name,dataset,Opal.ID,Source.ID,Value,SOURCE,
           mapping=sapply(Opal.ID,function(x) ifelse(x!="",paste0("'",x,"':null,"),NA)))]
})) ; setkey(tb,'SOURCE')

tbb = Reduce(rbind,lapply(unique(tb$SOURCE),function(x){
  a = tb[x,]
  b = a[1,list(Variable.Name,dataset,SOURCE,
               correction=paste0("$id().map({",Reduce(paste,tb[x,mapping]),"})"))]
  return (b)
}))

xlsx::write.xlsx(tbb,"outliers.xlsx",sheetName = 'opal',showNA = T,append = F,row.names = F)
xlsx::write.xlsx(tb,"outliers.xlsx",sheetName = 'liste',showNA = T,append = T,row.names = F)
