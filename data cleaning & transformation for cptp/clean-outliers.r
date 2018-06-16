##
## Clean outliers from dataset
##

#clean
rm(list = ls(all.names = TRUE))
cat("\014")

#load packages
require(data.table);require(opal);require(magrittr);require('Hmisc');require(foreign)

atlp <- '/home/yamadou/export/Atlantic_R2-20170220125933/ATL_Prelimqx/data.csv' %>%  file.path(.) %>% 
  opal.file(o,.) %>% read.csv(text=.) %>% data.table(.)

out =  xlsx::read.xlsx("~/ATL_.xlsx",sheetIndex = 1,startRow = 5) %>% data.table
out <- out[,lapply(.SD,function(x) as.character(x)),.SDcols = 1:ncol(out)]

fd = 'Atlantic_R2'; ds = 'ATL_Measurements'

a=Reduce(rbind,lapply(opal.variables(o,fd,ds),function(x){
  a = opal.attribute_values(x$attributes,name='script')
  b = stringr::str_extract_all(a,"['][a-zA-Z_.-]+[']") %>% unlist %>% gsub("['/]",'',.)
  c = data.table(rep(x$name,length(b)),b)
  return (c)
})) ; names(a) <- c("HARMO","SOURCE") ; setkey(a,HARMO)

tab = c('AtlanticPath_PhysicalMeasuresSet1_April18_2016',
        'AtlanticPATH_PhysicalMeasuresSet2_Jan27_2017_Final')

var <- lapply(tab,function(x){
  sapply(opal.variables(o,'Atlantic_R2',x),function(x) x$name)})

opal.assign.table(o,'cptp','Atlantic_R2.AtlanticPath_PhysicalMeasuresSet1_April18_2016',id.name = "entity_id");
ds1 <- opal.execute(o,'cptp') %>% data.table
opal.assign.table(o,'cptp','Atlantic_R2.AtlanticPATH_PhysicalMeasuresSet2_Jan27_2017_Final',id.name = "entity_id")
ds2 <- opal.execute(o,'cptp') %>% data.table

ids = list(ds1 %>% .[,entity_id],ds2 %>% .[,entity_id])

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