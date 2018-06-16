rm(list = ls(all.names = TRUE))
cat("\014")
require(opal);require(xlsx);require(XLConnect)
require(data.table);require(opal);require(magrittr);require('Hmisc');require(foreign)

opvars = c('oltc','arthritis','cancer','cancer_t','language','other_tobacco_ever',
           'other_tobacco_current','current_work_sched','longest_work_sched',
           'cancer_t_fam','birth_country','birth_country_fam','birth_country_mother_fam','birth_country_father_fam')

##########Importing Meta-Data
meta=list()
wb <- loadWorkbook("var.xlsx")
for (d in 1:length(opvars)) {
  
  t <- readWorksheet(wb, sheet=d,check.names = F)
  tmp0 = paste0("meta$",opvars[d])
  tmp1 = lapply(1:ncol(t), function(x) {var = list(); var['project']=names(t[x]);var['table']=as.character(t[1,x]);
                                        tmp = strsplit(as.character(t[2,x]),split = ",");tmp=trimws(unlist(tmp));
                                        var['variables']=list(tmp);return(var)})
  eval(parse(text=paste0(tmp0,"=tmp1")))
}
save.image('opt')
###########

#############Importing Data
library(opal)
library(xlsx)
o <- opal.login('yamadou','Na96wal2','https://cptp-harmo.oicr.on.ca:8443/')
DATA=list()
for (d in 1:length(meta)) {
  ds = list()
  tmp0 = paste0("DATA$",opvars[d])
  ds = lapply(1:length(meta[[d]]), function(k) {
    
    if (!is.element('TRUE',is.na(meta[[d]][[k]]$variables))) {
      v = paste0("name().matches(",paste0("/(^",paste0(meta[[d]][[k]]$variables,collapse="$)|(^"),"$)/"),")")
      opal.assign(o,'cptp',paste0(meta[[d]][[k]]$project,".",meta[[d]][[k]]$table),variables=v,id.name = "entity_id")
      t0 = data.table(opal.execute(o,'cptp'))} else {t0=NULL}
    return (t0)})
  eval(parse(text=paste0(tmp0,"=ds")))
}
save.image('opt_new')
###########



#############CLeaning open-text
#textd = list()
#for (j in 1:length(DATA)) {
#  text = lapply(1:length(DATA[[j]]), function(k){
#    if (!is.null(DATA[[j]][[k]])) {
#    level = lapply(1:ncol(DATA[[j]][[k]]), function(x) {levels(eval(parse(text=paste0("DATA[[j]][[k]]$",colnames(DATA[[j]][[k]])[x]))))})}
#    else {level=NULL}
#    return (unlist(level))})
#  optext = trimws(unlist(text));
#  t = trimws(levels(as.factor(optext)))
#  #l1 = list(toupper(levels(as.factor(optext))))
#  #l2 = list(levels(as.factor(toupper(optext))))
#  #p = lapply(1:length(l2[[1]]),function(z) {temp = which(is.element(l1[[1]],l2[[1]][z]))[1]; return(temp)})
#  #t = levels(as.factor(optext))[unlist(p)]
#  T = data.table(t) ; colnames(T) = opvars[j]
#  
#  tmp0 = paste0("textd$",opvars[j])
#  eval(parse(text=paste0(tmp0,"=T")))
#}

textdb = llply(DATA,function(x){
a = llply(Filter(function(x) !is.null(x),x),function(x){
  Reduce(list('c','trimws'),lapply(x,levels))
}) %>% unlist %>% trimws %>% factor %>% levels}) ; 
WriteXLS::WriteXLS(textdb,"text.xls", SheetNames = names(a),row.names = F,na = "")

save.image('~/data/opt')    


write.xlsx(textd[[1]],file = "text.xlsx",sheetName = opvars[1],showNA = FALSE,row.names = FALSE)
for (i in 2:length(textd)) {
  write.xlsx(textd[[i]],file = "text.xlsx",sheetName = opvars[i],showNA = FALSE,row.names = FALSE,append = TRUE)}

#END

