##
## Dataset exploration : Test of ranges
##

#clean
rm(list = ls(all.names = TRUE))
cat("\014")

#load packages
require(data.table);require(opal);require(magrittr);require('Hmisc');require(foreign)

#connect to database
source('~/scripts/opal-login.r')


# Looking at data ranges
fstat_cont = function(x,y){ 
  temp1 = array(dim=c(length(pm_vars$Names),length(x)))
  for (j in 1:length(pm_vars$Names)) {
    temp0 = data.table(eval(parse(text=paste0(x,"$",pm_vars$Names[j]))))
    N0 = nrow(temp0)
    if (!is.element(pm_vars$Names[j],c('PM_BD_TSCORE','PM_BD_ZSCORE'))) {temp2 = temp0[V1!='0' & V1!='999']} else
    {temp2 = temp0[V1!='999']}
    N1 = nrow(temp2)
    min = eval(parse(text=paste0(y,"$",'Min[j]')))
    max = eval(parse(text=paste0(y,"$",'Max[j]')))
    temp1[j] =  if (sum(is.na(temp0))==N0) {""} else
    {
      if (is.na(min) & is.na(max)) {paste0("No dd range"," (",round(min(na.omit(temp2)),digits=2)," - ",round(max(na.omit(temp2)),digits=2),")")} else {
        paste0(eval(parse(text=paste0(y,"$",'Min[j]')))," < x < ",eval(parse(text=paste0(y,"$",'Max[j]'))),
      " (",round(min(na.omit(temp2)),digits=2)," - ",round(max(na.omit(temp2)),digits=2),")")}
  
    }}
    return (temp1)}
  desc_pm=data.frame(mapply(fstat_cont,studies,'pm_vars'))
  rownames(desc_pm)=pm_vars$Names
write.csv(desc_pm, "desc_pm.csv")

# Calculating How Many Missings Generated When Applying Ranges
fstat_cont = function(x,y){ 
  temp1 = array(dim=c(length(pm_vars$Names),length(x)))
  for (j in 1:length(pm_vars$Names)) {
    temp0 = data.table(eval(parse(text=paste0(x,"$",pm_vars$Names[j]))))
    N0 = nrow(temp0)
    if (!is.element(pm_vars$Names[j],c('PM_BD_TSCORE','PM_BD_ZSCORE'))) {temp2 = temp0[V1!='0' & V1!='999']} else
    {temp2 = temp0[V1!='999']}
    N1 = nrow(temp2)
    min = eval(parse(text=paste0(y,"$",'Min[j]')))
    max = eval(parse(text=paste0(y,"$",'Max[j]')))
    temp2b = temp2[V1<min | V1>max]
    N2 = nrow(temp2b)
    temp1[j] =  if (sum(is.na(temp0))==N0 | is.na(min) & is.na(max)) {""} else {N2}
  }
  return (temp1)}
desc_pm=data.frame(mapply(fstat_cont,studies,'pm_vars'))
rownames(desc_pm)=pm_vars$Names
write.csv(desc_pm, "desc_pm.csv")

