library(shiny);library(magrittr);library(data.table);library(ggplot2);library(plyr)

variables_meta <- opal.variables(o,"CAG_R2_SAMPLE","CAG_Sample")

var <- sapply(variables_meta,function(x){return (x$name)})

datetime = sapply(Filter(function(x){return (grepl("(^yyyy/MM/dd HH:mm$)|(^yyyy/MM/dd$)",x$unit,ignore.case = TRUE))},
                         variables_meta),function(x){return (x$name)})
cat = sapply(Filter(function(x){return (!is.null(x$categories) && length(x$categories) >=2)},
                    variables_meta),function(x){return (x$name)})
cont = sapply(Filter(function(x){return (length(x$categories)==0 && grepl("integer|decimal",x$valueType,ignore.case = TRUE))},
                     variables_meta),function(x){return (x$name)})
time = sapply(Filter(function(x){return (grepl("^HH:mm$",x$unit,ignore.case = TRUE))},
                     variables_meta),function(x){return (x$name)})
misc1 = sapply(Filter(function(x){return (grepl("text",x$valueType,ignore.case = TRUE) && x$unit=="")},
                      variables_meta),function(x){return (x$name)})
misc2 = names(atlH)[which(!names(atlH) %in% Reduce(c,list(cont,cat,datetime,time,misc1)))]

var_label <- data.table(Reduce(rbind,lapply(variables_meta,function(x){
  return (c(x$name,opal.attribute_values(x$attributes,name='label')))
  }))) ; names(var_label) <- c("Name","Label") ; setkey(var_label,Name)

cat_catg <- lapply(Filter(function(x){return (!is.null(x$categories) && length(x$categories) >=2)},
                          variables_meta),function(x){
                            tmp1 = x$categories
                            tmp2 = tmp1  %>% unlist %>% matrix(ncol=length(tmp1)) %>% .[c(1,4),] %>% t
                            tmp3 = array("",dim=list(length(tmp1)+1,2)) ; #tmp3[1,1] <- c(label) ; 
                            tmp3[,2] <- c(tmp2[,1],"NA") ; tmp3[,1] <- c(tmp2[,2],"Missing") ; 
                            colnames(tmp3) <- c("Category","Code") ; 
                            t = data.table(tmp3) ; setkey(t,Code)
                            return (t)
                          })

tabid <- data.table(c('CaG','OHS','ATP','BCGP','ATL'),
                   c('CAG_R2_SAMPLE.CAG_Sample','OHS_R2_SAMPLE.OHS_Sample','TTP_R2_SAMPLE.ATP_Sample',
                     'BCGP_R2_BCGP_Sample','Atlantic_R2_SAMPLE.ATL_Sample'),
                   c('cagH',"ohsH","atpH","bcgpH","atlH")) ; names(tabid) <- c("N","D","DS") ; setkey(tabid,N)

sup20 <- Filter(function(x) {
  tmp0 = eval(parse(text=paste0(stud,"$",x)))
  return (length(levels(factor(tmp0))) > 20)
},names(eval(parse(text = stud))))

sub20 <- Filter(function(x) {
  tmp0 = eval(parse(text=paste0(stud,"$",x)))
  return (length(levels(factor(tmp0))) <= 20)
},names(eval(parse(text = stud))))


fcatp <- function(y,x) {
  a = eval(parse(text=paste0(y,'$',x)));
  b = table(a,useNA = "always") ; names(b)[length(b)] <- "NA"
  c = round(prop.table(b)*100,digits = 2)
  d = merge(data.table(b),data.table(c),by="V1",all=T)
  t = d[,list(V1,paste0(N.x," (",N.y,")"))]
  colnames(t) <-  c('Code',y)
  return (t)}



fcont <- function(table,var) {
  return (lapply(var,function(x){
    tmp = eval(parse(text=paste0(table,'$',x)));  ifelse (class(tmp)=='factor',
                                                          tmp <- as.character.factor(tmp) %>% as.numeric,
                                                          tmp <- as.character(tmp) %>% as.numeric)
    ifelse(length(na.exclude(tmp)) == 0,
           tmp1 <- data.table(x,t(rep("No data",6))),
           tmp1 <- data.table(x,cbind(min(tmp, na.rm = TRUE),max(tmp, na.rm = TRUE),mean(tmp, na.rm = TRUE),
                                      median(tmp, na.rm = TRUE),quantile(tmp,0.05, na.rm = TRUE),quantile(tmp,0.95, na.rm = TRUE)))
    )
    
    colnames(tmp1)=c("Variable","Min","Max","Mean","Median","5th-Q","95th-Q") #tmp1[1,'Variable'] <- x
    return (tmp1)}
  ))
}

master <- data.table(c('cont','cat'),
                     c('fcont','fcatp')) ; colnames(master) <- c('Group','Function') ; setkey(master,'Group')

properties <- lapply(variables_meta,function(x){
  a = Reduce(rbind,lapply(x$attributes, function(x){
    unlist(x)})) %>% data.table ; setkey(a,'name')
  return (cbind(c('Label','ValueType','unit'),
  c(a['label',value],x$valueType,x$unit)))
})

atlH <- '/home/yamadou/export/Atlantic_R2_SAMPLE-ATL_Sample-20170124172046/ATL_Sample/data.csv' %>%
  file.path(.) %>% opal.file(o,.) %>% read.csv(text=.) %>% data.table(.)
atpH <- '/home/yamadou/export/TTP_R2_SAMPLE-20170124172720/ATP_Sample/data.csv' %>%
  file.path(.) %>% opal.file(o,.) %>% read.csv(text=.) %>% data.table(.)





