# Rscript
#
# Par Youssef de Madeen Amadou
#
# Comparer deux datasets a partir d'une cle commune
#


rm(list = ls(all.names = TRUE))
cat("\014")
require(data.table);require(magrittr);require(data.table);require(opal);
require(ggplot2);require(lubridate);require(knitr);require(htmlTable);require(xlsx)


explr = function(x,y){
  tn = eval(parse(text=x[2])) ; to = eval(parse(text=x[1]))
  setkeyv(tn,y); setkeyv(to,y)
  to = to[,lapply(.SD,function(x) as.character(x)),.SDcols=1:ncol(to)]
  tn = tn[,lapply(.SD,function(x) as.character(x)),.SDcols=1:ncol(tn)]
  
  #variables communes
  var = intersect(names(tn),names(to))
  var = var[-eval(parse(text=paste0("!grepl('",y,"',var,ignore.case = T)")))]
  #cle commune
  key = unique(intersect(eval(parse(text=paste('tn$',y))),eval(parse(text=paste('to$',y)))))
  a = lapply(var, function(z){
    if (fsetequal(tn[key,z,with=F],to[key,z,with=F])) {
      a = NULL} else {
        id = fsetdiff(tn[key,c(y,z),with=F],to[key,c(y,z),with=F])[,y,with=F] %>% c
        a = merge(tn[id,c(y,z),with=F],to[id,c(y,z),with=F],by=y,suffixes=c(paste0('_',x[2]),paste0('_',x[1])),all=T)}
    return (a)}) %>% Filter(function(x) !is.null(x),.)
  
  b = which(!eval(parse(text=paste('tn$',y))) %in% eval(parse(text=paste('to$',y)))) %>%  tn[.,y,with=F]
  c = which(!eval(parse(text=paste('to$',y))) %in% eval(parse(text=paste('tn$',y)))) %>%  to[.,y,with=F]
  d = Filter(function(x) !x %in% names(to),names(tn))
  e = Filter(function(x) !x %in% names(tn),names(to))

  f = data.table(info = c(paste0(x[2],'_vars_notin_',x[1]),
                          paste0(x[1],'_vars_notin_',x[2]),
                          paste0(x[2],'_entities_notin_',x[1]),
                          paste0(x[1],'_entities_notin_',x[2])),
                 count = c(c(length(d),length(e),nrow(b),nrow(c))))
  
  r = eval(parse(text=paste0(
    'list(',
    'report = f,',
    x[2],'_vars_notin_',x[1],'=d,',
    x[1],'_vars_notin_',x[2],'=e,',
    x[2],'_entities_notin_',x[1],'=b,',
    x[1],'_entities_notin_',x[2],'=c,',
    'changes=a) %>% Filter(function(x) length(x)!=0 || nrow(x)!=0,.)')))
  return (r)
}


do.call(explr,list(c('ohs_samp_old','ohs_samp_new'),'CPAC_Sample_ID'))


## en developpement
fwrite = function(x){
wb <- createWorkbook(type = "xlsx")
sheet = createSheet(wb, sheetName=x)
c0 = 1
  sapply(names(eval(parse(text=x))),function(y){
    l = eval(parse(text=paste0(x,"$",y)))
    if (is.data.table(l)) {
      c = createCell(createRow(sheet, rowIndex=1), colIndex=c0)
      mapply(setCellValue, c[1,1], y)
      addDataFrame(l, sheet, col.names=T, row.names=F,
                 startRow=2, startColumn=c0,
                 showNA=T, characterNA="na", byrow=FALSE)
      c0 <<- c0 + ncol(l)+1
      } else if (is.character(l)) {
        c = createCell(createRow(sheet, rowIndex=1:(length(l)+1)), colIndex=c0)
        mapply(setCellValue, c[1,1], y)
        mapply(setCellValue, c[2:length(l),1], l)
        c0 <<- c0 + ncol(l)+1
        } else {
          c = createCell(createRow(sheet, rowIndex=1), colIndex=c0)
          mapply(setCellValue, c[1,1], y)
          for(i in 1:length(l)) {
            addDataFrame(l[[i]], sheet, col.names=T, row.names=F,
                         startRow=2, startColumn=c0,
                         showNA=T, characterNA="na", byrow=FALSE)
            c0 <<- c0 + ncol(l[[i]])+1
            }
          }
    })
saveWorkbook(wb, 'comparison.xlsx')
}

#do.call(fwrite,as.list('id_check_cag','id_check_ohs','id_check_atp'))
#fwrite('id_check_cag')

#END