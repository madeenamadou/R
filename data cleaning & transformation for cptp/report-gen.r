# Rscript
#
# Par Youssef de Madeen Amadou
#
# Generer un rapport HTML de description statistique et graphique des donnees
#


rm(list = ls(all.names = TRUE))
cat("\014")
knitr::opts_chunk$set(echo = TRUE)
require(data.table);require(magrittr);require(data.table);require(opal);
require(ggplot2);require(lubridate);require(knitr);require(htmlTable)


# parametres
stud = 'OHS_Pilot_Sample_Report'
fd ='OHS_R2_SAMPLE'
ds = 'OHS_Pilot_Sample'
eval(parse(text=paste0(stud,'$SAMPLE_ID','<-',stud,'$SAMPLE_ID','%>% as.character')))
eval(parse(text=paste0(stud,'[,entity_id:=NULL]')))
setkey(eval(parse(text=stud)),'SAMPLE_ID');

master <- data.table(c('cont','cat','datetime','time','misc1','misc2'),
                     c('fcont','fsub20','fdt','fsup20','fsup20','fsub20')) ; colnames(master) <- c('Group','Function') ; setkey(master,'Group')
datetime = sapply(Filter(function(x){return (grepl("(^yyyy/MM/dd HH:mm$)|(^yyyy/MM/dd$)",ifelse(is.null(x$unit),'',x$unit),
                                                   ignore.case = TRUE))},
                         opal.variables(o,fd,ds)),function(x){return (x$name)})
cat = sapply(Filter(function(x){return (!is.null(x$categories) && length(x$categories) >=2)},
                    opal.variables(o,fd,ds)),function(x){return (x$name)})
cont = sapply(Filter(function(x){return (length(x$categories)<=1 && grepl("integer|decimal",ifelse(is.null(x$valueType),'',x$valueType),ignore.case = TRUE))},
                     opal.variables(o,fd,ds)),function(x){return (x$name)})
cont = cont[-which(grepl("(SAMPLE_VITAL_STATE)|(SAMPLE_CENTRI2_NA)",cont,ignore.case = T))]
time = sapply(Filter(function(x){return (grepl("^HH:mm$",ifelse(is.null(x$unit),'',x$unit),ignore.case = TRUE))},
                     opal.variables(o,fd,ds)),function(x){return (x$name)})
misc1 = sapply(Filter(function(x){return (grepl("text",ifelse(is.null(x$valueType),'',x$valueType),ignore.case = TRUE) && ifelse(is.null(x$unit),'',x$unit)=="")},
                      opal.variables(o,fd,ds)),function(x){return (x$name)})
misc2 = Filter(function(x) !x %in% Reduce(union,list(cont,cat,datetime,time,misc1)),names(eval(parse(text=stud))))

#create functions for summary statistics
#fsub20 summarises variables with <20 entry levels
fsub20 <- function(table,var) {
  return (Reduce(rbind,lapply(var,function(x) {
    tmp = eval(parse(text=paste0(table,'$',x)));
    tmp1 = table(tmp,useNA = "ifany");
    tmp2 = merge(tmp1,round(prop.table(tmp1)*100,digits = 2),by='tmp') %>% data.table
    tmp2[is.na(tmp),tmp:="NA"] ; names(tmp2)[1]<-'Code'
    if(x %in% cat) {
      a = cat_catg[[which(cat==x)]] ; b = merge(tmp2,a,by='Code',all.x=T);
      b[is.na(Category),Category:=Code][,Code:=NULL] ; setcolorder(b,c('Category','Freq.x','Freq.y'))
    } else {b = tmp2}
    colnames(b) <- c("Value","Count","Frequency (%)") ; b$Value <- paste0("'",b$Value,"'")
    return (b)})))}

#fdt gives a glimpse of datetime variables
fdt <- function(table,var) {
  return (Reduce(rbind,lapply(var,function(x) {
    tmp = eval(parse(text=paste0(table,"[,",x,",","key(",table,")]")));
    #tmp = eval(parse(text=paste0('data.table','(',x,'=',stud,'$',x,')')));
    
    eval(parse(text=paste0("tmp$",x," <- parse_date_time(tmp$",x,",\"ymd HMS\",truncated = 3,quiet=TRUE)")))
    eval(parse(text=paste0("tmp[,",x,":=as.character.factor(as.factor(",x,"))]")))
    
    eval(parse(text=paste0("setorder(tmp,'",x,"')")))
    eval(parse(text=paste0("tmp <- tmp[!is.na(",x,")]")))
    
    tmp1 = rbind(tmp %>% head(.,5),tmp %>% tail(.,5))
    ifelse (nrow(tmp1) == 0,
            tmp2 <- data.table("No data","No data"),
            #tmp2 <- data.table("No data"),
            tmp2 <- tmp1)
    
    colnames(tmp2) = c("Entity ID","Value")
    #colnames(tmp2) = "Value"
    return (tmp2)})
  ))}

#fcont summarises continuous variables
fcont <- function(table,var) {
  return (Reduce(rbind,lapply(var,function(x){
    tmp = eval(parse(text=paste0(table,'$',x)));  if (class(tmp) %in% c('factor')) {
      tmp <- as.character(tmp) %>% as.numeric} else{
        tmp <- as.numeric(tmp)}
    
    ifelse(length(na.exclude(tmp)) == 0,
           tmp1 <- data.table(t(rep("No data",7))),
           tmp1 <- data.table(cbind(min(tmp, na.rm = TRUE),max(tmp, na.rm = TRUE),round(mean(tmp, na.rm = TRUE),2),
                                    median(tmp, na.rm = TRUE),quantile(tmp,0.05, na.rm = TRUE),
                                    quantile(tmp,0.95, na.rm = TRUE),
                                    paste0(which(is.na(tmp)) %>% length," (",length(tmp),")")))
    )
    
    colnames(tmp1)=c("Min","Max","Mean","Median","5th-Q","95th-Q","NAs (Tot.)") #tmp1[1,'Variable'] <- x
    return (tmp1)}
  )))
} ; 

#fsup20 gives a glimpse of variables with >20 entry levels
fsup20 <- function(table,var) {
  return (Reduce(rbind,lapply(var,function(x) {
    tmp = eval(parse(text=paste0(table,"[,",x,",","key(",table,")]")));
    #tmp = eval(parse(text=paste0('data.table','(',x,'=',stud,'$',x,')')));
    eval(parse(text=paste0("ifelse (class(tmp$",x,")==\'factor\',tmp$",x,
                           "<- as.character.factor(tmp$",x,"),tmp$",x,"<- as.character(tmp$",x,"))")))
    tmp = data.table(tmp) ; lapply(tmp,function(x) as.character(x))
    eval(parse(text=paste0("tmp <- tmp[!is.na(",x,")]")));
    tmp1 = rbind(tmp %>% head(.,5),tmp %>% tail(.,5))
    ifelse (nrow(tmp1) == 0,
            tmp2 <- data.table("No data","No data"),
            #tmp2 <- data.table("No data"),
            tmp2 <- tmp1)
    colnames(tmp2) = c("Entity ID","Value")
    #colnames(tmp2) = c("Value")
    return (tmp2)}))
  )}

#get variables properties  from datasets
properties <- lapply(opal.variables(o,fd,ds),function(x){
  a = sapply(x$attributes,function(x) {
    data.table(Attrib = x$name, Value = x$value) %>% unlist}) %>% t %>% data.table
  setkey(a,'Attrib')
  return (data.table(Label=a['label',Value],ValueType=ifelse(is.null(x$valueType),'',x$valueType),Unit=ifelse(is.null(x$unit),'',x$unit)))
})

#get variables labels
var_label <- data.table(Reduce(rbind,lapply(opal.variables(o,fd,ds),function(x){
  return (c(x$name,opal.attribute_values(x$attributes,name='label')))
}))) ; names(var_label) <- c("Name","Label") ; setkey(var_label,Name)

#get variables complete list
var <- sapply(opal.variables(o,fd,ds),function(x){return (x$name)})

#get variables categories
cat_catg <- lapply(Filter(function(x){return (!is.null(x$categories) && length(x$categories) >=2)},
                          opal.variables(o,fd,ds)),function(x){
                            tmp1 = x$categories
                            tmp2 = tmp1  %>% unlist %>% matrix(ncol=length(tmp1)) %>% .[c(1,4),] %>% t
                            tmp3 = array("",dim=list(length(tmp1)+1,2)) ; #tmp3[1,1] <- c(label) ; 
                            tmp3[,2] <- c(tmp2[,1],"NA") ; tmp3[,1] <- c(tmp2[,2],"Missing") ; 
                            colnames(tmp3) <- c("Category","Code") ; 
                            t = data.table(tmp3) ; setkey(t,Code)
                            return (t)
                          })

#create the markdown document content
freport = function(stud,varname){
  dir.create("~/figures",showWarnings = F,recursive = F);unlink('~/figures/*.*')
  md <- paste0(
    "---","\n",
    "title: 'Report'","\n",
    "date: '",paste(months(Sys.Date()),year(now())),"'\n",
    "output:","\n", 
    " html_document:","\n",
    "  theme: paper","\n",
    "  toc: true","\n",
    "  toc_depth: 2","\n",
    "  depth: 2","\n",
    "  number_sections: false","\n",
    "  toc_float: ","\n",
    "   collapsed: true","\n",
    "   smooth_scroll: true","\n",
    "---","\n","\n",
    
    Reduce(paste0,lapply(varname[
      #Reduce(paste0,lapply(names(eval(parse(text=stud)))[5:10][
      #-1
      eval(parse(text=paste0("!grepl('_ID$',varname,ignore.case = T)")))
      #-eval(parse(text=paste0("grep('_ID$',names(",stud,"),ignore.case = T)")))
      ],function(x){
        paste0(
          "\n","\n","\n","\n",
          "\n",var_label[x,Label]," {.tabset .tabset-fade .tabset-pills}","\n", #.tabset-fade .tabset-pills
          "---","\n","\n",
          
          paste0(
            "<pre style=\"line-height: 0.75;\"><b>Label     :</b> ",opal.attribute_values(opal.variable(o,fd,ds,x)$attributes, name = "description"),"<br>","\n",
            "<b>ValueType :</b> ",opal.variable(o,fd,ds,x)$valueType,"<br>","\n",
            "<b>Unit      :</b> ",opal.variable(o,fd,ds,x)$unit,"<br>","\n",
            "</pre>","\n"
          ),
          "\n","\n","\n",
          
          
          "### JavaScript Algorithm","\n",
          "```{javascript}","\n",
          opal.attribute_values(opal.variable(o,fd,ds,x)$attributes,name = 'script'),"\n",
          "```","\n","\n","\n",
          
          '<strong>Notes</strong>','\n',
          '<div contenteditable=\"true\" placeholder=\"Add comments here...\" style=\"border:1px dashed #AAA;padding: 5px;background:#EEE;\"></div>',"\n",
          
          "### Summary","\n","\n",
          htmlTable(do.call(master[Filter(function(y){eval(parse(text=paste0("is.element('",x,"',",eval(y),")")))},
                                          master$Group),Function][1],list(stud,x))
                    ,align='l',align.header='c',rnames=F,
                    #caption="Table of 7 head and tail entries",
                    css.cell = 'padding-left: 1em;'),
          
          
          if (x %in% cat) {
            paste0(
              do.call(function(x){
                a = cat_catg[[which(cat==x)]]
                b = eval(parse(text=stud))[,x,with=F]
                c = table(b,useNA='ifany'); if (anyNA(names(c))) {names(c)[length(c)] <- 'NA'}
                d = round(prop.table(c)*100,digits = 2)
                if (dim(d)>1) {
                  res = do.call(function(y){
                    e = as.character(trunc(as.numeric(Sys.time())*1000))
                    png(file = paste0("~/figures/",e,".png"), bg = "transparent")
                    ab = a[names(y),][is.na(Category),Category:=Code]
                    bp <- barplot(y,main=var_label[x,Label],xlab=var_label[x,Label], ylab = 'Freq.',
                                  col=rainbow(dim(y)),names.arg = ab$Category,
                                  legend.text = ab$Category,axisnames = F,
                                  args.legend = list("topright", bty = "n"),xlim=c(0,20),width=2)
                    text(bp, 0, round(y, 1),cex=1,pos=3)
                    dev.off()
                    return (paste0("### Barplot","\n","\n","![](figures/",e,".png)","\n"))},list(d))} else {res = "\n"}
                return (res)  
              },list(x)))
          } else if (x %in% cont) {
            
            paste0(
              do.call(function(x){
                a = eval(parse(text=paste0(stud,'$',x)));  if (class(a) %in% c('factor')) {
                  a <- as.character(a) %>% as.numeric} else{
                    a <- as.numeric(a)}
                
                if (length(na.exclude(a)) != 0) {
                  res = do.call(function(y){
                    e = as.character(trunc(as.numeric(Sys.time())*1000))
                    png(file = paste0("~/figures/",e,".png"), bg = "transparent")
                    qqnorm(y,
                           xlab="Theoretical Quantiles", 
                           ylab="Samples Quantiles", 
                           main=paste0("Normal Q-Q Plot"))
                    qqline(y)
                    dev.off()
                    return (paste0("### QQ-Plot","\n","\n","![](figures/",e,".png)","\n"))},list(a))} else {res = "\n"}
                return (res)  
              },list(x)))
            
          },'\n','\n'
          
          
        )
      })))
  
  #write the document content to a file
  write.table(md, "~/report.Rmd",quote = F,eol = "\n",row.names = F, col.names = F, append = F)
}

do.call(freport,list(stud,var));rmarkdown::render('~/report.Rmd',"html_document")

#END