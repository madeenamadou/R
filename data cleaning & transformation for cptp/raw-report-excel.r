##
## Raw dataset Excel report
##

#clean
rm(list = ls(all.names = TRUE))
cat("\014")

#load packages
require(data.table);require(opal);require(magrittr);require('Hmisc');require(foreign);require(lubridate)

#create some variables
stud = 'cag'
eval(parse(text=paste0(stud,'$entity_id','<-',stud,'$entity_id','%>% as.character')))
setkey(eval(parse(text=stud)),'entity_id');

#classify variables
sup20 <- Filter(function(x) {
  tmp0 = eval(parse(text=paste0(stud,"$",x)))
  return (length(levels(factor(tmp0))) > 20)
},names(eval(parse(text = stud))))

sub20 <- Filter(function(x) {
  tmp0 = eval(parse(text=paste0(stud,"$",x)))
  return (length(levels(factor(tmp0))) <= 20)
},names(eval(parse(text = stud))))

datetime <- c()
cont <- c()


#fdt gives a glimpse of datetime variables
fdt <- function(table,var) {
  return (lapply(var,function(x) {
    #tmp = eval(parse(text=paste0(table,"[,",x,",","key(",table,")]")));
    tmp = eval(parse(text=paste0('data.table','(',x,'=',stud,'$',x,')')));

    eval(parse(text=paste0("tmp$",x," <- parse_date_time(tmp$",x,",\"ymd HMS\",truncated = 3,quiet=TRUE)")))
    eval(parse(text=paste0("tmp[,",x,":=as.character.factor(as.factor(",x,"))]")))
    
    eval(parse(text=paste0("setorder(tmp,'",x,"')")))
    eval(parse(text=paste0("tmp <- tmp[!is.na(",x,")]")))
    
    tmp1 = rbind(tmp %>% head(.,5),tmp %>% tail(.,5))
    ifelse (nrow(tmp1) == 0,
            #tmp2 <- data.table("No data","No data"),
            tmp2 <- data.table("No data"),
            tmp2 <- tmp1)
    
    #colnames(tmp2) = c("Entity ID","Value")
    colnames(tmp2) = "Value"
    return (tmp2)})
  )}
do.call(fdt,list(stud,datetime))

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
do.call(fcont,list(stud,cont))

#fsup20 gives a glimpse of variables with >20 entry levels
fsup20 <- function(table,var) {
  return (lapply(var,function(x) {
    #tmp = eval(parse(text=paste0(table,"[,",x,",","key(",table,")]")));
    tmp = eval(parse(text=paste0('data.table','(',x,'=',stud,'$',x,')')));
    eval(parse(text=paste0("ifelse (class(tmp$",x,")==\'factor\',tmp$",x,
                           "<- as.character.factor(tmp$",x,"),tmp$",x,"<- as.character(tmp$",x,"))")))
    tmp = data.table(tmp) ; lapply(tmp,function(x) as.character(x))
    eval(parse(text=paste0("tmp <- tmp[!is.na(",x,")]")));
    tmp1 = rbind(tmp %>% head(.,5),tmp %>% tail(.,5))
    ifelse (nrow(tmp1) == 0,
            #tmp2 <- data.table("No data","No data"),
            tmp2 <- data.table("No data"),
            tmp2 <- tmp1)
    #colnames(tmp2) = c("Entity ID","Value")
    colnames(tmp2) = c("Value")
    return (tmp2)})
  )}
do.call(fsup20,list(stud,sup20))

#fsub20 summarises variables with <20 entry levels
fsub20 <- function(table,var) {
  return (lapply(var,function(x) {
    tmp = eval(parse(text=paste0(table,'$',x)));
    tmp1 = table(tmp,useNA = "ifany");
    tmp2 = merge(tmp1,round(prop.table(tmp1)*100,digits = 2),by='tmp') %>% data.table
    tmp2[is.na(tmp),tmp:="NA"] ; names(tmp2)[1]<-'Code'
    if(x %in% cat) {
           a = cat_catg[[which(cat==x)]] ; b = merge(tmp2,a,by='Code',all.x=T);
           b[is.na(Category),Category:=Code][,Code:=NULL] ; setcolorder(b,c('Category','Freq.x','Freq.y'))
    } else {b = tmp2}
    colnames(b) <- c("Value","Count","Frequency (%)") ; b$Value <- paste0("'",b$Value,"'")
    return (b)}))}
do.call(fsub20,list(stud,sub20))

#computing
R <- list(do.call(fsub20,list(stud,sub20)),
          do.call(fdt,list(stud,datetime)),
          do.call(fcont,list(stud,cont)),
          do.call(fsup20,list(stud,sup20)))
		  
#storing computations to excel file
WriteXLS::WriteXLS(R, ExcelFileName = "t.xls", SheetNames = c("sub20","datetime",'cont','sup20'),na = "NA")

