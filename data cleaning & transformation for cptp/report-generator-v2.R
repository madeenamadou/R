##
## Generate a report
##

#clean the environment
rm(list = ls(all.names = TRUE))
cat("\014")

#load packages and set knitr
knitr::opts_chunk$set(echo = TRUE)
require(data.table);require(magrittr);require(data.table);require(opal);
require(ggplot2);require(lubridate);require(knitr);require(htmlTable);require(lattice)

#connect to R Server on OPAL
source('~/cptp/opal-login.r')

#create some variables
stud = 'atlH'
fd = 'Atlantic_R2_SAMPLE'
ds = 'ATL_Sample'
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

##classification for PM dataset
#sub20 <- "PM_FRACTURE_RISK"
#cont <- cont[which(cont!="PM_FRACTURE_RISK")]

##classification for raw datasset
#sup20 <- Filter(function(x) {
#  tmp0 = eval(parse(text=paste0(stud,"$",x)))
#  return (length(levels(factor(tmp0))) > 20)
#},names(eval(parse(text = stud))))

#sub20 <- Filter(function(x) {
#  tmp0 = eval(parse(text=paste0(stud,"$",x)))
#  return (length(levels(factor(tmp0))) <= 20)
#},names(eval(parse(text = stud))))


#create functions for summary statistics
#fsub20 summarises variables with <20 entry levels
fsub20 <- function(tables,var) {
  t = Reduce(function(x,y) merge(x,y,by='Value',all=TRUE),
    lapply(tables,function(table){
    Reduce(rbind,lapply(var,function(x) {
    tmp = eval(parse(text=paste0(table,'$',x)));
    tmp1 = table(tmp,useNA = "ifany");
    tmp2 = merge(tmp1,round(prop.table(tmp1)*100,digits = 2),by='tmp') %>% data.table
    tmp2[is.na(tmp),tmp:="NA"] ; names(tmp2)[1]<-'Code'
    if(x %in% cat) {
      a = cat_catg[[which(cat==x)]] ; b = merge(tmp2,a,by='Code',all.x=T);
      b[is.na(Category),Category:=Code][,Code:=NULL] ; setcolorder(b,c('Category','Freq.x','Freq.y'))
    } else {b = tmp2}
    colnames(b) <- c("Value",paste0("Count","_",table),paste0("Freq(%)","_",table)) ; b$Value <- paste0("'",b$Value,"'")
    return (b)}))})) 
  t %>% setcolorder(.,c("Value",grep("count",colnames(.),ignore.case=TRUE,value=TRUE),
                      grep("freq",colnames(.),ignore.case=TRUE,value=TRUE)))
  return (t)
  }

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
fcont <- function(tables,var) {
  t = Reduce(rbind,
  lapply(tables,function(table) {
    Reduce(rbind,lapply(var,function(x){
    tmp = eval(parse(text=paste0(table,'$',x)));  if (class(tmp) %in% c('factor')) {
      tmp <- as.character(tmp) %>% as.numeric} else{
        tmp <- as.numeric(tmp)}
    
    ifelse(length(na.exclude(tmp)) == 0,
           tmp1 <- data.table(Study = table,Min = "No data",Max = "No data",Mean = "No data",
             Median = "No data",'5th_Q'= "No data",'95th-Q'= "No data",'NAs (Tot.)' = "No data"
             #t(rep("No data",8))
             ),
           tmp1 <- data.table(
             cbind(Study = table,
                   Min = min(tmp, na.rm = TRUE),
                   Max = max(tmp, na.rm = TRUE),
                   Mean = round(mean(tmp, na.rm = TRUE),2),
                   Median = median(tmp, na.rm = TRUE),
                   '5th_Q'= quantile(tmp,0.05, na.rm = TRUE),
                   '95th-Q'= quantile(tmp,0.95, na.rm = TRUE),
                   'NAs (Tot.)' = paste0(which(is.na(tmp)) %>% length," (",length(tmp),")")
                   ))
    )
    
    #colnames(tmp1)=c("Study","Min","Max","Mean","Median","5th-Q","95th-Q","NAs (Tot.)") #tmp1[1,'Variable'] <- x
    return (tmp1)}
  ))}));
  return (t)
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

#create directory to store plots figures
dir.create("~/figures",showWarnings = F,recursive = F);unlink('~/figures/*.*')

#create the markdown document content
freport = function(stud,vars){
dir.create("~/figures",showWarnings = F);unlink('~/figures/*.*')
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
  
  Reduce(paste0,lapply(vars[
  #Reduce(paste0,lapply(names(eval(parse(text=stud)))[5:10][
    #-1
    eval(parse(text=paste0("!grepl('_ID$',vars,ignore.case = T)")))
    #-eval(parse(text=paste0("grep('_ID$',names(",stud,"),ignore.case = T)")))
    ],function(var){
      paste0(
        "\n","\n","\n","\n",
        "\n",var_label[var,Label]," {.tabset .tabset-fade .tabset-pills}","\n", #.tabset-fade .tabset-pills
        "---","\n","\n",
        
        paste0(
          "<pre style=\"line-height: 0.75;\"><b>Label     :</b> ",opal.attribute_values(opal.variable(o,fd,ds,var)$attributes, name = "description"),"<br>","\n",
          "<b>ValueType :</b> ",opal.variable(o,fd,ds,var)$valueType,"<br>","\n",
          "<b>Unit      :</b> ",opal.variable(o,fd,ds,var)$unit,"<br>","\n",
          "</pre>","\n"
        ),
        "\n","\n","\n",
        
        
        "### JavaScript Algorithm","\n",
        "```{javascript}","\n",
        opal.attribute_values(opal.variable(o,fd,ds,var)$attributes,name = 'script'),"\n",
        "```","\n","\n","\n",
        
        '<strong>Notes</strong>','\n',
        '<div contenteditable=\"true\" placeholder=\"Add comments here...\" style=\"border:1px dashed #AAA;padding: 5px;background:#EEE;\"></div>',"\n",
        
        "### Summary","\n","\n",
        htmlTable(
          #Reduce(function(x,y) merge(x,y,by='Value',all=TRUE),
          #       lapply(stud,function(x) do.call(
          #         master[Filter(function(y){eval(parse(text=paste0("is.element('",var,"',",eval(y),")")))},
          #                       master$Group),Function][1],list(x,var)))) %>%
          #  setcolorder(.,c("Value",grep("count",colnames(.),ignore.case=TRUE,value=TRUE),
          #              grep("freq",colnames(.),ignore.case=TRUE,value=TRUE)))
          do.call(master[Filter(function(y){eval(parse(text=paste0("is.element('",var,"',",eval(y),")")))},
                                        master$Group),Function][1],list(stud,var))
                  ,align='l',align.header='c',rnames=F,
                  #caption="Table of 7 head and tail entries",
                  css.cell = 'padding-left: 1em;'),
        
        
        if (var %in% cat) {
          paste0(
            do.call(function(stud,var){
              a = cat_catg[[which(cat==var)]]
              b = Reduce(function(x,y) merge(x,y,by='b',all=TRUE),lapply(stud,function(studx){
                b = eval(parse(text=studx))[,var,with=F]
                c = table(b,useNA='ifany'); if (anyNA(names(c))) {names(c)[length(c)] <- 'NA'}
                d = round(prop.table(c)*100,digits = 2)
                return (d)
              })) %>% data.table;colnames(b)<-c('Code',stud)
              if (nrow(b)>1) {
                res = do.call(function(y){
                  ab = a[as.integer(y$Code),][is.na(Category),Category:=Code]
                  bb = melt(b,'Code',variable.name='Study',value.name='Freq')
                  
                  e = as.character(trunc(as.numeric(Sys.time())*1000))
                  png(file = paste0("~/figures/",e,".png"), bg = "transparent")
                  #barchart(Freq~Code,data=bb,groups=Study, 
                  #         scales=list(x=list(rot=70,cex=0.8,labels=ab$Category)),
                  #         xlab=var_label[var,Label], ylab = 'Freq.',
                  #         col=rainbow(2))
                  print({ggplot(bb, aes(Code, Freq, fill = Study)) + 
                    geom_bar(stat="identity", position = "dodge",na.rm = TRUE) + 
                    scale_fill_brewer(palette = "Set1") + 
                    theme(legend.justification = "center") +
                    scale_x_discrete(breaks=y$Code,
                                     labels=ab$Category) + 
                    theme(axis.text.x = element_text(angle = 65, hjust = 1))})
                  
                  #bp <- barplot(y$atlH,main=var_label[x,Label],xlab=var_label[x,Label], ylab = 'Freq.',
                  #              col=rainbow(dim(y)),names.arg = ab$Category,
                  #              legend.text = ab$Category,axisnames = F,
                  #              args.legend = list("topright", bty = "n"),xlim=c(0,20),width=2)
                  #text(bp, 0, round("a", 1),cex=1,pos=3)
                  dev.off()
                  return (paste0("### Barplot","\n","\n","![](figures/",e,".png)","\n"))},list(b))} else {res = "\n"}
              return (res)  
            },list(stud,var)))
        } else if (var %in% cont) {
          
          paste0(
            do.call(function(stud,var){
              a = Reduce(function(x,y) qpcR:::cbind.na(x,y),
              lapply(stud,function(studx){
              a = eval(parse(text=paste0(studx,'$',var)));  if (class(a) %in% c('factor')) {
                a <- as.character(a) %>% as.numeric} else{
                  a <- as.numeric(a)}
              return (a)})) %>% data.table ; colnames(a)<-stud
              
              b = melt(a,variable.name='Study',value.name='Value',measure.vars = stud)
              
              #qpcR:::cbind.na(cagH$SAMPLE_VOLUME_MEASURED,atpH$SAMPLE_VOLUME_MEASURED) %>% data.table
              #sapply(list(cagH$SAMPLE_VOLUME_MEASURED,atpH$SAMPLE_VOLUME_MEASURED),function(x) na.exclude(x) %>% length)
              #sapply(c('cagH','atpH'),function(x) na.exclude(x) %>% length)
              
              #if (length(na.exclude(a)) != 0) {
              
              #return (paste0("### BoxPlot","\n","\n","\n",
              #       "```{r, ggplotly=TRUE ,error=FALSE ,message=FALSE,echo=FALSE, include=TRUE}","\n",
              #       "ggp <- ggplot(b, aes(Study, Value)) + geom_boxplot(outlier.colour = \"red\",na.rm = TRUE)","\n",
              #       "ggplotly(ggp)","\n",
              #       "```","\n","\n"))
              
              #ggp <- ggplot(b, aes(Study, Value)) + geom_boxplot(outlier.colour = "red",na.rm = TRUE)
              #gg = ggplotly(ggp)
              
              #htmlwidgets::saveWidget(as_widget(gg),file = 'index.html')

              
                res = do.call(function(y){
                  e = as.character(trunc(as.numeric(Sys.time())*1000))
                  png(file = paste0("~/figures/",e,".png"), bg = "transparent")
                  print({ggplot(y, aes(Study, Value)) + geom_boxplot(outlier.colour = "red",na.rm = TRUE)})
                  dev.off()
                  return (paste0("### BoxPlot","\n","\n","![](figures/",e,".png)","\n"))},list(b))
                } else {res = "\n"}
              return (res)  
            },list(stud,var)))
          
        },'\n','\n'
        
        
      )
    })))

#write the document content to a file
write.table(md, "~/report.Rmd",quote = F,eol = "\n",row.names = F, col.names = F, append = F)
}

do.call(freport,list(c('atlH','cagH','atpH'),var))
rmarkdown::render(input = '~/report.Rmd',output_format = "html_document",output_file = 'report.html')

##end