# Rscript
#
# Par Youssef de Madeen Amadou
#
# List JS scripts for any cohort study
#


rm(list = ls(all.names = TRUE))
cat("\014")
knitr::opts_chunk$set(echo = TRUE)
require(data.table);require(magrittr);require(data.table);require(opal);
require(ggplot2);require(lubridate);require(knitr);require(htmlTable)


# define the cohort study
fd ='TTP_R2_SAMPLE'
ds = 'ATP_Sample'

# get variables properties from Opal
properties <- lapply(opal.variables(o,fd,ds),function(x){
  a = sapply(x$attributes,function(x) {
    data.table(Attrib = x$name, Value = x$value) %>% unlist}) %>% t %>% data.table
  setkey(a,'Attrib')
  return (data.table(Label=a['label',Value],ValueType=ifelse(is.null(x$valueType),'',x$valueType),Unit=ifelse(is.null(x$unit),'',x$unit)))
})

# get variables labels
var_label <- data.table(Reduce(rbind,lapply(opal.variables(o,fd,ds),function(x){
  return (c(x$name,opal.attribute_values(x$attributes,name='label')))
}))) ; names(var_label) <- c("Name","Label") ; setkey(var_label,Name)

# get variables list
var <- sapply(opal.variables(o,fd,ds),function(x){return (x$name)})

# build markdown document
freport = function(varname){
  dir.create("~/figures",showWarnings = F,recursive = F);unlink('~/figures/*.*')
  md <- paste0(
    "---","\n",
    "title: 'Co-integrating data using JavaScript'","\n",
    "author: 'Youssef de Madeen Amadou'","\n",
    "date: '",paste(day(Sys.Date()),months(Sys.Date()),year(now())),"'\n",
    "output:","\n", 
    " html_document:","\n",
    " theme: paper","\n",
    #"  toc: false","\n",
    #"  toc_depth: 2","\n",
    #"  depth: 2","\n",
    #"  number_sections: false","\n",
    #"  toc_float: ","\n",
    #"   collapsed: true","\n",
    #"   smooth_scroll: true","\n",
    "---","\n","\n","\n","\n","\n","\n","\n","\n",
    
    "Following are JavaScript algorithms developed to co-integrate Biosample data for the CPTP project.","\n","\n","\n","\n",
    
    Reduce(paste0,lapply(varname[
      eval(parse(text=paste0("!grepl('_ID$',varname,ignore.case = T)")))
      ],function(x){
        paste0(
          var_label[x,Label],"\n",
          "---","\n","\n",

          paste0(
                 "**Variable Name  :** ",x,"\n","\n",
                 "**Variable Label :** ",
                 opal.attribute_values(opal.variable(o,fd,ds,x)$attributes, name = "description") %>%
                   gsub("\\n","",.,ignore.case = TRUE),"\n","\n",
                 "**Value Type     :** ",opal.variable(o,fd,ds,x)$valueType
                 ),"\n","\n",
          
          "**JS script**","\n","\n",
          "```{javascript}","\n",
          opal.attribute_values(opal.variable(o,fd,ds,x)$attributes,name = 'script'),"\n",
          "```","\n","\n","\n","\n"
          )
        }
      )
      )
    )
  
  # write down markdown content to a file
  write.table(md, "~/atp_biosample.md",quote = F,eol = "\n",row.names = F, col.names = F, append = F)
}

do.call(freport,list(var));
rmarkdown::render("~/js.Rmd","html_document")

`#END