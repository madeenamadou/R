#install.packages("shiny")
library(shiny);library(magrittr);library(data.table);library(ggplot2);library(plyr)

shinyServer(function(input, output) {
  
  output$meta <- renderText({var_label[input$var,Label]})
  output$tab2 <- renderTable(align = c('l'),bordered = T,spacing = c("xs"),striped = T,
                             {
                            if (input$var %in% cat) {   
                             a = cat_catg[[which(cat==input$var)]]  
                             b = lapply(tabid[input$table,DS],fcatp,input$var)
                             c = Reduce(function(x,y){return (merge(x,y,by="Code",all=T))},b)
                             d = merge(a,c,by="Code",all=T) ; names(d)[-c(1:2)] <- input$table ; d[,Code:=NULL]
                            } else if (input$var %in% cont) {
                             a = sapply(tabid[input$table,DS],fcont,input$var)
                             b = cbind(input$table,Reduce(rbind,a)[,Variable := NULL]) ; names(b)[1] <- 'Cohort'
                             b
                             
                            }
                             
                             
                            })
  output$properties <- renderTable(rownames = F, colnames = F,align = c('l'),bordered = T,spacing = c("xs"),striped = T,
                                   {properties[[which(var==input$var)]]})
  
  
  output$plot1 <- renderPlot({
    
    a = cat_catg[[which(cat==input$var)]]
    b = Reduce(rbind,lapply(input$table,function(y,x) {
      a = eval(parse(text=paste0(tabid[y,DS])))[,x,with=F];
      a$Cohort = y ; return (a)},input$var))
    c = eval(parse(text=paste0(
        "with(b,table(Cohort,",input$var,",useNA='ifany'))"))) ; if (anyNA(colnames(c))) {colnames(c)[ncol(c)] <- "NA"}
    d = round(prop.table(c,1)*100,digits = 2)
    
    bp <- barplot(d,main=paste("Grouped Bar Plot"),
             xlab=var_label[input$var,Label], ylab = "Percentage count",
            legend = input$table,beside=TRUE,col=rainbow(length(input$table))
              ,names.arg = a[colnames(d),Category])
    text(bp, 0, round(d, 1),cex=1,pos=3) 
    
    #qplot(factor(SAMPLE_TYPE), N,fill=Cohort, data=data.table(c), geom="boxplot") + theme_bw()
    #qplot(x=factor(SAMPLE_TYPE), fill=Cohort,data=b, geom="bar")
    #ggplot(b, aes(factor(SAMPLE_TYPE),fill=Cohort)) + geom_bar(position='dodge')


})

  
  
})
