library(shiny);library(magrittr);library(data.table);library(ggplot2);

shinyUI(fluidPage(
  
  headerPanel("Integrated Data Reporting Tool"),
  sidebarLayout(
    sidebarPanel(style="padding:25px",
                selectInput(inputId = "var", label = "Select variable", 
                             choices = var, selected = NULL, multiple = FALSE,
                             selectize = TRUE, width = NULL, size = NULL))
                 
     ,mainPanel(style="padding:0px 0px 0px 35px"
       ,span(strong("Properties")
            ,tableOutput("properties"),style="color:black; font-family:arial"))
    ,position = c("right")),
  
  hr(),
  
  sidebarLayout(
    sidebarPanel(selectInput(inputId = "table",label = "Pick cohort(s)",
                             choices = tabid$N, selected = NULL, multiple = TRUE,
                             selectize = TRUE, width = NULL, size = NULL)
                 ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(title =  strong("Contingency tables"),
               conditionalPanel(condition = "input.table",
                                mainPanel(style="padding:35px",
                                  span(tableOutput("tab2"))
                                  )
               ))
      ,tabPanel(title =  strong("Bar Plots"),
                conditionalPanel(condition = "input.table",
                  mainPanel(
                    span(style="text-indent:25px; color:blue; font-family:tahoma;",
                         plotOutput("plot1"))))
                )
      ,tabPanel(title =  strong("Values"),
                conditionalPanel(condition = "input.table",
                                 mainPanel(
                                   span(style="text-indent:25px; background-color:red; font-family:tahoma;"
                                        #tableOutput("tab3") 
                                        )))
      )
    )), position = c("right"))
))