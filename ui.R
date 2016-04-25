library(shiny)
library(shinydashboard)

outcome_data=read.csv("data/outcome-of-care-measures-of-Medicare-certfied hospitals.csv",stringsAsFactors = F)

outcome_data=outcome_data[outcome_data[,6]!='Not Available',]
outcome_data=outcome_data[outcome_data[,7]!='Not Available',]
outcome_data=outcome_data[outcome_data[,8]!='Not Available',]



dashboardPage(
  dashboardHeader(title="US Hospital Ranking"),
  
  dashboardSidebar(width = 200,
                   selectInput("state", 
                               label = em("Select State",style="text-align:center;color:#FFA319;font-size:100%"),
                               unique(outcome_data$State),selected = 'MD'), 
        
            selectInput("outcome", 
                               label = em("Select Outcome",style="text-align:center;color:#FFA319;font-size:100%"),
                               choices = c("heart failure","heart attack","pneumonia"),
                               selected = "heart attack"),
                   
      selectInput('columns',em('Choose Hospital',style="text-align:center;color:#FFA319;font-size:100%"),"",
                  selectize = FALSE,selected = '')
      
      
     
      
   
      
    ),
    
 
  dashboardBody(   
    
    fluidRow(
      column(width = 7,
  
          plotOutput("myplot")),
     
      
      column(width = 5,
    
          h5(strong("Best Hospitals in the State",style="text-align:right;color:darkblue;font-size:100%")),
          
          div(tableOutput("table1"), style = "font-size:80%",collapsible = TRUE)))
   
    )
    )

  

