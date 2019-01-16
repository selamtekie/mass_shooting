#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shinythemes)
# Define UI
shinyUI(
  fluidPage(theme = shinythemes::shinytheme("superhero"),
            dashboardPage(
              dashboardHeader(title = 'Mass Shootings In America', titleWidth = 1600),
              dashboardSidebar(tags$blockquote('Mass Shooting state by state analysis'),
                               selectInput("State", 
                                           label = "State", 
                                           choices = states,
                                           selected = 'TN')),
                               
              
              dashboardBody(
                
                tags$head( 
                  tags$style(HTML(".main-sidebar { font-size: 20px; }")) 
                ),
                
                fluidRow(
                  box(title ='...', status = 'primary',solidHeader = T,plotOutput("plot", height = 300, width=600),width=6),
                  box(title = '...', status = 'primary',solidHeader = T,plotOutput("plot3", height = 300),width = 6),
                  
                  fluidRow(
                    box(title = '...', status = 'primary',solidHeader = T,plotOutput("plot1", height = 300, width = 600), width = 6),
                    box(title = 'Mass Shootings by State 1982-2018', status = 'primary',solidHeader = T,plotlyOutput("plot2", height = 300, width =600),width = 6)
                    
                    
                  )
                )
              )
              )
  )
)