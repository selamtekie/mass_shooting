#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

server <- function(input, output) {
  
  output$plot <- renderPlot({
    # specify some map projection/options
    
    # filter overdoses_shiny based on Year
    data <- gun_control %>% 
      filter(State == input$State)
    ggplot(data, aes(x=State,y=value,fill=Laws)) + geom_bar(stat="identity",position = "fill") +
      xlab('State') + ylab('Gun Control Laws') + theme_minimal()+ scale_colour_brewer(palette="Set3")
    
    })
  
  output$plot1 <- renderPlot({
    # specify some map projection/options
    
    # filter overdoses_shiny based on Year
    data <- weapon_status %>% 
      filter(State == input$State)
    
    ggplot(data, aes(x=State, y=weapon_legal_status,fill=weapons_obtained_legally)) + 
      geom_bar(stat='identity',position = 'fill') +
      xlab('State') + ylab('Weapon Legal Status') + 
      theme_minimal() 
    
  })
  
  output$plot2 <- renderPlotly({
    # specify some map projection/options
    
    # filter overdoses_shiny based on Year
      mass_shooting_count <- mass_shooting_df %>%
      group_by(state_2) %>%
      summarize(state_count=n())
      mass_shooting_count 
      
    l <- list(color = toRGB("white"), width = 2)
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = FALSE,
      lakecolor = toRGB('white')
    )
    plot_geo(mass_shooting_count, locationmode = 'USA-states') %>%
      add_trace(
        z = ~state_count, locations = ~state_2,zmin = 1, zmax = 10,
        colors = 'Blues'
      ) %>%
      colorbar(title = "mass shooting count") %>%
      layout(
        geo = g)
    
  })
    
  output$plot3 <- renderPlot({ 
      mass_shooting_locations <- mass_shooting_df %>% 
      group_by(state_2,location_1) %>%
      summarize(location_count = n())
      mass_shooting_locations$location_1[mass_shooting_locations$location_1 == "Other\n"] <- "Other" 
      mass_shooting_locations <- mass_shooting_locations %>% rename(State=state_2)
    
    data <- mass_shooting_locations %>%
      filter(State == input$State)
    
    ggplot(data, aes(x=State, y=location_count,fill=location_1)) + geom_bar(stat='identity') +
      xlab('State') + ylab('mass shotting locations') + theme_minimal()
  })
  
}



