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
   
    
   
    data <- gun_control %>% 
      filter(State == input$State)
    ggplot(data, aes(x=State,y=value,fill=Laws)) + 
      geom_bar(stat="identity",position = "fill") +
       theme_minimal()+ scale_colour_brewer(palette="Set3") +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title = element_blank(),
            legend.text=element_text(size=15),
            legend.title = element_text(size=15),
            legend.key.height = unit(2, "cm"),
            legend.key.width = unit(2, "cm"))
      
    
    })
  output$mytable = renderDataTable({
    data.table_df %>%
   filter(state == input$State)
    
  
 }) 
  
  
  output$plot2 <- renderLeaflet({
   
    leaflet() %>%
      addProviderTiles(provider="Esri") %>% 
      addMarkers(clusterOptions = markerClusterOptions(),lng = mass_shooting_df$longitude, 
                 lat = mass_shooting_df$latitude,
                 popup =paste("State:",mass_shooting_df$state,"<br>",
                              "City:",mass_shooting_df$city,"<br>",
                              "Case:",mass_shooting_df$case,"<br>",
                              "Location:",mass_shooting_df$incident_location,"<br>",
                              "Fatalities:",mass_shooting_df$fatalities, "<br>",
                              "Injured:",mass_shooting_df$injured, "<br>",
                              "Total_victims:",mass_shooting_df$total_victims, "<br>",
                              "Weapons_obtained_legally:",mass_shooting_df$weapons_obtained_legally, "<br>",
                              "Weapon_description:",mass_shooting_df$weapon_description, "<br>",
                              "Gender:",mass_shooting_df$gender, "<br>",
                              "Year:",mass_shooting_df$year, "<br>",
                              "Summary:",mass_shooting_df$summary,"<br>"))
  })
  

  

output$plot4 <- renderLeaflet({
  leaflet() %>%
    addProviderTiles(provider="Esri") %>% 
    addCircles(data=gun_states,color="red",radius=gun_states$registered_gun_18,lng = gun_states$Longitude, 
               lat = gun_states$Latitude,
               popup = paste("State:",gun_states$State,"<br>",
                             "registered_guns_2017:",gun_states$registered_gun_17,"<br>",
                             "registered_guns_2018:",gun_states$registered_gun_18,"<br>",
                             "guns_per_capita_2017:",gun_states$per_capita_17,"<br>",
                             "guns_per_capita_2018:",gun_states$per_capita_18,"<br>",
                             "per_capita_change:",gun_states$per_capita_change,"<br>"))
})



}



