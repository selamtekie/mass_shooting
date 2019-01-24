library(tidyverse)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(tidyverse)
library(splitstackshape)
library(janitor)
library(plotly)
library(ggplot2)
library(maptools)
library(ggmap)
library(data.table)
library(stringr)
library(leaflet)



#mass Shooting data
mass_shooting_data <- read_csv("~/midstone_project/data/Mother Jones - Mass Shootings Database, 1982 - 2018 - Sheet1.csv")
mass_shooting_data <- mass_shooting_data %>% separate(location, c('city','state'), sep=",")

get_abbrev <- function(long_name) {
  print(long_name)
  if(long_name=="D.C.") {
    state_abbrev <- "DC"
    }
    else if(nchar(long_name) > 2) {
      index_value <- match(long_name,state.name)
      state_abbrev <- state.abb[index_value]
    } 
  else {
    state_abbrev <-long_name
    }
return(state_abbrev)
  }
   
# trim the white space from the state column 
mass_shooting_data$state <- sapply(mass_shooting_data$state,str_trim)

# use get_abbrev to return 2 charachter abbrev for every state
mass_shooting_data$state_2 <- sapply(mass_shooting_data$state,get_abbrev)

mass_shooting_data$prior_signs_mental_health_issues[mass_shooting_data$prior_signs_mental_health_issues == "-"] <- "Unknown" 
mass_shooting_data$prior_signs_mental_health_issues[mass_shooting_data$prior_signs_mental_health_issues == "Unclear"] <- "Unknown" 

#deselecting states 
mass_shooting_df <- mass_shooting_data %>% select(., -state,-mental_health_details,-mental_health_sources,
                                                  -sources_additional_age,-date,
                                                  -weapon_details,-sources,-type,-race)

# renaming columns for clarity
mass_shooting_df<- mass_shooting_df %>% rename(weapon_description=weapon_type,incident_location=location_1,state=state_2)

# moving columns columns for clarity
mass_shooting_df <- mass_shooting_df %>%
  select(state, everything())

mass_shooting_df <- mass_shooting_df %>%
  select(case, everything())

# normalizing data
mass_shooting_df$weapons_obtained_legally[mass_shooting_df$weapons_obtained_legally == "Kelley passed federal criminal background checks; the US Air Force failed to provide information on his criminal history to the FBI"] <-"Yes" 
mass_shooting_df$weapons_obtained_legally[mass_shooting_df$weapons_obtained_legally == "Yes (\"some of the weapons were purchased legally and some of them may not have been\")"] <- "Yes"
mass_shooting_df$weapons_obtained_legally[mass_shooting_df$weapons_obtained_legally == "\nYes"] <- "Yes" 
mass_shooting_df$weapons_obtained_legally[mass_shooting_df$weapons_obtained_legally == "-"] <- "Unknown" 
mass_shooting_df$where_obtained[mass_shooting_df$where_obtained==  "NA" ] <-"Unknown"
mass_shooting_df$where_obtained[mass_shooting_df$where_obtained=="-"] <-"Unknown"
mass_shooting_df$where_obtained[mass_shooting_df$where_obtained=="(Unclear; investigators confirmed he owned 10 guns in total, 
                                                                                     all purchased and possessed legally, 
                                                                                     and had a handgun license)"] <- "Unclear"




# pltoing fatalities by state
ggplot(mass_shooting_df, aes(x=reorder(state_2,total_victims), y=total_victims)) + geom_bar(stat='identity') +
  xlab('State') + ylab('fatalities by State') + theme_minimal()+coord_flip()


#mass shooting locations by state 

mass_shooting_locations <- mass_shooting_df %>% 
  group_by(state_2,location_1) %>%
  summarize(location_count = n())

ggplot(mass_shooting_locations, aes(x=state_2, y=location_count,fill=location_1)) + geom_bar(stat='identity') +
  xlab('State') + ylab('mass shotting locations') + theme_minimal()


# splitting variables separarted by comma

library(splitstackshape)
mass_shooting_weapons <- cSplit(as.data.table(mass_shooting_df)[, weapon_type := gsub("[][\"]", "", weapon_type)], 
       "weapon_type", ",", "long")

# cleaning up weapons column
mass_shooting_status <- mass_shooting_weapons %>%
  group_by(state_2,weapons_obtained_legally) %>%
  summarize(weapon_legal_status = n())



# plotting weapons legal status
ggplot(mass_shooting_status, aes(x=state_2, y=weapon_legal_status,fill=weapons_obtained_legally)) + geom_bar(stat='identity',position = "fill") +
  xlab('State') + ylab('weapons') + theme_minimal()


# mental health status

mental_health_df <- mass_shooting_data %>%
  group_by(state_2,prior_signs_mental_health_issues) %>%
  summarize(total = n())

pie <- ggplot(mental_health_df, aes(x = "", fill = factor(prior_signs_mental_health_issues))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
         panel.grid  = element_blank()) + 
  
      labs(fill="Mental Health Issues", 
       x=NULL, 
       y=NULL)
      
       pie + coord_polar(theta = "y")


# mass shooting incidents by year
shooting_year <- mass_shooting_df %>%
  group_by(year,state_2) %>%
  summarise(year_total=n())
  
ggplot(shooting_year, aes(x=factor(year), y=year_total)) + geom_bar(stat='identity') +
  scale_y_continuous(breaks = seq(1,12, by = 1)) +
  xlab('State') + ylab('fatalities by city') + theme_minimal()  + coord_flip()


#Registered gun licenses and population by state 2016

guns_state_df <- read_csv("selam_midstone/data/licenses_state.csv")
View(guns_state_df)
colnames(guns_state_df)
colnames(guns_state_df)[colnames(guns_state_df)=="FFL Population"] <- "total"
colnames(guns_state_df)

# abbrevaiting stat names
guns_state_df$State<- c(guns_state_df$State)
guns_state_df$State<-state.abb[match(guns_state_df$State,state.name)]
View(guns_state_df)

# getting rid of headers
gun_states <- guns_state_df %>%
  select(., -X2,-X3,-X4,-X5)

# plotting guns by state
ggplot(gun_states, aes(x=reorder(State,total), y=total)) + geom_bar(stat='identity') +
     xlab('State') + ylab('Registered Gun Owners By State')+coord_flip()

# choropleth map for gun ownership by state
l <- list(color = toRGB("white"), width = 2)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = FALSE,
  lakecolor = toRGB('white')
)
plot_geo(gun_states, locationmode = 'USA-states') %>%
  add_trace(
    z = ~total, locations = ~State,zmin = 300, zmax = 8000,
    colors = 'Reds'
  ) %>%
  colorbar(title = "Fatatlities") %>%
  layout(
    geo = g)



#GUN CONTROL LEGISLATIONS  DATA

gun_control_state <- read_csv("~/midstone_project/data/gun_control.csv")
names(gun_control_state) <- gsub(" ", "_", names(gun_control_state))

setnames(gun_control_state, old = c('High_capacity_magazines_ban','Prohibitions_high_risk_individuals',
                                    'Prohibitions_for_individual_with_domestic_violence_convictions',
                                    'Mandatory_universal_background-checks'), 
         new = c('High_capacity_ban','High_risk_individuals',
                 'Domestic_violence_convictions','Background_checks'))

gun_control_state <- gun_control_state %>% select(-Total)

#gun_control_types <- gun_control_types %>% select(-total)

gun_control_state$State<- c(gun_control_state$State)
gun_control_state$State<-state.abb[match(gun_control_state$State,state.name)]
gun_control_state$total <- rowSums(gun_control_state[,2:8], na.rm=TRUE)
gun_control_state <- gun_control_state%>% select(-total)




library(reshape2)
gun_control_types <- melt(gun_control_state, id.vars=c('State'),var='Laws')
gun_control_types

#gun_control_types <- gun_control_types %>% select(-total)
#gun_control_types <- gun_control_types %>% select(-total)

# gun control legislations by state

ggplot(gun_control_types, aes(x=State,y=value,fill=Laws)) + 
  geom_bar(stat="identity",position = "fill") +
  theme_minimal()+ scale_colour_brewer(palette="Set3") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title = element_blank(),
        legend.text=element_text(size=15))
 
  
# choropleth map gun control 

l <- list(color = toRGB("white"), width = 2)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = FALSE,
  lakecolor = toRGB('white')
)
plot_geo(gun_control_state, locationmode = 'USA-states') %>%
  add_trace(
    z = ~total, locations = ~State,zmin = 0, zmax = 5,
    colors = 'Blues'
  ) %>%
  colorbar(title = "number of legislations") %>%
  layout(
    geo = g)

# guns per Capita by state

library(readxl)
population_guns_state <- read_excel("~/midstone_project/data/population_by_state (1).xlsx",col_names = FALSE)                                      
                                       
names(population_guns_state) <- NULL
population_guns_state<- population_guns_state[-c(1), ]

names(population_guns_state) <- as.matrix(population_guns_state[1, ])
#guns_per_capita <- population_by_state_1_[-1, ]
population_guns_state[] <- lapply(population_guns_state, function(x) type.convert(as.character(x)))
population_guns_state <- population_guns_state[-c(1), ]
population_guns_state$State<- gsub(".", "", population_guns_state$State, fixed = TRUE)
 
# abbrevaite state names
 
population_guns_state$State<- c(population_guns_state$State)
population_guns_state$State<-state.abb[match(population_guns_state$State,state.name)]

# calculate per capita gun by state
population_guns_state$registered_gun_17 <- as.numeric(as.character(population_guns_state$registered_gun_17))
population_guns_state$registered_gun_18 <- as.numeric((as.character(population_guns_state$registered_gun_18)))
population_guns_state$per_capita_17 <- (population_guns_state$registered_gun_17 / population_guns_state$`2017`)
population_guns_state$per_capita_18 <- (population_guns_state$registered_gun_18 / population_guns_state$`2018`)
population_guns_state$change_per_capita <-(population_guns_state$per_capita_18 - population_guns_state$per_capita_17)
 
 #States with the most guns per capita

#paste0( population_guns_state$registered_gun_18, collapse=",")
#head(arrange(population_guns_state,desc(registered_gun_18)), n = 20) 
#population_guns_state <-paste0( population_guns_state$registered_gun_18, collapse=",")



# plotting states highest gun ownership
guns.ordered_17 <- arrange(population_guns_state, registered_gun_17)
guns.top10_17 <- guns.ordered[1:10,]

ggplot(guns.top10_17,aes(x=reorder(State,-registered_gun_17),y=registered_gun_17)) + 
  geom_bar(stat='identity') + xlab('State') + ylab('Guns Registered')

 guns.ordered <- arrange(population_guns_state, -registered_gun_18)
 guns.top10 <- Africa.ordered[1:10,]
 
 
ggplot(guns.top10,aes(x=reorder(State,-registered_gun_18),y=registered_gun_18)) + 
   geom_bar(stat='identity') + xlab('State') + ylab('Guns Registered')
 
 
  #GUNS PER CAPITA BY STATE
 
 l <- list(color = toRGB("white"), width = 2)
 g <- list(
   scope = 'usa',
   projection = list(type = 'albers usa'),
   showlakes = FALSE,
   lakecolor = toRGB('white')
 )
 plot_geo(population_guns_state, locationmode = 'USA-states') %>%
   add_trace(
     z = ~per_capita_17, locations = ~State,zmin = 0.01, zmax = 0.025,
     colors = 'Blues'
   ) %>%
   colorbar(title = "Guns Per Capita") %>%
   layout(
     geo = g)
 
 
 
 # calculating chnage in guns per capita from 2017 - 2018 
 
 ggplot(population_guns_state,aes(x=State,y=change_per_capita)) + 
   geom_bar(stat='identity') + xlab('State') + ylab('Value')
 
 
 population_guns_state$Change <- ifelse(population_guns_state$change_per_capita < 0, "negative","positive")
 ggplot(population_guns_state,aes(State,change_per_capita,label="", width=.5))+
   geom_bar(stat="identity",position="identity",aes(fill = Change))
   scale_fill_manual(values=c(positive="firebrick1",negative="steelblue"))
 
 
   
  
   
   
 
 # merging latand lng file to per capita
library(readxl)
states_lat_lng <- read_excel("midstone_project/data/states_lat_lng.xlsx",skip=1)
states_lat_lng$State<- c(states_lat_lng$State)
states_lat_lng$State<-state.abb[match(states_lat_lng$State,state.name)]
gun_states <- merge(population_guns_state,states_lat_lng,by="State")
  
 
 
 # leaflet map r Mass Shooting Count
   leaflet() %>%
   addProviderTiles(provider="Esri") %>% 
   addMarkers(clusterOptions = markerClusterOptions(),lng = mass_shooting_df$longitude, 
              lat = mass_shooting_df$latitude,
              popup =paste("State:",mass_shooting_df$state_2,"<br>","City:",mass_shooting_df$city,
                           "<br>","Location:",mass_shooting_df$location_1,"<br>",
                           "Fatalities:",mass_shooting_df$fatalities, "<br>",
                           "Injured:",mass_shooting_df$injured, "<br>",
                           "Total_victims:",mass_shooting_df$total_victims, "<br>",
                           "Weapons_obtained_legally:",mass_shooting_df$weapons_obtained_legally, "<br>",
                           "Weapon_type:",mass_shooting_df$weapon_type, "<br>",
                           "Gender:",mass_shooting_df$gender, "<br>",
                           "Age_of_shooter:",mass_shooting_df$age_of_shooter, "<br>",
                           "Year:",mass_shooting_df$year, "<br>",
                           "Summary:",mass_shooting_df$summary, "<br>"))
              
 
# leaflet for guns ownershipe by state
#num<- as.numeric(gun_states$per_capita_18)
#x <- data.frame(gun_states,num)
   addCircles(data = breweries91, color = "red", radius = 100) # circle radius constant
   

   leaflet() %>%
   addProviderTiles(provider="Esri") %>% 
   addCircles(data=gun_states,color="red",radius=gun_states$registered_gun_18,lng = gun_states$Longitude, 
              lat = gun_states$Latitude,
              popup = paste("State:",gun_states$State,"<br>","registered_guns_2017:",gun_states$registered_gun_17,"<br>",
                            "registered_guns_2018:",gun_states$registered_gun_18,"<br>",
                            "guns_per_capita_2017:",gun_states$per_capita_17,"<br>",
                            "guns_per_capita_2017:",gun_states$per_capita_18,"<br>",
                            "change_per_capita:",gun_states$change_per_capita,"<br>"))
            
   # Mass Shootings by state
state_count <- mass_shooting_df %>%
     group_by(state_2) %>%
     summarise(state_total=n())


l <- list(color = toRGB("white"), width = 2)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = FALSE,
  lakecolor = toRGB('white')
)
plot_geo(state_count, locationmode = 'USA-states') %>%
  add_trace(
    z = ~state_total, locations = ~state_2,zmin = 1, zmax = 19,
    colors = 'Blues'
  ) %>%
  colorbar(title = "Mass Shootings by State") %>%
  layout(
    geo = g)





# merging mass shooting by state annd guns per capita data

guns_and_shootings <- merge(x=state_count,y=population_guns_state,by.x="state_2",by.y="State")

ggplot(guns_and_shootings, aes(x=per_capita_18, y=state_total,size=per_capita_18)) +
     geom_point(color='#8e82fe') + 
     geom_smooth(method = "lm") +
     theme_minimal() +
     xlab('Gun Per Capita 2018') + ylab('Mass Shooting 1982-2018')
   
   
   
   
   ggplotly(ggplot(guns_and_shootings, aes(x=per_capita_18, y=state_total)) + 
     geom_point(aes(size=state_2)) + 
     geom_smooth(method = "lm") +
     theme_minimal() + theme(legend.position="none") +
     xlab('Gun Per Capita 2018') + ylab('Mass Shooting 1982-2018'))
    
   
   
   
   
   
 
 