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



#mass Shooting data
mass_shooting_data <- read_csv("selam_midstone/data/Mother Jones - Mass Shootings Database, 1982 - 2018 - Sheet1.csv")

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


mass_shooting_df <- mass_shooting_data %>% select(., -state,-mental_health_details,-case,-prior_signs_mental_health_issues,-mental_health_sources,-sources_additional_age,-summary,-date,-where_obtained,-weapon_details,-sources,-type,-race)



ggplot(mass_shooting_df, aes(x=reorder(state_2,total_victims), y=total_victims)) + geom_bar(stat='identity') +
  xlab('State') + ylab('fatalities by city') + theme_minimal()+coord_flip()


#mass shooting locations by state 

mass_shooting_locations <- mass_shooting_df %>% 
  group_by(state_2,location_1) %>%
  summarize(location_count = n())

ggplot(mass_shooting_locations, aes(x=state_2, y=location_count,fill=location_1)) + geom_bar(stat='identity') +
  xlab('State') + ylab('mass shotting locations') + theme_minimal()




library(splitstackshape)
mass_shooting_weapons <- cSplit(as.data.table(mass_shooting_df)[, weapon_type := gsub("[][\"]", "", weapon_type)], 
       "weapon_type", ",", "long")

mass_shooting_status <- mass_shooting_weapons %>%
  group_by(state_2,weapons_obtained_legally) %>%
  summarize(weapon_legal_status = n())


ggplot(mass_shooting_status, aes(x=state_2, y=weapons_obtained_legally)) + geom_bar(stat='identity') +
  xlab('State') + ylab('fatalities by city') + theme_minimal()

mass_shooting_status$weapons_obtained_legally[mass_shooting_status$weapons_obtained_legally == "Kelley passed federal criminal background checks; the US Air Force failed to provide information on his criminal history to the FBI"] <-"Yes" 
mass_shooting_status$weapons_obtained_legally[mass_shooting_status$weapons_obtained_legally == "Yes (\"some of the weapons were purchased legally and some of them may not have been\")"] <- "Yes"
mass_shooting_status$weapons_obtained_legally[mass_shooting_status$weapons_obtained_legally == "\nYes"] <- "Yes" 



ggplot(mass_shooting_status, aes(x=state_2, y=weapon_legal_status,fill=weapons_obtained_legally)) + geom_bar(stat='identity',position = "fill") +
  xlab('State') + ylab('fatalities by city') + theme_minimal()










#Registered gun licenses and population by state 2016

guns_state_df <- read_csv("selam_midstone/data/licenses_state.csv")
View(guns_state_df)
colnames(guns_state_df)
colnames(guns_state_df)[colnames(guns_state_df)=="FFL Population"] <- "total"
colnames(guns_state_df)

guns_state_df$State<- c(guns_state_df$State)
guns_state_df$State<-state.abb[match(guns_state_df$State,state.name)]
View(guns_state_df)

gun_states <- guns_state_df %>%
  select(., -X2,-X3,-X4,-X5)


ggplot(gun_states, aes(x=reorder(State,total), y=total)) + geom_bar(stat='identity') +
     xlab('State') + ylab('Registered Gun Owners By State')+coord_flip()

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

gun_control_state <- read_csv("selam_midstone/data/gun_control.csv")
View(gun_control_state)
gun_control_state
names(gun_control_state) <- gsub(" ", "_", names(gun_control_state))

setnames(gun_control_state, old = c('High_capacity_magazines_ban','Prohibitions_high_risk_individuals',
                                    'Prohibitions_for_individual_with_domestic_violence_convictions',
                                    'Mandatory_universal_background-checks'), 
         new = c('High_capacity_ban','high_risk_individuals',
                 'domestic_violence_convictions','background-checks'))

gun_control_state <- gun_control_state %>% select(-Total)
View(gun_control_state)


gun_control_state$State<- c(gun_control_state$State)
gun_control_state$State<-state.abb[match(gun_control_state$State,state.name)]
View(gun_control_state)

gun_control_state$total <- rowSums(gun_control_state[,2:8], na.rm=TRUE)
  View(gun_control_state)



library(reshape2)

 gun_control_types <- melt(gun_control_state, id.vars=c('State'),var='Laws')
gun_control_types

gun_control_types <- gun_control_types %>% select(-total)
View(gun_control_types)

gun_control_chart <- gun_control_types %>%
  select(1, 2, 3) %>%
  ggplot(aes(x=State, y=Laws, fill=value)) +
  geom_bar(stat = "identity")
  ggplotly(gun_control_chart)

           
  ggplot(gun_control_types, aes(x=State, y=value,fill=Laws)) + geom_bar(stat="identity",position = "fill") +
    xlab('State') + ylab('Gun Control Laws') + theme_minimal()
  




View(gun_control_state$total)


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

#guns per Capita

library(readxl)
population_guns_state <- read_excel("selam_midstone/data/population_by_state (1).xlsx",col_names = FALSE)
                                                                                                                        
                                       
View(population_guns_state)
names(population_guns_state) <- NULL


#names(population_guns_state) <- NULL

population_guns_state<- population_guns_state[-c(1), ]

 
 
 
 names(population_guns_state) <- as.matrix(population_guns_state[1, ])
  #guns_per_capita <- population_by_state_1_[-1, ]
 population_guns_state[] <- lapply(population_guns_state, function(x) type.convert(as.character(x)))
 population_guns_state <- population_guns_state[-c(1), ]
  
population_guns_state$State<- gsub(".", "", population_guns_state$State, fixed = TRUE)
 
 
 
 

population_guns_state$State<- c(population_guns_state$State)
population_guns_state$State<-state.abb[match(population_guns_state$State,state.name)]



population_guns_state$registered_gun_17 <- as.numeric(as.character(population_guns_state$registered_gun_17))
population_guns_state$registered_gun_18 <- as.numeric((as.character(population_guns_state$registered_gun_18)))

 population_guns_state$per_capita_17 <- (population_guns_state$registered_gun_17 / population_guns_state$`2017`)
 population_guns_state$per_capita_18 <- (population_guns_state$registered_gun_18 / population_guns_state$`2018`)
 
 
 

 
 #States with the most guns per capita
 
 ggplot(population_guns_state,aes(x=reorder(State,registered_gun_17),y=registered_gun_17,fill=(registered_gun_18))) + geom_bar(stat='identity') + xlab('State') + ylab('Value')
 
 ggplot(population_guns_state,aes(x=reorder(State,registered_gun_18),y=registered_gun_18)) + geom_bar(stat='identity') + xlab('State') + ylab('Value')
 
 ggplot(population_guns_state,aes(x=reorder(State,per_capita_17),y=per_capita_17)) + geom_bar(stat='identity') + xlab('State') + ylab('Value')
 
 #PCT CHANGE
 

 
 
 l <- list(color = toRGB("white"), width = 2)
 g <- list(
   scope = 'usa',
   projection = list(type = 'albers usa'),
   showlakes = FALSE,
   lakecolor = toRGB('white')
 )
 plot_geo(population_guns_state, locationmode = 'USA-states') %>%
   add_trace(
     z = ~per_capita_17, locations = ~State,zmin = 0.00, zmax = 0.025,
     colors = 'Blues'
   ) %>%
   colorbar(title = "Guns Per Capita") %>%
   layout(
     geo = g)
