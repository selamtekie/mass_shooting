library(shinydashboard)
library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)
library(shinythemes)
library(leaflet)
library(data.table)


mass_shooting_df <- readRDS("~/midstone_project/data/mass_shooting_df.rds")
population_guns_state <- readRDS("~/midstone_project/data/population_guns_state.rds")
gun_states <- readRDS("~/midstone_project/data/gun_states.rds")
guns_and_shootings <- readRDS("~/midstone_project/data/guns_and_shootings.rds")
gun_control_types <- readRDS("~/midstone_project/data/gun_control_types.rds")
mass_shooting_status <- readRDS("~/midstone_project/data/mass_shooting_status.rds")
mass_shooting_status <- as.data.table(mass_shooting_status)
gun_control <- readRDS("~/midstone_project/data/gun_control_types.rds")




# gun control state

states <- as.data.frame(gun_control) %>% 
  select(State) %>% 
  unique()

states <- sort(states$State)





# data table data
data.table_df <- mass_shooting_df %>% select(.,-summary,-latitude,-longitude,-fatalities,
                                             -injured,-total_victims,-age_of_shooter,-gender,-where_obtained)
states <- as.data.frame(data.table_df) %>% 
  select(state) %>% 
  unique()

states <- sort(states$state)


