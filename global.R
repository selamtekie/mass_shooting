library(shinydashboard)
library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)
library(shinythemes)


mass_shooting_df <- readRDS("~/selam_midstone/data/mass_shooting_df.rds")
population_guns_state <- readRDS("~/selam_midstone/data/population_guns_state.rds")
gun_control <- readRDS("~/selam_midstone/data/gun_control.rds")
states <- as.data.frame(gun_control) %>% 
  select(State) %>% 
  unique()

states <- sort(states$State)


weapon_status <- readRDS("~/selam_midstone/data/weapon_status.rds")
weapon_status <- weapon_status %>% rename(State=state_2)


states <- as.data.frame(weapon_status) %>% 
  select(State) %>% 
  unique()

states <- sort(states$State)
