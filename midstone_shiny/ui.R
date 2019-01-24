
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

## Only run this example in interactive R sessions
library(shiny)
library(tidyverse)
library(shinythemes)
library(data.table)
source("./UI_COMPONENTS/header.R")
source("./UI_COMPONENTS/sidebar.R")
source("./UI_COMPONENTS/body.R")

ui <- dashboardPage(
  header,
  sidebar,
  body
)
  
  
  











        