sidebar <- dashboardSidebar(
  dashboardSidebar(
    sidebarMenu(
      menuItem("Guns and Mass Shootings",tabName="map"),
      menuItem("State By State Analysis", tabName = "states"),
      menuItem("Sources",tabName = "ATF", icon = icon("database"))
      
    )
  )
)
