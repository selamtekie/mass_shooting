body <- dashboardBody(
  tabItems(
    tabItem(tabName = "map",
            fluidRow(
              box(title = 'US Mass Shooting by State', 
                  status = 'primary',width='12', solidHeader = T,
                  leafletOutput("plot2", height = '400px')
                  ),
             box(title = 'Gun Ownership by State',
                  status = 'primary',width='12', solidHeader = T,
                  leafletOutput("plot4",height = '400px'))
            )
    ),
    # first tab content
    tabItem(tabName = "states",
            title = "State", status = "primary", solidHeader = TRUE,
            h3(""),
            fluidRow(
              box(
                selectInput("State", 
                            label = "State:", 
                            choices = states,
                            selected = 'CA'))),
            fluidRow(
              box(title ='Gun Control Regulations', 
                  status = 'primary',width=12,solidHeader = T,
                  plotOutput("plot", height = 400,width=800)),
              
                box(title = "",
                  status = "primary",width=12,solidheader=T,searching = FALSE,
                  dataTableOutput("mytable"))
            )
            
    ),
              
              tabItem(tabName = "ATF",
                      h3('Firearms Commerce in the United States Annual Statistical Update 2017'),
                      ATF_url <-a('Bureau of Alcohol, Tobacco, Firearms and Explosives',href="https://www.atf.gov/resource-center/docs/undefined/firearms-commerce-united-states-annual-statistical-update-2017/download",target = "_blank"),
                      h3('Firearms Commerce in the United States Annual Statistical Update 2018'),
                      ct_url <- a('Bureau of Alcohol, Tobacco, Firearms and Explosives',href="https://www.atf.gov/file/130436/download",target = "_blank"),
                      h3('State Firearm Laws'),
                      ct_url <-a('State Firearm Laws',href='https://statefirearmlaws.org/',target = "_blank")
              
              
              
              
    )
  )
)


