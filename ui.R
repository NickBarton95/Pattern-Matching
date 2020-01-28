# header ####
header = dashboardHeader(title = title.logo)
# sidebar ####
sidebar = dashboardSidebar(
  sidebarMenu(
    id = "menu",

    menuItem("Pattern Matching", tabName = "pattern",icon = icon("chart-line"))
 
  )
)

# send to ui ####

body = dashboardBody(

  extendShinyjs(text = jscode),
  shinyDashboardThemes(theme = "poor_mans_flatly"),
  
  tabItems(
    tabItem(
      tabName = "pattern", 
      fluidPage(PatternMatchingUI("test3", data))
    )
  )
  
)


ui = dashboardPage(header, sidebar, body, useShinyjs())
