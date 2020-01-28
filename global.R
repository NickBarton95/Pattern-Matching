# Library Management ####

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(roll)
library(zoo)
library(TTR)
library(tidyverse)
library(caret)
library(visNetwork)
library(rpart)
library(scales)
library(treeClust)
library(sparkline)
library(plotly)
library(scales)
library(DT)
library(data.table)
library(lubridate)
library(TTR)
library(shinyWidgets)
library(randomForest)
library(Hmisc)
library(flexdashboard)
library(dygraphs)
library(xts)
library(tictoc)
library(quantmod)
library(Hmisc)
library(ANN2)
library(shinyjs)
#library("V8")
library("dashboardthemes")
library(MLmetrics)
library(shinycssloaders)
library(shinyMatrix)
library(corrr)

options(warn = -1)

source("prep functions.R")
source("Pattern Matching Module.R")
source("Pattern Matching functions.R")

plot.colours = c("#003f5c",
                 "#007d9b",
                 "#00bfb1",
                 "#00ff95")

op.colours = paste0(plot.colours, "50")


data = read.csv("AllFoaming_assoc.csv")
data[,1] = parse_date_time(data[,1], c("dmY_HM"))

colnames(data) = c("Date Time", "Gas Flow", "Headspace Pressure", "Level", "Mixer Power",
                   "Temperature", "Power Output", "Digester Feed Flowrate", "Feed Pump Pressure", 
                   "Pasteuriser Pump Pressure", "Gas Flow ROC", "Headspace Pressure ROC", "Level ROC", "Mixer Power ROC",
                   "Temperature ROC", "Power Output ROC", "Digester Feed Flowrate ROC", "Feed Pump Pressure ROC", 
                   "Pasteuriser Pump Pressure ROC", "FoamAssoc")

title.logo = shinyDashboardLogoDIY(
  boldText = "Pattern",
  mainText = "Search", 
  textSize = 20,
  badgeBackColor = "#40E0D0",
  badgeTextColor = "white",
  badgeText = "v1"
)

jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"
style.str = "font-size: 80%; padding:2px; color: 
white; background-color: lime; width: 70px; display: inline-block; 
font-style: oblique; font-size: 15px; font-weight: bold;
text-shadow: 1px 1px silver"




