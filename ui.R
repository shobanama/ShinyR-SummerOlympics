
##################### load libraries ###################################.

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2)
library(reshape2)
library(ggrepel)

########################## Load Data File and extract data frames for reporting #########

joindata <- read.csv('JoinedData.csv')
joindata$Population <- joindata$Population/1000000 # scale population for analysis 

## Extract rows which have medals only and count of medals for country, year and sex 
medaltally1 <- count(subset(joindata, is.na(joindata$Medal) == FALSE), Country,Year, Sex,Sport, Medal)
# Syukri reorder the level of Medal
medaltally1$Medal <- factor(medaltally1$Medal, levels = c("Gold","Silver","Bronze"))


## dcast is a function from the reshape2 library to bring it to wide format where separate columns for gold sliver bronze are there
medaltally <- dcast(medaltally1, Country + Year + Sex + Sport ~ Medal, fill = 0)


##################  data preparation  for  Medal Graph ##############################################

d1 <- medaltally1   
d1 <- group_by(medaltally1,Country)  %>%   summarise(sum(n)) %>% setNames(., c("Country" ,"TotMedals"))
d2 <- inner_join(d1, distinct(joindata,Country, joindata$GDP....per.capita., Population), by = "Country") %>%
  setNames(.,c("Country","Totmedals","Population","Percapita"))


################### Olympics UI File ########################################################
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Summer Olympics Analysis"),
                    dashboardSidebar( width = 120, 
                                      sidebarMenu(
                                        #menuItem(".", tabName = "blank", icon = icon(" ")),
                                        menuItem("Graphs", tabName = "graphs", icon = icon("chart-bar"), selected = TRUE),
                                        menuItem("XYPlots", tabName = "xyplots", icon = icon("chart-line")) ,
                                        menuItem("Analysis", tabName = "analysis", icon = icon("lightbulb")) ,
                                        menuItem("Help", tabName = "help", icon = icon("question"))  
                                        
                                      )
                    ) ,
                    
                    dashboardBody(
                      #  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),
                      tags$head(tags$style(HTML('.content-wrapper {background-color:black;}')                                            )), 
                      tabItems( 
                        tabItem(tabName = "graphs",
                                fluidRow(
                                  box(color = "black",title =  "Medal Tally", width =6, DT::dataTableOutput("medalTally")), 
                                  box(title = "Medal Tally", width = 6, plotOutput("tallyGraph" , height = 500, width = 620) )  
                                ) , 
                                fluidRow( 
                                  box(width = 12, background= "black", color = "black", title = "  "),     
                                  box(width = 8,color = "black",
                                      checkboxGroupInput("Sport",  "Sport",  choices = c("ALL",unique(as.character(medaltally$Sport)) ),width = 800, inline = TRUE,selected = "ALL" ) 
                                  ) , 
                                  box(width = 2, color = "black",sliderInput("daterange", "Year:",
                                                                             min = min(medaltally$Year), max = max(medaltally$Year),
                                                                             value = c(2004,2016),width = 250))  ,
                                  box(width = 1,color = "black",
                                      checkboxGroupInput("Sex",  "Sex",  choices = c("ALL",unique(as.character(medaltally$Sex)) ), selected = "ALL",width =20 )
                              ) 
                                )       
                        ) , 
                        
                        tabItem(tabName = "xyplots",
                        fluidRow(
                          box(title = "Medals vs Population", width = 6, plotOutput("Population", height = 320, width = 580)) , 
                            box(color ="black",title = "Medals vs GDP Per Capita", width = 6, plotOutput("perCapita" , height = 320, width = 580) )
                        ),
                        fluidRow( 
                          box(width = 12, background= "black", color = "black", title = "      "),
                          
                          box(width = 3, color = "black",sliderInput("Poprange", "Population in Million:",
                                                                   min = min(d2$Population), max = max(d2$Population),
                                                                   value = c(20,1400)))  ,                
                        box(width = 3, color = "black", sliderInput("Percapita", "GDP Per Capita:",
                                                                    min = min(d2$Percapita), max = max(d2$Percapita),
                                                                    value = c(4000,100000)))  , 
                        
                        box(width = 3, color = "black",sliderInput("Medalrange", "Total Medals:",
                                                                   min = min(d2$Totmedals), max = max(d2$Totmedals),
                                                                   value = c(30,8000)))       
                          )
                        
                        ), 
                        
                        tabItem(tabName = "analysis",
                                fluidRow(
                                  box(width = 6, plotOutput("Regresspop" ,  width = 550)) , 
                                  box( color = 'black' , width =6, plotOutput("Regresscap",width = 550)) ,   
                                  box(width = 6, color = 'black', textInput("Popdata", label =  "Enter Population  Data in Millions",value = '23,36,42' )),
                                  box(color = 'black', textInput("Capdata", label =  "Enter GDP Per Capita  Data ",value = '23000,36000,82000' )),
                                  box( color = 'black',title = "Prediction Medals - Population Millions", width =3, tableOutput("Popresults"))  , 
                                  box( color = 'black',title = "Prediction Medals - GDP Per Capita", width =3, tableOutput("Capresults"))   
                                )
                        )  ,
                        
                        tabItem(tabName = "help", 
                                
                                fluidRow(
                                   box(width = 10, height = 100, textOutput("helptext")),
                                   box(width = 5 , background = "yellow", color = "blue", uiOutput("helpui"))   
                                  
                                )
                                
                                )
                      )  
                    )   
            
)


