#########################################################################
#                                                                       #
#############       Olympic Data Analysis        ########################

#########################################################################

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2)
library(reshape2)
library(ggrepel)

########################## Load Data File and extract data frames for reporting #########

joindata <- read.csv('JoinedData.csv')
joindata$Population <- joindata$Population/1000000


## Extract rows which have medals only and count of medals for country, year and sex 
medaltally1 <- count(subset(joindata, is.na(joindata$Medal) == FALSE), Country,Year, Sex,Sport, Medal)


## dcast is a function from the reshape2 library
medaltally <- dcast(medaltally1, Country + Year + Sex + Sport ~ Medal, fill = 0)
Top20 <- medaltally1 %>% group_by(Country) %>% 
  summarise(totalMedal = sum(n)) %>% 
  arrange(desc(totalMedal)) %>% 
  head(20) %>% 
  pull(Country)

#########################  data preparation for  Graph ######################################

d1 <- medaltally1   
d1 <- group_by(medaltally1,Country)  %>%   summarise(sum(n)) %>% setNames(., c("Country" ,"TotMedals"))
d2 <- inner_join(d1, distinct(joindata,Country, joindata$GDP....per.capita., Population), by = "Country") %>%
  setNames(.,c("Country","Totmedals","Population","Percapita"))

######################### Server Functions ##################################################

server <- function(input, output) {

#########################    DATA TABLE SELECTION  - SPORT ,  SEX and YEARS  #################    
  
  dat <- reactive({ 
    temp <- medaltally
    if (input$Sport[1] != "ALL") {
      temp <- filter(medaltally, Sport %in% input$Sport) 
    }
    if (input$Sex != "ALL") {
      temp <- filter(temp, Sex %in% input$Sex) 
    }
    temp <- subset(temp,Year >=  input$daterange[1] &  Year <= input$daterange[2])
    temp
  })
  dat
  dat1 <- reactive({
    datx  <-subset(d1,d1$Year >=  input$daterange[1] & d1$Year <= input$daterange[2])
  })          
  dat1         
  output$medalTally <- DT::renderDataTable({ 
    DT::datatable(dat() , options = list(pageLength=10, pagingType = "full_numbers" )) %>% 
      formatStyle(colnames(dat()), background = 'white')    
  })
  
  
#########################    MEDAL GRAPH SELECTION AND OUTPUT  ################################# 
  
  
    dat2 <- reactive ({ 
    temp1 <- medaltally1 
    temp1$Medal <- factor(temp1$Medal, levels = c("Gold", "Silver", "Bronze"))
    ###Syukri:Added code to limit the view to top 20 of accumulated medals
    if (input$Sport[1] == "ALL") {
      temp9 <- temp1 %>% group_by(Country) %>% 
        summarise(totalMedal = sum(n)) %>% 
        arrange(desc(totalMedal)) %>% 
        head(30) %>% 
        pull(Country)
      temp1 <- temp1 %>% filter(Country %in% temp9)
    }
    else if(input$Sport[1] != "ALL") {
      temp1 <- filter(temp1, Sport %in% input$Sport) 
      temp9 <- temp1 %>% group_by(Country) %>% 
        summarise(totalMedal = sum(n)) %>% 
        arrange(desc(totalMedal)) %>% 
        head(30) %>% 
        pull(Country)
      temp1 <- temp1 %>% filter(Country %in% temp9)
    } 
    
    if (input$Sex[1] != "ALL") {
      temp1 <- filter(temp1, Sex %in% input$Sex) 
    } 
          
    temp1 <-subset(temp1, Year >=  input$daterange[1] & Year <= input$daterange[2])
    temp1
  })        
  dat2     
  
###to be edit and filter...Syukri:done pls see above instead. I change the temp1 input1 by adding if else statement
  output$tallyGraph <- renderPlot({  
    
    ggplot(dat2(), aes(x=Country, y= n, fill = Medal))  + geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      ylab("Total Medals")
    
  } )             
  
  
#############################  PLOT - MEDALS AND POPULATION RANGE #############################
  
  dat3 <- reactive ({ 
    temp4 <-subset(d2, Population  <= input$Poprange[2]  & Population  >= input$Poprange[1] )
    temp4  <- subset(temp4, Totmedals  >= input$Medalrange[1]   & Totmedals <= input$Medalrange[2])
    temp4
  })        
  dat3                           
  # Population Graph vs Total Medals 
   output$Population <- renderPlot({
    ggplot(dat3(), aes(x=Population, y= Totmedals)) + geom_point(color = 'red') + geom_text_repel(label=  dat3()$Country) + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      ylab("Total Medals")
  } )                
    
   
  output$medal1Plot <- renderPlot({
    ggplot(dat2(), aes(x=Country, y= n, fill = Medal)) + geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      ylab("Total Medals")
  })
  
   
#############################  PLOT - MEDALS AND PER CAPITA  RANGE #############################
   
  dat4 <- reactive ({ 
    
    temp5 <-subset(d2, Percapita  <= input$Percapita[2]  & Population  >= input$Percapita[1] )
    temp5  <- subset(d2, Totmedals  >= input$Medalrange[1]   & Totmedals <= input$Medalrange[2])
    temp5
  })        
  dat4    
  
 # Graph for Total medals vs GDP Per capita 
  output$perCapita <- renderPlot({
    
    ggplot(dat4(), aes(x=Percapita, y= Totmedals)) + geom_point(color = 'blue')    + geom_text_repel(label=  dat4()$Country) +
      #                    ggplot(dat2(), aes(x=Country, y= n, fill = Medal)) + geom_bar(stat = "identity") +
      
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      ylab("Total Medals") 
      
  } )                
  

  
###############################  Prepare Data for Regression ############################### 
  
  #list data for all countries with 0 or more total medals  
  medaltally2 <- count(joindata, Country, Year,Medal,Population,`GDP....per.capita.`)
  
  #count NA   
  f1  = function(df, y)  { 
    if    (is.na(df[3]) ) { 
      df[6] <- as.numeric(0)  
    } 
    else {  df[6] <- as.numeric(df[6])}
  }
  
  medaltally2[6] <- apply(medaltally2,1,f1)
  
  #final count for plotting               
  medaltally3 <- group_by(medaltally2,Country,Year,Population, `GDP....per.capita.`) %>%  summarise(sum(n)) 
  
  
############################### PLot -  Regression Plots  ###############################
  
  output$Regresspop   <- renderPlot   ({ 
    ggplot(medaltally3, aes(x = Population, y = `sum(n)`, color = Year))  + geom_point() + scale_color_gradient(low="blue", high="red") + geom_smooth(method = lm) +
      labs(title = "Medals vs Population",  x= "Population In Millions", y  = "Total Medals")
  })
  output$Regresscap   <- renderPlot   ({
    ggplot(medaltally3, aes(x = `GDP....per.capita.`, y = `sum(n)`,color = Year))  + geom_point() +geom_smooth(method = lm)  +
      labs(title = "Medals vs GDP Per Capita ",  x= "GDP Per Capita", y  = "Total Medals")
  })  
  
##############################   Predictions   ############################################
  
#Predictions - Population 
  
  newpop <-  reactive({ 
    temp <- unlist(strsplit(input$Popdata, split = ","))
    tempx <- data.frame(Population = temp)
    tempx[,'Population'] <- as.numeric(as.character(tempx[,'Population']))
    tempx
  })
  predpop <- lm(`sum(n)`~ Population,data = medaltally3)
  predcap <- lm(`sum(n)`~ `GDP....per.capita.` ,data = medaltally3)
  output$Popresults <-renderTable ({ #newpop()
    predict(predpop,newpop())
  })
  
#Predictions - GDP Per Capita  
  newcap <-  reactive({ 
    temp <- unlist(strsplit(input$Capdata, split = ","))
    tempx <- data.frame(`GDP....per.capita.` = temp)
    tempx[,'GDP....per.capita.'] <- as.numeric(as.character(tempx[,'GDP....per.capita.']))
    tempx
  })

  output$Capresults <-renderTable ({   
    predict(predcap,newcap())
  })
   
  ##############################   Help   ############################################
  
   url <- a("Help Page here", href="https://www.google.com/")
    helpdoc <- ("For application guide please click the link button below.")
  output$helptext <- renderText({
    print(helpdoc)
  })  
  output$helpui <-   renderUI({
    tagList("***", url)
  })
} 
               
