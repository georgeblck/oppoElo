options(scipen = 999)
options(stringsAsFactors = FALSE)

# load packages
library(formatR)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(shiny)
local <- TRUE

# read data
if (local) {
  eloDat <- read.table("538elo.csv", header = TRUE, dec = ".", sep = ";")
  rm(local)
} else {
  url538 <- "https://projects.fivethirtyeight.com/nba-model/nba_elo.csv"
  eloDat <- read.table(url538, header = TRUE, sep = ",", dec = ".", na.strings = "")
  eloDat$date <- ymd(eloDat$date)
}

# Make conference dat
confDat <- data.frame(team1 = sort(unique(eloDat$team1[eloDat$season >= 1977])), 
                      conference = c(rep("East", 8), "West", "West", "East", "West", "West", "East", "West", 
                                     rep("West", 3), "East", "East", "West", "East", "East", "West", "West", 
                                     "East", "East", "West", "East", "East", rep("West", 6), "East", "West","West", "East", "East"))

# Pre-Calc data
tempElo <- eloDat %>% mutate(playoffgame = !is.na(playoff)) %>% group_by(season, team1) %>% 
  summarise_if(is.logical, sum) %>% right_join(eloDat, by = c("season", "team1")) %>% mutate(playoffteam = playoffgame > 0) %>% 
  filter(season >= 1977) %>% left_join(confDat, by = "team1") %>% mutate(playoffgame = !is.na(playoff))

# Make shiny
ui <- fluidPage(
   
   # Application title
   titlePanel("Retrospective strength of schedule in the NBA with 538s Elo Rating"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("choiceSeason",
                     "Which Seasons to include:",
                     min = 1977,
                     max = 2019,
                     value = c(2019,2019),
                     ticks = FALSE,
                     sep = ""),
         selectInput("choicePlayoffs", "",
                     choices = list("Regular Season" = FALSE,
                                    "Playoffs" = TRUE), multiple = TRUE,
                     selected = FALSE),
         checkboxInput("choiceConf", "Split by conference", value = TRUE),
         checkboxInput("choiceCarmelo", "Use 538s carmELO when possible (since 2015)", value = FALSE),
         width = 2
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         width = 10
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     iseason <- input$choiceSeason[1]:input$choiceSeason[2]
     
     if(input$choiceCarmelo){
       outDat <- tempElo %>% mutate(elo1_pre = ifelse(is.na(carmelo1_pre), ifelse(is.na(carm.elo1_pre), elo1_pre, carm.elo1_pre), carmelo1_pre),
                                     elo1_pre = ifelse(is.na(carmelo2_pre), ifelse(is.na(carm.elo2_pre), elo1_pre, carm.elo2_pre), carmelo2_pre))
     } else {
       outDat <- tempElo
     }
     
     # Make Data for graphic
     outDat <- outDat %>% filter(season %in% iseason, playoffgame %in% input$choicePlayoffs) %>% group_by(team1) %>% 
       summarise_at(vars(elo1_pre, elo2_pre, playoffteam), mean) %>% ungroup() %>% left_join(confDat, by = "team1")
     meanNorms <- outDat %>% group_by(conference) %>% summarise_at(vars(elo1_pre, elo2_pre), mean) %>% ungroup()
     if(input$choiceConf == FALSE){
       meanNorms <- meanNorms %>% summarise_if(is.numeric, mean)
     } 
     
     # Construct Plot
     if (length(iseason) > 1) {
       outPlot <- ggplot(outDat, aes(x = elo1_pre, y = elo2_pre, label = team1)) + 
         geom_hline(data = meanNorms,aes(yintercept = elo2_pre), linetype = "dotted", col = "grey") + 
         geom_vline(data = meanNorms, aes(xintercept = elo1_pre), linetype = "dotted", col = "grey") + 
         geom_label_repel(min.segment.length = 0.2,force = 3, box.padding = 0.2, label.padding = 0.2) + geom_point() + 
         theme_tufte(base_size = 15) + geom_rangeframe(col = "black") + xlab("Average Elo of Team") + ylab("Average Elo of opponent Team")
     } else {
       outPlot <- ggplot(outDat, aes(x = elo1_pre, y = elo2_pre, label = team1, colour = factor(playoffteam))) + 
         geom_hline(data = meanNorms, aes(yintercept = elo2_pre), linetype = "dotted", col = "grey") + 
         geom_vline(data = meanNorms, aes(xintercept = elo1_pre), linetype = "dotted", col = "grey") + 
         geom_label_repel(min.segment.length = 0.2, force = 3, box.padding = 0.2, label.padding = 0.2) + 
         geom_point() + theme_tufte(base_size = 15) + geom_rangeframe(col = "black") + xlab("Average Elo of Team") + 
         ylab("Average Elo of opponent Team") + 
         scale_colour_manual("Made Playoffs", guide = guide_legend(reverse = TRUE), breaks = 0:1, 
                             values = c("grey30", "goldenrod1"), labels = c("No", "Yes")) + 
         theme(legend.justification = c(0, 1), legend.position = c(0.01, 1), legend.background = element_rect(size = 0.5, linetype = "dotted"))
     }
     if(input$choiceConf){
       outPlot <- outPlot + facet_wrap(~conference, strip.position = "top", scales = "fixed", nrow = 1) 
     }
     outPlot
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

