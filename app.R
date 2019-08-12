options(scipen = 999)
options(stringsAsFactors = FALSE)

# load packages
library(tidyverse)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(shiny)
library(stringi)

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

# Double the data
eloDatRe <- eloDat
colnames(eloDatRe) <- stri_replace_all_fixed(colnames(eloDatRe),
                                             c(1,2), c(2,1), mode = "first")
eloList <- list(eloDat, eloDatRe)
eloDat <- data.table::rbindlist(eloList, use.names=TRUE, idcol=TRUE)

# Make conference dat
confDat <- data.frame(team1 = sort(unique(eloDat$team1[eloDat$season >= 1977])), 
                      conference = c(rep("East", 8), "West", "West", "East", "West", "West", "East", "West", 
                                     rep("West", 3), "East", "East", "West", "East", "East", "West", "West", 
                                     "East", "East", "West", "East", "East", rep("West", 6), "East", "West","West", "East", "East"))

replaceVec <- c("VAN" = "MEM", "WSB" = "WAS", "SEA" = "OKC", "SDC" = "LAC", "NYN" = "BRK",
                "NOK" ="NOP", "NJN" = "BRK", "NOJ" = "UTA", "KCK" = "SAC", "BUF" = "LAC",
                "CHH" = "CHO", "DNA" = "DEN", "INA" = "IND")

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
        conditionalPanel(condition = "input.tabselected==1",
         sliderInput("choiceSeason",
                     "Which seasons to include:",
                     min = 1977,
                     max = 2019,
                     value = c(2019,2019),
                     ticks = FALSE,
                     sep = "")),
        
         radioButtons("choicePlayoffs", "Which part of season to include:",
                     choices = list("Only Regular" = 0,
                                    "Only Playoffs" = 1,
                                    "Both" = 2), selected = 0),
        conditionalPanel(condition = "input.tabselected==2",
                         p("Use custom table filters to change its content."),
                         strong("Relative Season Strength:"), p("z-Transformed average Team Elo of Season. 
                           E.g. the value 1 = Team was one StDev stronger than the average NBA team")),
         conditionalPanel(condition = "input.tabselected==1",
         h4("Plot options"),
         checkboxInput("choiceConf", "Split by conference", value = TRUE),
         checkboxInput("choiceSplitSeason", "Split by season", value = FALSE),
         checkboxInput("choiceCarmelo", "Use 538s carmELO when possible (available since 2015)", value = FALSE),
         checkboxInput("choiceHomo", "Unify old Franchise Names (e.g. WSB to WAS)", value = TRUE)),
         width = 2
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Elo vs. Opponent-Elo", value = 1, plotOutput("distPlot", height="auto")),
          tabPanel("Who had the hardest Season/Playoffs ever?",value = 2, DT::dataTableOutput("table")),
          id = "tabselected"), width = 10
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
   output$distPlot <- renderPlot({
     # Render the input information
     iseason <- input$choiceSeason[1]:input$choiceSeason[2]
     ifelse(input$choicePlayoffs==0, iplayoff <-FALSE, ifelse(input$choicePlayoffs == 1, iplayoff <-TRUE, iplayoff <- c(TRUE, FALSE)))
     
     # If CarmeELO replace Elo
     if(input$choiceCarmelo){
       outDat <- tempElo %>% mutate(elo1_pre = ifelse(is.na(carmelo1_pre), ifelse(is.na(carm.elo1_pre), elo1_pre, carm.elo1_pre), carmelo1_pre),
                                     elo2_pre = ifelse(is.na(carmelo2_pre), ifelse(is.na(carm.elo2_pre), elo2_pre, carm.elo2_pre), carmelo2_pre))
     } else {
       outDat <- tempElo
     }
     # If franchise names should be homogenized
     if(input$choiceHomo){
       outDat <- outDat %>% mutate(team1 = str_replace_all(team1, replaceVec),
                                   team2 = str_replace_all(team2, replaceVec))
     }
     
    # Make Data for graphic
     if(input$choiceSplitSeason == FALSE){
       outDat <- outDat %>% filter(season %in% iseason, playoffgame %in% iplayoff) %>% group_by(team1) %>% 
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
           theme_tufte(base_size = 17) + geom_rangeframe(col = "black") + xlab(paste0("Average Elo of Team ", "(", min(iseason), "-", max(iseason), ")"))+ 
           ylab(paste0("Average Elo of Opponent ", "(", min(iseason), "-", max(iseason), ")"))
       } else {
         outPlot <- ggplot(outDat, aes(x = elo1_pre, y = elo2_pre, label = team1, colour = factor(playoffteam))) + 
           geom_hline(data = meanNorms, aes(yintercept = elo2_pre), linetype = "dotted", col = "grey") + 
           geom_vline(data = meanNorms, aes(xintercept = elo1_pre), linetype = "dotted", col = "grey") + 
           geom_label_repel(min.segment.length = 0.2, force = 3, box.padding = 0.2, label.padding = 0.2) + 
           geom_point() + theme_tufte(base_size = 17) + geom_rangeframe(col = "black") + xlab(paste0("Average Elo of Team (", iseason,")")) + 
           ylab(paste0("Average Elo of Opponent (", iseason,")")) + 
           scale_colour_manual("Made Playoffs", guide = guide_legend(reverse = TRUE), breaks = 0:1, 
                               values = c("grey30", "orange"), labels = c("No", "Yes")) + 
           theme(legend.justification = c(0, 1), legend.position = c(0.01, 1), legend.background = element_rect(size = 0.5, linetype = "dotted"))
       }
     } else {
       outDat <- outDat %>% filter(season %in% iseason, playoffgame %in% iplayoff) %>% group_by(season, team1) %>% 
         summarise_at(vars(elo1_pre, elo2_pre, playoffteam), mean) %>% ungroup() %>% left_join(confDat, by = "team1") %>%
         mutate(index = paste0(season, team1))
       outPlot <- ggplot(outDat, aes(x = elo1_pre, y = elo2_pre, label = index, colour = factor(playoffteam))) + 
         geom_label() + theme_tufte(base_size = 17) + geom_rangeframe(col = "black") + xlab("Average Elo of Team per Season") + 
         ylab("Average Elo of Opponent per Season") + 
         scale_colour_manual("Made Playoffs", guide = guide_legend(reverse = TRUE), breaks = 0:1, 
                             values = c("grey30", "orange"), labels = c("No", "Yes")) + 
         theme(legend.justification = c(0, 1), legend.position = c(0.01, 1), legend.background = element_rect(size = 0.5, linetype = "dotted"))
     }
     
     # Remove all legends if only playoffs
     if(input$choicePlayoffs == 1){
       outPlot <- outPlot + theme(legend.position="none")
     }
     if(input$choiceConf){
       outPlot <- outPlot + facet_wrap(~conference, strip.position = "top", scales = "fixed", nrow = 1) 
     }
     outPlot
   }, height = function() {
     session$clientData$output_distPlot_width/2.2
   })
   
   # Make the table output
   output$table <- DT::renderDataTable({
     ifelse(input$choicePlayoffs==0, iplayoff <-FALSE, ifelse(input$choicePlayoffs == 1, iplayoff <-TRUE, iplayoff <- c(TRUE, FALSE)))
     # Get relative performance during season
     zDat <- tempElo %>% filter(is.na(playoff)) %>% group_by(season, team1) %>% 
       summarise_at(vars(elo1_pre, elo2_pre, playoffteam), mean) %>% ungroup() %>% 
       group_by(season) %>% mutate(elo1_z = as.numeric(scale(elo1_pre)), elo2_z = as.numeric(scale(elo2_pre))) %>% ungroup() %>%
       select(-playoffteam, -elo2_pre , -elo1_pre)
     
     # Make Data frame for table
     tableDat <- tempElo %>% filter(playoffgame %in% iplayoff) %>% add_count(season, team1, name = "playoffgames") %>% 
       group_by(season, team1) %>% mutate(playoffseries = n_distinct(team2)) %>% ungroup() %>%
       group_by(season, team1) %>% summarise_at(vars(elo1_pre, elo2_pre, playoffgames, playoffteam, playoffseries), mean) %>% 
       ungroup() %>% left_join(confDat, by = "team1") %>%
       arrange(-elo2_pre) %>% left_join(zDat, by = c("season", "team1")) %>%
       select(-elo1_pre, -elo2_z) %>% mutate(playoffteam = factor(playoffteam, levels = 0:1, labels = c("No", "Yes")),
                                             conference = factor(conference), playoffseries = as.integer(playoffseries), 
                                             playoffgames = as.integer(playoffgames),
                                             elo2_pre = round(elo2_pre, 3), elo1_z = round(elo1_z, 3))
     
     if(any(iplayoff == FALSE)){
       tableDat <- tableDat %>% select(-playoffgames, -playoffseries)
       colVector <- c("Season", "Team", "Average Elo of Opponent", "Playoffs Reached?",
                      "Conference", "Relative Season Strength")
     } else {
       tableDat <- tableDat %>% select(-playoffteam)
       colVector <- c("Season", "Team", "Average Elo of Opponent","#Playoff Games", "#Playoff Series",
                      "Conference", "Relative Season Strength")
     }
     
     DT::datatable(tableDat,rownames = FALSE, filter = "top",colnames = colVector) %>%
       DT::formatStyle(
         'elo1_z',
         color = DT::styleInterval(0, c('red', 'black'))
       ) %>% 
       DT::formatStyle(
         'elo2_pre',
         backgroundColor = "lightblue"
       ) %>%
       DT::formatRound(c("elo2_pre",'elo1_z'), digits = 2, interval = 6)
   })
   
  
}

# Run the application 
shinyApp(ui = ui, server = server)

