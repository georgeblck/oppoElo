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
                                             pattern = c(1,2), replacement = c(2,1), mode = "first")
eloList <- list(eloDat, eloDatRe)
eloDat <- data.table::rbindlist(eloList, use.names=TRUE, idcol=TRUE)
# Replace the problematic New Orleans Hornets Season with NOH
eloDat <- eloDat %>% mutate(team1 = ifelse((team1 == "NOP") & (season %in% 2003:2004), "NOH", team1))%>% 
  mutate(team2 = ifelse((team2 == "NOP") & (season %in% 2003:2004), "NOH", team2))

# Make conference dat
confDat <- data.frame(team1 = sort(unique(eloDat$team1[eloDat$season >= 1977])), 
                      conference = c(rep("East", 8), "West", "West", "East", "West", "West", "East", "West", 
                                     rep("West", 3), "East", "East", "West", "East","East", "East", "West", "West", 
                                     "East", "East", "West", "East", "East", rep("West", 6), "East", "West","West", "East", "East"))

replaceVec <- c("VAN" = "MEM", "WSB" = "WAS", "SEA" = "OKC", "SDC" = "LAC", "NYN" = "BRK",
                "NOK" ="NOP", "NJN" = "BRK", "NOJ" = "UTA", "KCK" = "SAC", "BUF" = "LAC",
                "CHH" = "CHO", "DNA" = "DEN", "INA" = "IND")

# Pre-Calc data
# * Make indicators for if teams reached playoffs or became champions. Count playoff games
# Filter seasons
tempElo <- eloDat %>% mutate(lubDate = ymd(date)) %>% mutate(playoffgame = !is.na(playoff)) %>% 
   group_by(.id, season) %>% arrange(date) %>% 
   mutate(champion = (row_number() == n()) & (score1 > score2)) %>% ungroup() %>%
   group_by(season, team1) %>% summarise_if(is.logical, sum) %>% 
   right_join(eloDat, by = c("season", "team1")) %>% mutate(playoffteam = playoffgame > 0) %>% 
   filter(season >= 1977) %>% left_join(confDat, by = "team1") %>% 
   mutate(playoffgame = !is.na(playoff)) %>% 
   mutate(playoffteam_v2 = ifelse(playoffteam & champion,2,as.numeric(playoffteam)))
   

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
                     value = c(1977,2019),
                     ticks = FALSE,
                     sep = "")),
        selectInput(inputId="choicePlayoffs", label="Which part of season to include:", 
                    choices = list("Only Regular" = 0,"Only Playoffs" = 1, "Both" = 2), 
                    selected = 0, multiple = FALSE, selectize = FALSE),
         #radioButtons("choicePlayoffs", "Which part of season to include:",
         #            choices = list("Only Regular" = 0,
         #                           "Only Playoffs" = 1,
         #                           "Both" = 2), selected = 0),
        conditionalPanel(condition = "input.tabselected==2",
                         p("Use custom table filters to change its content."),
                         strong("Relative Season Strength:"), p("z-Transformed average Team Elo of Season. 
                           E.g. the value 1 = Team was one StDev stronger than the average NBA team in that conference")),
         conditionalPanel(condition = "input.tabselected==1",
         #h4("Plot options"),
         selectInput(inputId="choiceCarmelo2", label="Which Elo rating to use:", 
                     choices = list("Elo Rating" = 0,"carmELO (2016-2019)" = 1, "Raptor (2019-2020)" = 2), 
                     selected = 0, multiple = FALSE, selectize = FALSE),
         checkboxInput("choiceConf", "Split by conference", value = FALSE),
         checkboxInput("choiceSplitSeason", "Split by season", value = TRUE),
         #checkboxInput("choiceCarmelo", "Use 538s carmELO when possible (available since 2015)", value = FALSE),
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
     
     if(input$choiceCarmelo2 == 2){
        # Choose Raptor
        outDat <- tempElo %>% mutate(elo1_pre = raptor1_pre, elo2_pre = raptor2_pre) %>% filter(season >= 2019)
        #observe({updateSliderInput(session, "choiceSeason", value = c(2019,2019))})
        updateSliderInput(session, "choiceSeason", value = c(2019,2019))
     } else if(input$choiceCarmelo2 == 1){
        # Choose carmElo
        outDat <- tempElo %>% mutate(elo1_pre = carm.elo1_pre, elo2_pre = carm.elo2_pre) %>% filter(season >= 2016, season < 2020)
        updateSliderInput(session, "choiceSeason", value = c(2016,2019))
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
         summarise_at(vars(elo1_pre, elo2_pre, playoffteam, playoffteam_v2), mean) %>% ungroup() %>% left_join(confDat, by = "team1")
       meanNorms <- outDat %>% group_by(conference) %>% summarise_at(vars(elo1_pre, elo2_pre), mean) %>% ungroup()
       if(input$choiceConf == FALSE){
         meanNorms <- meanNorms %>% summarise_if(is.numeric, mean)
       } 
       
       # Construct Plot
       if (length(iseason) > 1) {
         outPlot <- ggplot(outDat, aes(x = elo1_pre, y = elo2_pre, label = team1)) + 
           geom_hline(data = meanNorms,aes(yintercept = elo2_pre), linetype = "dotted", col = "grey50") + 
           geom_vline(data = meanNorms, aes(xintercept = elo1_pre), linetype = "dotted", col = "grey50") + 
           geom_label_repel(min.segment.length = 0.2,force = 3, box.padding = 0.2, label.padding = 0.2, size = 5, label.size = 0.4) + geom_point() + 
           theme_tufte(base_size = 15) + geom_rangeframe(col = "black") + xlab(paste0("Average Strength/Elo of Team ", "(", min(iseason), "-", max(iseason), ")"))+ 
           ylab(paste0("Average Strength/Elo of Opponent ", "(", min(iseason), "-", max(iseason), ")"))+
            coord_cartesian(clip = 'off')+theme(strip.text.x = element_text(size = 15))
       } else {
         outPlot <- ggplot(outDat, aes(x = elo1_pre, y = elo2_pre, label = team1, colour = factor(playoffteam_v2))) + 
           geom_hline(data = meanNorms, aes(yintercept = elo2_pre), linetype = "dotted", col = "grey50") + 
           geom_vline(data = meanNorms, aes(xintercept = elo1_pre), linetype = "dotted", col = "grey50") + 
           geom_label_repel(min.segment.length = 0.2, force = 3, box.padding = 0.2, label.padding = 0.2, label.size = 0.4,
                            show.legend = FALSE,size = 5) + 
           geom_point() + theme_tufte(base_size = 15) + geom_rangeframe(col = "black") + xlab(paste0("Average Strength/Elo of Team (", iseason,")")) + 
           ylab(paste0("Average Strength/Elo of Opponent (", iseason,")")) + 
           scale_colour_manual("Made Playoffs?", guide = guide_legend(reverse = FALSE), breaks = 0:2, 
                               values = c("grey30", "orange", "red"), labels = c("No", "Yes", "Champions")) + 
           theme(legend.justification = c(0, 1), legend.position = c(0.01, 1))+
           theme(legend.title = element_text(size = 17), 
                 legend.text = element_text(size = 13),
                 strip.text.x = element_text(size = 15))+
            coord_cartesian(clip = 'off')
       }
     } else {
       outDat <- outDat %>% filter(season %in% iseason, playoffgame %in% iplayoff) %>% group_by(season, team1) %>% 
         summarise_at(vars(elo1_pre, elo2_pre, playoffteam, playoffteam_v2), mean) %>% ungroup() %>% left_join(confDat, by = "team1") %>%
         mutate(index = paste0(season, team1))
       outPlot <- ggplot(outDat, aes(x = elo1_pre, y = elo2_pre, label = index, colour = factor(playoffteam_v2))) + 
         geom_label( size = 4, show.legend = FALSE)+ geom_point(alpha=0)+ 
          theme_tufte(base_size = 15) + geom_rangeframe(col = "black") + xlab("Average Strength/Elo of Team per Season") + 
         ylab("Average Strength/Elo of Opponent per Season") + 
         scale_colour_manual("Made Playoffs", guide = guide_legend(reverse = FALSE,override.aes = list(alpha = 1)), breaks = 0:2, 
                             values = c("grey30", "orange", "red"), labels = c("No", "Yes", "Champion")) + 
          theme(legend.justification = c(0, 1), legend.position = c(0.01, 1))+
          theme(legend.title = element_text(size = 17), 
                legend.text = element_text(size = 13),
                strip.text.x = element_text(size = 15))+
          coord_cartesian(clip = 'off')
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
       summarise_at(vars(elo1_pre, elo2_pre, playoffteam), mean) %>% ungroup() %>% left_join(confDat, by = "team1") %>%
       group_by(season, conference) %>% mutate(elo1_z = as.numeric(scale(elo1_pre)), elo2_z = as.numeric(scale(elo2_pre))) %>% ungroup() %>%
       select(-playoffteam, -elo2_pre , -elo1_pre, -conference)
     
     # Make Data frame for table
     tableDat <- tempElo %>% filter(playoffgame %in% iplayoff) %>% add_count(season, team1, name = "playoffgames") %>% 
       group_by(season, team1) %>% mutate(playoffseries = n_distinct(team2)) %>% ungroup() %>%
       group_by(season, team1) %>% summarise_at(vars(elo1_pre, elo2_pre, playoffgames, playoffteam_v2, playoffseries), mean) %>% 
       ungroup() %>% left_join(confDat, by = "team1") %>%
       arrange(-elo2_pre) %>% left_join(zDat, by = c("season", "team1")) %>%
       select(-elo1_pre, -elo2_z) %>% mutate(#playoffteam = factor(playoffteam, levels = 0:1, labels = c("No", "Yes")),
                                             playoffteam_v2 = factor(playoffteam_v2, levels = 0:2, labels = c("No", "Yes", "Champion")),
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

