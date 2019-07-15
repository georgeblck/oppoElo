# Clear Workspace
rm(list = ls())
options(scipen = 999)
options(stringsAsFactors = FALSE)

# load packages
library(formatR)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(ggrepel)
local <- TRUE

# carmELO since 2016/2019. t = playoff any Options: Only/INclude Playoff ... choose year ... split
# conference(default)

# read data
if (local) {
    eloDat <- read.table("538elo.csv", header = TRUE, dec = ".", sep = ";")
    rm(local)
} else {
    url538 <- "https://projects.fivethirtyeight.com/nba-model/nba_elo.csv"
    eloDat <- read.table(url538, header = TRUE, sep = ",", dec = ".", na.strings = "")
    eloDat$date <- ymd(eloDat$date)
}

confDat <- data.frame(team1 = sort(unique(eloDat$team1[eloDat$season >= 2013])), conference = c(rep("E", 
    6), "W", "W", "E", "W", "W", "E", rep("W", 3), "E", "E", "W", "W", "E", "W", "E", "E", rep("W", 4), 
    "E", "W", "E"))

# Make Data
eloNew <- eloDat %>% mutate(playoffgame = !is.na(playoff)) %>% group_by(season, team1) %>% summarise_if(is.logical, 
    sum) %>% right_join(eloDat, by = c("season", "team1")) %>% mutate(playoffteam = playoffgame > 0) %>% 
    filter(season >= 2013) %>% group_by(season, team1) %>% summarise_at(vars(elo1_pre, elo2_pre, playoffteam), 
    mean) %>% ungroup() %>% left_join(confDat, by = "team1")

# Filter conf, season and playoffs
eloNew %>% filter(season %in% 2019) %>% ggplot(aes(x = elo1_pre, y = elo2_pre, label = team1, colour = factor(playoffteam))) + 
    geom_label_repel(min.segment.length = 0.1, force = 3, box.padding = 0.1, label.padding = 0.1) + geom_point() + 
    theme_tufte(base_size = 14) + geom_rangeframe(col = "black") + xlab("Average Team Elo") + ylab("Average Opponent Team Elo") + 
    scale_colour_manual("Made Playoffs", guide = guide_legend(reverse = TRUE), breaks = 0:1, values = c("grey30", 
        "goldenrod1"), labels = c("No", "Yes")) + theme(legend.justification = c(1, 1), legend.position = c(1, 
    0.5), legend.background = element_rect(fill = "white", size = 0.5, linetype = "dotted"), plot.title = element_text(hjust = 0.5)) + 
    facet_wrap(~season, strip.position = "top") + ggtitle("Retrospective strength of schedule in the Western Conference")





# Look at Variance in opp elo over time
eloDat %>% filter(is.na(playoff), season >= 2004) %>% group_by(season, team1) %>% summarise_at(vars(elo1_pre, 
    elo2_pre), mean) %>% ungroup() %>% group_by(season) %>% summarise_at(vars(elo1_pre, elo2_pre), sd, 
    na.rm = TRUE) %>% ungroup() %>% ggplot(aes(x = season, y = elo2_pre)) + geom_point() + geom_line() + 
    theme_minimal() + geom_rangeframe(sides = "l")













# With facetwrap
east <- ggplot(eloNew[eloNew$conference == "W", ], aes(x = elo1_pre, y = elo2_pre, label = team1, colour = factor(playoffteam))) + 
    geom_label_repel(min.segment.length = 0.1, force = 3, box.padding = 0.1, label.padding = 0.1) + geom_point() + 
    theme_tufte(base_size = 14) + geom_rangeframe(col = "black") + xlab("Average Team Elo") + ylab("Average Opponent Team Elo") + 
    scale_colour_manual("Made Playoffs", guide = guide_legend(reverse = TRUE), breaks = 0:1, values = c("grey30", 
        "goldenrod1"), labels = c("No", "Yes")) + theme(legend.justification = c(1, 1), legend.position = c(1, 
    0.5), legend.background = element_rect(fill = "white", size = 0.5, linetype = "dotted"), plot.title = element_text(hjust = 0.5)) + 
    facet_wrap(~season, strip.position = "top") + ggtitle("Retrospective strength of schedule in the Western Conference")
west <- ggplot(eloNew[eloNew$conference == "E", ], aes(x = elo1_pre, y = elo2_pre, label = team1, colour = factor(playoffteam))) + 
    geom_label_repel(min.segment.length = 0.1, force = 3, box.padding = 0.1, label.padding = 0.1) + geom_point() + 
    theme_tufte(base_size = 14) + geom_rangeframe(col = "black") + xlab("Average Team Elo") + ylab("Average Opponent Team Elo") + 
    scale_colour_manual("Made Playoffs", guide = guide_legend(reverse = TRUE), breaks = 0:1, values = c("grey30", 
        "goldenrod1"), labels = c("No", "Yes")) + theme(legend.justification = c(1, 1), legend.position = c(1, 
    0.5), legend.background = element_rect(fill = "white", size = 0.5, linetype = "dotted"), plot.title = element_text(hjust = 0.5)) + 
    facet_wrap(~season, strip.position = "top") + ggtitle("Retrospective strength of schedule in the Eastern Conference")
all <- ggplot(eloNew, aes(x = elo1_pre, y = elo2_pre, label = team1, colour = factor(playoffteam), fill = factor(conference))) + 
    geom_label_repel(min.segment.length = 0.1, force = 3, box.padding = 0.1, label.padding = 0.1) + geom_point() + 
    theme_tufte(base_size = 14) + geom_rangeframe(col = "black") + xlab("Average Team Elo") + ylab("Average Opponent Team Elo") + 
    scale_colour_manual("Made Playoffs", guide = guide_legend(reverse = TRUE), breaks = 0:1, values = c("grey30", 
        "goldenrod1"), labels = c("No", "Yes")) + theme(legend.justification = c(1, 1), legend.position = c(1, 
    0.5), legend.background = element_rect(fill = "white", size = 0.5, linetype = "dotted"), plot.title = element_text(hjust = 0.5)) + 
    facet_wrap(~season, strip.position = "top") + ggtitle("Retrospective strength of schedule in the NBA") + 
    scale_fill_manual(guide = FALSE, values = c("white", "grey75"))
ggsave(plot = east, filename = "east.png", width = 20, height = 13, units = "cm", scale = 2)
ggsave(plot = west, filename = "west.png", width = 20, height = 13, units = "cm", scale = 2)
ggsave(plot = all, filename = "nba.png", width = 20, height = 13, units = "cm", scale = 2)
