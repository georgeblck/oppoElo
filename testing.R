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
library(knitr)
library(kableExtra)
library(patchwork)
library(stringi)
library(ggridges)
library(viridis)

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
colnames(eloDatRe) <- stri_replace_all_fixed(colnames(eloDatRe), c(1, 2), c(2, 1), 
    mode = "first")
eloList <- list(eloDat, eloDatRe)
eloDat <- data.table::rbindlist(eloList, use.names = TRUE, idcol = TRUE)
rm(eloList)
# Replace the problematic New Orleans Hornets Season with NOH
eloDat <- eloDat %>% mutate(team1 = ifelse((team1 == "NOP") & (season %in% 2003:2004), "NOH", team1))%>% 
  mutate(team2 = ifelse((team2 == "NOP") & (season %in% 2003:2004), "NOH", team2))

# Make conference dat
confDat <- data.frame(team1 = sort(unique(eloDat$team1[eloDat$season >= 1977])), 
    conference = c(rep("East", 8), "West", "West", "East", "West", "West", "East", 
        "West", rep("West", 3), "East", "East", "West", "East", "East","East", "West", "West", 
        "East", "East", "West", "East", "East", rep("West", 6), "East", "West", "West", 
        "East", "East"))

# Pre-Calc data
tempElo <- eloDat %>% mutate(playoffgame = !is.na(playoff)) %>% group_by(season, 
    team1) %>% summarise_if(is.logical, sum) %>% right_join(eloDat, by = c("season", 
    "team1")) %>% mutate(playoffteam = playoffgame > 0) %>% filter(season >= 1977) %>% 
    left_join(confDat, by = "team1") %>% mutate(playoffgame = !is.na(playoff)) 


#### Investigate carmElo weirdness ####
tempElo %>% filter(season == 2019, is.na(playoff)) %>% group_by(team1) %>%
  summarise_at(vars(carmelo1_pre, carmelo2_pre, playoffteam), mean, na.rm = TRUE) %>% left_join(confDat, by = "team1") %>%
  ggplot(aes(x=carmelo1_pre, y = carmelo2_pre, colour = factor(playoffteam), label = team1))+geom_label()+
  facet_wrap(~conference)+theme_dark()+theme(legend.position="none")


#### Ridge-Plots ####

seasonData <- tempElo %>% filter(is.na(playoff)) %>% group_by(season, team1) %>% 
  summarise_at(vars(elo1_pre, elo2_pre), mean) %>% ungroup() %>% left_join(confDat, by = "team1") %>%
  mutate(semidecade = season - season %% 5)

seasonData %>% filter(season >= 1980) %>%  ggplot( aes(x = elo2_pre, y = factor(semidecade), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 0.95) +
  scale_fill_viridis(name = "", option = "C")+
  xlab("Average Opponent Elo") + ylab("Seasons starting from ...") + theme_tufte(base_size = 15)+facet_wrap(~conference)+
  theme(legend.position="none")+scale_x_continuous(breaks = seq(1470,1530,by=20))

ggplot( seasonData,aes(x=elo2_pre, y=conference, fill=factor(..quantile..))) +
  stat_density_ridges(scale = 0.95,
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+theme_tufte(base_size = 15)+xlab("Average Opponent Elo")+
  ylab("Conference")

#### Unlucky lucky ####

# Biggest Range in oppo-Elo per season & conference 
luckyDat <- tempElo %>% filter(is.na(playoff)) %>% group_by(season, team1) %>% 
  group_by(season, team1) %>% summarise_at(vars(elo1_pre, elo2_pre), mean) %>% ungroup() %>% left_join(confDat, by = "team1") %>%
  group_by(season, conference) %>% summarise(oppoMax = max(elo2_pre),  oppoMin = min(elo2_pre)) %>% 
  mutate(luck = oppoMax - oppoMin) %>% ungroup() %>% arrange(-luck) %>% mutate(index = paste0(season, conference))

# Biggest 
luckyDat2 <- tempElo %>% filter(is.na(playoff)) %>% group_by(season, team1) %>% 
  group_by(season, team1) %>% summarise_at(vars(elo1_pre, elo2_pre), mean) %>% ungroup()  %>% left_join(confDat, by = "team1") %>%
  group_by(season, conference) %>% mutate(elo1_z = as.numeric(scale(elo1_pre)), elo2_z = as.numeric(scale(elo2_pre))) %>% 
  ungroup()%>% mutate(index = paste0(season, conference))

# Biggest Range in z-Score Elo-Rating per season & conference
luckyDat3 <- tempElo %>% filter(is.na(playoff)) %>% group_by(season, team1) %>% 
  group_by(season, team1) %>% summarise_at(vars(elo1_pre, elo2_pre), mean) %>% ungroup()  %>% left_join(confDat, by = "team1") %>%
  group_by(season, conference) %>% mutate(elo1_z = as.numeric(scale(elo1_pre)), elo2_z = as.numeric(scale(elo2_pre))) %>% 
  ungroup() %>% group_by(season, conference) %>% summarise(oppoMax = max(elo2_z),  oppoMin = min(elo2_z)) %>% 
  mutate(luck = oppoMax - oppoMin) %>% ungroup() %>% arrange(-luck) %>% mutate(index = paste0(season, conference))



temp <- luckyDat2 %>% group_by(season, conference) %>% slice(which.min(elo2_z), which.max(elo2_z)) %>% 
  mutate(kind = rep(c("min", "max"),n()/2))
temp <- data.table::dcast(data.table::setDT(temp),season+conference~kind, value.var = c("elo2_z", "team1"))

ggplot(data = temp, mapping = aes(x = season,group = factor(conference))) +
  geom_linerange(mapping = aes(ymin =elo2_z_min,
                                  ymax =  elo2_z_max)) +
  labs(x= "Season", y= "Range of Deviation") + coord_flip()#+
  facet_wrap(~conference)
  
ggplot(data = temp, aes(x = season, y = elo2_z_max-elo2_z_min, colour = conference)) +
    labs(x= "Season", y= "Range of Deviation") +geom_line()+ylim(c(0,5))+xlab("Season")+ylab("Spread of Luck")
ggplot(luckyDat2, aes(x = season, ymin = 0, ymax =luck))+geom_linerange()+facet_wrap(~conference)

intersect(luckyDat$index[1:10], luckyDat3$index[1:10])

selected <- tempElo %>% filter(season == 2012, is.na(playoff)) %>% 
  filter(team1 %in% c("MIA", "CHI"))  %>% group_by(team1) %>% arrange(date, .by_group=TRUE) %>% ungroup() %>%
  group_by(team1, team2) %>% mutate(wat = 1:n()) %>% ungroup() %>% mutate(team2rep = ifelse(team2 %in% c("MIA", "CHI"), "OPO", team2)) %>% 
  mutate(matchup = paste0(team2rep, wat)) %>% add_count(matchup) 

yas <- selected %>% filter(n> 1) %>% group_by(matchup) %>% summarise_at(vars(elo2_pre), diff) %>% ungroup() %>% arrange(-elo2_pre)

#### random ####
eloDelta <- tempElo %>% group_by(season, team1) %>% summarise(oppoMax = max(elo2_pre), 
    oppoMin = min(elo2_pre), ownMax = max(elo1_pre), ownMin = min(elo1_pre)) %>% 
    mutate(oppoDiff = oppoMax - oppoMin, ownDiff = ownMax - ownMin) %>% ungroup() %>% 
    select(team1, season, ownDiff, oppoDiff)

eloDelta %>% select(-oppoDiff) %>% arrange(-ownDiff) %>% top_n(10, ownDiff) %>% kable(format = "rst", 
    digits = 1, col.names = c("Team", "Season", "EloDiff"))

eloDelta %>% select(-ownDiff) %>% arrange(-oppoDiff) %>% top_n(10, oppoDiff) %>% 
    kable(format = "rst", digits = 1, col.names = c("Team", "Season", "EloDiff")) %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "responsive"))


iseason <- c(2019, 2019)
iconf <- TRUE
iplayoffs <- FALSE
iseason <- iseason[1]:iseason[2]
iElo <- TRUE
if (iElo) {
    tempElo <- tempElo %>% mutate(elo1_pre = ifelse(is.na(carmelo1_pre), ifelse(is.na(carm.elo1_pre), 
        elo1_pre, carm.elo1_pre), carmelo1_pre), elo1_pre = ifelse(is.na(carmelo2_pre), 
        ifelse(is.na(carm.elo2_pre), elo1_pre, carm.elo2_pre), carmelo2_pre))
}
# Make Data for graphic
outDat <- tempElo %>% filter(season %in% iseason, playoffgame %in% iplayoffs) %>% 
    group_by(team1) %>% summarise_at(vars(elo1_pre, elo2_pre, playoffteam), mean) %>% 
    ungroup() %>% left_join(confDat, by = "team1")
meanNorms <- outDat %>% group_by(conference) %>% summarise_at(vars(elo1_pre, elo2_pre), 
    mean) %>% ungroup()
if (iconf == FALSE) {
    meanNorms <- meanNorms %>% summarise_if(is.numeric, mean)
}

# Construct Plot
if (length(iseason) > 1) {
    outPlot <- ggplot(outDat, aes(x = elo1_pre, y = elo2_pre, label = team1)) + geom_hline(data = meanNorms, 
        aes(yintercept = elo2_pre), linetype = "dotted", col = "grey") + geom_vline(data = meanNorms, 
        aes(xintercept = elo1_pre), linetype = "dotted", col = "grey") + geom_label_repel(min.segment.length = 0.2, 
        force = 3, box.padding = 0.2, label.padding = 0.2) + geom_point() + theme_tufte(base_size = 15) + 
        geom_rangeframe(col = "black") + xlab("Average Elo of Team") + ylab("Average Elo of opponent Team")
} else {
    outPlot <- ggplot(outDat, aes(x = elo1_pre, y = elo2_pre, label = team1, colour = factor(playoffteam))) + 
        geom_hline(data = meanNorms, aes(yintercept = elo2_pre), linetype = "dotted", 
            col = "grey") + geom_vline(data = meanNorms, aes(xintercept = elo1_pre), 
        linetype = "dotted", col = "grey") + geom_label_repel(min.segment.length = 0.2, 
        force = 3, box.padding = 0.2, label.padding = 0.2) + geom_point() + theme_tufte(base_size = 15) + 
        geom_rangeframe(col = "black") + xlab("Average Elo of Team") + ylab("Average Elo of opponent Team") + 
        scale_colour_manual("Made Playoffs", guide = guide_legend(reverse = TRUE), 
            breaks = 0:1, values = c("grey30", "goldenrod1"), labels = c("No", "Yes")) + 
        theme(legend.justification = c(0, 1), legend.position = c(0.01, 1), legend.background = element_rect(size = 0.5, 
            linetype = "dotted"))
}

if (iconf) {
    outPlot <- outPlot + facet_wrap(~conference, strip.position = "top", scales = "fixed", 
        nrow = 1)
}

print(outPlot)






# Look at Variance in opp elo over time
eloDat %>% filter(is.na(playoff), season >= 2004) %>% group_by(season, team1) %>% 
    summarise_at(vars(elo1_pre, elo2_pre), mean) %>% ungroup() %>% group_by(season) %>% 
    summarise_at(vars(elo1_pre, elo2_pre), sd, na.rm = TRUE) %>% ungroup() %>% ggplot(aes(x = season, 
    y = elo2_pre)) + geom_point() + geom_line() + theme_minimal() + geom_rangeframe(sides = "l")

# Make Data
eloNew <- eloDat %>% mutate(playoffgame = !is.na(playoff)) %>% group_by(season, team1) %>% 
    summarise_if(is.logical, sum) %>% right_join(eloDat, by = c("season", "team1")) %>% 
    mutate(playoffteam = playoffgame > 0) %>% filter(season >= 2013) %>% group_by(season, 
    team1) %>% summarise_at(vars(elo1_pre, elo2_pre, playoffteam), mean) %>% ungroup() %>% 
    left_join(confDat, by = "team1")












# With facetwrap
east <- ggplot(eloNew[eloNew$conference == "W", ], aes(x = elo1_pre, y = elo2_pre, 
    label = team1, colour = factor(playoffteam))) + geom_label_repel(min.segment.length = 0.1, 
    force = 3, box.padding = 0.1, label.padding = 0.1) + geom_point() + theme_tufte(base_size = 14) + 
    geom_rangeframe(col = "black") + xlab("Average Team Elo") + ylab("Average Opponent Team Elo") + 
    scale_colour_manual("Made Playoffs", guide = guide_legend(reverse = TRUE), breaks = 0:1, 
        values = c("grey30", "goldenrod1"), labels = c("No", "Yes")) + theme(legend.justification = c(1, 
    1), legend.position = c(1, 0.5), legend.background = element_rect(fill = "white", 
    size = 0.5, linetype = "dotted"), plot.title = element_text(hjust = 0.5)) + facet_wrap(~season, 
    strip.position = "top") + ggtitle("Retrospective strength of schedule in the Western Conference")
west <- ggplot(eloNew[eloNew$conference == "E", ], aes(x = elo1_pre, y = elo2_pre, 
    label = team1, colour = factor(playoffteam))) + geom_label_repel(min.segment.length = 0.1, 
    force = 3, box.padding = 0.1, label.padding = 0.1) + geom_point() + theme_tufte(base_size = 14) + 
    geom_rangeframe(col = "black") + xlab("Average Team Elo") + ylab("Average Opponent Team Elo") + 
    scale_colour_manual("Made Playoffs", guide = guide_legend(reverse = TRUE), breaks = 0:1, 
        values = c("grey30", "goldenrod1"), labels = c("No", "Yes")) + theme(legend.justification = c(1, 
    1), legend.position = c(1, 0.5), legend.background = element_rect(fill = "white", 
    size = 0.5, linetype = "dotted"), plot.title = element_text(hjust = 0.5)) + facet_wrap(~season, 
    strip.position = "top") + ggtitle("Retrospective strength of schedule in the Eastern Conference")
all <- ggplot(eloNew, aes(x = elo1_pre, y = elo2_pre, label = team1, colour = factor(playoffteam), 
    fill = factor(conference))) + geom_label_repel(min.segment.length = 0.1, force = 3, 
    box.padding = 0.1, label.padding = 0.1) + geom_point() + theme_tufte(base_size = 14) + 
    geom_rangeframe(col = "black") + xlab("Average Team Elo") + ylab("Average Opponent Team Elo") + 
    scale_colour_manual("Made Playoffs", guide = guide_legend(reverse = TRUE), breaks = 0:1, 
        values = c("grey30", "goldenrod1"), labels = c("No", "Yes")) + theme(legend.justification = c(1, 
    1), legend.position = c(1, 0.5), legend.background = element_rect(fill = "white", 
    size = 0.5, linetype = "dotted"), plot.title = element_text(hjust = 0.5)) + facet_wrap(~season, 
    strip.position = "top") + ggtitle("Retrospective strength of schedule in the NBA") + 
    scale_fill_manual(guide = FALSE, values = c("white", "grey75"))
ggsave(plot = east, filename = "east.png", width = 20, height = 13, units = "cm", 
    scale = 2)
ggsave(plot = west, filename = "west.png", width = 20, height = 13, units = "cm", 
    scale = 2)
ggsave(plot = all, filename = "nba.png", width = 20, height = 13, units = "cm", scale = 2)
