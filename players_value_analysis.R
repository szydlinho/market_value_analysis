library(ggplot2)
library(plotly)
library(scales)
library(ggrepel)
library(dplyr)



dir <- "C:/Users/pawel.szydlik/PycharmProjects/FootballML/player_values/LaLiga/"

direct_path <- "C:/Users/pawel.szydlik/PycharmProjects/FootballML/player_values/LaLiga/"

files_names <- list.files(direct_path, pattern=NULL, all.files=FALSE,
                          full.names=FALSE)

files_names <- files_names[grepl(".csv", files_names)]

all_data <- data.frame()

league_name <- "La Liga"


for (file in files_names){
  
  team <- strsplit(file, "[.]")[[1]][1]
  df <- read.csv2(paste0(direct_path,team ,".csv"))
  df$team <- team
  
  all_data <- rbind(all_data, df)
  df <- df[df$season <= 2021,]
  
  selected_players <- df[df$Price > as.numeric(quantile(df$Price, 0.92, na.rm = TRUE)) , "player"]
  df[df$player %in% selected_players,] %>% group_by(player)  %>% slice_max(season)  %>% slice(1) %>% as.data.frame() -> sliced_df
  
  
  
  p <-  ggplot(data = df[df$player %in% selected_players, ], aes(x = season , y = Price,  color=player)) +
    geom_point(size = 2) + geom_line(size = 1.5) + 
    scale_y_continuous(name = 'Wartość rynkowa', labels = unit_format(unit = "M €", scale = 1e-6)) +
    scale_x_continuous(breaks=seq(2010, 2021, by = 1), limits =  c(2010, 2021), name = "Sezon") + 
    theme_classic(base_size = 10) +
    geom_label_repel(data=sliced_df, 
                     label=sliced_df$player,
                     x = sliced_df$season, y =sliced_df$Price
    ) +
    theme(axis.text.x = element_text(angle = 60, 
                                     hjust = 1, vjust = 0),
          axis.line = element_blank(),
          axis.ticks.x = element_blank(),legend.position = "none"
          ) +   
    labs(title = team, subtitle = "Kwantyl 0.92 najdroższych zawodników w klubie",  caption  = "źródło: transfermarkt.com")
            
  #p
  #ggplotly(p)
  
  #ggsave(filename = paste0(dir, "plots/",team, ".pdf"), p, width=20,  height = 10)
  
  ggsave(filename = paste0(dir, "plots/",team, ".png"), p, width=10,  height = 5)
  
  
  # 
  # df[,c("season", "player", "Price")] %>% group_by(player) %>%
  #   arrange(season , .by_group = TRUE) %>%
  #   mutate(pct_change = (Price/lag(Price) - 1) * 100) %>% as.data.frame() -> df_prc
  # 
  #df_prc$pct_change <- df_prc$pct_change / 100
  # 
  # df_prc[df_prc$player %in% selected_players,] %>% 
  #   group_by(player)  %>% slice_max(season)  %>% slice(1) %>% as.data.frame() -> sliced_df
  # 
  # 
  # 
  # ggplot(data = df_prc[df_prc$player %in% selected_players, ], aes(x = season , y = pct_change,  color=player)) +
  #   geom_point(size = 2) + geom_line(size = 1.5) + 
  #   scale_y_continuous(name = 'Wartość rynkowa', labels =function(x) paste0(x, "%")) +
  #   scale_x_continuous(breaks=seq(2010, 2021, by = 1), limits =  c(2010, 2021), name = "Sezon") + 
  #   theme_bw() + theme(legend.position = "none") + 
  #   geom_label_repel(data=sliced_df, 
  #                    label=sliced_df$player,
  #                    x = sliced_df$season, y =sliced_df$pct_change
  #   )
  # 
  # library(ggforce)
  # prc_p <- ggplot(data = df_prc[df_prc$player %in% selected_players, ], aes(x = season , y = pct_change,  color=player)) +
  #   geom_point(size = 2) + geom_line(size = 1.5) + 
  #   scale_y_continuous(name = 'Porcentowy wzrost wartości rynkowej', labels =function(x) paste0(x, "%") ) +
  #   scale_x_continuous(breaks=seq(2010, 2021, by = 1), limits =  c(2010, 2021), name = "Sezon") + 
  #   theme_bw() + theme(legend.position = "none") + 
  #   geom_label_repel(data=sliced_df, 
  #                    label=sliced_df$player,
  #                    x = sliced_df$season, y =sliced_df$pct_change
  #   ) + 
  #   facet_zoom(ylim = c(-80, 200))
  # #prc_p
  # ggsave(filename = paste0(dir, "plots/",team, "_prc_change.png"), prc_p, width=20,  height = 10)
  # 
  # 
  
  
}



all_data[c("team", "season", "Price")] %>% group_by(team, season) %>%
  summarise(sum = sum(Price, na.rm = T)) %>% as.data.frame() -> df_sum_by_team


df_sum_by_team %>% group_by(team)  %>% slice_max(season)  %>% slice(1) %>% as.data.frame() -> sliced_df


p <-  ggplot(data = df_sum_by_team, aes(x = season , y = sum,  color=team)) +
  geom_point(size = 2) + geom_line(size = 1.5) + 
  scale_y_continuous(name = 'Suma wartosci druzyny', labels = unit_format(unit = "M €", scale = 1e-6)) +
  scale_x_continuous(breaks=seq(2010, 2021, by = 1), limits =  c(2010, 2021), name = "Sezon") + 
  theme_bw() + theme(legend.position = "none") + 
  geom_label_repel(data=sliced_df, 
                   label=sliced_df$team,
                   x = sliced_df$season, y =sliced_df$sum
  )



ggsave(filename = paste0(dir, "plots/", league_name, "_values_over_season.png"), p, width=20,  height = 10)



last_season_df <- df_sum_by_team[df_sum_by_team$season == 2021,]


selected_teams <- last_season_df[last_season_df$sum > as.numeric(quantile(last_season_df$sum, 0.6, na.rm = TRUE)) , "team"]
all_data[all_data$team %in% selected_teams,] %>% group_by(player)  %>% slice_max(season)  %>% slice(1) %>% as.data.frame() -> sliced_df

all_data[all_data$team %in% selected_teams, c("team", "season", "Price")] %>% group_by(team, season) %>%
  summarise(sum = sum(Price, na.rm = T)) %>% as.data.frame() -> df_sum_by_team


df_sum_by_team %>% group_by(team)  %>% slice_max(season)  %>% slice(1) %>% as.data.frame() -> sliced_df


p <-  ggplot(data = df_sum_by_team, aes(x = season , y = sum,  color=team)) +
  geom_point(size = 2) + geom_line(size = 1.5) + 
  scale_y_continuous(name = 'Suma wartosci druzyny', labels = unit_format(unit = "M €", scale = 1e-6)) +
  scale_x_continuous(breaks=seq(2010, 2021, by = 1), limits =  c(2010, 2021), name = "Sezon") + 
  theme_classic(base_size = 10) +
  geom_label_repel(data=sliced_df, 
                   label=sliced_df$team,
                   x = sliced_df$season, y =sliced_df$sum
  ) +
  theme(axis.text.x = element_text(angle = 60, 
                                   hjust = 1, vjust = 0),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),legend.position = "none"
  ) +   
  labs(title = league_name, subtitle = "Kwantyl 0.6 klubów z największą wartością",  caption  = "źródło: transfermarkt.com")




ggsave(filename = paste0(dir, "plots/", league_name, "_values_over_season_selected.png"), p, width=20,  height = 10)



df_sum_by_team %>% group_by(team) %>%
  arrange(season , .by_group = TRUE) %>%
  mutate(pct_change = (sum/lag(sum) - 1) * 100) %>% as.data.frame() -> df_prc

df_prc %>% arrange(-pct_change)

df_prc %>% group_by(team) %>%
  summarise(sum = mean(pct_change, na.rm = T)) %>% as.data.frame() -> mean_team_change

## set the levels in order we want
mean_team_change <- within(mean_team_change, 
                   team <- factor(team, 
                                      levels=names(sort(table(team), 
                                                        decreasing=TRUE))))


p <- ggplot(data=mean_team_change, aes(x=reorder(team, sum), y=sum)) +
  geom_bar(stat="identity", fill="#468189") +
  scale_y_continuous(name = 'Sredni procentowy wzrost od 2010', labels =function(x) paste0(x, "%") ) +
  geom_text(aes(label=paste(format(round(sum,2), trim = TRUE), "%")), hjust=1.6, color="white", size=3.5)+
  theme_minimal() + coord_flip() +
  theme(axis.text.x = element_text(angle = 60, 
                                   hjust = 1, vjust = 0),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),legend.position = "none"
  ) +   
  labs(title = league_name, subtitle = "Wybrane. Kwantyl 0.6 klubów z największą wartością",  caption  = "źródło: transfermarkt.com")


ggsave(filename = paste0(dir, "plots/", league_name, "_mean_prc_selected.png"), p, width=20,  height = 10)




all_data %>% group_by(player, team) %>%
  arrange(season , .by_group = TRUE) %>%
  mutate(pct_change = (Price/lag(Price) - 1) * 100) %>% as.data.frame() -> df_prc




df_sum_by_team %>% group_by(team)  %>% slice_max(season)  %>% slice(1) %>% as.data.frame() -> sliced_df







all_data[c("team", "season", "Price")] %>% group_by(season) %>%
  summarise(sum = sum(Price, na.rm = T)) %>% as.data.frame() -> df_sum_by_season

ggplot(data=df_sum_by_season, aes(x = season, y=sum)) +
  geom_bar(stat="identity", fill="#468189") +
  scale_y_continuous(name = '', labels = unit_format(unit = "Mld €", scale = 1e-9) 
                     ) +
  scale_x_continuous(breaks=seq(2010, 2021, by = 1),  name = "Sezon") + 
  theme_classic(base_size = 18) +
  geom_text(aes(label=paste(format(round(sum / 1e6, 1), trim = TRUE), "M")), vjust=2, color="white", size=3.5)+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, 
                                   hjust = 1, vjust = 0),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),legend.position = "none"
  ) +   
  labs(title = league_name, subtitle = "Suma wartosci klubów w kolejnych sezonach",  caption  = "źródło: transfermarkt.com")


ggsave(filename = paste0(dir, "plots/", league_name, "_sum_of_league_by_seaons.png"), p, width=20,  height = 10)

