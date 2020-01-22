data_clean <- data %>%
  separate_rows(genres, sep=";", convert = TRUE)

data_clean %>%
  group_by(genres)%>%
  summarize(n=n())%>%
  arrange(desc(n))

data_clean$genres <- ifelse(data_clean$genres %in% c("Indie",
                                                     "Action",
                                                     "Casual",
                                                     "Adventure",
                                                     "Strategy",
                                                     "Simulation",
                                                     "RPG",
                                                     "Massively Multiplayer",
                                                     "Racing",
                                                     "Sports"), data_clean$genres,"Other")

data_clean %>%
  group_by(genres)%>%
  summarize(n=n())%>%
  arrange(desc(n))

data_clean<-data_clean %>%
  group_by(appid)%>%
  mutate(genres=paste(unique(genres),collapse=","))

data_clean <- data_clean %>%
  mutate(Action =ifelse(str_detect(genres, "Action"),1,0),
         Adventure=ifelse(str_detect(genres,"Adventure"),1,0),
         Indie=ifelse(str_detect(genres,"Indie"),1,0),
         Casual=ifelse(str_detect(genres,"Casual"),1,0),
         Simulation=ifelse(str_detect(genres,"Simulation"),1,0),
         RPG=ifelse(str_detect(genres,"RPG"),1,0),
         Massively_multiplayer=ifelse(str_detect(genres,"Massively Multiplayer"),1,0),
         Racing=ifelse(str_detect(genres,"Racing"),1,0),
         Sports=ifelse(str_detect(genres,"Sports"),1,0),
         Other=ifelse(str_detect(genres,"Other"),1,0)) %>%
  distinct(appid,.keep_all = T) %>%
  select(-genres)


data_clean <- data_clean %>%
  mutate(Singleplayer =ifelse(str_detect(categories, "Single-player"),1,0),
         Multiplayer=ifelse(str_detect(categories,";Multi-player"),1,0)) %>%
  select(-categories)

write.csv(data_clean,file="SteamAnalyzer/data/steam_final.csv")
