library(robotstxt)
library(rvest)
library(stringr)
library(plyr)
library(dplyr)


base_url <- "https://www.basketball-reference.com/leagues/NBA_2021_per_game.html"
paths_allowed(base_url)

pages <- read_html(file.path(base_url)) %>%
  html_nodes("#per_game_stats") %>%
  html_attr("href")

playerstats <- read_html("https://www.basketball-reference.com/leagues/NBA_2021_per_game.html") %>%
  html_nodes("td") %>%
  html_text()

View(playerstats)
View(playerstats1)
playerstats1 <- matrix(playerstats,ncol = 29,byrow = TRUE)

colnames(playerstats1)[c(1:29)] <- c("Player","Pos","Age","TM","G","GS","MP","FG","FGA","FG%","3P",
                                     "3PA","3P%","2P","2PA","2P%","eFG%","FT","FTA","FT%","ORB","DRB",
                                     "TRB","AST","STL","BLK","TOV","PF","PTS")

playerstats1 <- as.data.frame(playerstats1)
playerstats <- playerstats1

playerstats[playerstats==""] <- NA
playerstats[is.na(playerstats)] <- 0
is.atomic(playerstats)
##Coerce all appropriate columns to numeric using sapply
playerstats[,c(5:29)] <- sapply(playerstats[,(5:29)], as.numeric)

#Deal with duplicate entries due to mid season trades
dupes <- duplicated(playerstats$Player)
df = data.frame()
for(i in 1:nrow(playerstats) ) {
  if(isTRUE(dupes[i])){
    df <-  rbind(df , playerstats[i,]) 
  }
}
trades <- as.numeric((rownames(df)))
playerstats <- playerstats[-trades,]

#Write to csv
write.csv(playerstats,"/Users/WebbWilliams/Downloads/NBABasicStats.csv")

base_urladv <- "https://www.basketball-reference.com/leagues/NBA_2021_advanced.html"
paths_allowed(base_urladv)

pages <- read_html(file.path(base_urladv)) %>%
  html_nodes("#advanced_stats") %>%
  html_attr("href")

playerstatsadv <- read_html("https://www.basketball-reference.com/leagues/NBA_2021_advanced.html") %>%
  html_nodes("td") %>%
  html_text()

View(playerstatsadv1)
playerstatsadv1 <- matrix(playerstatsadv,ncol = 28,byrow = TRUE)
playerstatsadv1 <- playerstatsadv1[,-c(19,24)]

colnames(playerstatsadv1)[c(1:26)] <- c("Player","Pos","Age","TM","G","MP","PER","TS%","3PAr","FTr",
                                     "ORB%","DRB%","TRB%", "AST%","STL%","BLK%","TOV%","USG%","OWS","DWS","WS","WS/48",
                                     "OBPM","DBPM","BPM","VORP")

playerstatsadv1 <- as.data.frame(playerstatsadv1)
playerstatsadv <- playerstatsadv1

playerstatsadv[playerstatsadv==""] <- NA
playerstatsadv[is.na(playerstatsadv)] <- 0
is.atomic(playerstatsadv)
##Coerce all appropriate columns to numeric using sapply
playerstatsadv[,c(3,5:26)] <- sapply(playerstatsadv[,c(3,5:26)], as.numeric)

#Deal with duplicate entries due to mid season trades
dupes1 <- duplicated(playerstatsadv$Player)
df1 = data.frame()
for(i in 1:nrow(playerstatsadv) ) {
  if(isTRUE(dupes1[i])){
    df1 <-  rbind(df1 , playerstatsadv[i,]) 
  }
}
trades1 <- as.numeric((rownames(df1)))
playerstatsadv <- playerstatsadv[-trades1,]

playerstats <- read.csv("/Users/WebbWilliams/Downloads/NBABasicStats.csv")
playerstats <- playerstats[,-1]
#Merging basic and advanced
combo <- inner_join(playerstats,playerstatsadv,by="Player")
combo <- combo[,-c(30:33)]

colnames(combo)[c(1:50)] <- c("Player","Pos","Age","TM","G","GS","MP","FG","FGA","FG%","3P",
                                     "3PA","3P%","2P","2PA","2P%","eFG%","FT","FTA","FT%","ORB","DRB",
                                     "TRB","AST","STL","BLK","TOV","PF","PTS","MPTOT","PER","TS%","3PAr","FTr",
                              "ORB%","DRB%","TRB%", "AST%","STL%","BLK%","TOV%","USG%","OWS","DWS","WS","WS/48",
                              "OBPM","DBPM","BPM","VORP")
#Writing the merged file to csv
write.csv(combo,"/Users/WebbWilliams/Downloads/NBABasicAndAdvStats2021.csv")

