library(tidyverse)
library(dplyr)
library(tidyr)
library(shiny)
library(knitr)
library(kableExtra)
  #EPL_Standings is a function to pull records given a date and season to create a standings table in 
  #a given season.  An Example is: EPL_Standings("01/20/2019", "2018/19") <- (about half way through season)
  #Note: Seasons begin in August and end in May
  #Note: Personally a Liverpool, Leicester and Swasnsea fan
  #Note: Personally a Man U/Man City despiser
  EPL_Standings <- function(date,season){
  date <- as.Date(date,format = '%m/%d/%Y')
 
   # Pulls csv file from http://www.football-data.co.uk/englandm.php given season and year given user input 
  url_seasondate <- paste0(substr(season,3,4), substr(season,6,7),'/')
  url <- paste0('http://www.football-data.co.uk/mmz4281/',url_seasondate,'E0.csv')
  df_reader <- read.csv(url)
    
  df <- df_reader %>%
    select(Date, HomeTeam, AwayTeam, FTHG, FTAG,FTR) %>%
    
  # Reformats year into same uniform year
    mutate(year = ifelse(nchar(Date) == 10,substring(Date,9,10),
                         ifelse(nchar(Date) == 8,substring(Date,7,8),'Check Date format')),
           Date = paste0(substring(Date,1,6),year),
           Date1 = as.Date(Date,format = '%d/%m/%y')) %>% 
    
  # df_final filters date into date specified
    filter(Date1 <= date) %>%
    mutate(point_home = ifelse(FTR == 'D', 1, ifelse(FTR == 'H', 3, ifelse(FTR == 'A', 0,NA))), 
           point_away = ifelse(FTR == 'D', 1, ifelse(FTR == 'H', 0, ifelse(FTR == 'A', 3,NA))), 
           win_home = ifelse(FTR == 'H',1,0), 
           win_away = ifelse(FTR == 'A', 1,0), 
           draw_home = ifelse(FTR == 'D',1,0), 
           draw_away = ifelse(FTR == 'D',1,0), 
           losse_home = ifelse(FTR == 'A',1,0), 
           losse_away = ifelse(FTR == 'H',1,0)) 
  
  # Home matches dataframe
  home <- df %>%
    select(Date, HomeTeam, FTHG,FTAG, FTR,Date1,point_home, win_home,draw_home,losse_home) %>%
    group_by(TeamName = HomeTeam) %>% 
    summarise(count_home = n(), 
              point_home = sum(point_home), 
              wins_home = sum(win_home), 
              draws_home = sum(draw_home), 
              losses_home = sum(losse_home), 
              goals_for_home = sum(FTHG), 
              goals_against_home = sum(FTAG)) 
  
  # Away matches dataframe
  away <- df %>%
    select(Date, Date1, AwayTeam, FTHG,FTAG, FTR, point_away, win_away, draw_away, losse_away) %>%
    group_by(TeamName = AwayTeam) %>% 
    summarise(count_away = n(), 
              point_away = sum(point_away), 
              wins_away = sum(win_away), 
              draws_away = sum(draw_away), 
              losses_away = sum(losse_away), 
              goals_for_away = sum(FTAG), 
              goals_against_away = sum(FTHG)) 
  
  # join home and away subsets by 'TeamName'
  join_df1 <- home %>%
    full_join(away, by = c('TeamName'))
  
  # Convert any NA's to 0
  join_df1[is.na(join_df1)] <- 0
  
  # Calculates & creates columns to answer your query
  #3 points for W, 1 for D, 0 for L
  join_df <- join_df1 %>%
    mutate(MatchesPlayed = count_home + count_away, 
           Points = point_home + point_away, 
           PPM = round(Points/MatchesPlayed,3), 
           PtPct = round(Points/(3*MatchesPlayed),2), 
           Wins = wins_home + wins_away, 
           Draws = draws_home + draws_away, 
           Losses = losses_home + losses_away, 
           Record = paste0(Wins,'-',Losses,'-',Draws), 
           HomeRec = paste0(wins_home,'-',losses_home,'-',draws_home), 
           AwayRec = paste0(wins_away,'-',losses_away,'-',draws_away), 
           GS = goals_for_home + goals_for_away, 
           GSM = round(GS/MatchesPlayed,2), 
           GA = goals_against_home + goals_against_away, 
           GAM = round(GA/MatchesPlayed,2)) 
  
  # Create a subset to calculate the team's record over the last 10 games played 
  
  # last 10 home
  last10_home <- df %>%
    select(Date1, TeamName = HomeTeam, win = win_home,draw = draw_home,losse = losse_home)
  
  # last 10 away
  last10_away <- df %>%
    select(Date1, TeamName = AwayTeam, win = win_away, draw = draw_away,losse = losse_away)
  
  # join last 10 home and last 10 away
  last_10df <- rbind(last10_home, last10_away)
  
  # Joins the last 20 games and returns only get lasted 10 games played
  join_df <- last_10df %>%
    group_by(TeamName) %>%
    arrange(desc(Date1)) %>%
    top_n(10,wt = Date1) %>%
    summarise(Wins = sum(win),
              Losses = sum(losse),
              Draws = sum(draw)) %>%
    mutate(Last10 = paste0(Wins,'-',Losses,'-',Draws)) %>%
    select(TeamName,Last10) %>%
    inner_join(join_df, by = c('TeamName')) %>% 
    arrange(TeamName) 
  
  # Given a subset, calculate the team's current streak
  
  # streak home
  streak_home <- df %>%
    select(Date1,TeamName = HomeTeam,FTR) %>%
    mutate(result = ifelse(FTR == 'D', 'D', ifelse(FTR == 'H', 'W','L'))) %>%
    select(-c(FTR))
  
  # streak away
  streak_away <- df %>%
    select(Date1,TeamName = AwayTeam,FTR) %>%
    mutate(result = ifelse(FTR == 'D', 'D', ifelse(FTR == 'A', 'W','L'))) %>%
    select(-c(FTR)) 
  
  # join streak home and streak away
  streak <- rbind(streak_home,streak_away)
  
  streak <- rbind(streak_home,streak_away) %>%
    spread(key = 'Date1', value = result) %>%
    arrange(TeamName)
  
  # Rename TeamName to include streaks
  TeamName <- streak$TeamName
  
  #streak1() function counts the consecutive win, draw, lost
  streak1 <- apply(streak, 1, function(x) {
    x <- x[!is.na(x)] 
    r <- rle(x)
    streak1 = tail(unlist(r[1]),n=1)
    streak2 =tail(unlist(r[2]),n=1)
    paste0(streak1,streak2)
  })
  streak1 <- as.vector(unlist(streak1)) 
 
  # Streak2 joins the TeamName
  streak2 <- data.frame(cbind(TeamName,streak1)) %>%
    mutate(TeamName = as.character(TeamName)) 
 
  #final_df uses join_df to select the necessary columns
  final_df <- join_df %>%
    inner_join(streak2, by = c('TeamName')) %>%
    arrange(desc(PPM), desc(Wins),desc(GSM),GAM) %>%
    select(TeamName, Record, HomeRec,AwayRec,MatchesPlayed,Points,PPM,PtPct,GS,GSM,GA,GAM,Last10,Streak = streak1)
  return(final_df) 
  }
