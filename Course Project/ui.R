library(shiny)
library(data.table)

AllTeamsFPIByDate <- data.table(read.table(file = "All Teams FPI By Date.csv", header = TRUE, sep = ","))

SeasonChoices <- data.table(SEASON = unique(AllTeamsFPIByDate$SEASON), key = c("SEASON"))
TeamChoices <- data.table(TEAM = as.character(unique(AllTeamsFPIByDate$TEAM)), key = c("TEAM"))

MaxSeason <- max(SeasonChoices$SEASON)
MaxSeasonFPINo1Tm <- AllTeamsFPIByDate[SEASON == MaxSeason & FPI_RANK == 1]$TEAM
MaxSeasonFPINo2Tm <- AllTeamsFPIByDate[SEASON == MaxSeason & FPI_RANK == 2]$TEAM
  
# Define UI for application that calculates FPI-based matchup prediction
shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("College Football Matchup Predictor", align = "center")),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Pick the season, teams, and site for a given college football matchup.
               Then, click the \"Get Prediction\" button to see the chances of each team winning
               that matchup based on each team's most updated Football Power Index (FPI) rating 
               for that season."),
      
      selectInput("season", label = strong("Season"), choices = SeasonChoices,
        selected = MaxSeason, multiple = FALSE),

      selectInput("tm", label = strong("Team"), choices = TeamChoices,
        selected = MaxSeasonFPINo1Tm, multiple = FALSE),
      
      radioButtons("tm_site", label = strong("Team Site"), 
        choices = list("Home", "Neutral", "Road"), selected = "Neutral"),

      selectInput("opp", label = strong("Opponent"), choices = TeamChoices,
        selected = MaxSeasonFPINo2Tm, multiple = FALSE),
      
      actionButton("getbutton", strong("Get Prediction"))
    ),
    
    mainPanel(h4(textOutput("text1"), align = "center"),
              h6(textOutput("text2"), align = "center"),
              br(),
              h5(textOutput("text3"), align = "center"),
              plotOutput("plot")
            )
  )
))
