library(shiny)
library(data.table)

CFBGameWinProb <- function(tm_pregame_fpi, opp_pregame_fpi, tm_site)
{
  tm_hfa <- ifelse(toupper(tm_site) %in% c("HOME","H"), 1, 
      ifelse(toupper(tm_site) %in% c("VISITOR", "V", "AWAY", "A", "ROAD", "R"), -1, 
      ifelse(toupper(tm_site) %in% c("NEUTRAL","N"), 0, NA)))
  
  tm_pregame_fpidiff <- tm_pregame_fpi - opp_pregame_fpi
  
  INTERCEPT <- 0
  COEF_tm_hfa <- 3.066772
  COEF_tm_pregame_fpidiff <- 1
  CONST_STDERR <- 15.85718
  
  predicted_mean <- INTERCEPT + COEF_tm_hfa*tm_hfa + COEF_tm_pregame_fpidiff*tm_pregame_fpidiff
  predicted_stderr <- CONST_STDERR
  
  predicted_winprob <- pnorm(0, predicted_mean, predicted_stderr, lower.tail = FALSE)
  
  return(predicted_winprob)
}

AllTeamsFPIByDate <- data.table(read.table(file ="All Teams FPI By Date.csv", 
  header = TRUE, sep = ","))

TeamColors <- data.table(read.table(file ="Team Colors.csv", header = TRUE, sep = ","))

# Define server logic required
shinyServer(function(input, output) {

  tm_fpi_info <- reactive({AllTeamsFPIByDate[SEASON == input$season & TEAM == input$tm]})
  opp_fpi_info <- reactive({AllTeamsFPIByDate[SEASON == input$season & TEAM == input$opp]})
  
  tm_color <- reactive({paste("#", ifelse(input$tm == "NON-FBS TEAMS", "000000", 
    as.character(TeamColors[TEAM == input$tm]$TEAM_COLOR)), sep = "")})
  opp_color <- reactive({paste("#", ifelse(input$opp == "NON-FBS TEAMS", "000000", 
    as.character(TeamColors[TEAM == input$opp]$TEAM_COLOR)), sep = "")})
  
  fpi_date <- reactive({tm_fpi_info()$FPI_DATE})
  
  tm_fpi <- reactive({tm_fpi_info()$FPI})
  tm_fpi_rank <- reactive({tm_fpi_info()$FPI_RANK})
  
  opp_fpi <- reactive({opp_fpi_info()$FPI})
  opp_fpi_rank <- reactive({opp_fpi_info()$FPI_RANK})
  
  pred_winpct <- reactive({CFBGameWinProb(tm_fpi(), opp_fpi(), input$tm_site)})
  
  slices <- reactive({c(pred_winpct(), 1 - pred_winpct())})
  lbls <- reactive({c(paste(input$tm, ": ", round(pred_winpct()*100, 1), "%", sep = ""),
            paste(input$opp, ": ", round((1-pred_winpct())*100, 1), "%", sep = ""))})

  output$text1 <- renderText({if(input$getbutton >= 1){isolate(
      paste(input$season, "Season Matchup Prediction Based on Football Power Index")
      )} })
  
  output$text2 <- renderText({if(input$getbutton >= 1){isolate(
      paste("Using FPI As Of", fpi_date())
      )} })

  output$text3 <- renderText({if(input$getbutton >= 1){isolate(
      paste(ifelse(input$tm == "NON-FBS TEAMS", "", paste("(FPI #", tm_fpi_rank(), ") ", sep = "") ),
            input$tm, 
            ifelse(input$tm_site == "Road", " at ", " vs "),
            ifelse(input$opp == "NON-FBS TEAMS", "", paste("(FPI #", opp_fpi_rank(), ") ", sep = "") ),
            input$opp,
            ifelse(input$tm_site == "Neutral", " at Neutral Site", ""),
            sep = "")
      )} })

  output$plot <- renderPlot({if(input$getbutton >= 1){isolate(
      pie(slices(), labels = lbls(), col = c(tm_color(), opp_color()),
          clockwise = TRUE, init.angle = 180*(1 + pred_winpct()), 
          main = "Chances To Win Matchup", radius = 0.8)
      )} })
})