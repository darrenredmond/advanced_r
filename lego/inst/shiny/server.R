# Darren Redmond 92026265 lab 8 - design a shiny app based on my functions for my project.
# This is the server.R file which lays out the backend server apis for this n-tier shiny application.

if (!require('shiny')) install.packages('shiny')
library(shiny)

if (!require('lego')) install.packages('lego')
library(lego)

# Load data processing file
if (!exists("lego_data")) {
  print('Loading Default Data File')
  lego_data <- lego::read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
}

# get the unique lego themes that I own and sort them alphabetically for the left hand side subselection.
themes <- sort(unique(lego_data$theme))
# get the subthemes
subthemes <- sort(unique(lego_data$subtheme))

# Create the shiny server
shinyServer(
  # create the function to receive the input and set the output
  function(input, output) {
    output$setid <- renderText({input$setid})

    # Initialize reactive values
    values <- reactiveValues()
    values$themes <- themes
    values$subthemes <- subthemes

    # Create event type checkbox
    output$themesControl <- renderUI({
      checkboxGroupInput('themes', 'LEGO Themes:', themes, selected=values$themes)
    })

    # Create event type checkbox
    output$subthemesControl <- renderUI({
      checkboxGroupInput('subthemes', 'LEGO SubThemes:', subthemes, selected=values$subthemes)
    })

    # Add observer on select-all button
    observe({
      if (input$selectAll == 0) return()
      values$themes <- themes
    })

    # Add observer on clear-all button
    observe({
      if (input$clearAll == 0) return()
      values$themes <- c() # empty list
    })

    # Add observer on select-all sub button
    observe({
      if (input$selectAllSub == 0) return()
      values$subthemes <- subthemes
    })

    # Add observer on clear-all sub button
    observe({
      if (input$clearAllSub == 0) return()
      values$subthemes <- c() # empty list
    })

    # Prepare datasets for the different graphs
    dataTable <- reactive({
      lego::filterByYearPricePieceTheme(lego_data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes, input$subthemes)
    })

    dataTableByPiecesYear <- reactive({
      lego::groupByPiece(lego_data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes, input$subthemes)
    })

    dataTableByPriceYear <- reactive({
      lego::groupByPrice(lego_data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes, input$subthemes)
    })

    dataTableBySetYear <- reactive({
      lego::groupByYear(lego_data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes, input$subthemes)
    })

    dataTableByThemeYear <- reactive({
      lego::groupByTheme(lego_data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes, input$subthemes)
    })

    dataTableByPiece <- reactive({
      lego::filterByYearPricePieceTheme(lego_data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes, input$subthemes)
    })

    dataTableByPrice <- reactive({
      lego::filterByYearPricePieceTheme(lego_data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes, input$subthemes)
    })

    dataTableByPieceAvg <- reactive({
      lego::groupByPieceAvg(lego_data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes, input$subthemes)
    })

    dataTableByPieceThemeAvg <- reactive({
      lego::groupByPieceThemeAvg(lego_data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes, input$subthemes)
    })

    # Render data table
    output$dTable <- renderDataTable({
      dataTable()
    })

    output$setsByYear <- renderChart({
      lego::plotSetsCountByYear(dataTableBySetYear())
    })

    output$priceCountByYear <- renderChart({
      lego::plotPriceCountByYear(dataTableByPriceYear())
    })

    output$piecesCountByYear <- renderChart({
      lego::plotPiecesCountByYear(dataTableByPiecesYear())
    })

    output$themesByYear <- renderChart({
      lego::plotThemesCountByYear(dataTableByThemeYear())
    })

    output$piecesByYear <- renderChart({
      lego::plotPiecesByYear(dataTableByPiece())
    })

    output$priceByYear <- renderChart({
      lego::plotPriceByYear(dataTableByPrice())
    })

    output$piecesByYearAvg <- renderChart({
      lego::plotPiecesByYearAvg(dataTableByPieceAvg())
    })

    output$piecesByThemeAvg <- renderChart({
      lego::plotPiecesByThemeAvg(dataTableByPieceThemeAvg())
    })

  }

)
