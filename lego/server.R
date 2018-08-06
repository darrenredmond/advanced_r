# Darren Redmond 92026265 lab 8 - design a shiny app based on my functions for my project.
# This is the server.R file which lays out the backend server apis for this n-tier shiny application.

if (!require('shiny')) install.packages('shiny')
library(shiny)

# Load data processing file
source("lego.R")

# get the unique lego themes that I own and sort them alphabetically for the left hand side subselection.
themes <- sort(unique(data$theme))
# get the subthemes
subthemes <- sort(unique(data$subtheme))

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
      checkboxGroupInput('themes', 'LEGO Themes:', themes, selected = values$themes)
    })
    
    # Create event type checkbox
    output$subthemesControl <- renderUI({
      checkboxGroupInput('subthemes', 'LEGO SubThemes:', subthemes, selected = values$subthemes)
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
      groupByTheme(data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes, input$subthemes)
    })

    dataTableByPiecesYear <- reactive({
      groupByPiece(data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes, input$subthemes)
    })

    dataTableByPriceYear <- reactive({
      groupByPrice(data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes, input$subthemes)
    })

    dataTableBySetYear <- reactive({
      groupByYear(data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes, input$subthemes)
    })
    
    dataTableByYear <- reactive({
      groupByYearAgg(data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes, input$subthemes)
    })
    
    dataTableByPiece <- reactive({
      filterByYearPricePieceTheme(data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes, input$subthemes)
    })
    
    dataTableByPrice <- reactive({
      filterByYearPricePieceTheme(data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes, input$subthemes)
    })
    
    dataTableByPieceAvg <- reactive({
      groupByPieceAvg(data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes, input$subthemes)
    })
    
    dataTableByPieceThemeAvg <- reactive({
      groupByPieceThemeAvg(data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes, input$subthemes)
    })
    
    # Render data table
    output$dTable <- renderDataTable({
      dataTable()
    })
    
    output$setsByYear <- renderChart({
      plotSetsCountByYear(dataTableBySetYear())
    })
    
    output$priceCountByYear <- renderChart({
      plotPriceCountByYear(dataTableByPriceYear())
    })

    output$piecesCountByYear <- renderChart({
      plotPiecesCountByYear(dataTableByPiecesYear())
    })

    output$themesByYear <- renderChart({
      plotThemesCountByYear(dataTableByYear())
    })
    
    output$piecesByYear <- renderChart({
      plotPiecesByYear(dataTableByPiece())
    })
    
    output$priceByYear <- renderChart({
      plotPriceByYear(dataTableByPrice())
    })
    
    output$piecesByYearAvg <- renderChart({
      plotPiecesByYearAvg(dataTableByPieceAvg())
    })
    
    output$piecesByThemeAvg <- renderChart({
      plotPiecesByThemeAvg(dataTableByPieceThemeAvg())
    })
    
  }

)
