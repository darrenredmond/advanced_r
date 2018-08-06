if (!require('shiny')) install.packages('shiny')
library(shiny)

# Load data processing file
source("lego.R")

# get the unique lego themes that I own and sort them alphabetically for the left hand side subselection.
themes <- sort(unique(data$theme))

# Create the shiny server
shinyServer(
  # create the function to receive the input and set the output
  function(input, output) {
    output$setid <- renderText({input$setid})
    
    # Initialize reactive values
    values <- reactiveValues()
    values$themes <- themes
    
    # Create event type checkbox
    output$themesControl <- renderUI({
      checkboxGroupInput('themes', 'LEGO Themes:', themes, selected = values$themes)
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
    
    # Prepare dataset
    dataTable <- reactive({
      groupByTheme(data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes)
    })
    
    dataTableBySetYear <- reactive({
      groupByYear(data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes)
    })
    
    dataTableByYear <- reactive({
      groupByYearAgg(data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes)
    })
    
    dataTableByPiece <- reactive({
      filterByYearPricePieceTheme(data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes)
    })
    
    dataTableByPrice <- reactive({
      filterByYearPricePieceTheme(data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes)
    })
    
    dataTableByPieceAvg <- reactive({
      groupByPieceAvg(data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes)
    })
    
    dataTableByPieceThemeAvg <- reactive({
      groupByPieceThemeAvg(data, input$timeline[1], input$timeline[2],
        input$price[1], input$price[2], input$pieces[1], input$pieces[2], input$themes)
    })
    
    # Render data table
    output$dTable <- renderDataTable({
      dataTable()
    })
    
    output$setsByYear <- renderChart({
      plotSetsCountByYear(dataTableBySetYear())
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
