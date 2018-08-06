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
    
    output$address <- renderText({
      input$goButtonAdd
      isolate(paste("http://brickset.com/sets/", 
                    input$setid, sep=""))
      
    })
    
    openPage <- function(url) {
      return(tags$a(href=url, "Click here!", target="_blank"))
    }
    
    output$inc <- renderUI({ 
      input$goButtonDirect
      isolate(openPage(paste("http://brickset.com/sets/", 
                             input$setid, sep="")))
      ## Can't open iframe below 
      # Got This request has been blocked; 
      # the content must be served over HTTPS error msg
      # Mixed Content: The page at 'https://xiaodan.shinyapps.io/LegoDatasetVisualization/' 
      # was loaded over HTTPS, but requested an insecure resource 'http://brickset.com/sets/'. 
      # This request has been blocked; the content must be served over HTTPS.
      #isolate(getPage(paste("//brickset.com/sets/", 
      #                       input$setid, sep="")))  
    })
    
    
    # Initialize reactive values
    values <- reactiveValues()
    values$themes <- themes
    
    # Create event type checkbox
    output$themesControl <- renderUI({
      checkboxGroupInput('themes', 'LEGO Themes:', 
                         themes, selected = values$themes)
    })
    
    # Add observer on select-all button
    observe({
      if(input$selectAll == 0) return()
      values$themes <- themes
    })
    
    # Add observer on clear-all button
    observe({
      if(input$clearAll == 0) return()
      values$themes <- c() # empty list
    })
    
    # Prepare dataset
    dataTable <- reactive({
      groupByTheme(data, input$timeline[1], 
                   input$timeline[2], input$pieces[1],
                   input$pieces[2], input$themes)
    })
    
    dataTableBySetYear <- reactive({
      groupByYear(data, input$timeline[1], 
                     input$timeline[2], input$pieces[1],
                     input$pieces[2], input$themes)
    })
    
    dataTableByYear <- reactive({
      groupByYearAgg(data, input$timeline[1], 
                     input$timeline[2], input$pieces[1],
                     input$pieces[2], input$themes)
    })
    
    dataTableByPiece <- reactive({
      groupByYearAll(data, input$timeline[1], 
                     input$timeline[2], input$pieces[1],
                     input$pieces[2], input$themes)
    })
    
    dataTableByPrice <- reactive({
      groupByYearAll(data, input$timeline[1], 
                     input$timeline[2], input$pieces[1],
                     input$pieces[2], input$themes)
    })
    
    dataTableByPieceAvg <- reactive({
      groupByPieceAvg(data, input$timeline[1], 
                      input$timeline[2], input$pieces[1],
                      input$pieces[2], input$themes)
    })
    
    dataTableByPieceThemeAvg <- reactive({
      groupByPieceThemeAvg(data, input$timeline[1], 
                           input$timeline[2], input$pieces[1],
                           input$pieces[2], input$themes)
    })
    
    # Render data table
    output$dTable <- renderDataTable({
      dataTable()
    } #, options = list(bFilter = FALSE, iDisplayLength = 50)
    )
    
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
