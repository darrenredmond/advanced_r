# The user-interface definition of the Shiny web app.
if (!require('shiny')) install.packages('shiny')
library(shiny)
if (!require('BH')) install.packages('BH')
library(BH)
library(rCharts)

if (!require('markdown')) install.packages('markdown')
library(markdown)
if (!require('data.table')) install.packages('data.table')
library(data.table)
if (!require('dplyr')) install.packages('dplyr')
library(dplyr)
if (!require('DT')) install.packages('DT')
library(DT)

# create the shiny ui.
shinyUI(
  # create the navigation bar page.
  navbarPage("Darren Redmond's LEGO Sets", 
    # multi-page user-interface that includes a navigation bar - show the data
    tabPanel("Explore the Data",
      # define the sidebar panel.
      sidebarPanel(
        # add a timeline slide input for the years the lego set were released.
        sliderInput("timeline", "Timeline:", sep = "", min = 2006, max = 2018, value = c(2006, 2018)),
        # add a price slide input for the years the lego set were released.
        sliderInput("price", "Price:", min = 0, max = 1000, value = c(0, 1000)),
        # add a pieces slide input for the number of pieces in each lego set.
        sliderInput("pieces", "Number of Pieces:", sep = "", min = 100, max = 7541, value = c(100, 7541)),
        # the id
        uiOutput("themesControl"),
        # add a clear all selection action button
        actionButton(inputId = "clearAll", label = "Clear selection", icon = icon("square-o")),
        # add a select all action button
        actionButton(inputId = "selectAll", label = "Select all", icon = icon("check-square-o"))
      ),
      # define the main panel for the results output.
      mainPanel(
        # create tabs on the main panel.
        tabsetPanel(
          # Provide the ability to view the dataset.
          tabPanel(p(icon("table"), "Dataset"), dataTableOutput(outputId="dTable")),
          # The tab panel to display all of the graphs.
          tabPanel(p(icon("line-chart"), "Graphs"),
            h4('Number of Sets by Year', align = "center"),
            h5('Please hover over each point to see the Year and Total Number of Sets.', align ="center"),
            showOutput("setsByYear", "nvd3"),
            h4('Price by Year', align = "center"),
            h5('Please hover over each point to see the Year and Total Price Spent.', align ="center"),
            showOutput("priceCountByYear", "nvd3"),
            h4('Number of Pieces by Year', align = "center"),
            h5('Please hover over each point to see the Year and Total Number of Pieces.', align ="center"),
            showOutput("piecesCountByYear", "nvd3"),
            h4('Number of Themes by Year', align = "center"),
            h5('Please hover over each bar to see the Year and Total Number of Themes.', align ="center"),
            showOutput("themesByYear", "nvd3"),
            h4('Number of Pieces by Year', align = "center"),
            h5('Please hover over each point to see the Set Name, ID and Theme.', align ="center"),
            showOutput("piecesByYear", "nvd3"),
            h4('Price by Year', align = "center"),
            h5('Please hover over each point to see the Set Name, ID and Theme.', align ="center"),
            showOutput("priceByYear", "nvd3"),
            h4('Average Number of Pieces by Year', align = "center"),
            showOutput("piecesByYearAvg", "nvd3"),
            h4('Average Number of Pieces by Theme', align = "center"),
            showOutput("piecesByThemeAvg", "nvd3")
          )
        )
      )
    ),
    # the about panel which uses markdown to display the about.md file.
    tabPanel("About", mainPanel(includeMarkdown("about.md")))
  )
)
