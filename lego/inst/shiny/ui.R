# Darren Redmond 92026265 lab 8 - design a shiny app based on my functions for my project.
# This is the ui.R file which lays out the ui for this n-tier shiny application.

# The user-interface definition of the Shiny web app.
if (!require('shiny')) install.packages('shiny')
library(shiny)
if (!require('devtools')) install.packages('devtools')
library(devtools)
if (!require('BH')) install.packages('BH')
library(BH)
if (!require('rCharts')) devtools::install_github('ramnathv/rCharts')
library(rCharts)
if (!require('markdown')) install.packages('markdown')
library(markdown)
if (!require('data.table')) install.packages('data.table')
library(data.table)
if (!require('dplyr')) install.packages('dplyr')
library(dplyr)
if (!require('DT')) install.packages('DT')
library(DT)
if (!require('lego')) install.packages('lego')
library(lego)

# create the shiny ui.
shinyUI(
  # create the navigation bar page.
  navbarPage("Darren Redmond's LEGO Sets",
    # multi-page user-interface that includes a navigation bar - show the data
    tabPanel("Explore the Data",
      # define the sidebar panel.
      sidebarPanel(
        # add a timeline slide input for the years the lego set were released.
        sliderInput("timeline", "Timeline:", sep="", min=min(lego_data$year), max=max(lego_data$year),
            value=c(min(lego_data$year), max(lego_data$year))),
        # add a price slide input for the years the lego set were released.
        sliderInput("price", "Price:", min=min(lego_data$price), max=max(lego_data$price), value=c(min(lego_data$price), max(lego_data$price))),
        # add a pieces slide input for the number of pieces in each lego set.
        sliderInput("pieces", "Number of Pieces:", sep="", min=100, max=max(lego_data$pieces),
            value=c(100, max(lego_data$pieces))),
        # the id
        uiOutput("themesControl"),
        # add a clear all selection action button
        actionButton(inputId="clearAll", label="Clear selection", icon=icon("square-o")),
        # add a select all action button
        actionButton(inputId="selectAll", label="Select all", icon=icon("check-square-o")),
        # the id
        uiOutput("subthemesControl"),
        # add a clear all selection action button
        actionButton(inputId="clearAllSub", label="Clear selection", icon=icon("square-o")),
        # add a select all action button
        actionButton(inputId="selectAllSub", label="Select all", icon=icon("check-square-o"))
      ),
      # define the main panel for the results output.
      mainPanel(
        # create tabs on the main panel.
        tabsetPanel(
          # Provide the ability to view the dataset.
          tabPanel(p(icon("table"), "Dataset"), dataTableOutput(outputId="dTable")),
          # The tab panel to display all of the graphs.
          tabPanel(p(icon("line-chart"), "Graphs"),
            h4('Number of Sets by Year', align="center"),
            h5('Please hover over each point to see the Year and Total Number of Sets.', align ="center"),
            showOutput("setsByYear", "nvd3"),
            h4('Price by Year', align="center"),
            h5('Please hover over each point to see the Year and Total Price Spent.', align ="center"),
            showOutput("priceCountByYear", "nvd3"),
            h4('Number of Pieces by Year', align="center"),
            h5('Please hover over each point to see the Year and Total Number of Pieces.', align ="center"),
            showOutput("piecesCountByYear", "nvd3"),
            h4('Number of Themes by Year', align="center"),
            h5('Please hover over each bar to see the Year and Total Number of Themes.', align ="center"),
            showOutput("themesByYear", "nvd3"),
            h4('Number of Pieces by Year', align="center"),
            h5('Please hover over each point to see the Set Name, ID and Theme.', align ="center"),
            showOutput("piecesByYear", "nvd3"),
            h4('Price by Year', align="center"),
            h5('Please hover over each point to see the Set Name, ID and Theme.', align ="center"),
            showOutput("priceByYear", "nvd3"),
            h4('Average Number of Pieces by Year', align="center"),
            showOutput("piecesByYearAvg", "nvd3"),
            h4('Average Number of Pieces by Theme', align="center"),
            showOutput("piecesByThemeAvg", "nvd3")
          )
        )
      )
    ),
    # the about panel which uses markdown to display the about.md file.
    tabPanel("About", mainPanel(includeMarkdown(system.file('extdata', 'about.md', package ='lego'))))
  )
)
