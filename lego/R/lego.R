# Darren Redmond 92026265 lab 8 - design a shiny app based on my functions for my project.
# Darren Redmond's Lego Explorer Helper Functions
# This is the lego.R file which lays out the helper functions for interfacing with the data in this n-tier shiny application.

if (!require('devtools')) install.packages('devtools')
library(devtools)
if (!require('rmarkdown')) install.packages('rmarkdown')
library(rmarkdown)

createProject <- function(dir, name) {
  setwd(dir)
  devtools::create(name)
  setwd(dir + name)
  devtools::document()
  setwd('..')
  devtools::use_package('dplyr')
  devtools::use_package('magrittr')
  devtools::use_package('data.table')
  devtools::use_package('DT')
  devtools::use_package('rlang')
  devtools::use_package('devtools')
  devtools::use_package('rCharts')
  devtools::use_package('rmarkdown')
  devtools::use_package('knitr')
  devtools::use_package('shiny')
  devtools::use_mit_license()
  devtools::use_vignette(name)
  devtools::install(build_vignettes = TRUE)
  devtools::install(name)
}
explore <- function(data) {
  # Exploratory data analysis
  sum(is.na(data$pieces)) # 0
  # 80 pieces have na's - set them 0
  data$pieces[is.na(data$pieces)] <- 0
  # 90 price have na's - set them 0
  sum(is.na(data$price)) # 0
  data$price[is.na(data$price)] <- 0
  length(unique(data$setId)) # I own 402 different lego sets
  table(data$year) # 2006 - 2018
  length(table(data$year)) # 11
  years <- sort(unique(data$year))
  length(table(data$theme)) # 18
  min(data$pieces) # 0
  max(data$pieces) # 7541
  min(data$price) # 0
  max(data$price) # 799.99
}

examples <- function() {
  # Read data
  #data <- read.lego(system.file('extdata', 'sets.csv', package ='lego'))
  #fread("./inst/extdata/sets.csv")
  data <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
  explore(data)
  #str(data)
  themes <- sort(unique(data$theme))
  subthemes <- sort(unique(data$subtheme))
  print(filterByYearPricePieceTheme(data))
  print(filterByYearPricePieceTheme(data, themes=('Nexo Knights')))
  print(filterByYearPricePieceTheme(data, 1950, 2018, 0, 10000, 0, 10000, themes, subthemes))
  print(filterByYearPricePieceTheme(data, 1950, 2018, 0, 10000, 0, 10000, c('Star Wars'), subthemes))
  print(filterByYearPricePieceTheme(data, 2014, 2015, themes=c('Star Wars'), subthemes=c('Episode I')))
}

# Load required libraries
#source("helper.R")

#createProject('~/dev/advanced_r/', 'lego')
#examples()

#runShinyLego()

#if (!require('lego')) install.packages('lego')
#library(lego)

#vignette('lego')

#browseVignettes()
