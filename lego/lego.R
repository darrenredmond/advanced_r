# Darren Redmond 92026265 lab 8 - design a shiny app based on my functions for my project.
# Darren Redmond's Lego Explorer Helper Functions
# This is the lego.R file which lays out the helper functions for interfacing with the data in this n-tier shiny application.

# Load required libraries
source("R/helper.R")

#setwd('~/dev/advanced_r')
#create('lego')
#setwd('~/dev/advanced_r/lego')
#document()
#setwd('..')
#install('lego')
#devtools::use_package('dplyr')
#devtools::use_package('magrittr')
#devtools::use_package('data.table')
#devtools::use_package('DT')
#devtools::use_package('rlang')
#devtools::use_mit_license()

# Read data
#data <- read.lego("./inst/extdata/sets.csv")
#fread("./inst/extdata/sets.csv")
data <- read.lego('./inst/extdata/brickset-mysets-owned.csv')
#str(data)

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

themes <- sort(unique(data$theme))
subthemes <- sort(unique(data$subtheme))

filterByYearPricePieceTheme(data)
filterByYearPricePieceTheme(data, themes=('Nexo Knights'))
filterByYearPricePieceTheme(data, 1950, 2018, 0, 10000, 0, 10000, themes, subthemes)
filterByYearPricePieceTheme(data, 1950, 2018, 0, 10000, 0, 10000, c('Star Wars'), subthemes)
filterByYearPricePieceTheme(data, 2014, 2015, themes=c('Star Wars'), subthemes=c('Episode I'))
