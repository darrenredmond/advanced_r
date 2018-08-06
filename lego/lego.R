# Darren Redmond's Lego Explorer Helper Functions

# Load required libraries
if (!require('data.table')) install.packages('data.table')
library(data.table)
if (!require('dplyr')) install.packages('dplyr')
library(dplyr)
if (!require('DT')) install.packages('DT')
library(DT)
if (!require('devtools')) install.packages('devtools')
library(devtools)
if (!require('rCharts')) install_github('rCharts', 'ramnathv')
library(rCharts)

# Read data
#data <- fread("./sets.csv")
data <- fread('./brickset-mysets-owned.csv')

#head(data)
setnames(data, "Theme", "theme")
setnames(data, "SetName", "name")
setnames(data, "Number", "setId")
setnames(data, "Year", "year")
setnames(data, "Pieces", "pieces")
setnames(data, "USPrice", "price")

data <- subset(data, select=c(theme, name, setId, year, pieces, price))
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
min(data$pieces) # -1
max(data$pieces) # 7591
min(data$price) # -1
max(data$price) # 7591

explore_data(data)
themes <- sort(unique(data$theme))

#' Filter dataset only by year
#' @param dataset the dataset.
#' @param minYear the minimum year.
#' @param maxYear the maximum year.
#' @return the filtered dataset with the filters applied.
#'
filterByYear <- function(dataset, minYear, maxYear) {
  dataset %>% filter(year >= minYear, year <= maxYear) 
}

#' Filter dataset by year, piece and theme
#' @param dataSet the dataset.
#' @param minYear the minimum year.
#' @param maxYear the maximum year.
#' @param minPrice the minimum price count.
#' @param maxPrice the maximum price count.
#' @param minPiece the minimum piece count.
#' @param maxPiece the maximum piece count.
#' @param themes the themes to include in the filtered dataset.
#' @return the filtered dataset with the filters applied.
#'
filterByYearPricePieceTheme <- function(dataset, minYear, maxYear, minPrice, maxPrice, minPiece, maxPiece, themes) {
  dataset %>% filter(year >= minYear, year <= maxYear, price >= minPrice, price <= maxPrice,
    pieces >= minPiece, pieces <= maxPiece, theme %in% themes) 
}

#' Group by year and summarise by set.
#' @param dataSet the dataset.
#' @param minYear the minimum year.
#' @param maxYear the maximum year.
#' @param minPrice the minimum price count.
#' @param maxPrice the maximum price count.
#' @param minPiece the minimum piece count.
#' @param maxPiece the maximum piece count.
#' @param themes the themes to include in the filtered dataset.
#' @return the filtered dataset with the filters applied.
#' 
groupByYear <- function(dataset, minYear, maxYear, minPrice, maxPrice, minPiece, maxPiece, themes) {
  filterByYearPricePieceTheme(dataset, minYear, maxYear, minPrice, maxPrice, minPiece, maxPiece, themes) %>% 
    group_by(year) %>% 
    summarise(total_sets = n_distinct(setId)) %>%
    arrange(year)
}

#' Group by themes
#' @param dataSet the dataset.
#' @param minYear the minimum year.
#' @param maxYear the maximum year.
#' @param minPrice the minimum price count.
#' @param maxPrice the maximum price count.
#' @param minPiece the minimum piece count.
#' @param maxPiece the maximum piece count.
#' @param themes the themes to include in the filtered dataset.
#' @return the filtered dataset with the filters applied.
#' 
groupByTheme <- function(dataset, minYear, maxYear, minPrice, maxPrice, minPiece, maxPiece, themes) {
  datatable(filterByYearPricePieceTheme(dataset, minYear, maxYear, minPrice, maxPrice, minPiece, maxPiece, themes),
    options = list(iDisplayLength = 50))
}

#' Group by year to get total count of themes.
#' @param dataSet the dataset.
#' @param minYear the minimum year.
#' @param maxYear the maximum year.
#' @param minPrice the minimum price count.
#' @param maxPrice the maximum price count.
#' @param minPiece the minimum piece count.
#' @param maxPiece the maximum piece count.
#' @param themes the themes to include in the filtered dataset.
#' @return data.table 2 columns
#'
groupByYearAgg <- function(dataset, minYear, maxYear, minPrice, maxPrice, minPiece, maxPiece, themes) {
  filterByYear(dataset, minYear, maxYear) %>% 
    group_by(year)  %>% 
    summarise(count = n_distinct(theme)) %>%
    arrange(year)
}

#' Aggregate dataset by year to get total count of average number of pieces.
#' @param dataSet the dataset.
#' @param minYear the minimum year.
#' @param maxYear the maximum year.
#' @param minPrice the minimum price count.
#' @param maxPrice the maximum price count.
#' @param minPiece the minimum piece count.
#' @param maxPiece the maximum piece count.
#' @param themes the themes to include in the filtered dataset.
#' @return data.table 2 columns
#'
groupByPieceAvg <- function(dataset, minYear, maxYear, minPrice, maxPrice, minPiece, maxPiece, themes) {
  filterByYearPricePieceTheme(dataset, minYear, maxYear, minPrice, maxPrice, minPiece, maxPiece, themes) %>% 
    group_by(year) %>% 
    summarise(avg = mean(pieces)) %>%
    arrange(year)
}

#' Average pieces for each theme.
#' @param dataSet the dataset.
#' @param minYear the minimum year.
#' @param maxYear the maximum year.
#' @param minPrice the minimum price count.
#' @param maxPrice the maximum price count.
#' @param minPiece the minimum piece count.
#' @param maxPiece the maximum piece count.
#' @param themes the themes to include in the filtered dataset.
#' @return data.table 2 columns
#'
groupByPieceThemeAvg <- function(dataset, minYear, maxYear, minPrice, maxPrice, minPiece, maxPiece, themes) {
  filterByYearPricePieceTheme(dataset, minYear, maxYear, minPrice, maxPrice, minPiece, maxPiece, themes) %>%
    group_by(theme) %>%
    summarise(avgPieces = mean(pieces)) %>%
    arrange(theme)
}

#' Plot number of sets by year
#' @param dataset data.table
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel number of sets
#' @return setsByYear plot
plotSetsCountByYear <- function(dataset, dom = "setsByYear", 
    xAxisLabel = "Year", yAxisLabel = "Number of Sets") {
  tooltipContent <- "#! function(key, x, y, e) {
   return '<h5><b>Year</b>: ' + e.point.year + '<br>' + '<b>Total Sets</b>: '
  + e.point.total_sets + '<br>' + '</h5>' } !#"
  setsByYear <- nPlot(total_sets ~ year, data = dataset, type = "stackedAreaChart",
    dom = dom, width = 650)
  setsByYear$chart(margin = list(left = 100))
  setsByYear$chart(color = c('purple', 'blue', 'green'))
  setsByYear$chart(tooltipContent = tooltipContent)
  setsByYear$yAxis(axisLabel = yAxisLabel, width = 80)
  setsByYear$xAxis(axisLabel = xAxisLabel, width = 70)
  setsByYear 
}

#' Plot number of themes by year
#' 
#' @param dataset data.table
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel number of themes
#' @return themesByYear plot
plotThemesCountByYear <- function(dataset, dom = "themesByYear", 
    xAxisLabel = "Year", yAxisLabel = "Number of Themes") {
  tooltipContent <- "#! function(key, x, y, e) {
    return '<h5><b>Year</b>: ' + e.point.year + '<br>' + '<b>Total Themes</b>: ' + e.point.count + '<br>'
  + '</h5>' } !#"
  themesByYear <- nPlot(count ~ year, data = dataset, type = "multiBarChart",
    dom = dom, width = 650)
  themesByYear$chart(margin = list(left = 100))
  themesByYear$yAxis(axisLabel = yAxisLabel, width = 80)
  themesByYear$xAxis(axisLabel = xAxisLabel, width = 70)
  themesByYear$chart(tooltipContent = tooltipContent)
  themesByYear
}

#' Plot number of pieces by year
#' @param dataset data.table
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel number of pieces
#' @return plotPiecesByYear plot
plotPiecesByYear <- function(dataset, dom = "piecesByYear", 
    xAxisLabel = "Year", yAxisLabel = "Number of Pieces") {
  tooltipContent <- "#! function(key, x, y, e) {
    return '<h5><b>Set Name</b>: ' + e.point.name + '<br>'
        + '<b>Set ID</b>: ' + e.point.setId + '<br>' + '<b>Theme</b>: ' + e.point.theme + '</h5>' } !#"
  piecesByYear <- nPlot(pieces ~ year, data = dataset, type = "scatterChart",
    dom = dom, width = 650)
  piecesByYear$chart(margin = list(left = 100), showDistX = TRUE, showDistY = TRUE)
  piecesByYear$chart(color = c('green', 'orange', 'blue'))
  piecesByYear$chart(tooltipContent = tooltipContent)
  piecesByYear$yAxis(axisLabel = yAxisLabel, width = 80)
  piecesByYear$xAxis(axisLabel = xAxisLabel, width = 70)
  #     piecesByYear$chart(useInteractiveGuideline = TRUE)
  piecesByYear
}

#' Plot price by year
#' @param dataset data.table
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel price
#' @return plotPriceByYear plot
plotPriceByYear <- function(dataset, dom = "priceByYear", 
    xAxisLabel = "Year", yAxisLabel = "Price") {
  tooltipContent <- "#! function(key, x, y, e) {
  return '<h5><b>Set Name</b>: ' + e.point.name + '<br>'
  + '<b>Set ID</b>: ' + e.point.setId + '<br>' + '<b>Theme</b>: ' + e.point.theme + '</h5>' } !#"
  priceByYear <- nPlot(price ~ year, data = dataset, type = "scatterChart",
    dom = dom, width = 650)
  priceByYear$chart(margin = list(left = 100), showDistX = TRUE, showDistY = TRUE)
  priceByYear$chart(color = c('green', 'orange', 'blue'))
  priceByYear$chart(tooltipContent = tooltipContent)
  priceByYear$yAxis(axisLabel = yAxisLabel, width = 80)
  priceByYear$xAxis(axisLabel = xAxisLabel, width = 70)
  #     priceByYear$chart(useInteractiveGuideline = TRUE)
  priceByYear
}

#' Plot the average number of pieces by year.
#' @param dataset
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel number of pieces
#' @return themesByYear plot
plotPiecesByYearAvg <- function(dataset, dom = "piecesByYearAvg", 
    xAxisLabel = "Year", yAxisLabel = "Number of Pieces") {
  piecesByYearAvg <- nPlot(avg ~ year, data = dataset, type = "lineChart", dom = dom, width = 650)
  piecesByYearAvg$chart(margin = list(left = 100))
  piecesByYearAvg$chart(color = c('orange', 'blue', 'green'))
  piecesByYearAvg$yAxis(axisLabel = yAxisLabel, width = 80)
  piecesByYearAvg$xAxis(axisLabel = xAxisLabel, width = 70)
  piecesByYearAvg
}

#' Plot number of average pieces by theme
#' 
#' @param dataset data.table
#' @param dom
#' @param xAxisLabel theme
#' @param yAxisLabel number of pieces
#' @return piecesByThemeAvg plot
plotPiecesByThemeAvg <- function(dataset, dom = "piecesByThemeAvg", 
    xAxisLabel = "Themes", yAxisLabel = "Number of Pieces") {
  piecesByThemeAvg <- nPlot(avgPieces ~ theme, data = dataset, type = "multiBarChart",
    dom = dom, width = 650)
  piecesByThemeAvg$chart(margin = list(left = 100))
  piecesByThemeAvg$chart(color = c('pink', 'blue', 'green'))
  piecesByThemeAvg$yAxis(axisLabel = yAxisLabel, width = 80)
  piecesByThemeAvg$xAxis(axisLabel = xAxisLabel, width = 200, rotateLabels = -20, height = 200)
  piecesByThemeAvg
}

