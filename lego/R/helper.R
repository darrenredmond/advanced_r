
# Darren Redmond 92026265 Project
# Darren Redmond's Lego Explorer Helper Functions
# This is the helper.R file which lays out the helper functions for interfacing with the data in this lego package
# and the n-tier shiny application.

if (!require('dplyr')) install.packages('dplyr')
library(dplyr)
if (!require('magrittr')) install.packages('magrittr')
library(magrittr)
if (!require('data.table')) install.packages('data.table')
library(data.table)
if (!require('DT')) install.packages('DT')
library(DT)
if (!require('rlang')) install.packages('rlang')
library(rlang)

#' Ensure a column exists in the dataset
#' @param dataset the dataset.
#' @param oldName the old column name.
#' @param name the new column name.
#' @export
#' @importFrom utils head
#' @importFrom data.table setnames
ensureColumn <- function(dataset, oldName, name) {
  if (oldName %in% colnames(dataset)) {
    setnames(dataset, oldName, name)
  } else {
    dataset$temp_lego_col <- NA
    setnames(dataset, 'temp_lego_col', name)
    print(head(dataset))
  }
}

#' Read a brickset lego file.
#' @param file the file.
#' @return the lego brickset lego file.
#' @export
#' @importFrom data.table fread
read.lego <- function(file) {
  dataset <- fread(file,fill=TRUE)
  #head(dataset)
  ensureColumn(dataset, 'Theme', 'theme')
  ensureColumn(dataset, 'Subtheme', 'subtheme')
  ensureColumn(dataset, 'SetName', 'name')
  ensureColumn(dataset, 'Number', 'setId')
  ensureColumn(dataset, 'Year', 'year')
  ensureColumn(dataset, 'Pieces', 'pieces')
  ensureColumn(dataset, 'USPrice', 'price')
  print(head(dataset))
  dataset <- subset(dataset, select=c(theme, name, setId, year, pieces, price, subtheme))
  dataset$pieces[is.na(dataset$pieces)] <- 0
  dataset$price[is.na(dataset$price)] <- 0
  print(head(dataset))
  dataset
}

#' Filter dataset only by year
#' @param dataset the dataset.
#' @param minYear the minimum year.
#' @param maxYear the maximum year.
#' @return the filtered dataset with the filters applied.
#' @export
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
filterByYear <- function(dataset, minYear, maxYear) {
  dataset %>% filter(.data$year >= minYear, .data$year <= maxYear)
}

#' Filter dataset by year, price, number of pieces and theme
#' @param dataset the dataset - expects a dataset.
#' @param minYear the minimum year. Defaults to be minimum year in the dataset.
#' @param maxYear the maximum year. Defaults to be maximum year in the dataset.
#' @param minPrice the minimum price count. Defaults to be minimum price in the dataset.
#' @param maxPrice the maximum price count. Defaults to be maximum price in the dataset.
#' @param minPieces the minimum piece count. Defaults to be minimum piece count in the dataset.
#' @param maxPieces the maximum piece count. Defaults to be maximum piece count in the dataset.
#' @param themes the themes to include in the filtered dataset. Defaults to be all themes.
#' @param subthemes the subthemes to include in the filtered dataset. Defaults to be all subthemes.
#' @return the filtered dataset with the filter parameters applied.
#' @keywords lego
#' @export
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples
#' dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
#' filterByYearPricePieceTheme(dataset)
#' filterByYearPricePieceTheme(dataset, themes=c('Star Wars'))
#' filterByYearPricePieceTheme(dataset, 2010, 2016, 5, 10, 100, 1000, c('Star Wars'), c('Episode I'))
#' filterByYearPricePieceTheme(dataset, 2014, 2015, themes=c('Star Wars'), subthemes=c('Episode I'))
#'
filterByYearPricePieceTheme <- function(dataset,
    minYear=min(dataset$year), maxYear=max(dataset$year),
    minPrice=min(dataset$price), maxPrice=max(dataset$price),
    minPieces=min(dataset$pieces), maxPieces=max(dataset$pieces),
    themes=sort(unique(dataset$theme)), subthemes=sort(unique(dataset$subtheme))) {
  dataset %>% filter(year >= minYear, year <= maxYear,
      price >= minPrice, price <= maxPrice,
      pieces >= minPieces, pieces <= maxPieces,
      theme %in% themes, subtheme %in% subthemes)
}

#' Group by year and summarise by set.
#' @param dataset the dataset.
#' @param minYear the minimum year.
#' @param maxYear the maximum year.
#' @param minPrice the minimum price count.
#' @param maxPrice the maximum price count.
#' @param minPieces the minimum piece count.
#' @param maxPieces the maximum piece count.
#' @param themes the themes to include in the filtered dataset.
#' @param subthemes the subthemes to include in the filtered dataset.
#' @return the filtered dataset with the filters applied.
#' @keywords lego
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr n_distinct
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#'
groupByYear <- function(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes) {
  filterByYearPricePieceTheme(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes) %>%
    group_by(year) %>%
    summarise(total_sets = n_distinct(setId)) %>%
    arrange(year)
}

#' Group by year to get total count of pieces.
#' @param dataset the dataset.
#' @param minYear the minimum year.
#' @param maxYear the maximum year.
#' @param minPrice the minimum price count.
#' @param maxPrice the maximum price count.
#' @param minPieces the minimum piece count.
#' @param maxPieces the maximum piece count.
#' @param themes the themes to include in the filtered dataset.
#' @param subthemes the subthemes to include in the filtered dataset.
#' @return data.table 2 columns
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#'
groupByPiece <- function(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes) {
  filterByYearPricePieceTheme(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes) %>%
    group_by(year)  %>%
    summarise(count = sum(pieces)) %>%
    arrange(year)
}

#' Group by year to get total price.
#' @param dataset the dataset.
#' @param minYear the minimum year.
#' @param maxYear the maximum year.
#' @param minPrice the minimum price count.
#' @param maxPrice the maximum price count.
#' @param minPieces the minimum piece count.
#' @param maxPieces the maximum piece count.
#' @param themes the themes to include in the filtered dataset.
#' @param subthemes the subthemes to include in the filtered dataset.
#' @return data.table 2 columns
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#'
groupByPrice <- function(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes) {
  filterByYearPricePieceTheme(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes) %>%
    group_by(year)  %>%
    summarise(count = sum(price)) %>%
    arrange(year)
}

#' Group by themes
#' @param dataset the dataset.
#' @param minYear the minimum year.
#' @param maxYear the maximum year.
#' @param minPrice the minimum price count.
#' @param maxPrice the maximum price count.
#' @param minPieces the minimum piece count.
#' @param maxPieces the maximum piece count.
#' @param themes the themes to include in the filtered dataset.
#' @param subthemes the subthemes to include in the filtered dataset.
#' @return the filtered dataset with the filters applied.
#' @export
#' @importFrom DT datatable
#'
groupByTheme <- function(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes) {
  datatable(filterByYearPricePieceTheme(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes),
    options = list(iDisplayLength = 50))
}

#' Group by year to get total count of themes.
#' @param dataset the dataset.
#' @param minYear the minimum year.
#' @param maxYear the maximum year.
#' @param minPrice the minimum price count.
#' @param maxPrice the maximum price count.
#' @param minPieces the minimum piece count.
#' @param maxPieces the maximum piece count.
#' @param themes the themes to include in the filtered dataset.
#' @param subthemes the subthemes to include in the filtered dataset.
#' @return data.table 2 columns
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr n_distinct
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#'
groupByYearAgg <- function(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes) {
  filterByYear(dataset, minYear, maxYear) %>%
    group_by(year)  %>%
    summarise(count = n_distinct(theme)) %>%
    arrange(year)
}

#' Aggregate dataset by year to get total count of average number of pieces.
#' @param dataset the dataset.
#' @param minYear the minimum year.
#' @param maxYear the maximum year.
#' @param minPrice the minimum price count.
#' @param maxPrice the maximum price count.
#' @param minPieces the minimum piece count.
#' @param maxPieces the maximum piece count.
#' @param themes the themes to include in the filtered dataset.
#' @param subthemes the subthemes to include in the filtered dataset.
#' @return data.table 2 columns
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#'
groupByPieceAvg <- function(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes) {
  filterByYearPricePieceTheme(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes) %>%
    group_by(year) %>%
    summarise(avg = mean(pieces)) %>%
    arrange(year)
}

#' Average pieces for each theme.
#' @param dataset the dataset.
#' @param minYear the minimum year.
#' @param maxYear the maximum year.
#' @param minPrice the minimum price count.
#' @param maxPrice the maximum price count.
#' @param minPieces the minimum piece count.
#' @param maxPieces the maximum piece count.
#' @param themes the themes to include in the filtered dataset.
#' @param subthemes the subthemes to include in the filtered dataset.
#' @return data.table 2 columns
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#'
groupByPieceThemeAvg <- function(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes) {
  filterByYearPricePieceTheme(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes) %>%
    group_by(theme) %>%
    summarise(avgPieces = mean(pieces)) %>%
    arrange(theme)
}

#' Plot number of sets by year
#' @param dataset the dataset.
#' @param dom the dom.
#' @param xAxisLabel the x axis label - the year.
#' @param yAxisLabel the y axis label - the number of sets.
#' @return the setsByYear plot.
plotSetsCountByYear <- function(dataset, dom = "setsByYear",
    xAxisLabel = "Year", yAxisLabel = "Number of Sets") {
  tooltipContent <- "#! function(key, x, y, e) {
   return '<h5><b>Year</b>: ' + e.point.year + '<br>' + '<b>Total Sets</b>: '
  + e.point.total_sets + '<br>' + '</h5>' } !#"
  setsByYear <- nPlot(total_sets ~ year, data = dataset, type = "stackedAreaChart",
    dom = dom, width = 600)
  setsByYear$chart(margin = list(left = 100))
  setsByYear$chart(color = c('purple', 'blue', 'green'))
  setsByYear$chart(tooltipContent = tooltipContent)
  setsByYear$yAxis(axisLabel = yAxisLabel, width = 80)
  setsByYear$xAxis(axisLabel = xAxisLabel, width = 70)
  setsByYear
}

#' Plot number of pieces by year.
#' @param dataset the dataset.
#' @param dom the dom.
#' @param xAxisLabel the x axis label - the year.
#' @param yAxisLabel the y axis label - the number of pieces.
#' @return the piecesCountByYear plot.
plotPiecesCountByYear <- function(dataset, dom = "piecesCountByYear",
    xAxisLabel = "Year", yAxisLabel = "Number of Pieces") {
  tooltipContent <- "#! function(key, x, y, e) {
  return '<h5><b>Year</b>: ' + e.point.year + '<br>' + '<b>Total Pieces</b>: ' + e.point.count + '<br>'
  + '</h5>' } !#"
  piecesCountByYear <- nPlot(count ~ year, data = dataset, type = "multiBarChart", dom = dom, width = 600)
  piecesCountByYear$chart(margin = list(left = 100))
  piecesCountByYear$yAxis(axisLabel = yAxisLabel, width = 80)
  piecesCountByYear$xAxis(axisLabel = xAxisLabel, width = 70)
  piecesCountByYear$chart(tooltipContent = tooltipContent)
  piecesCountByYear
}

#' Plot price by year.
#' @param dataset the dataset.
#' @param dom the dom.
#' @param xAxisLabel the x axis label - the year.
#' @param yAxisLabel the y axis label - the price.
#' @return the priceCountByYear plot.
plotPriceCountByYear <- function(dataset, dom = "priceCountByYear",
    xAxisLabel = "Year", yAxisLabel = "Price") {
  tooltipContent <- "#! function(key, x, y, e) {
  return '<h5><b>Year</b>: ' + e.point.year + '<br>' + '<b>Price</b>: ' + e.point.count + '<br>'
  + '</h5>' } !#"
  priceCountByYear <- nPlot(count ~ year, data = dataset, type = "multiBarChart", dom = dom, width = 600)
  priceCountByYear$chart(margin = list(left = 100))
  priceCountByYear$yAxis(axisLabel = yAxisLabel, width = 80)
  priceCountByYear$xAxis(axisLabel = xAxisLabel, width = 70)
  priceCountByYear$chart(tooltipContent = tooltipContent)
  priceCountByYear
}

#' Plot number of themes by year.
#' @param dataset the dataset.
#' @param dom the dom.
#' @param xAxisLabel the x axis label - the year.
#' @param yAxisLabel the y axis label - number of themes.
#' @return the themesByYear plot.
plotThemesCountByYear <- function(dataset, dom = "themesByYear",
    xAxisLabel = "Year", yAxisLabel = "Number of Themes") {
  tooltipContent <- "#! function(key, x, y, e) {
    return '<h5><b>Year</b>: ' + e.point.year + '<br>' + '<b>Total Themes</b>: ' + e.point.count + '<br>'
  + '</h5>' } !#"
  themesByYear <- nPlot(count ~ year, data = dataset, type = "multiBarChart",
    dom = dom, width = 600)
  themesByYear$chart(margin = list(left = 100))
  themesByYear$yAxis(axisLabel = yAxisLabel, width = 80)
  themesByYear$xAxis(axisLabel = xAxisLabel, width = 70)
  themesByYear$chart(tooltipContent = tooltipContent)
  themesByYear
}

#' Plot number of pieces by year.
#' @param dataset the dataset.
#' @param dom the dom.
#' @param xAxisLabel the x axis label - the year.
#' @param yAxisLabel the y axis label - number of pieces.
#' @return the piecesByYear plot.
plotPiecesByYear <- function(dataset, dom = "piecesByYear",
    xAxisLabel = "Year", yAxisLabel = "Number of Pieces") {
  tooltipContent <- "#! function(key, x, y, e) {
    return '<h5><b>Set Name</b>: ' + e.point.name + '<br>'
        + '<b>Set ID</b>: ' + e.point.setId + '<br>' + '<b>Pieces</b>: ' + e.point.pieces + '</h5>' } !#"
  piecesByYear <- nPlot(pieces ~ year, data = dataset, type = "scatterChart",
    dom = dom, width = 600)
  piecesByYear$chart(margin = list(left = 100), showDistX = TRUE, showDistY = TRUE)
  piecesByYear$chart(color = c('green', 'orange', 'blue'))
  piecesByYear$chart(tooltipContent = tooltipContent)
  piecesByYear$yAxis(axisLabel = yAxisLabel, width = 80)
  piecesByYear$xAxis(axisLabel = xAxisLabel, width = 70)
  #     piecesByYear$chart(useInteractiveGuideline = TRUE)
  piecesByYear
}

#' Plot price by year.
#' @param dataset the dataset.
#' @param dom the dom.
#' @param xAxisLabel the x axis label - the year.
#' @param yAxisLabel the y axis label - the price.
#' @return the priceByYear plot.
plotPriceByYear <- function(dataset, dom = "priceByYear",
    xAxisLabel = "Year", yAxisLabel = "Price") {
  tooltipContent <- "#! function(key, x, y, e) {
  return '<h5><b>Set Name</b>: ' + e.point.name + '<br>'
  + '<b>Set ID</b>: ' + e.point.setId + '<br>' + '<b>Price</b>: ' + e.point.price + '</h5>' } !#"
  priceByYear <- nPlot(price ~ year, data = dataset, type = "scatterChart",
    dom = dom, width = 600)
  priceByYear$chart(margin = list(left = 100), showDistX = TRUE, showDistY = TRUE)
  priceByYear$chart(color = c('green', 'orange', 'blue'))
  priceByYear$chart(tooltipContent = tooltipContent)
  priceByYear$yAxis(axisLabel = yAxisLabel, width = 80)
  priceByYear$xAxis(axisLabel = xAxisLabel, width = 70)
  priceByYear
}

#' Plot the average number of pieces by year.
#' @param dataset the dataset.
#' @param dom the dom.
#' @param xAxisLabel the x axis label - the year.
#' @param yAxisLabel the y axis label - number of pieces.
#' @return the themesByYear plot.
plotPiecesByYearAvg <- function(dataset, dom = "piecesByYearAvg",
    xAxisLabel = "Year", yAxisLabel = "Number of Pieces") {
  piecesByYearAvg <- nPlot(avg ~ year, data = dataset, type = "lineChart", dom = dom, width = 600)
  piecesByYearAvg$chart(margin = list(left = 100))
  piecesByYearAvg$chart(color = c('orange', 'blue', 'green'))
  piecesByYearAvg$yAxis(axisLabel = yAxisLabel, width = 80)
  piecesByYearAvg$xAxis(axisLabel = xAxisLabel, width = 70)
  piecesByYearAvg
}

#' Plot number of average pieces by theme
#' @param dataset the dataset.
#' @param dom the dom.
#' @param xAxisLabel the x axis label - the themes.
#' @param yAxisLabel the y axis label - number of pieces.
#' @return the piecesByThemeAvg plot.
plotPiecesByThemeAvg <- function(dataset, dom = "piecesByThemeAvg",
    xAxisLabel = "Themes", yAxisLabel = "Number of Pieces") {
  piecesByThemeAvg <- nPlot(avgPieces ~ theme, data = dataset, type = "multiBarChart",
    dom = dom, width = 600)
  piecesByThemeAvg$chart(margin = list(left = 100))
  piecesByThemeAvg$chart(color = c('pink', 'blue', 'green'))
  piecesByThemeAvg$yAxis(axisLabel = yAxisLabel, width = 80)
  piecesByThemeAvg$xAxis(axisLabel = xAxisLabel, width = 200, rotateLabels = -20, height = 200)
  piecesByThemeAvg
}
