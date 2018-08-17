
# Darren Redmond 92026265 Project
# Darren Redmond's Lego Explorer Helper Functions
# This is the helper.R file which lays out the helper functions for interfacing with the data in this lego package
# and the n-tier shiny application.

if (!require('devtools')) install.packages('devtools')
library(devtools)
if (!require('dplyr')) install.packages('dplyr')
library(dplyr)
if (!require('magrittr')) install.packages('magrittr')
library(magrittr)
if (!require('data.table')) install.packages('data.table')
library(data.table)
if (!require('DT')) install.packages('DT')
library(DT)
if (!require('ggplot2')) install.packages('ggplot2')
library(ggplot2)
if (!require('rlang')) install.packages('rlang')
library(rlang)
if (!require('rCharts')) devtools::install_github('ramnathv/rCharts')
library(rCharts)

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
    #print(head(dataset))
  }
}

#' Read a brickset lego file.
#' @param file the file.
#' @return the lego brickset lego file.
#' @export
#' @importFrom data.table fread
#' @importFrom rlang .data
#' @examples
#' dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
#' all_dataset <- read.lego(system.file('extdata', 'sets.csv', package ='lego'))
read.lego <- function(file) {
  dataset <- fread(file, fill=TRUE)
  ensureColumn(dataset, 'Theme', 'theme')
  ensureColumn(dataset, 'Subtheme', 'subtheme')
  ensureColumn(dataset, 'SetName', 'name')
  ensureColumn(dataset, 'Number', 'setId')
  ensureColumn(dataset, 'Year', 'year')
  ensureColumn(dataset, 'Pieces', 'pieces')
  ensureColumn(dataset, 'USPrice', 'price')
  dataset <- dataset[,c('theme','name','setId','year','pieces','price','subtheme')]
  dataset$pieces[is.na(dataset$pieces)] <- 0
  dataset$price[is.na(dataset$price)] <- 0
  dataset$subtheme[is.na(dataset$subtheme)] <- ''
  dataset
}

#' Filter dataset only by year
#' @param dataset the dataset.
#' @param minYear the minimum year. Defaults to be minimum year in the dataset.
#' @param maxYear the maximum year. Defaults to be maximum year in the dataset.
#' @return the filtered dataset with the filters applied.
#' @keywords lego
#' @export
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples
#' dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
#' filterByYear(dataset)
#' filterByYear(dataset, 2015)
#' filterByYear(dataset, maxYear=2016)
#'
filterByYear <- function(dataset, minYear=min(dataset$year), maxYear=max(dataset$year)) {
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
  dataset %>% filter(.data$year >= minYear, .data$year <= maxYear,
      .data$price >= minPrice, .data$price <= maxPrice,
      .data$pieces >= minPieces, .data$pieces <= maxPieces,
      .data$theme %in% themes, .data$subtheme %in% subthemes)
}

#' Group by year and summarise by set.
#' @param dataset the dataset.
#' @param minYear the minimum year. Defaults to be minimum year in the dataset.
#' @param maxYear the maximum year. Defaults to be maximum year in the dataset.
#' @param minPrice the minimum price count. Defaults to be minimum price in the dataset.
#' @param maxPrice the maximum price count. Defaults to be maximum price in the dataset.
#' @param minPieces the minimum piece count. Defaults to be minimum piece count in the dataset.
#' @param maxPieces the maximum piece count. Defaults to be maximum piece count in the dataset.
#' @param themes the themes to include in the filtered dataset. Defaults to be all themes.
#' @param subthemes the subthemes to include in the filtered dataset. Defaults to be all subthemes.
#' @return the filtered dataset with the filters applied.
#' @keywords lego
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr n_distinct
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples
#' dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
#' groupByYear(dataset)
#' groupByYear(dataset, themes=c('Star Wars'))
#' groupByYear(dataset, 2010, 2016, 5, 10, 100, 1000, c('Star Wars'), c('Episode I'))
#' groupByYear(dataset, 2014, 2015, themes=c('Star Wars'), subthemes=c('Episode I'))
#'
groupByYear <- function(dataset,
    minYear=min(dataset$year), maxYear=max(dataset$year),
    minPrice=min(dataset$price), maxPrice=max(dataset$price),
    minPieces=min(dataset$pieces), maxPieces=max(dataset$pieces),
    themes=sort(unique(dataset$theme)), subthemes=sort(unique(dataset$subtheme))) {
  filterByYearPricePieceTheme(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes) %>%
    group_by(.data$year) %>%
    summarise(count=n_distinct(.data$setId)) %>%
    arrange(.data$year)
}

#' Group by year to get total count of pieces.
#' @param dataset the dataset.
#' @param minYear the minimum year. Defaults to be minimum year in the dataset.
#' @param maxYear the maximum year. Defaults to be maximum year in the dataset.
#' @param minPrice the minimum price count. Defaults to be minimum price in the dataset.
#' @param maxPrice the maximum price count. Defaults to be maximum price in the dataset.
#' @param minPieces the minimum piece count. Defaults to be minimum piece count in the dataset.
#' @param maxPieces the maximum piece count. Defaults to be maximum piece count in the dataset.
#' @param themes the themes to include in the filtered dataset. Defaults to be all themes.
#' @param subthemes the subthemes to include in the filtered dataset. Defaults to be all subthemes.
#' @return data.table 2 columns
#' @keywords lego
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples
#' dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
#' groupByPiece(dataset)
#' groupByPiece(dataset, themes=c('Star Wars'))
#' groupByPiece(dataset, 2010, 2016, 5, 10, 100, 1000, c('Star Wars'), c('Episode I'))
#' groupByPiece(dataset, 2014, 2015, themes=c('Star Wars'), subthemes=c('Episode I'))
#'
groupByPiece <- function(dataset,
    minYear=min(dataset$year), maxYear=max(dataset$year),
    minPrice=min(dataset$price), maxPrice=max(dataset$price),
    minPieces=min(dataset$pieces), maxPieces=max(dataset$pieces),
    themes=sort(unique(dataset$theme)), subthemes=sort(unique(dataset$subtheme))) {
  filterByYearPricePieceTheme(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes) %>%
    group_by(.data$year) %>%
    summarise(count=sum(.data$pieces)) %>%
    arrange(.data$year)
}

#' Group by year to get total price.
#' @param dataset the dataset.
#' @param minYear the minimum year. Defaults to be minimum year in the dataset.
#' @param maxYear the maximum year. Defaults to be maximum year in the dataset.
#' @param minPrice the minimum price count. Defaults to be minimum price in the dataset.
#' @param maxPrice the maximum price count. Defaults to be maximum price in the dataset.
#' @param minPieces the minimum piece count. Defaults to be minimum piece count in the dataset.
#' @param maxPieces the maximum piece count. Defaults to be maximum piece count in the dataset.
#' @param themes the themes to include in the filtered dataset. Defaults to be all themes.
#' @param subthemes the subthemes to include in the filtered dataset. Defaults to be all subthemes.
#' @return data.table 2 columns
#' @keywords lego
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples
#' dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
#' groupByPrice(dataset)
#' groupByPrice(dataset, themes=c('Star Wars'))
#' groupByPrice(dataset, 2010, 2016, 5, 10, 100, 1000, c('Star Wars'), c('Episode I'))
#' groupByPrice(dataset, 2014, 2015, themes=c('Star Wars'), subthemes=c('Episode I'))
#'
groupByPrice <- function(dataset,
    minYear=min(dataset$year), maxYear=max(dataset$year),
    minPrice=min(dataset$price), maxPrice=max(dataset$price),
    minPieces=min(dataset$pieces), maxPieces=max(dataset$pieces),
    themes=sort(unique(dataset$theme)), subthemes=sort(unique(dataset$subtheme))) {
  filterByYearPricePieceTheme(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes) %>%
    group_by(.data$year) %>%
    summarise(count=sum(.data$price)) %>%
    arrange(.data$year)
}

#' Group by year to get the number of themes.
#' @param dataset the dataset.
#' @param minYear the minimum year. Defaults to be minimum year in the dataset.
#' @param maxYear the maximum year. Defaults to be maximum year in the dataset.
#' @param minPrice the minimum price count. Defaults to be minimum price in the dataset.
#' @param maxPrice the maximum price count. Defaults to be maximum price in the dataset.
#' @param minPieces the minimum piece count. Defaults to be minimum piece count in the dataset.
#' @param maxPieces the maximum piece count. Defaults to be maximum piece count in the dataset.
#' @param themes the themes to include in the filtered dataset. Defaults to be all themes.
#' @param subthemes the subthemes to include in the filtered dataset. Defaults to be all subthemes.
#' @return the filtered dataset with the filters applied.
#' @keywords lego
#' @export
#' @importFrom DT datatable
#' @examples
#' dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
#' groupByTheme(dataset)
#' groupByTheme(dataset, themes=c('Star Wars'))
#' groupByTheme(dataset, 2010, 2016, 5, 10, 100, 1000, c('Star Wars'), c('Episode I'))
#' groupByTheme(dataset, 2014, 2015, themes=c('Star Wars'), subthemes=c('Episode I'))
#'
groupByTheme <- function(dataset,
    minYear=min(dataset$year), maxYear=max(dataset$year),
    minPrice=min(dataset$price), maxPrice=max(dataset$price),
    minPieces=min(dataset$pieces), maxPieces=max(dataset$pieces),
    themes=sort(unique(dataset$theme)), subthemes=sort(unique(dataset$subtheme))) {
  filterByYearPricePieceTheme(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes) %>%
    group_by(.data$year) %>%
    summarise(count=n_distinct(.data$theme)) %>%
    arrange(.data$year)
}

#' Aggregate dataset by year to get total count of average number of pieces.
#' @param dataset the dataset.
#' @param minYear the minimum year. Defaults to be minimum year in the dataset.
#' @param maxYear the maximum year. Defaults to be maximum year in the dataset.
#' @param minPrice the minimum price count. Defaults to be minimum price in the dataset.
#' @param maxPrice the maximum price count. Defaults to be maximum price in the dataset.
#' @param minPieces the minimum piece count. Defaults to be minimum piece count in the dataset.
#' @param maxPieces the maximum piece count. Defaults to be maximum piece count in the dataset.
#' @param themes the themes to include in the filtered dataset. Defaults to be all themes.
#' @param subthemes the subthemes to include in the filtered dataset. Defaults to be all subthemes.
#' @return data.table 2 columns
#' @keywords lego
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples
#' dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
#' groupByPieceAvg(dataset)
#' groupByPieceAvg(dataset, themes=c('Star Wars'))
#' groupByPieceAvg(dataset, 2010, 2016, 5, 10, 100, 1000, c('Star Wars'), c('Episode I'))
#' groupByPieceAvg(dataset, 2014, 2015, themes=c('Star Wars'), subthemes=c('Episode I'))
#'
groupByPieceAvg <- function(dataset,
    minYear=min(dataset$year), maxYear=max(dataset$year),
    minPrice=min(dataset$price), maxPrice=max(dataset$price),
    minPieces=min(dataset$pieces), maxPieces=max(dataset$pieces),
    themes=sort(unique(dataset$theme)), subthemes=sort(unique(dataset$subtheme))) {
  filterByYearPricePieceTheme(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes) %>%
    group_by(.data$year) %>%
    summarise(avg=round(mean(.data$pieces), 2)) %>%
    arrange(.data$year)
}

#' Average pieces for each theme.
#' @param dataset the dataset.
#' @param minYear the minimum year. Defaults to be minimum year in the dataset.
#' @param maxYear the maximum year. Defaults to be maximum year in the dataset.
#' @param minPrice the minimum price count. Defaults to be minimum price in the dataset.
#' @param maxPrice the maximum price count. Defaults to be maximum price in the dataset.
#' @param minPieces the minimum piece count. Defaults to be minimum piece count in the dataset.
#' @param maxPieces the maximum piece count. Defaults to be maximum piece count in the dataset.
#' @param themes the themes to include in the filtered dataset. Defaults to be all themes.
#' @param subthemes the subthemes to include in the filtered dataset. Defaults to be all subthemes.
#' @return data.table 2 columns
#' @keywords lego
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples
#' dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
#' groupByPieceThemeAvg(dataset)
#' groupByPieceThemeAvg(dataset, themes=c('Star Wars'))
#' groupByPieceThemeAvg(dataset, 2010, 2016, 5, 10, 100, 1000, c('Star Wars'), c('Episode I'))
#' groupByPieceThemeAvg(dataset, 2014, 2015, themes=c('Star Wars'), subthemes=c('Episode I'))
#'
groupByPieceThemeAvg <- function(dataset,
    minYear=min(dataset$year), maxYear=max(dataset$year),
    minPrice=min(dataset$price), maxPrice=max(dataset$price),
    minPieces=min(dataset$pieces), maxPieces=max(dataset$pieces),
    themes=sort(unique(dataset$theme)), subthemes=sort(unique(dataset$subtheme))) {
  filterByYearPricePieceTheme(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes) %>%
    group_by(.data$theme) %>%
    summarise(avgPieces=round(mean(.data$pieces), 2)) %>%
    arrange(.data$theme)
}

#' Group By wrapper function to call each of the subfunctions simply.
#' @param dataset the dataset.
#' @param type the type to group by - can be one of 'year', 'piece', 'price', 'theme', 'piece_avg', 'piece_theme_avg'.
#' @param minYear the minimum year. Defaults to be minimum year in the dataset.
#' @param maxYear the maximum year. Defaults to be maximum year in the dataset.
#' @param minPrice the minimum price count. Defaults to be minimum price in the dataset.
#' @param maxPrice the maximum price count. Defaults to be maximum price in the dataset.
#' @param minPieces the minimum piece count. Defaults to be minimum piece count in the dataset.
#' @param maxPieces the maximum piece count. Defaults to be maximum piece count in the dataset.
#' @param themes the themes to include in the filtered dataset. Defaults to be all themes.
#' @param subthemes the subthemes to include in the filtered dataset. Defaults to be all subthemes.
#' @return data.table 2 columns
#' @keywords lego
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples
#' dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
#' groupBy(dataset, 'theme')
#' groupBy(dataset, 'piece', themes=c('Star Wars'))
#' groupBy(dataset, 'year', 2010, 2016, 5, 100, 1, 1000, c('Star Wars'), c('Episode I'))
#' groupBy(dataset, 'price', 2010, 2016, themes=c('Star Wars'), subthemes=c('Episode I'))
#'
groupBy <- function(dataset, type,
    minYear=min(dataset$year), maxYear=max(dataset$year),
    minPrice=min(dataset$price), maxPrice=max(dataset$price),
    minPieces=min(dataset$pieces), maxPieces=max(dataset$pieces),
    themes=sort(unique(dataset$theme)), subthemes=sort(unique(dataset$subtheme))) {
  # switch based on type to call the appropriate function.
  switch(type,
      year=groupByYear(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes),
      piece=groupByPiece(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes),
      price=groupByPrice(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes),
      theme=groupByTheme(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes),
      piece_avg=groupByPieceAvg(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes),
      piece_theme_avg=groupByPieceThemeAvg(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes),
      groupByYear(dataset, minYear, maxYear, minPrice, maxPrice, minPieces, maxPieces, themes, subthemes))
}

#' Plot number of sets by year
#' @param dataset the dataset.
#' @param dom the dom.
#' @param xAxisLabel the x axis label - the year.
#' @param yAxisLabel the y axis label - the number of sets.
#' @return the setsByYear plot.
#' @keywords lego
#' @export
#' @importFrom rCharts nPlot
plotSetsCountByYear <- function(dataset, dom="setsByYear",
    xAxisLabel="Year", yAxisLabel="Number of Sets") {
  tooltipContent <- "#! function(key, x, y, e) {
   return '<h5><b>Year</b>: ' + e.point.year + '<br>' + '<b>Total Sets</b>: '
  + e.point.count + '<br>' + '</h5>' } !#"
  setsByYear <- nPlot(count ~ year, data=dataset, type="stackedAreaChart",
    dom=dom, width=600)
  setsByYear$chart(margin=list(left=100))
  setsByYear$chart(color=c('purple', 'blue', 'green'))
  setsByYear$chart(tooltipContent=tooltipContent)
  setsByYear$yAxis(axisLabel=yAxisLabel, width=80)
  setsByYear$xAxis(axisLabel=xAxisLabel, width=70)
  setsByYear
}

#' Plot number of pieces by year.
#' @param dataset the dataset.
#' @param dom the dom.
#' @param xAxisLabel the x axis label - the year.
#' @param yAxisLabel the y axis label - the number of pieces.
#' @return the piecesCountByYear plot.
#' @keywords lego
#' @export
#' @importFrom rCharts nPlot
plotPiecesCountByYear <- function(dataset, dom="piecesCountByYear",
    xAxisLabel="Year", yAxisLabel="Number of Pieces") {
  tooltipContent <- "#! function(key, x, y, e) {
  return '<h5><b>Year</b>: ' + e.point.year + '<br>' + '<b>Total Pieces</b>: ' + e.point.count + '<br>'
  + '</h5>' } !#"
  piecesCountByYear <- nPlot(count ~ year, data=dataset, type="multiBarChart", dom=dom, width=600)
  piecesCountByYear$chart(margin=list(left=100))
  piecesCountByYear$yAxis(axisLabel=yAxisLabel, width=80)
  piecesCountByYear$xAxis(axisLabel=xAxisLabel, width=70)
  piecesCountByYear$chart(tooltipContent=tooltipContent)
  piecesCountByYear
}

#' Plot price by year.
#' @param dataset the dataset.
#' @param dom the dom.
#' @param xAxisLabel the x axis label - the year.
#' @param yAxisLabel the y axis label - the price.
#' @return the priceCountByYear plot.
#' @keywords lego
#' @export
#' @importFrom rCharts nPlot
plotPriceCountByYear <- function(dataset, dom="priceCountByYear",
    xAxisLabel="Year", yAxisLabel="Price") {
  tooltipContent <- "#! function(key, x, y, e) {
  return '<h5><b>Year</b>: ' + e.point.year + '<br>' + '<b>Price</b>: ' + e.point.count + '<br>'
  + '</h5>' } !#"
  priceCountByYear <- nPlot(count ~ year, data=dataset, type="multiBarChart", dom=dom, width=600)
  priceCountByYear$chart(margin=list(left=100))
  priceCountByYear$yAxis(axisLabel=yAxisLabel, width=80)
  priceCountByYear$xAxis(axisLabel=xAxisLabel, width=70)
  priceCountByYear$chart(tooltipContent=tooltipContent)
  priceCountByYear
}

#' Plot number of themes by year.
#' @param dataset the dataset.
#' @param dom the dom.
#' @param xAxisLabel the x axis label - the year.
#' @param yAxisLabel the y axis label - number of themes.
#' @return the themesByYear plot.
#' @keywords lego
#' @export
#' @importFrom rCharts nPlot
plotThemesCountByYear <- function(dataset, dom="themesByYear",
    xAxisLabel="Year", yAxisLabel="Number of Themes") {
  tooltipContent <- "#! function(key, x, y, e) {
    return '<h5><b>Year</b>: ' + e.point.year + '<br>' + '<b>Total Themes</b>: ' + e.point.count + '<br>'
  + '</h5>' } !#"
  themesByYear <- nPlot(count ~ year, data=dataset, type="multiBarChart",
    dom=dom, width=600)
  themesByYear$chart(margin=list(left=100))
  themesByYear$yAxis(axisLabel=yAxisLabel, width=80)
  themesByYear$xAxis(axisLabel=xAxisLabel, width=70)
  themesByYear$chart(tooltipContent=tooltipContent)
  themesByYear
}

#' Plot number of pieces by year.
#' @param dataset the dataset.
#' @param dom the dom.
#' @param xAxisLabel the x axis label - the year.
#' @param yAxisLabel the y axis label - number of pieces.
#' @return the piecesByYear plot.
#' @keywords lego
#' @export
#' @importFrom rCharts nPlot
plotPiecesByYear <- function(dataset, dom="piecesByYear",
    xAxisLabel="Year", yAxisLabel="Number of Pieces") {
  tooltipContent <- "#! function(key, x, y, e) {
    return '<h5><b>Set Name</b>: ' + e.point.name + '<br>'
        + '<b>Set ID</b>: ' + e.point.setId + '<br>' + '<b>Pieces</b>: ' + e.point.pieces + '</h5>' } !#"
  piecesByYear <- nPlot(pieces ~ year, data=dataset, type="scatterChart",
    dom=dom, width=600)
  piecesByYear$chart(margin=list(left=100), showDistX=TRUE, showDistY=TRUE)
  piecesByYear$chart(color=c('green', 'orange', 'blue'))
  piecesByYear$chart(tooltipContent=tooltipContent)
  piecesByYear$yAxis(axisLabel=yAxisLabel, width=80)
  piecesByYear$xAxis(axisLabel=xAxisLabel, width=70)
  #     piecesByYear$chart(useInteractiveGuideline=TRUE)
  piecesByYear
}

#' Plot price by year.
#' @param dataset the dataset.
#' @param dom the dom.
#' @param xAxisLabel the x axis label - the year.
#' @param yAxisLabel the y axis label - the price.
#' @return the priceByYear plot.
#' @keywords lego
#' @export
#' @importFrom rCharts nPlot
plotPriceByYear <- function(dataset, dom="priceByYear",
    xAxisLabel="Year", yAxisLabel="Price") {
  tooltipContent <- "#! function(key, x, y, e) {
  return '<h5><b>Set Name</b>: ' + e.point.name + '<br>'
  + '<b>Set ID</b>: ' + e.point.setId + '<br>' + '<b>Price</b>: ' + e.point.price + '</h5>' } !#"
  priceByYear <- nPlot(price ~ year, data=dataset, type="scatterChart",
    dom=dom, width=600)
  priceByYear$chart(margin=list(left=100), showDistX=TRUE, showDistY=TRUE)
  priceByYear$chart(color=c('green', 'orange', 'blue'))
  priceByYear$chart(tooltipContent=tooltipContent)
  priceByYear$yAxis(axisLabel=yAxisLabel, width=80)
  priceByYear$xAxis(axisLabel=xAxisLabel, width=70)
  priceByYear
}

#' Plot the average number of pieces by year.
#' @param dataset the dataset.
#' @param dom the dom.
#' @param xAxisLabel the x axis label - the year.
#' @param yAxisLabel the y axis label - number of pieces.
#' @return the themesByYear plot.
#' @keywords lego
#' @export
#' @importFrom rCharts nPlot
plotPiecesByYearAvg <- function(dataset, dom="piecesByYearAvg",
    xAxisLabel="Year", yAxisLabel="Number of Pieces") {
  piecesByYearAvg <- nPlot(avg ~ year, data=dataset, type="lineChart", dom=dom, width=600)
  piecesByYearAvg$chart(margin=list(left=100))
  piecesByYearAvg$chart(color=c('orange', 'blue', 'green'))
  piecesByYearAvg$yAxis(axisLabel=yAxisLabel, width=80)
  piecesByYearAvg$xAxis(axisLabel=xAxisLabel, width=70)
  piecesByYearAvg
}

#' Plot number of average pieces by theme
#' @param dataset the dataset.
#' @param dom the dom.
#' @param xAxisLabel the x axis label - the themes.
#' @param yAxisLabel the y axis label - number of pieces.
#' @return the piecesByThemeAvg plot.
#' @keywords lego
#' @export
#' @importFrom rCharts nPlot
plotPiecesByThemeAvg <- function(dataset, dom="piecesByThemeAvg",
    xAxisLabel="Themes", yAxisLabel="Number of Pieces") {
  piecesByThemeAvg <- nPlot(avgPieces ~ theme, data=dataset, type="multiBarChart",
    dom=dom, width=600)
  piecesByThemeAvg$chart(margin=list(left=100))
  piecesByThemeAvg$chart(color=c('pink', 'blue', 'green'))
  piecesByThemeAvg$yAxis(axisLabel=yAxisLabel, width=80)
  piecesByThemeAvg$xAxis(axisLabel=xAxisLabel, width=200, rotateLabels=-20, height=200)
  piecesByThemeAvg
}

#' Run the shiny application.
#' @keywords lego
#' @export
#' @importFrom shiny runApp
runShinyLego <- function() {
  shiny::runApp(system.file('shiny', package='lego'))
}

#' Ggplot count by group
#' @param dataset the dataset.
#' @param title the title.
#' @param xAxisLabel the x axis label - defaults to the year.
#' @param yAxisLabel the y axis label - the count.
#' @param caption the caption.
#' @param x - defaults to the year.
#' @param y - defaults to the count.
#' @return the count by year plot.
#' @keywords lego
#' @export
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
ggplotCountByGroup <- function(dataset, title="Number per Year",
    xAxisLabel="Year", yAxisLabel="Number",
    caption="Source: Darren Redmond's owned dataset",
    x=dataset$year, y=dataset$count) {
  g <- ggplot(dataset, aes(x, y))
  g + geom_bar(stat="identity", width=0.5, fill="tomato2") +
    labs(title=title, caption=caption) +
    xlab(xAxisLabel) +
    ylab(yAxisLabel) +
    theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
    scale_x_continuous(xAxisLabel, labels=as.character(x), breaks=x)
}

#' Ggplot count by group with a discrete set of values on the x axis.
#' @param dataset the dataset.
#' @param title the title.
#' @param xAxisLabel the x axis label - defaults to the year.
#' @param yAxisLabel the y axis label - the count.
#' @param caption the caption.
#' @param x - defaults to the year.
#' @param y - defaults to the count.
#' @return the count by year plot.
#' @keywords lego
#' @export
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
ggplotCountByGroupDiscrete <- function(dataset, title="Number per Year",
    xAxisLabel="Year", yAxisLabel="Number",
    caption="Source: Darren Redmond's owned dataset",
    x=dataset$year, y=dataset$count) {
  g <- ggplot(dataset, aes(x, y))
  g + geom_bar(stat="identity", width=0.5, fill="tomato2") +
    labs(title=title, caption=caption) +
    xlab(xAxisLabel) +
    ylab(yAxisLabel) +
    theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
    scale_x_discrete(xAxisLabel, labels=as.character(x), breaks=x)
}

#' Ggplot number of sets by year
#' @param dataset the dataset.
#' @param title the title.
#' @param xAxisLabel the x axis label - the year.
#' @param yAxisLabel the y axis label - the number of sets.
#' @param caption the caption.
#' @return the setsByYear plot.
#' @keywords lego
#' @export
#' @examples
#' dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
#' ggplotSetsCountByYear(groupByYear(dataset))
#' ggplotSetsCountByYear(groupByYear(dataset, themes=c('Star Wars')))
#' ggplotSetsCountByYear(groupByYear(dataset, 2010, 2016, 5, 10, 100, 1000,
#'     c('Star Wars'), c('Episode I')))
#' ggplotSetsCountByYear(groupByYear(dataset, 2014, 2015,
#'     themes=c('Star Wars'), subthemes=c('Episode I')))
ggplotSetsCountByYear <- function(dataset, title="Number of Sets per Year",
    xAxisLabel="Year", yAxisLabel="Number of Sets",
    caption="Source: Darren Redmond's owned dataset") {
  ggplotCountByGroup(dataset, title, xAxisLabel, yAxisLabel, caption)
}

#' Ggplot number of pieces by year
#' @param dataset the dataset.
#' @param title the title.
#' @param xAxisLabel the x axis label - the year.
#' @param yAxisLabel the y axis label - the number of pieces.
#' @param caption the caption.
#' @return the setsByPiecesCount plot.
#' @keywords lego
#' @export
#' @examples
#' dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
#' ggplotPiecesCountByYear(groupByPiece(dataset))
#' ggplotPiecesCountByYear(groupByPiece(dataset, themes=c('Star Wars')))
#' ggplotPiecesCountByYear(groupByPiece(dataset, 2010, 2016, 5, 10, 100, 1000,
#'     c('Star Wars'), c('Episode I')))
#' ggplotPiecesCountByYear(groupByPiece(dataset, 2014, 2015,
#'     themes=c('Star Wars'), subthemes=c('Episode I')))
ggplotPiecesCountByYear <- function(dataset, title="Number of Pieces per Year",
    xAxisLabel="Year", yAxisLabel="Number of Pieces",
    caption="Source: Darren Redmond's owned dataset") {
  ggplotCountByGroup(dataset, title, xAxisLabel, yAxisLabel, caption)
}

#' Ggplot price by year
#' @param dataset the dataset.
#' @param title the title.
#' @param xAxisLabel the x axis label - the year.
#' @param yAxisLabel the y axis label - the price.
#' @param caption the caption.
#' @return the setsByPrice plot.
#' @keywords lego
#' @export
#' @examples
#' dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
#' ggplotPriceByYear(groupByPrice(dataset))
#' ggplotPriceByYear(groupByPrice(dataset, themes=c('Star Wars')))
#' ggplotPriceByYear(groupByPrice(dataset, 2010, 2016, 5, 10, 100, 1000,
#'     c('Star Wars'), c('Episode I')))
#' ggplotPriceByYear(groupByPrice(dataset, 2014, 2015,
#'     themes=c('Star Wars'), subthemes=c('Episode I')))
ggplotPriceByYear <- function(dataset, title="Price per Year",
    xAxisLabel="Year", yAxisLabel="Price",
    caption="Source: Darren Redmond's owned dataset") {
  ggplotCountByGroup(dataset, title, xAxisLabel, yAxisLabel, caption)
}

#' Ggplot number of themes by year
#' @param dataset the dataset.
#' @param title the title.
#' @param xAxisLabel the x axis label - the year.
#' @param yAxisLabel the y axis label - the price.
#' @param caption the caption.
#' @return the themesByYear plot.
#' @keywords lego
#' @export
#' @examples
#' dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
#' ggplotThemesCountByYear(groupByTheme(dataset))
#' ggplotThemesCountByYear(groupByTheme(dataset, themes=c('Star Wars')))
#' ggplotThemesCountByYear(groupByTheme(dataset, 2010, 2016, 5, 10, 100, 1000,
#'     c('Star Wars'), c('Episode I')))
#' ggplotThemesCountByYear(groupByTheme(dataset, 2014, 2015,
#'     themes=c('Star Wars'), subthemes=c('Episode I')))
ggplotThemesCountByYear <- function(dataset, title="Number of Themes per Year",
    xAxisLabel="Year", yAxisLabel="Number of Themes",
    caption="Source: Darren Redmond's owned dataset") {
  ggplotCountByGroup(dataset, title, xAxisLabel, yAxisLabel, caption)
}

#' Ggplot average number of pieces by themes
#' @param dataset the dataset.
#' @param title the title.
#' @param xAxisLabel the x axis label - the theme.
#' @param yAxisLabel the y axis label - the average number of pieces.
#' @param caption the caption.
#' @return the piecesByTheme plot.
#' @keywords lego
#' @export
#' @examples
#' dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
#' ggplotPieceByTheme(groupByPieceThemeAvg(dataset))
#' ggplotPieceByTheme(groupByPieceThemeAvg(dataset, themes=c('Star Wars')))
#' ggplotPieceByTheme(groupByPieceThemeAvg(dataset, 2010, 2016, 5, 100, 100, 1000,
#'     c('Star Wars'), c('Episode I')))
#' ggplotPieceByTheme(groupByPieceThemeAvg(dataset, 2014, 2015,
#'     themes=c('Star Wars'), subthemes=c('Episode I')))
ggplotPieceByTheme <- function(dataset, title="Avg Num of Pieces by Theme",
    xAxisLabel="Theme", yAxisLabel="Avg Num of Pieces",
    caption="Source: Darren Redmond's owned dataset") {
  ggplotCountByGroupDiscrete(dataset, title, xAxisLabel, yAxisLabel, caption, x=dataset$theme, y=dataset$avgPieces)
}

#' ggplot - wrapper for all of the ggplot functionality.
#' @param dataset the dataset.
#' @param type the type of ggplot to create - can be one of 'year', 'piece', 'price', 'theme', 'piece_theme_avg'.
#' @return the plot.
#' @keywords lego
#' @export
#' @examples
#' dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
#' ggplotLego(dataset, 'year')
#' ggplotLego(dataset, 'piece')
#' ggplotLego(dataset, 'price')
#' ggplotLego(dataset, 'theme')
#' ggplotLego(dataset, 'piece_theme_avg')
ggplotLego <- function(dataset, type) {
  if (type == 'year') {
    plot <- ggplotSetsCountByYear(groupBy(dataset, type))
  } else if (type == 'piece') {
    plot <- ggplotPiecesCountByYear(groupBy(dataset, type))
  } else if (type == 'price') {
    plot <- ggplotPriceByYear(groupBy(dataset, type))
  } else if (type == 'theme') {
    plot <- ggplotThemesCountByYear(groupBy(dataset, type))
  } else if (type == 'piece_theme_avg') {
    plot <- ggplotPieceByTheme(groupBy(dataset, type))
  } else {
    plot <- ggplotSetsCountByYear(groupBy(dataset))
  }
  plot
}
