% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{plotPriceByYear}
\alias{plotPriceByYear}
\title{Plot price by year for each individual set.}
\usage{
plotPriceByYear(dataset, dom = "priceByYear", xAxisLabel = "Year",
  yAxisLabel = "Price")
}
\arguments{
\item{dataset}{the dataset.}

\item{dom}{the dom.}

\item{xAxisLabel}{the x axis label - the year.}

\item{yAxisLabel}{the y axis label - the price.}
}
\value{
the priceByYear plot.
}
\description{
Plot price by year for each individual set.
}
\examples{
dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
plotPriceByYear(filterByYearPricePieceTheme(dataset))
}
\keyword{lego}
