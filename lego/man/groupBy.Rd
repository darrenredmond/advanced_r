% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{groupBy}
\alias{groupBy}
\title{Group By wrapper function to call each of the subfunctions simply.}
\usage{
groupBy(dataset, type, minYear = min(dataset$year),
  maxYear = max(dataset$year), minPrice = min(dataset$price),
  maxPrice = max(dataset$price), minPieces = min(dataset$pieces),
  maxPieces = max(dataset$pieces),
  themes = sort(unique(dataset$theme)),
  subthemes = sort(unique(dataset$subtheme)))
}
\arguments{
\item{dataset}{the dataset.}

\item{type}{the type to group by - can be one of year, piece, price, theme.}

\item{minYear}{the minimum year.}

\item{maxYear}{the maximum year.}

\item{minPrice}{the minimum price count.}

\item{maxPrice}{the maximum price count.}

\item{minPieces}{the minimum piece count.}

\item{maxPieces}{the maximum piece count.}

\item{themes}{the themes to include in the filtered dataset.}

\item{subthemes}{the subthemes to include in the filtered dataset.}
}
\value{
data.table 2 columns
}
\description{
Group By wrapper function to call each of the subfunctions simply.
}
\examples{
dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
groupBy(dataset, 'theme')
groupBy(dataset, 'piece', themes=c('Star Wars'))
groupBy(dataset, 'year', 2010, 2016, 5, 100, 1, 1000, c('Star Wars'), c('Episode I'))
groupBy(dataset, 'price', 2010, 2016, themes=c('Star Wars'), subthemes=c('Episode I'))

}
\keyword{lego}