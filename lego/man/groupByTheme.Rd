% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{groupByTheme}
\alias{groupByTheme}
\title{Group by year to get the number of themes.}
\usage{
groupByTheme(dataset, minYear = min(dataset$year),
  maxYear = max(dataset$year), minPrice = min(dataset$price),
  maxPrice = max(dataset$price), minPieces = min(dataset$pieces),
  maxPieces = max(dataset$pieces),
  themes = sort(unique(dataset$theme)),
  subthemes = sort(unique(dataset$subtheme)))
}
\arguments{
\item{dataset}{the dataset.}

\item{minYear}{the minimum year. Defaults to be minimum year in the dataset.}

\item{maxYear}{the maximum year. Defaults to be maximum year in the dataset.}

\item{minPrice}{the minimum price count. Defaults to be minimum price in the dataset.}

\item{maxPrice}{the maximum price count. Defaults to be maximum price in the dataset.}

\item{minPieces}{the minimum piece count. Defaults to be minimum piece count in the dataset.}

\item{maxPieces}{the maximum piece count. Defaults to be maximum piece count in the dataset.}

\item{themes}{the themes to include in the filtered dataset. Defaults to be all themes.}

\item{subthemes}{the subthemes to include in the filtered dataset. Defaults to be all subthemes.}
}
\value{
the filtered dataset with the filters applied.
}
\description{
Group by year to get the number of themes.
}
\examples{
dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
groupByTheme(dataset)
groupByTheme(dataset, themes=c('Star Wars'))
groupByTheme(dataset, 2010, 2016, 5, 10, 100, 1000, c('Star Wars'), c('Episode I'))
groupByTheme(dataset, 2014, 2015, themes=c('Star Wars'), subthemes=c('Episode I'))

}
\keyword{lego}
