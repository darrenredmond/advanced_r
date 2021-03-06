% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{plotPiecesByThemeAvg}
\alias{plotPiecesByThemeAvg}
\title{Plot number of average pieces by theme}
\usage{
plotPiecesByThemeAvg(dataset, dom = "piecesByThemeAvg",
  xAxisLabel = "Themes", yAxisLabel = "Number of Pieces")
}
\arguments{
\item{dataset}{the dataset.}

\item{dom}{the dom.}

\item{xAxisLabel}{the x axis label - the themes.}

\item{yAxisLabel}{the y axis label - number of pieces.}
}
\value{
the piecesByThemeAvg plot.
}
\description{
Plot number of average pieces by theme
}
\examples{
dataset <- read.lego(system.file('extdata', 'brickset-mysets-owned.csv', package ='lego'))
plotPiecesByThemeAvg(groupByPieceThemeAvg(dataset))
}
\keyword{lego}
