% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{ggplotCountByGroup}
\alias{ggplotCountByGroup}
\title{Ggplot count by group}
\usage{
ggplotCountByGroup(dataset, title = "Number per Year",
  xAxisLabel = "Year", yAxisLabel = "Number",
  caption = "Source: Darren Redmond's owned dataset", x = dataset$year,
  y = dataset$count)
}
\arguments{
\item{dataset}{the dataset.}

\item{title}{the title.}

\item{xAxisLabel}{the x axis label - defaults to the year.}

\item{yAxisLabel}{the y axis label - the count.}

\item{caption}{the caption.}

\item{x}{- defaults to the year.}

\item{y}{- defaults to the count.}
}
\value{
the count by year plot.
}
\description{
Ggplot count by group
}
\keyword{lego}
