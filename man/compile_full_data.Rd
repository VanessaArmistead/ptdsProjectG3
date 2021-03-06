% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/webscrape.R
\name{compile_full_data}
\alias{compile_full_data}
\title{Compile Complete Wine Data}
\usage{
compile_full_data(all.countries)
}
\arguments{
\item{country}{\code{character}, the name of the country}
}
\value{
A \code{dataframe} containing the following variables:
\describe{
     \item{title}{Title of the wine}
     \item{quality}{Quality of the wine}
     \item{price}{Price of the wine}
     \item{country}{Country of origin}
}
}
\description{
This function creates a dataframe, containing the qualities,
prices, titles and countries of the wines retrieved from Winemag.com,
for multiple countries
}
