% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tomba.R
\name{autocomplete}
\alias{autocomplete}
\alias{autocomplete,Tomba-method}
\title{Company Autocomplete}
\usage{
autocomplete(obj, search)

\S4method{autocomplete}{Tomba}(obj, search)
}
\arguments{
\item{obj}{Tomba class}

\item{search}{a character name company or website.}
}
\value{
A list autocomplete data containing the: Total email on company,company website and name and logo.
}
\description{
\code{autocomplete} Company Autocomplete is an API that lets you auto-complete company names and retreive logo and domain information.
}
\examples{
\dontrun{
client <- Tomba(key="ta_xxxx",secret="ts_xxxx")
result <- autocomplete(obj,search="google")
}
}
\references{
\url{https://developer.tomba.io/#autocomplete}
}
