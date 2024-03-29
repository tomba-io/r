% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tomba.R
\name{email_sources}
\alias{email_sources}
\alias{email_sources,Tomba-method}
\title{Email Sources}
\usage{
email_sources(obj, email)

\S4method{email_sources}{Tomba}(obj, email)
}
\arguments{
\item{obj}{Tomba class}

\item{email}{a character email address you want to find sources.}
}
\value{
A list sources data data containing the: URL, URL extracted on and last seen on, check if the URL still on page (TRUE|FALSE).
}
\description{
\code{email_sources} Find email address source somewhere on the web.
}
\examples{
\dontrun{
client <- Tomba(key="ta_xxxx",secret="ts_xxxx")
result <- email_sources(obj,email="info@tomba.io")
}
}
\references{
\url{https://developer.tomba.io/#email-sources}
}
