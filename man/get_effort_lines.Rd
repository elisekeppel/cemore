% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{get_effort_lines}
\alias{get_effort_lines}
\title{get_effort_lines}
\usage{
get_effort_lines(effort)
}
\arguments{
\item{effort}{}
}
\description{
get_effort_lines
}
\examples{
dir <- file.path("OUTPUT FILES","dataEffort table")
effort <- read.delim(file.path(dir, paste0("dataEffortcemore_", year, "-",month, ".txt")))
effort_lines <- get_effort_lines(effort)
}
