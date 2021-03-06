% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/REQAO.R
\name{StudentLoad}
\alias{StudentLoad}
\title{df <- StudentLoad(3, YYYY, bident, datadir)}
\usage{
StudentLoad(grade, year, bident, datadir)
}
\description{
Use:   df <- StudentLoad(3, 2017, bident, datadir)
grades = 3, 6, 9, 10
}
\details{
- identify the grade, year and board id: used to construct EQAO file names
- for the DDSB use v and c to access network and c data directories
  otherwise pass a character string or vector with the directory containing the EQAO files
}
