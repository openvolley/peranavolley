% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/psvt.R
\name{pt_write}
\alias{pt_write}
\title{Write a Perana Sports tagger (psvt) data file}
\usage{
pt_write(x, filename)
}
\arguments{
\item{x}{character: data to write. See e.g the \code{raw} component of the object returned by \code{\link{pt_read}}}

\item{filename}{string: path to file}
}
\description{
This is somewhat experimental. It may be useful if one wants to read an existing file, modify the content, and re-write it back to a psvt file.
}
\seealso{
\code{\link{pt_read}}
}
