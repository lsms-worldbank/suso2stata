% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{replace_null}
\alias{replace_null}
\title{Replace SuSo \code{null} with Stata \code{.}}
\usage{
replace_null(suso_expr)
}
\arguments{
\item{suso_expr}{Character vector. SuSo enablement or validation condition.}
}
\value{
Character vector. If pattern found, modified expression.
Otherwise, unmodified expression.
}
\description{
Replace SuSo \code{null} with Stata \code{.}
}
