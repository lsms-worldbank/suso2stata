% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{replace_self}
\alias{replace_self}
\title{Replace \code{self} keyword with variable name}
\usage{
replace_self(suso_expr, varname)
}
\arguments{
\item{suso_expr}{Character vector. SuSo enablement or validation condition.}

\item{varname}{Character vector. Variable name to replace \code{self}}
}
\value{
Character vector. If pattern found, modified expression.
Otherwise, unmodified expression.
}
\description{
Replace \code{self} keyword with variable name
}
