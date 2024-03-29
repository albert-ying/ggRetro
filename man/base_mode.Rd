% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{base_mode}
\alias{base_mode}
\title{Mimic Base R break}
\usage{
base_mode(
  p,
  i = 1,
  smart_label = T,
  x_lab_fun = function(x) {     x },
  y_lab_fun = function(x) {     x },
  n_wrap = 10,
  flip = F,
  ...
)
}
\arguments{
\item{p}{string with which to prefix names of \code{ggplot2} functions in order to name the pipe-enabled functions. default: "add_".
Note: You could set this the empty string, in which case the new functions would mask the name of the library function}

\item{i}{Regular expression to filter the list of ggplot functions to make pipe-enabled.  The default regex will capture all}

\item{smart_label}{Regular expression to filter the list of ggplot functions to make pipe-enabled.  The default regex will capture all
stats and geoms and some other misc stuff.}
}
\description{
Mimic Base R break
}
