% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{base_facet}
\alias{base_facet}
\title{Mimic Base R break}
\usage{
base_facet(
  p,
  facets,
  scales = "free",
  label_format_number = "{var.name} = {var.value}",
  label_format_string = "{var.value}",
  label_column = NA,
  smart_label = T,
  guides = "collect",
  nrow = "auto",
  ncol = "auto",
  x_lab_fun = "auto",
  y_lab_fun = "auto",
  after_dat = NA,
  after_fun = NA,
  ...
)
}
\arguments{
\item{p}{plot}

\item{facets}{a vector of column names used for facet}
}
\description{
Mimic Base R break
}
