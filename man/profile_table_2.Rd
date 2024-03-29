% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/profile_table.R
\name{profile_table_2}
\alias{profile_table_2}
\title{Creates a list of data.frame elements. Each data.frame consists of 4 sets of cross tables.
These are raw count, col \%, row \% and col \% index tables. There will be as many list elements as there are banner_vars.}
\usage{
profile_table_2(
  df,
  category_vars = NULL,
  numeric_vars = NULL,
  weight_var = NULL,
  banner_vars,
  table_labels
)
}
\arguments{
\item{df}{data.frame of input variables}

\item{category_vars}{variables that are to be treated as factors in the table output.
Will produce counts for each value for each variable. If weight_var is provided, will
produce weighted counts.}

\item{numeric_vars}{variables that are to be treated as numeric in the table output.
Will produce means for each variable. if weight_var is provided, will produce weighted means.}

\item{weight_var}{if not NULL, a vector that contains weights for each observation. The NULL
case is equivalent to all cases being weighted 1.}

\item{banner_vars}{name of the variables to appear along the top of the table. The number of cross
tables produced is equal to the number of banner variables.}

\item{table_labels}{datamap with variable names and labels. See rsegmenter::extract_datamap}
}
\description{
Creates a list of data.frame elements. Each data.frame consists of 4 sets of cross tables.
These are raw count, col \%, row \% and col \% index tables. There will be as many list elements as there are banner_vars.
}
\examples{
df <- rsegmenter::test_seg_unlabelled

table_labels <- rsegmenter::extract_datamap(rsegmenter::test_seg_labelled)

profile_table_2(df, category_vars = c("demog1","demog2"), numeric_vars = NULL,
weight_var = "weight", banner_vars = "seg1", table_labels = table_labels)

}
