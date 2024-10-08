% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_pgamma.R
\name{calculate_pgamma}
\alias{calculate_pgamma}
\title{Calculates the partial gamma coefficient for DIF/DSF in polytomous items}
\usage{
calculate_pgamma(
  dat,
  split_group,
  purification,
  p.adj,
  threshold = c(0.21, 0.31),
  ...
)
}
\arguments{
\item{dat}{A dataframe with polytomous item responses}

\item{split_group}{An indicator of the two groups for which DIF/DSF analyses should be performed}

\item{purification}{A character indicating the type of purification that is used on the partial gamma coefficient. Options are "none", "2step", or "iterative"}

\item{p.adj}{A character indicating the correction method for multiple testing. Options are "none", "bonferroni" and "fdr"}

\item{threshold}{The threshold of partial gamma above which items should be labeled as DIF/DSF items, default is .21 and .31}

\item{...}{Further arguments}
}
\value{
A list with the partial gamma coefficients, the classification (A/B/C), the information on the correction method for multiple testing, the information on purification and convergence of purification if iterative purification was done.
}
\description{
Calculates the partial gamma coefficient for DIF/DSF in polytomous items
}
