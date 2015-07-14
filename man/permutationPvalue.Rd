\name{permutationPvalue}
\alias{permutationPvalue}
\alias{permutationPVALUE}
\alias{permutationP}
\alias{permutationp}
\alias{permutationPV}
\alias{permutationpv}
\title{Permutation Between Groups' p-Value}
\description{
We determine the p-value as the proportion of observations, under the null hypothesis, that are at least as extreme as the observed statistic.
}
\usage{
permutationPvalue(replication, data, group)
}
\arguments{
  \item{replication}{is a integer specifying the number of permutations to perform between the groups in order to obtain p-values.}
  
  \item{data}{is a matrix containing point positions.  The matrix has two columns. Each row of the matrix gives one point.}

  \item{group}{is a vector specifying the group each point belongs to. Notice that this method calculates distances of two groups by a time.}
  
}
