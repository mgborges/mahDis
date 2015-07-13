\name{permutationPvalue}
\alias{permutationPvalue}
\alias{permutationPVALUE}
\alias{permutationP}
\alias{permutationp}
\alias{permutationPV}
\alias{permutationpv}
\title{Permutation between groups' p-value}
\description{
The p-values are calculated as the ratio of the random distances higher than the real distance.
}
\usage{
permutationPvalue(replication, data, group)
}
\arguments{
  \item{replication}{is a integer specifying the number of permutations to perform between the groups in order to obtain p-values.}
  
  \item{data}{is a matrix containing point positions in a 2D projection.  The matrix has
two columns.  Each row of the matrix gives one point.}

  \item{group}{is a vector specifying the group each point belongs to. Notice that this method calculates distances of two groups by a time.}
  
}
