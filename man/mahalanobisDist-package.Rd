\name{mahalanobisDist-package}
\alias{mahalanobisDist-package}
\alias{mahalanobisDist}
\docType{package}
\title{
Mahalanobis Distance Between Samples with p-Values by Group Permutation
}
\description{
This comparison method combines multidimensional scaling (MDS) data with permutation tests. Our strategy uses MDS to project the data onto lower dimensions, while preserving the distance between the data points. Using these projections, we determine the Mahalanobis distance between the groups and estimates the null distribution of the distances between the groups using permutation. The strategy uses this distribution to assess the evidences of differences between the groups
}

\details{
\tabular{ll}{
Package: \tab mahalanobisDist\cr
Type: \tab Package\cr
Version: \tab 0.99.2\cr
Date: \tab 2015-07-06\cr
}

}
\author{
Murilo Guimaraes Borges and Benilton de Sa Carvalho.

Maintainer: Murilo G. Borges, murilogborges@hotmail.com.
}
\references{
Mahalanobis, P.C.: On the generalized distance in statistics. Proceedings of the National Institute of Sciences (Calcutta) 2 49-55 (1936).
}
\keyword{ package }
\seealso{
\code{\link[matrixStats]{colMedians}, \link[mahalanobisDist]{mahDis}, \link[mahalanobisDist]{permutationPvalue}}.
}
\examples{
# Creation of our data and groups
observations <- 60
data <- rbind(matrix(rexp(observations/2, rate=1), ncol = 2), 
              matrix(rexp(observations/2, rate=0.5), ncol = 2))
group <- c(rep("A", observations/4), rep("B", observations/4))

# Number of the desired replications to calculate p-values
replication <- 10000

# Function in use
realDist <- mahDis(data, group)
pvalue <- permutationPvalue(replication, data, group)
realDist; pvalue

# The p-values estimated by the null distribution of the distances between the groups using permutation
distVector <- replicate(replication, {mahDis (data, sample(group))})
hist(distVector)
abline(v=realDist, col=2)
}