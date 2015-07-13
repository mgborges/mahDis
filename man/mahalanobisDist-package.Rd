\name{mahalanobisDist-package}
\alias{mahalanobisDist-package}
\alias{mahalanobisDist}
\docType{package}
\title{
Mahalanobis distance between samples with p-values by group permutation
}
\description{
This comparison method combines multidimensional scaling, MDS, with permutation tests. Our strategy uses MDS to project the data onto lower dimensions, while preserving the distance between the data points. Using these projections, we determine the statistic of interest: Mahalanobis distance between the groups. The method empirically estimates the null distribution of the distances between the groups using permutation. The strategy uses this distribution to assess the evidences of differences between the groups.
}

\details{
\tabular{ll}{
Package: \tab mahalanobisDist\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
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
data <- rbind(matrix(rexp(observations/2, rate=1), ncol = 2), matrix(rexp(observations/2, rate=0.5), ncol = 2))
group <- c(rep("A", observations/4), rep("B", observations/4))

# Number of the desired replications to calculate p-values
replication <- 10000

# Function in use
realDist <- mahDis(data, group)
pvalue <- permutationPvalue(replication, data, group)
realDist; pvalue

# The p-values are calculated as the ratio of the ramdom distances higher than the real distance
distVector <- replicate(replication, {mahDis (data, sample(group))})
hist(distVector)
abline(v=realDist, col=2)
}