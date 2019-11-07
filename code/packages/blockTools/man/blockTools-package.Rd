\name{blockTools-package}
\alias{blockTools-package}
\alias{blockTools}
\docType{package}
\title{Block, Randomly Assign, and Diagnose Potential Interference in Randomized Experiments}
\description{Block units into experimental blocks, with one unit per
treatment condition, by creating a measure of multivariate distance
between all possible pairs of units.  Maximum, minimum, or an
allowable range of differences between units on one variable can be
set.  Randomly assign units to treatment conditions.  Diagnose
potential interference problems between units assigned to different
treatment conditions. Write outputs to .tex and .csv files.
}
\details{
\tabular{ll}{
Package: \tab blockTools\cr
Type: \tab Package\cr
Version: \tab 0.6-3\cr
Date: \tab 2016-12-02\cr
License: \tab  GPL (>=2)\cr
}
Given raw data, \code{block} creates experimental blocks,
\code{assignment} assigns units to treatment conditions, \code{diagnose}
detects possible interference problems, and \code{outTeX} and
\code{outCSV} write block or assignment output objects to a set of .tex
and .csv files, respectively.  In sequential experiments, \code{seqblock} assigns units to treatment conditions.
}
\author{
Ryan T. Moore \email{rtm@american.edu} and Keith Schnakenberg \email{keith.schnakenberg@gmail.com}

Maintainer: Ryan T. Moore \email{rtm@american.edu}
}

\references{
\url{http://www.ryantmoore.org/html/software.blockTools.html}
}

\keyword{ package }

\examples{
data(x100)

## block
out <- block(x100, groups = "g", n.tr = 2, id.vars = c("id"), block.vars
             = c("b1", "b2"), algorithm="optGreedy", distance =
             "mahalanobis", level.two = FALSE, valid.var = "b1",
             valid.range = c(0,500), verbose = TRUE)
## assign
assg <- assignment(out, seed = 123)
## diagnose
diag <- diagnose(object = assg, data = x100, id.vars = "id",
                suspect.var = "b2", suspect.range = c(0,50))
## create .tex files of assigned blocks
outTeX(assg)
## create .csv files of unassigned blocks
outCSV(out)
## create block IDs
createBlockIDs(out, x100, id.var = "id")
## block ID integers are unique, even with several groups
axb <- assg2xBalance(assg, x100, id.var = "id", bal.vars = c("b1", "b2"))
}
