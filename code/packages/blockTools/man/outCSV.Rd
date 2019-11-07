\name{outCSV}
\alias{outCSV}
\title{Export blocked or assigned data to .csv format files}
\description{Exports output from \code{block} or \code{assignment} to a
  set of .csv files using \code{write.csv}.
}
\usage{
outCSV(block.obj, namesCol = NULL, file.names = NULL, digits = 2, ...)
}
\arguments{
  \item{block.obj}{a list of dataframes, such as output from
    \code{block} or \code{assignment}.}
  \item{namesCol}{an optional vector of column names to be used in
    output files.}
  \item{file.names}{an optional list of strings specifying the output file names.}
  \item{digits}{number of digits to which to round multivariate
    distances in output files.}
  \item{\dots}{additional arguments passed to \code{write.csv}.}
}

\details{Under the default (\code{file.names =
NULL}), each file is named \dQuote{GroupXXX.csv}, where \dQuote{XXX}
is the group name taken from the input object. 
}

\value{ A set of .csv files, one for each element of the input list of
blocked or assigned units.}

\author{Ryan T. Moore}

\seealso{\code{\link{outTeX}}, \code{\link{write.csv}},
  \code{\link{block}}, \code{\link{assignment}}
}
\examples{
data(x100)

## First, block
out <- block(x100, groups = "g", n.tr = 2, id.vars = c("id"), block.vars
             = c("b1", "b2"), algorithm="optGreedy", distance =
             "mahalanobis", level.two = FALSE, valid.var = "b1",
             valid.range = c(0,500), verbose = TRUE)
## Second, assign
assg <- assignment(out, seed = 123)

## create three .csv files of blocks
outCSV(out)
## create three .csv files of assigned blocks
##   (note: overwrites blocked .csv files)
outCSV(assg)
## create three .csv files with custom file names
outCSV(assg, file.names = list("file1", "file2", "file3"))
}

\keyword{design}
\keyword{IO}
