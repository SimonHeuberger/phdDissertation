\name{outTeX}
\alias{outTeX}
\title{Export blocked or assigned data to .tex format files} 
\description{Exports output from \code{block} or \code{assignment} to a
  set of .tex files using \code{xtable}.
}
\usage{
outTeX(block.obj, namesCol = NULL, file.names = NULL, captions = NULL, digits = 2, ...)
}
\arguments{
  \item{block.obj}{a list of dataframes, such as output from
    \code{block} or \code{assignment}.}
  \item{namesCol}{an optional vector of column names to be used in
    output files.}
  \item{file.names}{an optional list of strings specifying the output file names.}
  \item{captions}{an optional list of strings specifying the table captions.  See Details below.}
  \item{digits}{the number of digits to which to round multivariate
    distances in output files.}
  \item{\dots}{additional arguments passed to \code{xtable}.}
}

\details{Under the default (\code{file.names =
NULL}), each file is named \dQuote{GroupXXX.tex}, where \dQuote{XXX}
is the group name taken from the input object.  Under the default (\code{captions =
NULL}), each caption is \dQuote{Group XXX.}, where \dQuote{XXX}
is the group name taken from the input object.

\code{outTeX} appends \code{.tex} to the user-specified \code{file.names}.

The table reference labels are created as \code{t:XXX}, where \code{XXX} is the file name (without \code{.tex}) for the \code{.tex} file containing that table.

\code{captions} takes a list of strings of length equal to the number of groups in \code{block.obj$blocks}, if \code{block.obj} is output from \code{block}, or the number of groups in \code{block.obj$assg}, if \code{block.obj} is output from \code{assignment}.

The tables in the output .tex files can easily be integrated into an existing .tex document
using LaTeX code \samp{\\include{GroupXXX}}.
}


\value{ A set of .tex files, one for each element of the input list of
blocked or assigned units. }

\author{Ryan T. Moore}

\seealso{\code{\link{outCSV}}, \code{\link{block}}, \code{\link{assignment}}
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

## create three .tex files of blocks
outTeX(out)
## create three .tex files of assigned blocks
##   (note: overwrites blocked .tex files)
outTeX(assg)
## create three .tex files with custom file names and captions
outTeX(assg, file.names = list("file1", "file2", "file3"), 
       captions = list("This is caption 1.", "Caption 2.", "Caption 3?"))
}
\keyword{design}
\keyword{IO}
