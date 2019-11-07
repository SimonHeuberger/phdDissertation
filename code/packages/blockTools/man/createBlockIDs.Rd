\name{createBlockIDs}
\alias{createBlockIDs}
\title{Create vector of integers containing block identifiers}
\description{
  Creates a vector of integers which represent unique blocks in an
  object output from \code{block} or \code{assignment}.
}
\usage{
createBlockIDs(obj, data, id.var)
}

\arguments{
  \item{obj}{an output object from \code{block} or \code{assignment}.}
  \item{data}{the data frame that was input into \code{block} for blocking.}
  \item{id.var}{a string specifying which column of \code{data} contains identifying information.}
}

\details{
Under the current implementation, \code{level.two} in \code{block}
should be set to \code{FALSE}.

If blocking was performed specifying a \code{groups} argument,
\code{createBlockIDs} will assign block ID values that are unique
across groups.  In other words, \code{createBlockIDs} does not restart
numbering when it encounters a new group of blocks.
}

\value{ 
A numeric vector of integers with \code{nrow(data)} elements with
lowest value equal to 1, corresponding to the block each unit is in.
For units in \code{data} that are not in \code{obj}, the value of NA
is assigned.
}

\author{Ryan T. Moore}

\seealso{\code{\link{block}}, \code{\link{assignment}}}

\examples{
data(x100)
out <- block(x100, groups = "g", n.tr = 2, id.vars = c("id"), block.vars
             = c("b1", "b2"))
createBlockIDs(out, x100, id.var = "id")
## block ID integers are unique, even with several groups
}

\keyword{design}

