% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dwnominate.R
\name{dwnominate}
\alias{dwnominate}
\title{Run DW-NOMINATE}
\usage{
dwnominate(
  rc_list,
  id = NULL,
  start = NULL,
  sessions = NULL,
  dims = 2,
  model = 1,
  niter = 4,
  beta = 5.9539,
  w = 0.3463,
  polarity = NULL,
  ...
)
}
\arguments{
\item{rc_list}{A list of \code{rollcall} objects from the
\code{pscl} package, in chronological order.}

\item{id}{Column name in the rollcall objects' \code{legis.data}
data frames providing a unique legislator ID. If not specified
legislator names will be used.}

\item{start}{A roll call scaling result of class \code{common space},
\code{wnominate}, \code{oc}, or \code{dwnominate} providing starting
estimates of legislator ideologies. If not provided, dwnominate will
calculate common space scores to get starting values. See the "details"
section for more information about providing starting values.}

\item{sessions}{A vector of length 2 providing the first and last
sessions to include. Defaults to \code{c(1, length(rc_list))}.}

\item{dims}{The number of dimensions to estimate. Can be either 1
or 2.}

\item{model}{The degree of the polynomial representing changes in
legislator ideology over time. \code{0} is constant, \code{1} is
linear, \code{2} is quadratic and \code{3} is cubic.}

\item{niter}{Number of iterations. 4 iterations are typically
enough for the results to converge.}

\item{beta}{Starting estimate of the parameter representing the
spatial error in legislator choices.}

\item{w}{Starting estimate for the weight of the second
dimension. The first dimension has a weight of 1, so w should be
<= 1.}

\item{polarity}{A vector of length 1 or \code{dims} specifying, for
each dimension, a legislator who should have a positive
coordinate value. Legislators can be specified either by name or
ID. If unspecified the first legislator in the data is used.}

\item{...}{Arguments passed to \code{wnominate} if starting
estimates are calculated.}
}
\value{
A list of class \code{dwnominate} containing: \itemize{
  \item{legislators} {A data frame of legislator information}
  \item{rollcalls} {A data frame of rollcall information}
  \item{start} {The \code{wnominate} or \code{oc} results used as
  starting points for DW-NOMINATE} }
}
\description{
Estimate legislator ideal points using the DW-NOMINATE roll call scaling
procedure featured in \insertCite{poole_ideology_2011;textual}{dwnominate}.
}
\details{
DW-NOMINATE was introduced in
\insertCite{mccarty_income_1997;textual}{dwnominate}. It's an extension of
the older D-NOMINATE program \insertCite{poole_patterns_1991}{dwnominate}
that uses the "weighted" distance model first used by W-NOMINATE.

The model assumes that legislators have Guassian utility functions centered
around their ideal points, along with an additional roll call-specific random
utility that makes the voting decisions probabilistic rather than
deterministic.

The returned values are a maximum likelihood estimate of the model parameters
(with minor exceptions around the boundary of the space). DW-NOMINATE uses a
three-step estimation procedure similar to expectation maximization to find
the maximum likelihood. The estimation procedure is a local optimization
algorithm, but the likelihood function is not globally convex, so the results
can be sensitive to starting estimates. Users must provide reasonable
starting values to get correct results. See
\code{vignette("starting-estimates", package = "dwnominate")} for more
discussion of the starting estimates.

A fairly complete technical description of the procedure is available in
\insertCite{poole_spatial_2005;textual}{dwnominate}.
}
\examples{
\donttest{
data(nhsenate)
results <- dwnominate(nhsenate)
plot(results)
}
}
\references{
\insertAllCited{}
}
\seealso{
\url{https://legacy.voteview.com/dw-nominate.htm} for the original
  fortran code and instructions. \code{\link{common_space}} to help generate
  starting estimates.
}
