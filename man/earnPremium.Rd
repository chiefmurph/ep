\name{earnPremium}
\alias{earnPremium}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Earn premium at the policy level
}
\description{
Given a policy level data.frame with columns named
writtenpremium, effectivedate, and expirationdate, and given an ascending sequence 
of 'asof' dates, this function will earn each
policy's writtenpremium between the asof dates.
}
\usage{
earnPremium(data, asof)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a data.frame with columns named 'writtenpremium' (numeric),
'effectivedate', and 'expirationdate' (see "Details")
}
  \item{asof}{
a vector of "dates" (see "Details")
}
}
\details{
'effectivedate' and 'expirationdate' can be any R class that 
represents "dates": e.g., POSIXt objects or Date objects.
'asof' should be of the same class.

The function uses linear interpolation to "earn" each policy's
writtenpremium incrementally and proportionally across the asof dates.
The term over which each policy's premium is earned is determined by 
its pair of effective and expiration dates.
The denominator in the proportionality factor is the length of 
the policy term calculated as expirationdate - effectivedate.
The units, therefore, depend on the class of the R object representing those dates. 
For example, if POSIXt, units = seconds; if Date, units = days.
}
\value{
A matrix of incremental earned premium values,
with one row per policy and one column per calendar period determined by
successive pairs of 'asof' dates.
}
\references{
See, for example, 
www.riskheads.org/how-to-calculate-earned-premium-example/
}
\author{
Yours Truly
}
\note{
The policy term must be completely determined by the instant coverage begins 
('effectivedate') and the instant coverage ends ('expirationdate').
Therefore, if the "date" class utilized is equivalent to the instant 
a \bold{day} begins
(as are most R "date" objects), 
then effectivedate would correspond to the first day of coverage and
expirationdate would correspond to the day after coverage ends.
}

\section{Warning }{
Beware!
No checks are performed on the integrity of the inputs:
neither the names or classes of the columns of 'data',
nor whether 'asof' has the correct class and is in ascending order.
}
\examples{
policytable <- data.frame(
  writtenpremium = rep(1000, 4),
  effectivedate  = as.Date(c("2014-01-01", "2014-04-01", 
                             "2014-07-01", "2014-10-01")),
  expirationdate = as.Date(c("2015-01-01", "2015-04-01", 
                             "2015-07-01", "2015-10-01")))
quarterbegindates <- as.Date(c("2014-01-01", "2014-04-01", 
                               "2014-07-01", "2014-10-01", 
                               "2015-01-01"))
earnPremium(policytable, quarterbegindates)
}
