\name{or2rr}
\alias{or2rr}
\title{ Transform OR to RR }
\description{
  The function transforms a given odds-ratio (OR) to the respective
  relative risk (RR).
}
\usage{
or2rr(or, p0, p1)
}
\arguments{
  \item{or}{ numeric vector: OR (odds-ratio). }
  \item{p0}{ numeric vector of length 1: incidence of the outcome of interest in the
             nonexposed group.}
  \item{p1}{ numeric vector of length 1: incidence of the outcome of interest in the
             exposed group.}
}
\details{
  The function transforms a given odds-ratio (OR) to the respective
  relative risk (RR). It can also be used to transform the limits
  of confidence intervals.

  The formulas can be derived by combining the formulas for RR and OR; see
  also Zhang and Yu (1998).
}
\value{
  relative risk.
}
\references{
  Zhang, J. and Yu, K. F. (1998). What's the relative risk? A method of correcting the odds ratio in cohort
  studies of common outcomes. \emph{JAMA}, \bold{280}(19):1690-1691.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
## We use data from Zhang and Yu (1998)

## OR to RR using OR and p0
or2rr(14.1, 0.05)

## compute p1
or2rr(14.1, 0.05)*0.05

## OR to RR using OR and p1
or2rr(14.1, p1 = 0.426)

## OR and 95% confidence interval
or2rr(c(14.1, 7.8, 27.5), 0.05)

## Logistic OR and 95% confidence interval
logisticOR <- rbind(c(14.1, 7.8, 27.5),
                    c(8.7, 5.5, 14.3),
                    c(27.4, 17.2, 45.8),
                    c(4.5, 2.7, 7.8),
                    c(0.25, 0.17, 0.37),
                    c(0.09, 0.05, 0.14))
colnames(logisticOR) <- c("OR", "2.5\%", "97.5\%")
rownames(logisticOR) <- c("7.4", "4.2", "3.0", "2.0", "0.37", "0.14")
logisticOR

## p0
p0 <- c(0.05, 0.12, 0.32, 0.27, 0.40, 0.40)

## Compute corrected RR
## helper function
or2rr.mat <- function(or, p0){
  res <- matrix(NA, nrow = nrow(or), ncol = ncol(or))
  for(i in seq_len(nrow(or)))
    res[i,] <- or2rr(or[i,], p0[i])
  dimnames(res) <- dimnames(or)
  res
}
RR <- or2rr.mat(logisticOR, p0)
round(RR, 2)

## Results are not completely identical to Zhang and Yu (1998)
## what probably is caused by the fact that the logistic OR values
## provided in the table are rounded and are not exact values.
}
\keyword{univar}
