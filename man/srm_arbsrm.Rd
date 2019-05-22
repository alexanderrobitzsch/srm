%% File Name: srm_arbsrm.Rd
%% File Version: 0.21

\name{srm_arbsrm}
\alias{srm_arbsrm}
\alias{coef.srm_arbsrm}
\alias{summary.srm_arbsrm}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Least Squares Estimation of the Social Relations Model (Bond & Malloy, 2018)
}

\description{
Provides least squares estimation of the bivariate social relations model
with missing completely at random data (Bond & Malloy, 2018a). The code is
basically taken from Bond and Malloy (2018b) and rewritten for
reasons of computation time reduction.  
}

\usage{
srm_arbsrm(data, serror = TRUE, use_srm = TRUE)

\method{coef}{srm_arbsrm}(object, ...)
\method{summary}{srm_arbsrm}(object, digits=3, file=NULL, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Rectangular dataset currently containing only one round robin group.
Bivariate observations are stacked one below the other (see
example dataset \code{data.bm1}). 
}
  \item{serror}{
Logical indicating whether standard errors should be calculated.
}
  \item{use_srm}{
Logical indicating whether the rewritten code (\code{TRUE}) or the
original code of Bond and Malloy (2018b) should be used. 
}
\item{object}{Object of class \code{srm_arbsrm}}
\item{file}{Optional file name for summary output}
\item{digits}{Number of digits after decimal in summary output}
\item{\dots}{Further arguments to be passed}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
List containing entries

\item{par_summary}{Parameter summary table}
\item{est}{Estimated parameters (as in Bond & Malloy, 2018b)}
\item{se}{Estimated standard errors (as in Bond & Malloy, 2018b)}
}

\references{
Bond, C. F., & Malloy, T. E. (2018a). Social relations analysis of dyadic data structure:
The general case. In T. E. Malloy. 
\emph{Social Relations Modeling of Behavior in Dyads and Groups} (Ch. 14).
Academic Press. \doi{10.1016/B978-0-12-811967-9.00014-X}

Bond, C. F., & Malloy, T. E. (2018b). \emph{ARBSRM - The general social relations model}.
\url{http://thomasemalloy.org/arbsrm-the-general-social-relations-model/}.
}


\author{
Rewritten code of Bond and Malloy (2018b). See
\url{http://thomasemalloy.org/arbsrm-the-general-social-relations-model/} and
\url{http://thomasemalloy.org/wp-content/uploads/2017/09/arbcodeR.pdf}.
}

\note{
If you use this function, please also cite Bond and Malloy (2018a).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Without missing data, ANOVA estimation can be conducted with the
\pkg{TripleR} package.
}


\examples{
#############################################################################
# EXAMPLE 1: Bond and Malloy (2018) illustration dataset
#############################################################################

data(data.bm2, package="srm")
dat <- data.bm2

#- estimation
mod1 <- srm::srm_arbsrm(dat)
mod1$par_summary
coef(mod1)
summary(mod1)

\dontrun{
#-- estimation with original Bond and Malloy code
mod1a <- srm::srm_arbsrm(dat, use_srm=FALSE)
summary(mod1a)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
