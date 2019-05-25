%% File Name: srm.Rd
%% File Version: 0.154

\name{srm}
\alias{srm}
\alias{coef.srm}
\alias{vcov.srm}
\alias{summary.srm}
\alias{logLik.srm}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Structural Equation Model for the Social Relations Model 
}

\description{
Provides an estimation routine for a multiple group structural equation model
for the social relations model (SRM; Warner, Kenny, & Soto, 1979).
The model is estimated by maximum likelihood (Gill & Swartz, 2001;
Nestler, 2018).
}


\usage{
srm(model.syntax = NULL, data = NULL, group.var = NULL, rrgroup_name = NULL, 
  person_names = c("Actor", "Partner"), fixed.groups = FALSE, var_positive = -1, 
  optimizer = "srm", maxiter = 300, conv_dev = 1e-08, conv_par = 1e-06, 
  do_line_search = TRUE, line_search_iter_max = 6, verbose = TRUE, use_rcpp = TRUE, 
  shortcut = TRUE)
  
\method{coef}{srm}(object, ...)
\method{vcov}{srm}(object, ...)
\method{summary}{srm}(object, digits=3, file=NULL, ...)
\method{logLik}{srm}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model.syntax}{
Syntax similar to \pkg{lavaan} language, see Examples.
}
  \item{data}{
Data frame containing round robin identifier variables and variables in the
round robin design 
}
  \item{group.var}{
Name of grouping variable
}
  \item{rrgroup_name}{
Name of variable indicating round robin group
}
  \item{person_names}{
Names for identifier variables for actors and partners
}
  \item{fixed.groups}{
Logical indicating whether groups should be handled with fixed effects
}
  \item{var_positive}{
Nonnegative value if variances are constrained to be positive
}
  \item{optimizer}{
Optimizer to be used: \code{"srm"} for internal optimization using 
Fisher scoring and \code{"nlminb"} for L-FBGS optimization.
}
  \item{maxiter}{
Maximum number of iterations
}
  \item{conv_dev}{
Convergence criterion for change relative deviance
}
  \item{conv_par}{
Convergence criterion for change in parameters
}
  \item{do_line_search}{
Logical indicating whether line search should be performed
}
  \item{line_search_iter_max}{
Number of iterations during line search algorithm
}
  \item{verbose}{
Logical indicating whether convergence progress should be displayed
}
  \item{use_rcpp}{
Logical indicating whether \pkg{Rcpp} package should be used
}
  \item{shortcut}{
Logical indicating whether shortcuts for round robin groups with 
same structure should be used
}
\item{object}{Object of class \code{srm}}
\item{file}{Optional file name for summary output}
\item{digits}{Number of digits after decimal in summary output}
\item{\dots}{Further arguments to be passed}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
List with following entries (selection)
\item{parm.table}{Parameter table with estimated values}
\item{coef}{Vector of parameter estimates}
\item{vcov}{Covariance matrix of parameter estimates}
\item{parm_list}{List of model matrices}
\item{\dots}{Further values}
}

\references{
Gill, P. S., & Swartz, T. B. (2001). Statistical analyses for round robin interaction 
data. \emph{Canadian Journal of Statistics, 29}(2), 321-331.
\doi{10.2307/3316080}

Nestler, S. (2018). Likelihood estimation of the multivariate social relations model. 
\emph{Journal of Educational and Behavioral Statistics, 43}(4), 387-406.
\doi{10.3102/1076998617741106}

Warner, R. M., Kenny, D. A., & Soto, M. (1979). A new round robin analysis of variance for
social interaction data. \emph{Journal of Personality and Social Psychology, 37}, 
1742-1757. \doi{10.1037/0022-3514.37.10.1742} 
}

%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \pkg{TripleR} and \pkg{amen} packages for alternative estimation
routines for the SRM.
}


\examples{
#############################################################################
# EXAMPLE 1: Univariate SRM
#############################################################################

data(data.srm01, package="srm")
dat <- data.srm01

#-- define model
mf <- '
\%Person
F1@A =~ 1*Wert1@A
F1@P =~ 1*Wert1@P
Wert1@A ~~ 0*Wert1@A + 0*Wert1@P
Wert1@P ~~ 0*Wert1@P

\%Dyad
F1@AP =~ 1*Wert1@AP
F1@PA =~ 1*Wert1@PA
Wert1@AP ~~ 0*Wert1@AP + 0*Wert1@PA
Wert1@PA ~~ 0*Wert1@PA
'

#-- estimate model
mod1 <- srm::srm(mf, data = dat, rrgroup_name="Group", conv_par=1e-4, maxiter=20)
summary(mod1)
round(coef(mod1),3)

\dontrun{
#############################################################################
# EXAMPLE 2: Bivariate SRM
#############################################################################

data(data.srm01, package="srm")
dat <- data.srm01

#-- define model
mf <- '
\%Person
F1@A =~ 1*Wert1@A
F1@P =~ 1*Wert1@P
F2@A =~ 1*Wert2@A
F2@P =~ 1*Wert2@P
Wert1@A ~~ 0*Wert1@A + 0*Wert1@P
Wert1@P ~~ 0*Wert1@P
Wert2@A ~~ 0*Wert2@A + 0*Wert2@P
Wert2@P ~~ 0*Wert2@P

\%Dyad
F1@AP =~ 1*Wert1@AP
F1@PA =~ 1*Wert1@PA
F2@AP =~ 1*Wert2@AP
F2@PA =~ 1*Wert2@PA
Wert1@AP ~~ 0*Wert1@AP + 0*Wert1@PA
Wert1@PA ~~ 0*Wert1@PA
Wert2@AP ~~ 0*Wert2@AP + 0*Wert2@PA
Wert2@PA ~~ 0*Wert2@PA
'

#-- estimate model
mod1 <- srm(mf, data = dat, rrgroup_name="Group", conv_par=1e-4, maxiter=20)
summary(mod1)

#############################################################################
# EXAMPLE 3: One-factor model
#############################################################################

data(data.srm01, package="srm")
dat <- data.srm01

#-- define model
mf <- '
# definition of factor for persons and dyad
\%Person
f1@A=~Wert1@A+Wert2@A+Wert3@A
f1@P=~Wert1@P+Wert2@P+Wert3@P

\%Dyad
f1@AP=~Wert1@AP+Wert2@AP+Wert3@AP    

# define some constraints
Wert1@AP ~~ 0*Wert1@PA
Wert3@AP ~~ 0*Wert3@PA
'
#-- estimate model
mod1 <- srm::srm(mf, data = dat, rrgroup_name="Group", conv_par=1e-4)
summary(mod1)
coef(mod1)

#- use stats::nlminb() optimizer
mod1 <- srm::srm(mf, data = dat, rrgroup_name="Group", optimizer="nlminb", conv_par=1e-4)
summary(mod1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
