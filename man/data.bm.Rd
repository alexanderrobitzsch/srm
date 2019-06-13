%% File Name: data.bm.Rd
%% File Version: 0.10

\name{data.bm}
\alias{data.bm}
\alias{data.bm1}
\alias{data.bm2}
\docType{data}

\title{
Dataset Bond and Malloy (2018)
}

\description{
This is the illustration dataset of Bond and Malloy (2018) for a bivariate
social relations model. The round robin design contains 16 persons and
some missing values for one person.
}

\usage{
data(data.bm1)
data(data.bm2)
}


\format{
\itemize{

\item The dataset \code{data.bm1} contains all ratings in a wide
format. The two outcomes are arranged one below the other.

\code{'data.frame':   32 obs. of  16 variables:} \cr
\code{ $ a: int  NA 12 13 14 15 15 14 14 13 13 ...} \cr
\code{ $ b: int  10 NA 10 18 7 15 14 8 12 12 ...} \cr
\code{ $ c: int  13 12 NA 14 13 14 13 13 11 12 ...} \cr
\code{ [...] } \cr
\code{ $ p: int  11 13 14 14 9 8 17 13 11 12 ...} \cr

\item The dataset \code{data.bm2} is a subdataset of \code{data.bm1}
which contains observations 9 to 16.
}

}


%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

\source{
\url{http://thomasemalloy.org/arbsrm-the-general-social-relations-model/}
}

\references{
Bond, C. F., & Malloy, T. E. (2018a). Social relations analysis of dyadic data structure:
The general case. In T. E. Malloy.
\emph{Social relations modeling of behavior in dyads and groups} (Ch. 14).
Academic Press. \doi{10.1016/B978-0-12-811967-9.00014-X}
}

%\examples{
%data(data.bm1)
%## maybe str(data.bm1) ; plot(data.bm1) ...
%}
%% \keyword{datasets}
