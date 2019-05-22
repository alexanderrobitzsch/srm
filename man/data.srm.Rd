%% File Name: data.srm.Rd
%% File Version: 0.04

\name{data.srm}
\alias{data.srm}
\alias{data.srm01}
\docType{data}

\title{
Example Datasets for the \pkg{srm} Package
}
\description{
Some simulated example datasets for the \pkg{srm} package.
}
\usage{
data(data.srm01)
}


\format{
\itemize{

\item The dataset \code{data.srm01} contains three variables, 10 round robin
groups with 10 members each.

\code{'data.frame':   900 obs. of  7 variables:} \cr
\code{ $ Group  : num  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ dyad   : num  1 2 3 4 5 6 7 8 9 10 ...} \cr
\code{ $ Actor  : num  1 1 1 1 1 1 1 1 1 2 ...} \cr
\code{ $ Partner: num  2 3 4 5 6 7 8 9 10 3 ...} \cr
\code{ $ Wert1  : num  -0.15 -0.95 0.82 1.15 -1.79 1.17 1.79 -0.57 -0.46 1.19 ...} \cr
\code{ $ Wert2  : num  -0.77 0.17 0.42 0.16 -0.44 0.89 1.67 -1.9 -0.74 2.67 ...} \cr
\code{ $ Wert3  : num  -0.49 0.08 -0.12 1.16 -2.78 -0.74 2.66 -1.28 -0.45 1.93 ...} \cr

}
}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
%\examples{
%data(data.srm01)
%## maybe str(data.srm01) ; plot(data.srm01) ...
%}
\keyword{datasets}
