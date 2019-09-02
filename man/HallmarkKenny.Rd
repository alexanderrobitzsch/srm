%% File Name: HallmarkKenny.Rd
%% File Version: 0.06

\name{HallmarkKenny}
\alias{HallmarkKenny}


\docType{data}

\title{
Hallmark and Kenny Round Robin Data
}

\description{
Data from Kenny et al. (1994)
}

\usage{
data(HallmarkKenny)
}


\format{
A data frame with 802 measurements of 30 round-robin groups on the following 7
round-robin variables (taken on unnumbered 7-point rating scales with higher numbers
indicating a higher value of the trait): \cr

\code{calm}: rating of dimension calm-anxious  \cr
\code{sociable} rating of dimension sociable-withdrawn \cr
\code{liking} rating of dimension like-do not like  \cr
\code{careful} rating of dimension careful-careless \cr
\code{relaxed} rating of dimension relaxed-tense \cr
\code{talkative} rating of dimension talkative-quiet \cr
\code{responsible} rating of dimension responsible-undependable \cr

The data frame also contains participants gender (\code{actor.sex}; \code{1} = F,
\code{2} = M) and their age in years (\code{actor.age}).
Note that the data was assessed in two conditions: odd round robin group numbers indicate
groups in which participants rated all traits for a person at a time whereas even numbers
refer to groups in which participants rated all the people for each trait.
}


%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

\source{
\url{http://davidakenny.net/srm/srmdata.htm}
}

\references{
Kenny, D. A., Albright, L., Malloy, T. E., & Kashy, D. A. (1994). Consensus in
interpersonal perception:  Acquaintance and the big five.
\emph{Psychological Bulletin, 116}(2), 245-258.
\doi{10.1037/0033-2909.116.2.245}
}

%\examples{
%data(data.bm1)
%## maybe str(data.bm1) ; plot(data.bm1) ...
%}
%% \keyword{datasets}
