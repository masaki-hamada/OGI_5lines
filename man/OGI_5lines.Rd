\name{OGI_5lines}
\alias{OGI_5lines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
OGI_5lines
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
It computes OGI.
}
\usage{
OGI_5lines(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{x} here~~
X: p-variate data matrix
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The Objective General Index (OGI) is the general index \\
\begin{align*}
  {\mbox{\boldmath g}} = \sum_{i=1}^p w_i{\mbox{\boldmath x_i}}
\end{align*}
which satisfies the following conditions: \\
(i) $w_1, \ldots, w_p > 0$. \\
(ii) $\text{Cov}({\mbox{\boldmath g}},w_i{\mbox{\boldmath x_i}})
      = 1$ for each $i = 1,\ldots, p$.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Masaki Hamada
}
\note{
%%  ~~further notes~~
This is the note.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

