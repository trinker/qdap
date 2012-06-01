\name{key_merge}
\alias{key_merge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Merge Demogrphic Information with Person/Text Transcript
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
key_merge(transcript.df, key.df, common.column, defualt.arrange = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{transcript.df}{
%%     ~~Describe \code{transcript.df} here~~
}
  \item{key.df}{
%%     ~~Describe \code{key.df} here~~
}
  \item{common.column}{
%%     ~~Describe \code{common.column} here~~
}
  \item{defualt.arrange}{
%%     ~~Describe \code{defualt.arrange} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
}
\note{
%%  ~~further notes~~
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
function (transcript.df, key.df, common.column, defualt.arrange = TRUE) 
{
    DF <- merge(transcript.df, key.df, by = c(common.column, 
        common.column), incomparables = NA)
    if (defualt.arrange) {
        DF[, c(1, 3:ncol(DF), 2)]
    }
    else {
        DF
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
