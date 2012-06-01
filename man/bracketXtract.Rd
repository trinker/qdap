\name{bracketXtract}
\alias{bracketXtract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transcript Apply of Extraction the Information Inside Brackets
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bracketXtract(txt, br = c("(", "[", "{", "all"), with = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{txt}{
%%     ~~Describe \code{txt} here~~
}
  \item{br}{
%%     ~~Describe \code{br} here~~
}
  \item{with}{
%%     ~~Describe \code{with} here~~
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
function (txt, br = c("(", "[", "{", "all"), with = FALSE) 
{
    br <- match.arg(br)
    left <- if ("all" == br) {
        "\\(|\\{|\\["
    }
    else {
        sprintf("\\\%s", br)
    }
    map <- c(`\(` = "\\)", `\[` = "\\]", `\{` = "\\}", `\(|\{|\[` = "\\)|\\}|\\]")
    fmt <- if (with == TRUE) {
        "(\%s).*?(\%s)"
    }
    else {
        "(?<=\%s).*?(?=\%s)"
    }
    re <- sprintf(fmt, left, map[left])
    if (length(txt) == 1) {
        unlist(regmatches(txt, gregexpr(re, txt, perl = TRUE)))
    }
    else {
        regmatches(txt, gregexpr(re, txt, perl = TRUE))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
