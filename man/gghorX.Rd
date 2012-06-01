\name{gghorX}
\alias{gghorX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Eliminate Horizontal Lines from ggplot2 Object
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gghorX(ggplot.object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ggplot.object}{
%%     ~~Describe \code{ggplot.object} here~~
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
function (ggplot.object) 
{
    guide_grid_orig <- ggplot2:::guide_grid
    guide_grid_no_hline <- function(theme, x.minor, x.major, 
        y.minor, y.major) {
        x.minor <- setdiff(x.minor, x.major)
        y.minor <- setdiff(y.minor, y.major)
        ggname("grill", grobTree(theme_render(theme, "panel.background"), 
            if (length(x.minor) > 0) 
                theme_render(theme, "panel.grid.minor", name = "x", 
                  x = rep(x.minor, each = 2), y = rep(0:1, length(x.minor)), 
                  id.lengths = rep(2, length(x.minor))), if (length(x.major) > 
                0) 
                theme_render(theme, "panel.grid.major", name = "x", 
                  x = rep(x.major, each = 2), y = rep(0:1, length(x.major)), 
                  id.lengths = rep(2, length(x.major)))))
    }
    environment(guide_grid_no_hline) <- environment(ggplot2:::guide_grid)
    assignInNamespace("guide_grid", guide_grid_no_hline, ns = "ggplot2")
    print(ggplot.object)
    assignInNamespace("guide_grid", guide_grid_orig, ns = "ggplot2")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
