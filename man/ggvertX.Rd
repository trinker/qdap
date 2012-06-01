\name{ggvertX}
\alias{ggvertX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Eliminate Vertical Lines from ggplot2 Object
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ggvertX(ggplot.object)
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
    guide_grid_no_vline <- function(theme, x.minor, x.major, 
        y.minor, y.major) {
        x.minor <- setdiff(x.minor, x.major)
        y.minor <- setdiff(y.minor, y.major)
        ggname("grill", grobTree(theme_render(theme, "panel.background"), 
            if (length(y.minor) > 0) 
                theme_render(theme, "panel.grid.minor", name = "y", 
                  x = rep(0:1, length(y.minor)), y = rep(y.minor, 
                    each = 2), id.lengths = rep(2, length(y.minor))), 
            if (length(y.major) > 0) 
                theme_render(theme, "panel.grid.major", name = "y", 
                  x = rep(0:1, length(y.major)), y = rep(y.major, 
                    each = 2), id.lengths = rep(2, length(y.major)))))
    }
    environment(guide_grid_no_vline) <- environment(ggplot2:::guide_grid)
    assignInNamespace("guide_grid", guide_grid_no_vline, ns = "ggplot2")
    print(ggplot.object)
    assignInNamespace("guide_grid", guide_grid_orig, ns = "ggplot2")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
