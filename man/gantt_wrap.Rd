\name{gantt_wrap}
\alias{gantt_wrap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A ggplot2 Wrapper for Gantt Plots
}
\description{
A ggplot2 wrapper for data frames produced from gantt and gantt_rep
}
\usage{
gantt.wrap(dataframe, plot.var, facet.vars = NULL, title = NULL, ylab = as.character(plot.var), xlab = "duration.defalut", rev.factor = TRUE, transform = FALSE, minor.line.freq = 5, major.line.freq = 25)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{plot.var}{
%%     ~~Describe \code{plot.var} here~~
}
  \item{facet.vars}{
%%     ~~Describe \code{facet.vars} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{rev.factor}{
%%     ~~Describe \code{rev.factor} here~~
}
  \item{transform}{
%%     ~~Describe \code{transform} here~~
}
  \item{minor.line.freq}{
%%     ~~Describe \code{minor.line.freq} here~~
}
  \item{major.line.freq}{
%%     ~~Describe \code{major.line.freq} here~~
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
function (dataframe, plot.var, facet.vars = NULL, title = NULL, 
    ylab = as.character(plot.var), xlab = "duration.defalut", 
    rev.factor = TRUE, transform = FALSE, minor.line.freq = 5, 
    major.line.freq = 25) 
{
    plot.var <- as.character(substitute(plot.var))
    if (rev.factor) {
        dataframe[, "new"] <- factor(dataframe[, plot.var], levels = rev(levels(dataframe[, 
            plot.var])))
    }
    if (xlab == "duration.defalut") {
        if (!is.null(comment(dataframe))) {
            xlab <- paste0("duration (", comment(dataframe), 
                ")")
        }
        else {
            xlab <- "duration"
        }
    }
    if (!is.null(facet.vars)) {
        dataframe[, "new2"] <- dataframe[, facet.vars[1]]
        if (length(facet.vars) == 2) {
            dataframe[, "new3"] <- dataframe[, facet.vars[2]]
        }
    }
    theplot <- ggplot(dataframe, aes(colour = new)) + geom_vline(xintercept = seq(0, 
        round(max(dataframe$end), -2), minor.line.freq), colour = "gray92", 
        size = 0.025) + geom_vline(xintercept = seq(0, round(max(dataframe$end), 
        -2), major.line.freq), colour = "gray50", size = 0.05) + 
        geom_segment(aes(x = start, xend = end, y = new, yend = new), 
            size = 2) + ylab(ylab) + xlab(xlab) + theme_bw() + 
        scale_x_continuous(expand = c(0, 0)) + theme(panel.background = element_rect(fill = NA, 
        color = "black"), legend.position = "none", legend.position = "none", 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks = element_blank()) + ggtitle(title)
    if (!is.null(facet.vars)) {
        if (length(facet.vars) == 1) {
            if (transform) {
                theplot <- theplot + facet_grid(. ~ new2)
            }
            else {
                theplot <- theplot + facet_grid(new2 ~ .)
            }
        }
        else {
            theplot <- theplot + facet_grid(new2 ~ new3)
        }
    }
    print(theplot)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
