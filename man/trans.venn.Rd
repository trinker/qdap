\name{trans.venn}
\alias{trans.venn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compares language use with a Venn Diagram
}
\description{
Compares language use with a Venn Diagram
}
\usage{
trans.venn(text.var, grouping.var, stopwords = Top200Words, title = TRUE, title.font = NULL, title.color = "black", title.cex = NULL, title.name = NULL, legend = TRUE, legend.cex = 0.8, legend.location = "bottomleft", legend.text.col = "black", legend.horiz = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{grouping.var}{
%%     ~~Describe \code{grouping.var} here~~
}
  \item{stopwords}{
%%     ~~Describe \code{stopwords} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{title.font}{
%%     ~~Describe \code{title.font} here~~
}
  \item{title.color}{
%%     ~~Describe \code{title.color} here~~
}
  \item{title.cex}{
%%     ~~Describe \code{title.cex} here~~
}
  \item{title.name}{
%%     ~~Describe \code{title.name} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
}
  \item{legend.cex}{
%%     ~~Describe \code{legend.cex} here~~
}
  \item{legend.location}{
%%     ~~Describe \code{legend.location} here~~
}
  \item{legend.text.col}{
%%     ~~Describe \code{legend.text.col} here~~
}
  \item{legend.horiz}{
%%     ~~Describe \code{legend.horiz} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (text.var, grouping.var, stopwords = Top200Words, title = TRUE, 
    title.font = NULL, title.color = "black", title.cex = NULL, 
    title.name = NULL, legend = TRUE, legend.cex = 0.8, legend.location = "bottomleft", 
    legend.text.col = "black", legend.horiz = FALSE, ...) 
{
    suppressWarnings(require(venneuler))
    if (is.list(grouping.var)) {
        m <- unlist(as.character(substitute(grouping.var))[-1])
        m <- sapply(strsplit(m, "$", fixed = TRUE), function(x) x[length(x)])
        NAME <- paste(m, collapse = "&")
    }
    else {
        G <- as.character(substitute(grouping.var))
        NAME <- G[length(G)]
    }
    group.var <- if (is.list(grouping.var) & length(grouping.var) > 
        1) {
        apply(data.frame(grouping.var), 1, function(x) {
            if (any(is.na(x))) {
                NA
            }
            else {
                paste(x, collapse = ".")
            }
        })
    }
    else {
        grouping.var
    }
    word.list <- qda(text.var, group.var, stopwords = stopwords)[["cwl"]]
    all <- unique(unlist(word.list))
    C1 <- lapply(word.list, function(x) as.numeric(all \%in\% x))
    Counts <- do.call(cbind, C1)
    v <- venneuler::venneuler(Counts)
    if (title) {
        if (!is.null(title.name)) {
            title.name <- title.name
        }
        else {
            title.name <- "Venn Diagram"
        }
    }
    else {
        title.name <- ""
    }
    plot(v, main = title.name, cex.main = title.cex, col.main = title.color, 
        family = title.font, ...)
    if (legend) {
        cols <- col.fn(v$colors)
        par(mar = rep(0, 4), xpd = TRUE)
        legend(legend.location[1], legend.location[2], horiz = legend.horiz, 
            legend = names(C1), fill = cols, cex = legend.cex, 
            text.col = legend.text.col)
        par(mar = c(5, 4, 4, 2) + 0.1, xpd = TRUE)
    }
    invisible(v)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
