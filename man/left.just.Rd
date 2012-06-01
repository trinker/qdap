\name{left.just}
\alias{left.just}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Left Justify Columns in a Data Frame
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
left.just(dataframe, column = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{column}{
%%     ~~Describe \code{column} here~~
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
function (dataframe, column = NULL) 
{
    left.j <- function(x) {
        n <- max(nchar(x))
        return(sprintf(paste("\%-", n, "s", sep = ""), x))
    }
    lj <- function(dataframe, column) {
        DF2 <- dataframe
        if (is.null(column)) 
            column <- names(dataframe)
        Q <- max(nchar(as.character(DF2[, column])))
        DF2[, column] <- left.j(as.character(DF2[, column]))
        if (is.character(column)) {
            col <- names(DF2)[which(names(DF2) == column)]
            names(DF2)[which(names(DF2) == column)] <- sprintf(paste("\%-", 
                Q, "s", sep = ""), col)
        }
        else {
            if (is.numeric(column)) {
                col <- names(DF2)[column]
                names(DF2)[column] <- sprintf(paste("\%-", Q, 
                  "s", sep = ""), col)
            }
        }
        return(DF2)
    }
    if (length(column) < 2) {
        if (!is.data.frame(dataframe)) {
            y <- as.character(substitute(dataframe))
            dataframe <- data.frame(dataframe)
            y <- if (y[1] \%in\% c("[", "$")) 
                y[2]
            else y[1]
            names(dataframe) <- y
        }
        DF3 <- lj(dataframe = dataframe, column = column)
    }
    else {
        if (!is.numeric(column)) 
            column <- match(column, names(dataframe))
        dat <- dataframe[, -c(column)]
        ndf <- colnames(dataframe)
        LIST <- lapply(column, function(x) lj(dataframe = dataframe[, 
            x, drop = FALSE], column = NULL))
        dat2 <- cbind(do.call("cbind", LIST), dat, checknames = FALSE)
        NAMES <- colnames(dat2)
        newloc <- match(ndf, Trim(NAMES))
        DF3 <- dat2[, newloc]
    }
    return(DF3)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
