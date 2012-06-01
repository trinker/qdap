\name{termco2mat}
\alias{termco2mat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a termco dataframe to a matrix 
}
\description{
Convert a termco dataframe to a matrix for use with visualization functions (e.g. heatmap2 of gplots)
}
\usage{
termco2mat(dataframe, drop.wc=TRUE, short.colnames=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
A termco dataframe
}
  \item{drop.wc}{
logical.  If TRUE the word count column will be dropped.
}
  \item{short.colnames}{
logical.  If TRUE the ``term()'' portion of column names will be dropped.
}
}
 
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
