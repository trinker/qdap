\name{pos.tags}
\alias{pos.tags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Penn Treebank Parts of Speech Key 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pos.tags(type = "pretty")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (type = "pretty") 
{
    POStags.df <- structure(list(Tag = structure(c(1L, 2L, 3L, 
        4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 15L, 13L, 14L, 
        16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 
        27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L), .Label = c("CC", 
        "CD", "DT", "EX", "FW", "IN", "JJ", "JJR", "JJS", "LS", 
        "MD", "NN", "NNP", "NNPS", "NNS", "PDT", "POS", "PRP", 
        "PRP$", "RB", "RBR", "RBS", "RP", "SYM", "TO", "UH", 
        "VB", "VBD", "VBG", "VBN", "VBP", "VBZ", "WDT", "WP", 
        "WP$", "WRB"), class = "factor"), Description = structure(c(8L, 
        7L, 9L, 10L, 11L, 23L, 1L, 2L, 3L, 13L, 14L, 16L, 15L, 
        25L, 24L, 22L, 19L, 18L, 20L, 4L, 5L, 6L, 17L, 26L, 27L, 
        12L, 29L, 33L, 30L, 32L, 31L, 28L, 35L, 36L, 21L, 34L), 
        .Label = c("Adjective", "Adjective, comparative", "Adjective, superlative", 
            "Adverb", "Adverb, comparative", "Adverb, superlative", 
            "Cardinal number", "Coordinating conjunction", "Determiner", 
            "Existential there", "Foreign word", "Interjection", 
            "List item marker", "Modal", "Noun, plural", "Noun, singular or mass", 
            "Particle", "Personal pronoun", "Possessive ending", 
            "Possessive pronoun", "Possessive wh-pronoun", "Predeterminer", 
            "Preposition or subordinating conjunction", "Proper noun, plural", 
            "Proper noun, singular", "Symbol", "to", "Verb, 3rd person singular present", 
            "Verb, base form", "Verb, gerund or present participle", 
            "Verb, non-3rd person singular present", "Verb, past participle", 
            "Verb, past tense", "Wh-adverb", "Wh-determiner", 
            "Wh-pronoun"), class = "factor")), .Names = c("Tag", 
        "Description"), row.names = c(NA, -36L), class = "data.frame", 
        comment = "http://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html")
    POStags.matrix <- as.matrix(POStags.df)
    POStags <- left.just(POStags.df, 1:2)
    x <- switch(type, pretty = POStags, matrix = POStags.matrix, 
        df = POStags.df, dataframe = POStags.df, all = {
            list(POStags.df = POStags.df, POStags.matrix = POStags.matrix, 
                POStags = POStags)
        }, stop("incorrect type specified"))
    return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
