#' Transcript Apply Summing of Polysyllables
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text.var The text variable
#' @param parallel logical.  If TRUE attempts to run the function on multiple 
#' cores.  Note that this may not mean a spead boost if you have one core or if 
#' the data set is smaller as the cluster takes time to create.
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
pollysyllable.sum <-
function(text.var, parallel = FALSE) {
    counter <- function(x) {
        v <- table(syllable.count(Trim(x))["syllables"])
        if (identical(c(v), integer(0))){
            return(0)
        }
        y <- as.data.frame(v)
        z <- subset(y, as.numeric(as.character(Var1)) >= 3)
        j <- sum(z$Freq)
        return(j)
    }
    if (!parallel) {
        unlist(lapply(as.character(text.var), function(x) counter(x)))
    } else {
        cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
        clusterExport(cl=cl, varlist=c("text.var", "counter", "strip",
            "Trim", "syllable.count", "scrubber", "bracketX", "env.syl"), 
            envir = environment())
        m <- parLapply(cl, as.character(text.var), function(x) {
                counter(x)
            }
        )
        stopCluster(cl)
        unlist(m)
    }
}
