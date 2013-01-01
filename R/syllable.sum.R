#' Transcript Apply Syllable Counts Per Row
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
#' 
syllable.sum <-
function(text.var, parallel = FALSE) {
    if (!parallel) {
        unlist(lapply(as.character(text.var), function(x) sum(syllable.count(Trim(x))['syllables'])))
    } else {
        cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
        clusterExport(cl=cl, varlist=c("text.var", "strip", "Trim",
            "syllable.count", "scrubber", "bracketX", "env.syl"), 
            envir = environment())
        m <- parLapply(cl, as.character(text.var), function(x) {
                sum(syllable.count(Trim(x))['syllables'])
            }
        )
        stopCluster(cl)
        unlist(m)
    }
}