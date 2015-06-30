#' Break Text Into Ordered Word Chunks
#' 
#' Some visualizations and algorithms require text to be broken into chunks of 
#' ordered words.  \code{chunker} breaks text, optionally by grouping 
#' variables, into equal chunks.  The chunk size can be specified by giving 
#' number of words to be in each chunk or the number of chunks.
#' 
#' @param text.var The text variable
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param n.words An integer specifying the number of words in each chunk (must 
#' specify n.chunks or n.words).
#' @param n.chunks An integer specifying the number of chunks (must specify 
#' n.chunks or n.words).
#' @param as.string logical.  If \code{TRUE} the chunks are returned as a single 
#' string.  If \code{FALSE} the chunks are returned as a vector of single words.
#' @param rm.unequal logical. If \code{TRUE} final chunks that are unequal in 
#' length to the other chunks are removed.
#' @return Returns a list of text chunks.
#' @keywords chunks group text
#' @export
#' @examples
#' with(DATA, chunker(state, n.chunks = 10))
#' with(DATA, chunker(state, n.words = 10))
#' with(DATA, chunker(state, n.chunks = 10, as.string=FALSE))
#' with(DATA, chunker(state, n.chunks = 10, rm.unequal=TRUE))
#' with(DATA, chunker(state, person, n.chunks = 10))
#' with(DATA, chunker(state, list(sex, adult), n.words = 10))
#' with(DATA, chunker(state, person, n.words = 10, rm.unequal=TRUE))
#' 
#' ## Bigger data
#' with(hamlet, chunker(dialogue, person, n.chunks = 10))
#' with(hamlet, chunker(dialogue, person, n.words = 300))
#' 
#' \dontrun{
#' ## with polarity hedonmetrics
#' dat <- with(pres_debates2012[pres_debates2012$person %in% qcv(OBAMA, ROMNEY), ], 
#'     chunker(dialogue, list(person, time), n.words = 300))
#' 
#' dat2 <- colsplit2df(list2df(dat, "dialogue", "person&time")[, 2:1])
#' 
#' dat3 <- split(dat2[, -2], dat2$time)
#' ltruncdf(dat3, 10, 50)
#' 
#' poldat <- lapply(dat3, function(x) with(x, polarity(dialogue, person, constrain = TRUE)))
#' 
#' 
#' m <- lapply(poldat, function(x) plot(cumulative(x)))
#' m <- Map(function(w, x, y, z) {
#'         w + ggtitle(x) + xlab(y) + ylab(z)
#'     }, 
#'         m, 
#'         paste("Debate", 1:3), 
#'         list(NULL, NULL, "Duration (300 Word Segment)"), 
#'         list(NULL, "Cumulative Average Polarity", NULL)
#' )
#' 
#' library(gridExtra)
#' do.call(grid.arrange, m)
#' 
#' ## By person
#' ## By person
#' poldat2 <- Map(function(x, x2){
#' 
#'     scores <- with(counts(x), split(polarity, person))
#'     setNames(lapply(scores, function(y) {
#'         y <- list(cumulative_average_polarity = y)
#'         attributes(y)[["constrained"]] <- TRUE
#'         qdap:::plot.cumulative_polarity(y) + xlab(NULL) + ylab(x2)
#'     }), names(scores))
#' 
#' }, poldat, paste("Debate", 1:3))
#' 
#' poldat2 <- lapply(poldat2, function(x) {
#'     x[[2]] <- x[[2]] + ylab(NULL)
#'     x
#' })
#' 
#' poldat2[[1]] <- Map(function(x, y) {
#'         x + ggtitle(y)
#'     },
#'         poldat2[[1]], qcv(Obama, Romney)
#' )
#' 
#' library(gridExtra)
#' do.call(grid.arrange, unlist(poldat2, recursive=FALSE))
#' }
chunker <- function(text.var, grouping.var = NULL, n.words, n.chunks,  
    as.string = TRUE, rm.unequal = FALSE){

    if(is.null(grouping.var)) {
        G <- "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            G <- paste(m, collapse="&")
        } else {
            G <- as.character(substitute(grouping.var))
            G <- G[length(G)]
        }
    }
    if(is.null(grouping.var)){
        grouping <- rep("all", length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- paste2(grouping.var)
        } else {
            grouping <- unlist(grouping.var)
        } 
    } 

    ## split into ordered words by grouping variable
    dat <- lapply(split(as.character(text.var), grouping), bag_o_words)

    if (missing(n.chunks) && missing(n.words)) {
        stop("Must supply either `n.chunks` or `n.words`")
    }

    if (!missing(n.chunks)){
        
        ## Check that n.chunks is integer
        if (!is.Integer(n.chunks)){
            stop("`n.chunks` must be an integer")
        }
        lapply(dat, chunker_help_groups, N = n.chunks, ub = as.string, rme = rm.unequal)

    } else {

        ## Check that n.words is integer
        if (!is.Integer(n.words)){
            stop("`n.words` must be an integer")
        }
        lapply(dat, chunker_help_words, N = n.words, ub = as.string, rme = rm.unequal)
    }

}


chunker_help_groups <- function(x, N, ub, rme){

    len <- length(x)
    size <- floor(len/N)

    ## make the groups, leftover are unequal sized last group
    grabs <- rep(seq_len(N), each = size)
    if (N * size < len){
        leftover <- rep(N + 1, len - N * size)
        grabs <- c(grabs, leftover)
    } 

    y <- suppressWarnings(split(x, grabs))
    if (rme){
        ylen <- length(y)
        ## if there is only one chunk it is returned
        if (ylen != 1) {
            lens <- sapply(y, length)
            if (!Reduce("==", utils::tail(lens, 2))) y <- y[1:(ylen-1)]
        }
    }
    if (ub) y <- lapply(y, unbag)
    y
}

chunker_help_words <- function(x, N, ub, rme){

    len <- length(x)
    groups <- ceiling(len/N)
    y <- suppressWarnings(split(x, rep(seq_len(groups), each = N)))
    if (rme){
        ylen <- length(y)
        if (ylen == 1) return(NULL)
        if (length(y[[ylen]]) != N) y <- y[1:(ylen-1)]
    }
    if (ub) y <- lapply(y, unbag)
    y
}


