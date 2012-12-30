#' Aggregate a wfm or wfdf Object By Supplied Word List(s)
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param wfdf %% ~~Describe \code{wfdf} here~~
#' @param word.lists %% ~~Describe \code{word.lists} here~~
#' @param matrix %% ~~Describe \code{matrix} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' y <- wfdf(DATA$state, DATA$person)
#' WL1 <- c(y[, 1])                                                                      
#' WL2 <- list(c("read", "the", "a"), c("you", "your", "your're"))                       
#' WL3 <- list(bob = c("read", "the", "a"), yous = c("you", "your", "your're"))          
#' WL4 <- list(bob = c("read", "the", "a"), yous = c("a", "you", "your", "your're"))     
#' WL5 <- list(yous = c("you", "your", "your're"))                                       
#' WL6 <- list(c("you", "your", "your're"))  #no name so will be called words 1          
#' WL7 <- c("you", "your", "your're")                                                    
#'                                                                                       
#' x <- wfdf(DATA$state, DATA$person)           #raw no margins (will work)       
#' y <- wfdf(DATA$state, DATA$person)       #raw with margin (will work) 
#' z <- wfdf(DATA$state, DATA$person, output = "proportion") #porportion (will not work)  
#' wfdf.combine(z, WL2) #Won't work not a raw frequency matrix                           
#' wfdf.combine(x, WL2) #Works (raw and no margins)                                      
#' wfdf.combine(y, WL2) #Works (raw with margins)                                        
#' wfdf.combine(y, c("you", "your", "your're"))                                          
#' wfdf.combine(y, WL1)                                                                  
#' wfdf.combine(y, WL3)                                                                  
#' wfdf.combine(y, WL4) #Error b/c there's overlapping words in the word lists           
#' wfdf.combine(y, WL5)                                                                  
#' wfdf.combine(y, WL6)                                                                  
#' wfdf.combine(y, WL7)                                                                  
#'                                                                                                                                                                                                                              
#' worlis <- c("you", "it", "it's", "no", "not", "we")                                                                
#' y <- wfdf(DATA$state, list(DATA$sex, DATA$adult), margins = TRUE)                         
#' z <- wfdf.combine(y, worlis, matrix = TRUE)                                           
#'                                                                                       
#' chisq.test(z)                                                                         
#' chisq.test(wfm(wfdf = y)) 
wfdf.combine <-
function(wfdf, word.lists, matrix = FALSE){
    suppressWarnings(if (is.list(word.lists) & length(word.lists) > 1 & 
        any(Reduce("%in%", word.lists))) {
        stop("overlapping words in word.lists")
    })
    if (comment(wfdf) == "t.df") {
        wfdf <- wfdf
    } else {
        if (comment(wfdf) %in% c("true.matrix", "m.df")) { 
            wfdf <- wfdf[-nrow(wfdf), -ncol(wfdf)]
        } else {
            stop("Object must be a raw word frequency data frame")
        }
    }
    if (is.list(word.lists) & is.null(names(word.lists))){
        NAMES <- paste("words", 1:length(word.lists))
    } else {
        if (is.list(word.lists) & !is.null(names(word.lists))){
            NAMES <- names(word.lists)
        } else {
            if (is.vector(word.lists)) {
                    G <- as.character(substitute(word.lists))
                if (G[1] == "c") {
                    NAMES <- "words"
                } else {
                    NAMES <- G[length(G)]
                }
            } else {
                stop("incorrect word.list argument")
            }
        }
    }
    if(!is.list(word.lists)) word.lists <- list(word.lists)
    j <- lapply(word.lists, function(x) wfdf[wfdf[, 1] %in% x, -1])
    if (!all(wfdf[, 1] %in% unlist(word.lists))) {
        j[[length(j) + 1]] <- wfdf[!wfdf[, 1] %in% unlist(word.lists), -1]
    }
    k <- lapply(j, function(x) if(is.vector(x)) { x } else { colSums(x)})
    m <- do.call("rbind", k)
    rownames(m) <- 1:nrow(m)
    NAMES <- if (all(wfdf[, 1] %in% unlist(word.lists))) {
        NAMES 
    } else {
        c(NAMES, "else.words")
    }
    DFF <- data.frame(word.group = NAMES, m)
    if (matrix) {
        DFF2 <- as.matrix(DFF[, -1])
        rownames(DFF2) <- as.character(DFF[, 1])
        DFF <- DFF2
    }
    comment(DFF) <- ifelse(!matrix, "t.df", "true.matrix")
    return(DFF)
}
