#' Diversity Statistics
#' 
#' Transcript apply diversity/richness indices.
#' 
#' @param text.var The text variable.         
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.                       
#' @return Returns a dataframe of various diversity related indices for Shannon, 
#' collision, Berger Parker and Brillouin.
#' @details These are the formulas used to calculate the indices:
#' 
#' \bold{Shannon index:}
#' \deqn{H_1(X)=-\sum\limits_{i=1}^R{p_i};log;p_i}
#' 
#' Shannon, C. E. (1948). A mathematical theory of communication. Bell System \cr
#' 
#' \bold{Simpson index:}
#' \deqn{D=\frac{\sum_{i=1}^R{p_i};n_i(n_i -1)}{N(N-1))}}
#' 
#' Simpson, E. H. (1949). Measurement of diversity. Nature 163, p. 688 \cr
#' 
#' \bold{Collision entropy:}
#' \deqn{H_2(X)=-log\sum_{i=1}^n{p_i}^2}
#' 
#' Renyi, A. (1961). On measures of information and entropy. Proceedings of the
#' 4th Berkeley Symposium on Mathematics, Statistics and Probability, 1960.  
#' pp. 547-5661. \cr
#' 
#' \bold{Berger Parker index:}
#' \deqn{D_{BP}=\frac{N_{max}}{N}}
#' 
#' Berger, W. H., & Parker, F. L.(1970). Diversity of planktonic Foramenifera in 
#' deep sea sediments. Science 168, pp. 1345-1347. \cr
#' 
#' \bold{Brillouin index:}
#' \deqn{H_B=\frac{ln(N!)-\sum{ln(n_1)!}}{N}}
#' 
#' Magurran, A. E. (2004). Measuring biological diversity. Blackwell.
#' @keywords diversity
#' @references https://arxiv.org/abs/physics/0512106
#' @export
#' @examples
#' \dontrun{
#' div.mod <- with(mraja1spl, diversity(dialogue, list(sex, died, fam.aff)))
#' colsplit2df(div.mod)
#' plot(div.mod, high = "red", low = "yellow")
#' plot(div.mod, high = "red", low = "yellow", values = TRUE)
#' }
diversity <-
function (text.var, grouping.var=NULL){
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
    DF <- stats::na.omit(data.frame(group = grouping,
        text.var = as.character(text.var), stringsAsFactors = FALSE))
    z <- split(DF[, "text.var"], DF[, "group"])
    y <- lapply(z, rm_stopwords, stopwords=NULL, unlist=TRUE, strip = TRUE)
    w <- lapply(y, function(x) data.frame(table(x)))
    v <- do.call(rbind, lapply(w, function(x) RICH(x[, 2])))
    o <- data.frame(rownames(v), v)
    o <- o[order(o[, 1]), ]
    rownames(o) <- NULL
    colnames(o)[1] <- G
    class(o) <- c("diversity", "data.frame")
    return(o)
}

#' Prints a diversity object
#' 
#' Prints a diversity object.
#' 
#' @param x The diversity object
#' @param digits Number of decimal places to print. 
#' @param \ldots ignored
#' @method print diversity
#' @export
print.diversity <-
function(x, digits = 3, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    class(x) <- "data.frame"
    if (!is.null(digits)) {
        x[, -c(1:2)] <- lapply(x[, -c(1:2)], round, digits = digits) 
    }
    print(x)
    options(width=WD)  
}

#' Plots a diversity object
#' 
#' Plots a diversity object.
#' 
#' @param x The diversity object
#' @param \ldots Other arguments passed to \code{qheat}
#' @method plot diversity
#' @export
plot.diversity <- function(x, ...) {
    class(x) <- "data.frame"
    qheat(x, ...)
}

## Helper functions for diersity:
shannon <- function(num.var, digits = 3){
    N <- sum(num.var)
    P <- num.var/N
    logP <- log(P)
    sum(-(P * logP))
}
simpson <- function(num.var, digits = 3){
    N <- sum(num.var)
    FUN <- function(x) x*(x-1)  
    n.sum <- sum(FUN(num.var))
    1 - (n.sum/(N*(N-1)))
}
collision <- function(num.var, digits=3){
    N <- sum(num.var)
    P <- num.var/N
    Epi2 <- sum(P^2)
    -log(Epi2)
}
berger_parker <- function(num.var, digits=3){
    N <- sum(num.var)
    Nm <- max(num.var)
    Nm/N
}
brillouin <- function(num.var, digits=3){
    N <- sum(num.var)
    (lfactorial(N) - sum(lfactorial(num.var)))/N
}
RICH <- function(x){
    c(wc=sum(x), simpson=simpson(x), 
        shannon = shannon(x), 
        collision = collision(x),
        berger_parker = berger_parker(x),
        brillouin = brillouin(x)
    )
}
