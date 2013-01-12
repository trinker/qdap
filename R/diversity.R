#' Diversity Statistics
#' 
#' Transcript apply diversity/richness indices.
#' 
#' @param text.var The text variable.         
#' @param grouping.var The grouping variables.  Default NULL generates one 
#' output for all text.  Also takes a single grouping variable or a list of 1 or 
#' more grouping variables.    
#' @param digits Number of decimal places to round.                        
#' @return Returns a dataframe of various diversity related indices for Shannon, 
#' collision, Berger Parker and Brillouin.
#' @details These are the formulas used to calculate the indices:
#' 
#' Shannon index:
#' \deqn{H_1(X)=-\sum\limits_{i=1}^R{p_i};log;p_i}
#' 
#' Shannon, C. E. (1948). A mathematical theory of communication. Bell System \cr
#' 
#' Simpson index:
#' \deqn{D=\frac{\sum_{i=1}^R{p_i};n_i(n_i -1)}{N(N-1))}}
#' 
#' Simpson, E. H. (1949). Measurement of diversity. Nature 163, p. 688 \cr
#' 
#' Collision entropy:
#' \deqn{H_2(X)=-log\sum_{i=1}^n{p_i}^2}
#' 
#' Renyi, A. (1961). On measures of information and entropy. Proceedings of the
#' 4th Berkeley Symposium on Mathematics, Statistics and Probability, 1960.  
#' pp. 547-5661. \cr
#' 
#' Berger Parker index:
#' \deqn{D_{BP}=\frac{N_{max}}{N}}
#' 
#' Berger, W. H., & Parker, F. L.(1970). Diversity of planktonic Foramenifera in 
#' deep sea sediments. Science 168, pp. 1345-1347. \cr
#' 
#' Brillouin index:
#' \deqn{H_B=\frac{ln(N!)-\sum{ln(n_1)!}}{N}}
#' 
#' Magurran, A. E. (2004). Measuring biological diversity. Blackwell.
#' @keywords diversity
#' @export
#' @examples
#' \dontrun{
#' colsplit2df(with(mraja1spl, diversity(dialogue, list(sex, died, fam.aff))))
#' }
diversity <-
function (text.var, grouping.var=NULL, digits = 3){
    NAME <- if (is.null(grouping.var)) {
        "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed = TRUE), 
                function(x) x[length(x)])
            paste(m, collapse = "&")
        } else {
            G <- as.character(substitute(grouping.var))
            G[length(G)]
        }
    }
    grouping <- if(is.null(grouping.var)){
        rep("all", length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            apply(data.frame(grouping.var), 1, function(x){
                if (any(is.na(x))){
                        NA
                    } else {
                        paste(x, collapse = ".")
                    }
                }
            )
        } else {
            unlist(grouping.var)
        } 
    }
    DF <- na.omit(data.frame(group = grouping,
        text.var = as.character(text.var), stringsAsFactors = FALSE))
    shannon <- function(num.var, digits = 3){
        N <- sum(num.var)
        P <- num.var/N
        logP <- log(P)
        return(round(sum(-(P * logP)), digits = digits))
    }
    simpson <- function(num.var, digits = 3){
        N <- sum(num.var)
        FUN <- function(x) x*(x-1)  
        n.sum <- sum(FUN(num.var))
        sim <- 1 - (n.sum/(N*(N-1)))
        return(round(sim, digits = digits))
    }
    collision <- function(num.var, digits=3){
        N <- sum(num.var)
        P <- num.var/N
        Epi2 <- sum(P^2)
        return(round((-log(Epi2)), digits = digits))
    }
    berger_parker <- function(num.var, digits=3){
        N <- sum(num.var)
        Nm <- max(num.var)
        return(round(Nm/N, digits = digits))
    }
    brillouin <- function(num.var, digits=3){
        N <- sum(num.var)
        br <- (lfactorial(N) - sum(lfactorial(num.var)))/N
        return(round(br, digits = digits))
    }
    RICH <- function(x){
        c(wc=sum(x), simpson=simpson(x, digits = digits), 
            shannon = shannon(x, digits = digits), 
            collision = collision(x, digits = digits),
            berger_parker = berger_parker(x, digits = digits),
            brillouin = brillouin(x, digits = digits)
        )
    }
    z <- split(DF[, "text.var"], DF[, "group"])
    y <- lapply(z, stopwords, stopwords=NULL, unlist=TRUE, strip = TRUE)
    w <- lapply(y, function(x) data.frame(table(x)))
    v <- do.call(rbind, lapply(w, function(x) RICH(x[, 2])))
    o <- data.frame(rownames(v), v)
    o <- o[order(o[, 1]), ]
    rownames(o) <- NULL
    colnames(o)[1] <- NAME
    return(o)
}
