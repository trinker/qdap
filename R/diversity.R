#' Diversity Statistics
#' 
#' Transcript apply diversity statistics
#' 
#' @param text.var The text variable.         
#' @param grouping.var The grouping variables.  Default NULL generates one output for all text.  Also takes a single grouping variable or a list of 1 or more grouping variables.    
#' @param digits Number of decimal places to round.                        
#' @return Returns a dataframe of various diversity related indices for Shannon, 
#' collision, Berger Parker and Brillouin
#' @keywords diversity
#' @export
#' @examples
#' colsplit2df(with(mraja1spl, diversity(dialogue, list(sex, died, fam.aff))))
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
