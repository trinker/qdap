#' Transcript Apply Linsear Write Readability Statistic
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param grouping.var %% ~~Describe \code{grouping.var} here~~
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
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (text.var, grouping.var = NULL) 
#' {
#'     G <- if (is.null(grouping.var)) {
#'         "all"
#'     }
#'     else {
#'         if (is.list(grouping.var)) {
#'             m <- unlist(as.character(substitute(grouping.var))[-1])
#'             m <- sapply(strsplit(m, "$", fixed = TRUE), function(x) x[length(x)])
#'             paste(m, collapse = "&")
#'         }
#'         else {
#'             G <- as.character(substitute(grouping.var))
#'             G[length(G)]
#'         }
#'     }
#'     grouping <- if (is.null(grouping.var)) {
#'         rep("all", length(text.var))
#'     }
#'     else {
#'         if (is.list(grouping.var) & length(grouping.var) > 1) {
#'             apply(data.frame(grouping.var), 1, function(x) {
#'                 if (any(is.na(x))) {
#'                   NA
#'                 }
#'                 else {
#'                   paste(x, collapse = ".")
#'                 }
#'             })
#'         }
#'         else {
#'             unlist(grouping.var)
#'         }
#'     }
#'     text <- as.character(text.var)
#'     DF <- na.omit(data.frame(group = grouping, text.var = text, 
#'         stringsAsFactors = FALSE))
#'     DF$word.count <- word.count(DF$text.var)
#'     DF$tot.n.sent <- 1:nrow(DF)
#'     DF <- DF[with(DF, order(group, DF$tot.n.sent)), ]
#'     DF$read.gr <- unlist(by(DF$word.count, DF$group, g))
#'     LIST <- names(which(tapply(DF$word.count, DF$group, function(x) {
#'         max(cumsum(x))
#'     }) >= 100))
#'     DF2 <- subset(DF, group %in% LIST & read.gr != "NA")
#'     DF2$group <- DF2$group[, drop = TRUE]
#'     DF2$sub <- with(DF2, paste(group, read.gr, sep = "_"))
#'     DF2b <- DF2[order(DF2$sub), ]
#'     DF2b$cumsum.gr <- unlist(tapply(DF2$word.count, DF2$sub, 
#'         cumsum))
#'     DF2 <- DF2b[order(DF2b$group, DF2b$tot.n.sent), ]
#'     DF2$wo.last <- DF2$cumsum.gr - DF2$word.count
#'     DF2$hun.word <- 100 - DF2$wo.last
#'     DF2$frac.sent <- ifelse(DF2$hun.word/DF2$word.count > 1, 
#'         1, round(DF2$hun.word/DF2$word.count, digits = 3))
#'     DF3b <- unique(data.frame(sub = DF2$sub, group = DF2$group))
#'     DF3 <- data.frame(sub = DF2$sub, group = DF2$group, text.var = DF2$text.var, 
#'         frac.sent = DF2$frac.sent)
#'     CHOICES <- tapply(as.character(DF3b$sub), DF3b$group, function(x) {
#'         sample(x, 1, replace = FALSE)
#'     })
#'     CHOICES <- as.character(unlist(CHOICES))
#'     DF4 <- DF3[which(DF3$sub %in% CHOICES), ]
#'     DF4$sub <- DF4$sub[, drop = TRUE]
#'     FUN <- function(x) paste(as.character(unlist(x)), collapse = " ")
#'     DF5 <- aggregate(text.var ~ sub + group, DF4, FUN)
#'     sent.per.100 <- as.data.frame(tapply(DF2$frac.sent, DF2$sub, 
#'         sum))
#'     names(sent.per.100) <- "x"
#'     DF5$sent.per.100 <- sent.per.100[as.character(DF5$sub), "x"]
#'     hun.grab <- function(x) paste(unblanker(unlist(word.split(reducer(unlist(strip(x))))))[1:100], 
#'         collapse = " ")
#'     DF5$SYL.LIST <- lapply(DF5$text.var, function(x) unlist(syllable.count(hun.grab(x))$syllables))
#'     DF5$hard_easy_sum <- lapply(DF5$SYL.LIST, function(x) sum(ifelse(x >= 
#'         3, 3, 1)))
#'     DF5$HE_tsent_ratio <- unlist(DF5$hard_easy_sum)/DF5$sent.per.100
#'     DF5$Linsear_Write <- ifelse(DF5$sent.per.100 > 20, round(DF5$HE_tsent_ratio/2, 
#'         digits = 2), round((DF5$HE_tsent_ratio - 2)/2, digits = 2))
#'     DF5 <- DF5[, c(2, 4, 6, 8)]
#'     names(DF5) <- c(G, "sent.per.100", "hard_easy_sum", "Linsear_Write")
#'     return(DF5)
#'   }
#' 
linsear_write <-
function(text.var, grouping.var = NULL, rm.incomplete = FALSE, ...) {
    G <- if(is.null(grouping.var)) {
             "all"
         } else {
             if (is.list(grouping.var)) {
                 m <- unlist(as.character(substitute(grouping.var))[-1])
                 m <- sapply(strsplit(m, "$", fixed=TRUE), 
                     function(x) x[length(x)])
                 paste(m, collapse="&")
             } else {
                  G <- as.character(substitute(grouping.var))
                  G[length(G)]
             }
         }
    grouping <- if(is.null(grouping.var)){
                     rep("all", length(text.var))
                 } else {
                     if(is.list(grouping.var) & length(grouping.var)>1) {
                         apply(data.frame(grouping.var), 1, function(x){
                             if(any(is.na(x))){NA}else{paste(x, collapse = ".")
                                 }
                             }
                         )
                     } else {
                        unlist(grouping.var)
                     } 
                 } 
    text <- as.character(text.var)
    DF <- na.omit(data.frame(group = grouping, text.var = text, 
        stringsAsFactors = FALSE))
    if (rm.incomplete) {
        DF <- endf(dataframe = DF, text.var = text.var, ...)
    }
    DF$word.count <- word.count(DF$text.var)
    DF$tot.n.sent <- 1:nrow(DF)
    DF <- DF[with(DF, order(group, DF$tot.n.sent)), ]
    DF$read.gr <- unlist(by(DF$word.count, DF$group, partition))
    LIST <- names(which(tapply(DF$word.count, DF$group, function(x) {
            max(cumsum(x))
        }
    ) >= 100))
    DF2 <- subset(DF, group %in% LIST & read.gr != "NA")
    DF2$group <- DF2$group[, drop = TRUE]
    DF2$sub <- with(DF2, paste(group, read.gr, sep = "_"))
    DF2b <- DF2[order(DF2$sub), ]
    DF2b$cumsum.gr <- unlist(tapply(DF2$word.count, DF2$sub, cumsum))
    DF2 <- DF2b[order(DF2b$group, DF2b$tot.n.sent), ]
    DF2$wo.last <- DF2$cumsum.gr - DF2$word.count
    DF2$hun.word <- 100 - DF2$wo.last
    DF2$frac.sent <- ifelse(DF2$hun.word/DF2$word.count > 1, 1, 
        round(DF2$hun.word/DF2$word.count, digits = 3))  
    DF3b <- unique(data.frame(sub = DF2$sub, group = DF2$group))
    DF3 <- data.frame(sub = DF2$sub, group = DF2$group, text.var = 
        DF2$text.var, frac.sent = DF2$frac.sent)
    CHOICES <- tapply(as.character(DF3b$sub), DF3b$group, function(x) {
            sample(x, 1, replace = FALSE)
        }
    )
    CHOICES <- as.character(unlist(CHOICES))
    DF4 <- DF3[which(DF3$sub %in% CHOICES), ]
    DF4$sub <- DF4$sub[, drop = TRUE]
    FUN <- function(x) paste(as.character(unlist(x)), collapse = " ")
    DF5 <- aggregate(text.var ~ sub + group, DF4, FUN)
    sent.per.100 <- as.data.frame(tapply(DF2$frac.sent, DF2$sub, 
        sum))
    names(sent.per.100) <- "x"
    DF5$sent.per.100 <- sent.per.100[as.character(DF5$sub), "x"]
    hun.grab <- function(x) paste(unblanker(unlist(word.split(reducer(
        unlist(strip(x))))))[1:100], collapse = " ")
    DF5$SYL.LIST <- unlist(lapply(DF5$text.var, function(x) unlist(syllable.count(
        hun.grab(x))$syllables)))
    DF5$hard_easy_sum <- unlist(lapply(DF5$SYL.LIST, function(x) sum(ifelse(x >= 
      3, 3, 1))))
    DF5$HE_tsent_ratio <- unlist(DF5$hard_easy_sum)/DF5$sent.per.100
    DF5$Linsear_Write <- ifelse(DF5$sent.per.100 > 20, 
        round(DF5$HE_tsent_ratio/2, digits = 2), 
        round((DF5$HE_tsent_ratio - 2)/2, digits = 2))
    DF5 <- DF5[, c(2, 4, 6, 8)]
    names(DF5) <- c(G, "sent.per.100", "hard_easy_sum", "Linsear_Write")
    return(DF5)
}
