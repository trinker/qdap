#' Generate Word Frequency Data Frame
#' 
#' Generate a word frequency data frame by grouping variable
#' 
#' @aliases word.freq.df wfdf
#' @param text.var A text variable or word frequency matrix object.
#' @param grouping.var The grouping variables.  Default NULL generates one word list for all text.  Also takes a single grouping variable or a list of 1 or more grouping variables.
#' @param stopwords A vector of stop words to remove.
#' @param margins logical. If true provides grouping.var and word variable totals
#' @param output output type (either "proportion", "proportion" or "percent")
#' @param digits integer indicating the number of decimal places (round) or significant digits (signif) to be used. Negative values are allowed
#' @return Returns a word frequency of the class data.frame
#' @seealso \code{\link[qdap]{word.freq.matrix}}
#' @examples
#' \dontrun{
#' with(DATA, wfdf(state, list(sex, adult)))
#' with(DATA, wfdf(state, person))
#' }
word.freq.df <-
function(text.var, grouping.var = NULL, stopwords = NULL,
    margins = FALSE, output = "raw", digits = 2){
    grouping.var <- if (is.list(grouping.var) & length(grouping.var)>1) {
        apply(data.frame(grouping.var), 1, function(x){
            if (any(is.na(x)))NA else paste(x, collapse = ".")
            }
        )
    } else {
        grouping.var
    }
    bl <- split(text.var, grouping.var)
    x <- lapply(bl, bag.o.words)
    tabs <- lapply(x, function(x) as.data.frame(table(x)))
    tabs <- tabs[sapply(tabs, nrow)!=0]
    lapply(seq_along(tabs), function(x) {
        names(tabs[[x]]) <<- c("Words", names(tabs)[x])  
        }
    ) 
    DF <- merge.all(tabs, by="Words", 0)
    DF <- DF[order(DF$Words), ]
    DF[, "Words"] <- as.character(DF[, "Words"])
    DF[, -1] <- sapply(DF[, -1], function(x) as.numeric(as.character(x)))
    if(!is.null(stopwords)) DF <- DF[!DF[, "Words"] %in% stopwords , ]
    rownames(DF) <- 1:nrow(DF)
    pro <- function(x) x/sum(x)       #helper function 1
    per <- function(x) 100*(x/sum(x)) #helper function 2     
    if (output != "raw") DF2 <- DF
    DF <- switch(output,
         raw = DF,
         proportion = {data.frame(DF[, 1, drop = FALSE], 
             sapply(DF[, -1], pro))},
         prop = data.frame(DF[, 1, drop = FALSE], sapply(DF[, -1], pro)),
         percent = data.frame(DF[, 1, drop = FALSE], sapply(DF[, -1], per)),
         per = data.frame(DF[, 1, drop = FALSE], sapply(DF[, -1], per))
    )
    if (margins){
        if (output == "raw"){
            DF <- rbind(DF, c(NA, colSums(DF[, -1])))
            DF[nrow(DF), 1] <- "TOTAL.WORDS ->"
            DF[, "TOTAL.USES"] <- rowSums(DF[, -1])
        } else {
            X <- rowSums(DF2[, -1])
            DF[, "TOTAL.USES"] <- c(X/sum(X))   
            X2 <- colSums(DF2[, -1])  
            DF <- rbind(DF, c(NA, X2/sum(X), 1))
            DF[nrow(DF), 1] <- "TOTAL.WORDS ->"
        }
    }
    if (!output == "raw") {
        DF2 <- lapply(DF[, -1], function(x) round(x, digits = digits))
        DF <- data.frame(DF[, 1, drop = FALSE], DF2)
    }
    if (!margins & output == "raw") {
        comment(DF) <- "t.df" 
    } else {
            if (margins & output == "raw") {
                comment(DF) <- "m.df"
            } else {
                comment(DF) <- "f.df"
        }
    }
    return(DF)
}

wfdf <- word.freq.df
