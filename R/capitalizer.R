#' Order a data frame by its columns.
#'
#' This function completes the subsetting, transforming and ordering triad
#' with a function that works in a similar way to \code{\link{subset}} and 
#' \code{\link{transform}} but for reordering a data frame by its columns.
#' This saves a lot of typing!
#'
#' @param df data frame to reorder
#' @param ... expressions evaluated in the context of \code{df} and 
#'   then fed to \code{\link{order}}
#' @keywords manip
#' @export
#' @examples
#' mtcars[with(mtcars, order(cyl, disp)), ]
#' arrange(mtcars, cyl, disp)
#' arrange(mtcars, cyl, desc(disp))
capitalizer <-
function(text, caps.list = NULL, I.list = TRUE, 
    no.apostrophe = FALSE) {
    I_list <- c("I'm", "I'll", "I'd", "I've", "I")
    IDF <- data.frame(from1 = sapply(I_list, function(x) strip(x, 
        apostrophe.remove = TRUE)), from2 = sapply(I_list, strip), 
        to = I_list)
    
    idf <- if (no.apostrophe == FALSE) {
        IDF[-1]
    } else {
        IDF2 <- IDF[-2]
        names(IDF2) <- c("from2", "to")
        data.frame(rbind(IDF[-1], IDF2))
    }
    names(idf) <- c("from", "to")
    rownames(idf) <- 1:nrow(idf)
    
    idf <- if (I.list) 
        idf else NULL
    names <- data.frame(from = tolower(caps.list), to = gsub("(\\w)(\\w*)", 
        "\\U\\1\\L\\2", tolower(caps.list), perl = T))
    names2 <- data.frame(from = paste(names$from, "'s", sep = ""), 
        to = paste(names$to, "'s", sep = ""))
    idf <- rbind(idf, names, names2)
    idf$from <- as.character(idf$from)
    idf$to <- as.character(idf$to)
    subber <- function(x) ifelse(x %in% idf$from, idf[match(x, 
        idf$from), "to"], x)
    unlist(lapply(text, subber))
}
