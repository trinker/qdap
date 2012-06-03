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
