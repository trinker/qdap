#' Proximity Matrix Between Words
#' 
#' Generate proximity measures to ascertain a mean distance measure between 
#' word uses.
#' 
#' @param text.var The text variable.
#' @param terms  A vector of quoted terms.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @scale logical.  If \code{TRUE} the mean distance is given in standardized 
#' units rather than in sentences as units.
#' @return Returns a matrix of proximity measures in the unit of average 
#' sentences between words.
#' @note The match.terms is character sensitive.  Spacing is an important way 
#' to grab specific words and requires careful thought.  Using "read" will find 
#' the words "bread", "read" "reading", and "ready".  If you want to search 
#' for just the word "read" you'd supply a vector of c(" read ", " reads", 
#' " reading", " reader").  
#' @export
#' @examples
#' wrds <- word_list(pres_debates2012$dialogue, 
#'     stopwords = c("it's", "that's", Top200Words))
#' wrds2 <- tolower(sort(wrds$rfswl[[1]][, 1]))
#' 
#' (x <- with(pres_debates2012, word_proximity(dialogue, wrds2)))
#' plot(x)
#' 
#' (x2 <- with(pres_debates2012, word_proximity(dialogue, wrds2, person)))
word_proximity <- function(text.var, terms, grouping.var = NULL, scale = TRUE) {
 
## check the grep use
## describe the non-symetrical matrix (look at cm_distance)
## pvals???
  
    if(!is.null(grouping.var)){
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- paste2(grouping.var)
        } else {
            grouping <- unlist(grouping.var)
        } 
    } 

    inds <- seq_along(terms)
    ncols <- max(inds)
    mat <- matrix(rep(NA, ncols^2), ncols)
    terms <- tolower(terms)
    colnames(mat) <- rownames(mat) <- terms

    if (is.null(grouping.var)) {
        out <- list(word_proximity_helper(text.var, terms, tofill = mat, inds = inds))
    } else {
        splits <- split(text.var, grouping)
        out <- suppressWarnings(lapply(splits, word_proximity_helper, terms = terms, tofill = mat, inds = inds))
    }

    out <- lapply(out, function(x) {
        rms <- apply(x, 1, function(y) !all(is.na(y)))
        x[rms, rms]
    })

    if (scale) {
        out <- lapply(out, scale2)
    } 
    class(out) <- c("word_proximity", class(out))
    out
}


#' Prints a word_proximity object
#' 
#' Prints a word_proximity object
#' 
#' @param x The word_proximity object
#' @param digits The number of duguts to print
#' @param \ldots ignored
#' @S3method print word_proximity
#' @method print word_proximity
print.word_proximity <-
function(x, digits = 3, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    class(x) <- "list"
    x <- lapply(x, function(y) round(y, digits = digits))
    if (length(x) == 1) {
        x <- x[[1]]
    }
    print(x)
    options(width=WD)
}


#' Plots a word_proximity object
#' 
#' Plots a word_proximity object.
#' 
#' @param x The word_proximity object
#' @param label logical.  If \code{TRUE} the cells of the heat map plot will be 
#' labeled with count and proportional values.
#' @param lab.digits Integer values specifying the number of digits to be 
#' printed if \code{label} is \code{TRUE}.
#' @param \ldots Other arguments passed to qheat.
#' @method plot word_proximity
#' @S3method plot word_proximity
plot.word_proximity <- function(x, label = TRUE, lab.digits = 3, high="blue", 
    low="white", grid=NULL, ...) {

    class(x) <- "list"
    if (length(x) == 1) {
        x <- x[[1]]
    } else {
        stop("plot method for `word_proximity` works when `grouping.var` not specified.\n",
            "  Use `qheat` and gridExtra package for multiple grouping variables.")
    }
    qheat(x, diag.na = TRUE, diag.value = "", by.column = NULL, 
        values = TRUE, digits = lab.digits, high = high, 
        low = low, grid = grid, ...)
}



scale2 <- function(x) (x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)

word_proximity_helper <- function(text.var, terms, tofill, inds) {

    text.var <- spaste(strip(na.omit(sent_detect(text.var)), 
         apostrophe.remove = FALSE))

    locs <- lapply(terms, grepl, text.var, fixed = TRUE, ignore.case = FALSE)
    locs <- lapply(locs, which)
    
    for (i in inds){
        for (j in inds){
            tofill[i, j] <- ifelse(i == j, NA, 
                mean(min_dist(locs[[i]], locs[[j]])))
        }   
    }
    tofill
}



min_dist <- function(x, y) {
    unlist(lapply(x, function(x2) {
        min(abs(x2 - y))
    }))
}


