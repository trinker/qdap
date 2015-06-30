#' Proximity Matrix Between Words
#' 
#' \code{word_proximity} - Generate proximity measures to ascertain a mean 
#' distance measure between word uses.
#' 
#' @param text.var The text variable.
#' @param terms  A vector of quoted terms.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param parallel logical.  If \code{TRUE} attempts to run the function on 
#' multiple cores.  Note that this may not mean a speed boost if you have one 
#' core or if the data set is smaller as the cluster takes time to create.  
#' @param cores The number of cores to use if \code{parallel = TRUE}.  Default 
#' is half the number of available cores.
#' @return Returns a list of matrices of proximity measures in the unit of average 
#' sentences between words (defaults to scaled).
#' @note The match.terms is character sensitive.  Spacing is an important way 
#' to grab specific words and requires careful thought.  Using "read" will find 
#' the words "bread", "read" "reading", and "ready".  If you want to search 
#' for just the word "read" you'd supply a vector of c(" read ", " reads", 
#' " reading", " reader").  
#' @details Note that row names are the first word and column names are the 
#' second comparison word. The values for Word A compared to Word B will not 
#' be the same as Word B compared to Word A. This is because, unlike a true 
#' distance measure, \code{word_proximity}'s matrix is asymmetrical. 
#' \code{word_proximity} computes the distance by taking each sentence position 
#' for Word A and comparing it to the nearest sentence location for Word B.
#' @seealso \code{\link[qdap]{word_proximity}}
#' @rdname word_proximity
#' @importFrom qdapTools v_outer
#' @importFrom parallel parLapply makeCluster detectCores stopCluster clusterEvalQ clusterExport
#' @export
#' @examples
#' \dontrun{
#' wrds <- word_list(pres_debates2012$dialogue, 
#'     stopwords = c("it's", "that's", Top200Words))
#' wrds2 <- tolower(sort(wrds$rfswl[[1]][, 1]))
#' 
#' (x <- with(pres_debates2012, word_proximity(dialogue, wrds2)))
#' plot(x)
#' plot(weight(x))
#' plot(weight(x, "rev_scale_log"))
#' 
#' (x2 <- with(pres_debates2012, word_proximity(dialogue, wrds2, person)))
#' 
#' ## The spaces around `terms` are important
#' (x3 <- with(DATA, word_proximity(state, spaste(qcv(the, i)))))
#' (x4 <- with(DATA, word_proximity(state, qcv(the, i))))
#' }
word_proximity <- function(text.var, terms, grouping.var = NULL, parallel = TRUE, 
    cores = parallel::detectCores()/2) {
  
    if(!is.null(grouping.var)){
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- paste2(grouping.var)
        } else {
            grouping <- unlist(grouping.var)
        } 
    } 

    inds <- seq_along(terms)
    ncols <- max(inds)
    terms <- tolower(terms)

    if (is.null(grouping.var)) {
        out <- list(word_proximity_helper(text.var, terms, inds = inds))
    } else {

        splits <- split(text.var, grouping)

        if (parallel && cores > 1){

            cl <- makeCluster(mc <- getOption("cl.cores", cores))
            vars <- c("splits", "word_proximity_helper", "terms", "inds",
                "spaste", "strip", "sent_detect", "v_outer", "locsfun",
                "min_dist")
            
            clusterExport(cl=cl, varlist=vars, envir = environment())
            
            out <- suppressWarnings(parLapply(cl, splits, word_proximity_helper, 
                terms = terms, inds = inds))
     
            stopCluster(cl)
        } else {

            out <- suppressWarnings(lapply(splits, word_proximity_helper, 
                terms = terms, inds = inds))
        }

    }

    out <- lapply(out, function(x) {
        rms <- apply(x, 1, function(y) !all(is.na(y)))
        new <- x[rms, rms]
        new[is.nan(new)] <- 0
        new
    })

    attributes(out) <- list(
        class = c("word_proximity", class(out)), 
        weight = c("none"),
        names = attributes(out)[["names"]] 
    )
    out
}

#' Prints a word_proximity object
#' 
#' Prints a word_proximity object
#' 
#' @param x The word_proximity object
#' @param digits The number of digits to print
#' @param \ldots ignored
#' @export
#' @method print word_proximity
print.word_proximity <-
function(x, digits = NULL, ...) {
    WD <- options()[["width"]]
    if (is.null(digits)) {
        if(attributes(x)[["weight"]] == "none") {
            digits <- 1
        } else {
            digits <- 3   
        }
    }
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
#' @param low The color to be used for lower values.
#' @param high The color to be used for higher values.
#' @param grid The color of the grid (Use \code{NULL} to remove the grid).  
#' @param \ldots Other arguments passed to qheat.
#' @method plot word_proximity
#' @export
plot.word_proximity <- function(x, label = TRUE, lab.digits = NULL, high="red", 
    low="white", grid=NULL, ...) {

    if (is.null(lab.digits)) {
        if(attributes(x)[["weight"]] == "none") {
            lab.digits <- 1
        } else {
            lab.digits <- 3   
        }
    }
    
    class(x) <- "list"
    if (length(x) == 1) {
        x <- x[[1]]
    } else {
        stop("plot method for `word_proximity` works when `grouping.var` not specified.\n",
            "  Use `qheat` and gridExtra package for multiple grouping variables.")
    }

    qheat(t(x), diag.na = TRUE, diag.values = "", by.column = NULL, 
        values = TRUE, digits = lab.digits, high = high, 
        low = low, grid = grid, ...)
}


word_proximity_helper <- function(text.var, terms, inds) {

    text.var <- spaste(strip(stats::na.omit(sent_detect(text.var)), 
         apostrophe.remove = FALSE))

    locs <- lapply(terms, function(x) which(grepl(x, text.var, fixed = TRUE, 
        ignore.case = FALSE))) 
    grab <- !sapply(locs, identical, integer(0))
    locs <- locs[grab]
    mat <- suppressWarnings(v_outer(locs, locsfun))
    colnames(mat) <- rownames(mat) <- terms[grab]
    diag(mat) <- NA
    mat
}

locsfun <- function(x, y) mean(abs(min_dist(x, y)))


min_dist <- function(xw, yw) {
   i <- findInterval(xw, yw, all.inside = TRUE)
   o <- pmin(xw - yw[i], yw[i+1L] - xw, na.rm = TRUE)
   if(identical(o, integer(0))) return(0)
   o
}


#' \code{weight} - weight Method for word_proximity.
#' 
#' @param x An object to be weighted.
#' @param type A weighting type of: c(\code{"scale_log"}, \code{"scale"}, 
#' \code{"rev_scale"}, \code{"rev_scale_log"}, \code{"log"}, \code{"sqrt"}, 
#' \code{"scale_sqrt"}, \code{"rev_sqrt"}, \code{"rev_scale_sqrt"}).  The 
#' weight type section name (i.e. \code{A_B_C} where \code{A}, \code{B}, and
#' \code{C} are sections) determines what action will occur.  \code{log} will 
#' use \code{\link[base]{log}}, \code{sqrt} will use \code{\link[base]{sqrt}},
#' \code{scale} will standardize the values.  \code{rev} will multiply by -1 to 
#' give the inverse sign.  This enables a comparison similar to correlations 
#' rather than distance.
#' @param \dots ignored.
#' @rdname word_proximity
#' @export
#' @method weight word_proximity
weight.word_proximity <- function(x, type = "scale", ...) {

    if (attributes(x)[["weight"]] != "none") {
        stop("Supply an unweighted `word_proximity` object")
    }
    
    names <- attributes(x)[["names"]]
    class(x) <- "list"
    fun <- switch(type,
        scale_log = wp_scale_log,
        scale = wp_scale,
        rev_scale = wp_rev_scale,
        rev_scale_log = wp_rev_scale_log,
        log = wp_log,
        sqrt = wp_sqrt,
        scale_sqrt = wp_scale_sqrt,
        rev_sqrt = wp_rev_sqrt,
        rev_scale_sqrt = wp_rev_scale_sqrt,
        stop("Please see `?weight` for weight types")
    )
    o <- lapply(x, fun)
    attributes(o) <- list(
        class = c("word_proximity", class(o)), 
        weight = type,
        names = names
        
    )
    o
}

scale2 <- function(x) (x-mean(x, na.rm = TRUE))/stats::sd(x, na.rm = TRUE)
wp_scale_log <- function(x) scale2(log(x + .000000000001))
wp_scale <- function(x) scale2(x)
wp_rev_scale <- function(x) (-1) * scale2(x)
wp_rev_scale_log <- function(x) (-1) * scale2(log(x+ .000000000001))
wp_log <- function(x) log(x+ .000000000001)
wp_sqrt <- function(x) sqrt(x)
wp_scale_sqrt <- function(x) scale2(sqrt(x))
wp_rev_sqrt <- function(x) (-1) * sqrt(x)
wp_rev_scale_sqrt <- function(x) (-1) * scale2(sqrt(x))
