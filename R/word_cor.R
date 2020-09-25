#' Find Correlated Words
#' 
#' Find associated words within grouping variable(s).
#' 
#' @param text.var The text variable (or frequency matrix).
#' @param grouping.var The grouping variables.  Default uses each row as a group.  
#' Also takes a single grouping variable or a list of 1 or more grouping 
#' variables.  Unlike other \pkg{qdap} functions, this cannot be \code{NULL}.
#' @param word The word(s) vector to find associated words for.
#' @param r The correlation level find associated words for.  If positive this
#' is the minimum value, if negative this is the maximum value.
#' @param values logical.  If \code{TRUE} returns the named correlates (names are 
#' the words).  If \code{FALSE} only the associated words are returned.
#' @param method A character string indicating which correlation coefficient is 
#' to be computed (\code{"pearson"}, \code{"kendall"}, or \code{"spearman"}).
#' @param \dots Other arguments passed to \code{\link[qdap]{wfm}}.
#' @return Returns a vector of associated words or correlation matrix if 
#' \code{r = NULL}.
#' @note Note that if a word has no variablity in it's usage across grouping 
#' variable(s) the \code{\link[stats]{sd}} will result in 0, thus 
#' \code{\link[stats]{cor}} will will likely return a warning as in this 
#' example: \code{cor(rep(3, 10), rnorm(10))}.
#' @export
#' @importFrom qdapTools list_vect2df
#' @references The plotting method for the list output was inspired by Ben 
#' Marwick; see https://stackoverflow.com/a/19925445/1000343 for more.
#' @seealso \code{\link[qdap]{word_proximity}},
#' \code{\link[tm]{findAssocs}},
#' \code{\link[qdap]{word_associate}},
#' \code{\link[qdap]{wfm}},
#' \code{\link[stats]{cor}}
#' @examples
#' \dontrun{
#' x <- factor(with(rajSPLIT, paste(act, pad(TOT(tot)), sep = "|")))
#' word_cor(rajSPLIT$dialogue, x, "romeo", .45)
#' word_cor(rajSPLIT$dialogue, x, "love", .5)  
#' 
#' ## Negative correlation
#' word_cor(rajSPLIT$dialogue, x, "you", -.1)
#' with(rajSPLIT, word_cor(dialogue, list(person, act), "hate"))
#' 
#' words <- c("hate", "i", "love", "ghost")
#' with(rajSPLIT, word_cor(dialogue, x, words, r = .5))
#' with(rajSPLIT, word_cor(dialogue, x, words, r = .4))
#' 
#' ## Set `r = NULL` to get matrix between words
#' with(rajSPLIT, word_cor(dialogue, x, words, r = NULL))
#' 
#' ## Plotting 
#' library(tm)
#' data("crude")
#' oil_cor1 <- apply_as_df(crude, word_cor, word = "oil", r=.7)
#' plot(oil_cor1)
#' 
#' oil_cor2 <- apply_as_df(crude, word_cor, word = qcv(texas, oil, money), r=.7)
#' plot(oil_cor2)
#' plot(oil_cor2, ncol=2)
#' 
#' oil_cor3 <- apply_as_df(crude, word_cor, word = qcv(texas, oil, money), r=NULL)
#' plot(oil_cor3)
#' 
#' ## Run on multiple times/person/nested
#' ## Split and apply to data sets
#' ## Suggested use of stemming
#' DATA3 <- split(DATA2, DATA2$person)
#' 
#' ## Find correlations between words per turn of talk by person
#' ## Throws multiple warning because small data set
#' library(qdapTools)
#' lapply(DATA3, function(x) {
#'     word_cor(x[, "state"], qdapTools::id(x), qcv(computer, i, no, good), r = NULL)
#' })
#' 
#' ## Find words correlated per turn of talk by person
#' ## Throws multiple warning because small data set
#' lapply(DATA3, function(x) {
#'     word_cor(x[, "state"], qdapTools::id(x), qcv(computer, i, no, good))
#' })
#' 
#' 
#' ## A real example
#' dat <- pres_debates2012 
#' dat$TOT <- factor(with(dat, paste(time, pad(TOT(tot)), sep = "|")))
#' dat <- dat[dat$person %in% qcv(OBAMA, ROMNEY), ]
#' dat$person <- factor(dat$person)
#' dat.split <- with(dat, split(dat, list(person, time)))
#' 
#' wrds <- qcv(america, debt, dollar, people, tax, health)
#' lapply(dat.split, function(x) {
#'     word_cor(x[, "dialogue"], x[, "TOT"], wrds, r=NULL)
#' })
#' 
#' ## Supply a matrix (make sure to use `t` on a `wfm` matrix)
#' worlis <- list(
#'     pronouns = c("you", "it", "it's", "we", "i'm", "i"),
#'     negative = qcv(no, dumb, distrust, not, stinks),
#'     literacy = qcv(computer, talking, telling)
#' )
#' y <- wfdf(DATA$state, qdapTools::id(DATA, prefix = TRUE))
#' z <- wfm_combine(y, worlis)
#' 
#' out <- word_cor(t(z), word = c(names(worlis), "else.words"), r = NULL)
#' out
#' plot(out)
#' 
#' ## Additional plotting/viewing
#' require(tm)
#' data("crude")
#' 
#' out1 <- word_cor(t(as.wfm(crude)), word = "oil", r=.7)
#' vect2df(out1[[1]], "word", "cor")
#' 
#' plot(out1)
#' qheat(vect2df(out1[[1]], "word", "cor"), values=TRUE, high="red", 
#'     digits=2, order.by ="cor", plot=FALSE) + coord_flip()
#' 
#' 
#' out2 <- word_cor(t(as.wfm(crude)), word = c("oil", "country"), r=.7)
#' plot(out2)
#' }
word_cor <- function(text.var, grouping.var = qdapTools::id(text.var), word, 
    r = .7, values = TRUE, method = "pearson", ...) {

    if (missing(grouping.var) & is.matrix(text.var) | is.data.frame(text.var)) {
        WFM <- text.var
    } else {
        if (is.null(grouping.var)) stop("Must supply a grouping variable")
        WFM <- t(wfm(text.var = text.var, grouping.var = grouping.var, ...))
    }

    WFM <- data.frame(WFM, check.names = FALSE)
    wordlen <- length(word) == 1

    test1 <- word %in% colnames(WFM)
    if (sum(!test1) > 0) {
        warning(paste0("The following words were not ",
            "found in the data set and were removed:\n",
            "=======================\n",
            paste(word[!test1], collapse=", "), "\n"))
        word <- word[test1]
    }

    if (sum(test1) < 2) {
        if (is.null(r) & sum(test1) == 1) {
            warning(sprintf("Only `%s` was found in the data set. NULL returned", 
                word))
            return(NULL)
        } else {
            if (sum(test1) == 0) {
                warning("No words found in the data set.  NULL returned")
                return(NULL)
            }
        }
    }

    if (!is.null(r)) {
        posit <- r > 0
        L1 <- lapply(word, cor_help1, m = WFM, o = r, sORw = wordlen, 
            vals = values, positive = posit, meth = method)
        names(L1) <- word
        out <- L1
        attributes(out) <- list(
            class = c("word_cor", class(out)), 
            type = c("cor_list"),
            names = names(out)
        ) 
    } else {
        out <- stats::cor(WFM[, word], method = method)
        attributes(out) <- list(
            class = c("word_cor", class(out)), 
            type = c("cor_matrix"),
            dim = attributes(out)[["dim"]],
            dimnames = attributes(out)[["dimnames"]]
        )        
    }
    out    
}

#' Prints a word_cor object
#' 
#' Prints a word_cor object
#' 
#' @param x The word_cor object
#' @param digits The number of digits to print
#' @param \ldots ignored
#' @export
#' @method print word_cor
print.word_cor <-
function(x, digits = 3, ...) {

    WD <- options()[["width"]]
    if (attributes(x)[["type"]] == "cor_matrix") {
        options(width=3000)
        class(x) <- "matrix"
        attributes(x)[["type"]] <- NULL
        print(round(x, digits = digits))
        options(width=WD)
        return()
    }
    if (attributes(x)[["type"]] == "cor_list") {
        class(x) <- "list"
        attributes(x)[["type"]] <- NULL
        print(lapply(x, function(y) {
            if (is.null(y)) return(NULL)
            round(y, digits = digits)
        }))
    }
}

#' Plots a word_cor object
#' 
#' Plots a word_cor object.
#' 
#' @param x The word_cor object
#' @param label logical.  If \code{TRUE} the cells of the heat map plot will be 
#' labeled with count and proportional values.
#' @param lab.digits Integer values specifying the number of digits to be 
#' printed if \code{label} is \code{TRUE}.
#' @param low The color to be used for lower values.
#' @param high The color to be used for higher values.
#' @param grid The color of the grid (Use \code{NULL} to remove the grid). 
#' @param ncol The number of columns to arrange the facets in (specifying an 
#' integer results in the use of \code{\link[ggplot2]{facet_wrap}}, specifying
#' \code{NULL} utilizes a single column with \code{\link[ggplot2]{facet_grid}}.  
#' The second approach limits columns but allows the y scale's space to be free.
#' @param \ldots Other arguments passed to qheat if matrix and other arguments 
#' passed to \code{\link[ggplot2]{geom_point}} if a list.
#' @importFrom ggplot2 ggplot aes facet_grid facet_wrap geom_point xlab ylab
#' @method plot word_cor
#' @export
plot.word_cor <- function(x, label = TRUE, lab.digits = 3, high="red", 
    low="white", grid=NULL, ncol=NULL, ...) {
    
    word <- cor <- comp_word <- NULL
    
    if (attributes(x)[["type"]] == "cor_matrix") {
        qheat(t(x), diag.na = TRUE, diag.values = "", by.column = NULL, 
            values = TRUE, digits = lab.digits, high = high, 
            low = low, grid = grid, ...)
    }
    if (attributes(x)[["type"]] == "cor_list") {
  
        x <- x[!sapply(x, is.null)]
        
        dat <- list_vect2df(x, "word", "comp_word", "cor")
       
        if (is.null(ncol)) {

            if (length(x) == 1) {
                facets <- stats::reformulate("word", ".")
            } else {
                facets <- stats::reformulate(".", "word")
            }
            ggplot(dat, aes(cor, comp_word)) +
                geom_point(...) +
                facet_grid(facets, space="free_y", scales="free_y") +
                ylab("Words") + xlab("Correlation")
         } else {

            ggplot(dat, aes(cor, comp_word)) +
                geom_point(...) +
                facet_wrap(~word, scales="free_y", ncol=ncol) +
                ylab("Words") + xlab("Correlation")
        }
    }

}


cor_help1 <- function(n, m, o, sORw, vals, positive, meth) {

    L <- sapply(m[, !colnames(m) %in% tolower(n)], function(x) {
        suppressWarnings(stats::cor(x, m[, tolower(n)], method = meth))
    })

    NAS <- is.na(L)
    if(sum(NAS) > 0) {
        warning("The sd on the following words was 0:\n", 
            paste(names(L[NAS]), collapse=", "))
        L <- L[!NAS]
    }

    if (positive) {
        extr <- L > o
        phr <- "at least"
    } else {
        extr <- L <= o
        phr <- "less than or equal to"
    }

    if (all(sapply(extr, is.na)) | sum(extr, na.rm = TRUE) == 0) {
        if (sORw) {
            stop(sprintf("No words correlate %s %s", phr, o))
        } else {
            return(NULL)
        }
    }
    if (!vals) {
        names(L)[extr]
    } else {
        L[extr]
    }
}
