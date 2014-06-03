#' Wrapper for colSplit that Returns Dataframe(s)
#' 
#' \code{colsplit2df} - Wrapper for \code{\link[qdap]{colSplit}} that returns a 
#' dataframe.
#' 
#' @param dataframe A dataframe with a column that has been pasted together.
#' @param splitcols The name/index of the column(s) that has been pasted together.
#' @param new.names A character vector of new names to assign to the columns (or
#' list of names if multiple columns are being split).  
#' Default attempts to extract the original names before the paste.
#' @param sep The character(s) that was used in \code{paste2} to paste the 
#' columns.
#' @param keep.orig logical.  If \code{TRUE} the original pasted column will be 
#' retained as well.
#' @param name.sep The character(s) that was used to paste the column names.
#' @param index.names logical.  If \code{TRUE} names of columns that are duplicated
#' are indexed with c("name.1", "name.2", ... "name.n").  
#' @return \code{colsplit2df} - returns a dataframe with the \code{paste2} 
#' column split into new columns.
#' @seealso \code{\link[qdap]{colSplit}}, 
#' \code{\link{colpaste2df}}
#' \code{\link{paste2}}
#' @rdname colsplit2df
#' @keywords column-split
#' @export
#' @examples 
#' \dontrun{
#' CO2$`Plant&Type&Treatment` <- paste2(CO2[, 1:3])
#' CO2 <- CO2[, -c(1:3)]
#' head(CO2)
#' head(colsplit2df(CO2, 3))
#' head(colsplit2df(CO2, 3, qcv(A, B, C)))
#' head(colsplit2df(CO2, 3, qcv(A, B, C), keep.orig=TRUE))
#' head(colsplit2df(CO2, "Plant&Type&Treatment"))
#' CO2 <- datasets::CO2
#' 
#' (dat <- colpaste2df(head(mtcars), list(1:3), sep = "|"))
#' colsplit2df(dat, 12, sep = "|")
#' 
#' ## Multiple split example
#' E <- list(
#'     c(1, 2, 3, 4, 5),
#'     qcv(mpg, hp),
#'     c("disp", "am")
#' )
#' 
#' (dat2 <- colpaste2df(head(mtcars), E, sep ="|"))
#' cols <- c("mpg&cyl&disp&hp&drat", "mpg&hp", "disp&am")
#' colsplit2df(dat2, cols, sep = "|")
#' 
#' ## lcolsplit2df example
#' (x <- with(DATA.SPLIT, question_type(state, list(sex, adult))))
#' ltruncdf(x)
#' z <- lcolsplit2df(x)
#' ltruncdf(z)
#' }
colsplit2df <- function(dataframe, splitcols = 1, new.names = NULL, sep=".", 
         keep.orig=FALSE, name.sep = "&", index.names = FALSE) {


    if (is.numeric(splitcols)) {
        splitcols <- names(dataframe)[splitcols]
    }

    if (!is.null(new.names) && !is.list(new.names) & is.vector(new.names)) {
        new.names <- list(new.names)
    }
    if ((length(sep) == 1) && (length(splitcols) > 1)) {
        sep <- rep(sep, length(splitcols))
    }
    if ((length(name.sep) == 1) && (length(splitcols) > 1)) {
        name.sep <- rep(name.sep, length(splitcols))
    }
    for (i in 1:length(splitcols)) {
        dataframe <- colsplit2df_helper(dataframe = dataframe, 
            splitcol = splitcols[i], new.names = new.names[[i]], 
            sep = sep[i], keep.orig = keep.orig, name.sep = name.sep[i],
            index.names = index.names
        )
    }
    nfix <- !is.na(suppressWarnings(as.numeric(colnames(dataframe))))
    colnames(dataframe)[nfix] <- paste0("X", colnames(dataframe)[nfix])
    dataframe
}


#' Wrapper for qdap lists that Returns Dataframes
#' 
#' \code{lcolsplit2df} - Wrapper for \code{colsplit2df} designed for qdap lists 
#' that returns a list dataframes.
#' @param qdap.list A qdap list object that contains dataframes with a leading 
#' \code{\link[qdap]{paste2}} column.
#' @note \code{\link[qdap]{lcolsplit2df}} is a convenience function that is less 
#' flexible than \code{\link[qdap]{colsplit2df}} but operates on multiple 
#' dataframes at once.
#' @section Warning: This will strip the class of the qdap object.
#' @return \code{lcolsplit2df} - returns a list of dataframes with the 
#' \code{\link[qdap]{paste2}} column split into new columns.
#' @rdname colsplit2df
#' @export
lcolsplit2df <- 
function(qdap.list, keep.orig=FALSE){
    apps <- sapply(qdap.list, is.data.frame)
    nms <- unlist(strsplit(colnames(qdap.list[[1]])[1], "\\&"))
    w <- lapply(qdap.list[apps], colsplit2df)
    return(unlist(list(w, qdap.list[!apps]), recursive = FALSE))
}

#' Prints a colsplit2df Object.
#' 
#' Prints a colsplit2df object.
#' 
#' @param x The colsplit2df object 
#' @param \ldots ignored
#' @method print colsplit2df
#' @export
print.colsplit2df <-
function(x,  ...) {
    WD <- options()[["width"]]
    options(width=3000)
    class(x) <- "data.frame"
    print(x)
    options(width=WD)  
}


colsplit2df_helper <- 
function(dataframe, splitcol = 1, new.names = NULL, sep=".", 
         keep.orig=FALSE, name.sep = "&", index.names = FALSE){

    ## change width of prrint screen temporarily
    WD <- options()$width
    options(width=10000)
    on.exit(options(width=WD))
    classRdf <- c("diversity")

    ## Check 1
    if (!is.data.frame(dataframe)){
        warning("dataframe object is not of the class data.frame")
    }

    ## Check 2 for splitcol == ""/ added 10/10/13
    if (splitcol == "") {
        splitcol <- which(colnames(dataframe) %in% splitcol)
    }

    ## Check 3
    if (is.numeric(dataframe[, splitcol])) {
        stop("splitcol class can not be numeric")
    }

    ## The column was split into list of vectors
    splits <- strsplit(as.vector(
        dataframe[, splitcol]), split = sep, fixed=TRUE) 
   
    ## Put together as a dataframe (do.call + rbind)
    splitcols <- data.frame(do.call(rbind, splits), check.names = FALSE, 
        stringsAsFactors = FALSE)

    ## determine split col location
    if (!is.numeric(splitcol)) {
        loc <- match(splitcol, names(dataframe))
    } else {
        loc <- splitcol
    }

    ## if new names apply them
    if (!is.null(new.names)) {
        colnames(splitcols) <- new.names
    } else {
        ## Grab name of split column
        splnm <- colnames(dataframe)[loc]
         
        ## Check if the lenght name splits of that column 
        ## are equal to ncols in splitcols
        newnms <- unlist(strsplit(splnm, name.sep, fixed = TRUE))
        lennms <- length(newnms)

        ## if split column name splits appropriately then 
        ## that's the names use; else use X1, X2, ... Xn
        if (ncol(splitcols) == lennms) {
            colnames(splitcols) <- newnms
        } else {
            colnames(splitcols) <- paste0("X", colnames(splitcols))
        }
    }

    ## get number of columns in original dataframe
    dn <- ncol(dataframe)
    
    ## splice it together
    dat <- data.frame(dataframe, splitcols, check.names = FALSE, 
        stringsAsFactors = FALSE)

    ## get number of columns in new dataframe 
    nn <- ncol(dat)
 
    ## See if there are any columns after the split column to grab
    if (dn >= (loc + 1)) {
        ending <- (loc + 1):dn
    } else {
        ending <- NULL
    }

    ## put it together in the correct order
    o <- dat[, c(1:loc, (dn + 1):nn, ending)]

    ## drop the numeric extensions of variables with repeated names
    if (!index.names) {
        newnames <- colnames(dat)[c(1:loc, (dn + 1):nn, ending)]
        colnames(o) <- newnames
    }

    ## Drop the original split column
    if(!keep.orig) {
        o[loc] <- NULL
    }

    ## give it a class
    class(o) <- c("colsplit2df", "data.frame")
    o
}

