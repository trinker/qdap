#' Replace Cells in a Matrix or Data Frame
#' 
#' Replace elements of a dataframe, matrix or vector with least restrictive 
#' class.
#' 
#' @param dat Data; either a dataframe, matrix or vector.
#' @param replace Element to replace.
#' @param with Replacement element.
#' @return Returns a dataframe, matrix or vector with the element replaced. 
#' @keywords replace
#' @export
#' @examples
#' \dontrun{
#' replacer(mtcars[1:10, ], 0, "REP")
#' replacer(mtcars[1:10, ], 4, NA)
#' replacer(c("a", "b"), "a", "foo")
#' #replace missing values (NA)
#' dat <- data.frame(matrix(sample(c(1:3, NA), 25, TRUE), ncol=5))
#' replacer(dat, NA, "FOO")
#' }
replacer <- 
function(dat, replace=0, with="-"){ 
    h <- is.vector(dat)
    i <- is.matrix(dat)
    j <- is.data.frame(dat)

    CLS <- sapply(dat, class)

    if (is.numeric(replace)){
        NAMES <- colnames(dat)
        if (h) {
            dat <- data.frame(dat)
        }
        not.num <- Negate(is.numeric)
        dat1 <- dat[, sapply(dat, not.num), drop = FALSE]
        inds <- which(sapply(dat, is.numeric))
        subdf <- function(df, ii) {
            do.call("data.frame", c(as.list(df)[ii, drop=FALSE], 
                check.names=FALSE))
        }
        dat2 <- subdf(dat, inds)
    } else {
        dat2 <- dat
    }
    y <- as.matrix(dat2)
    if (is.na(replace)) {
        y[is.na(y)] <- with
    } else { 
        y[y==replace] <- with
    }
    if (is.numeric(replace)){    
        y <- data.frame(dat1, y, check.names = FALSE)
        m <- NAMES[!NAMES%in%names(y)]
        if(length(m) == 1) names(y)[1] <- m
        y <- subdf(y, NAMES)
    }
    if(h) y <- as.vector(y)
    if(i) y <- as.matrix(y)
    if(j) {
        y <- as.data.frame(y, check.names = FALSE)
        for (i in 1:ncol(y)) {
            FUN <- match.fun(paste0("as.", CLS[i]))
            FUN2 <- match.fun(paste0("is.", CLS[i]))
            if (CLS[i] %in% c("numeric", "integer")) {
                numTest <- function(x) {
                    v <- sum(is.na(x))
                    w <- sum(is.na(suppressWarnings(as.numeric(as.character(x)))))
                    v == w
                }
                if (!numTest(y[, i])){
                    y[, i] <- as.character(y[, i])
                } else {
                    y[, i] <- FUN(as.character(y[, i]))  
                }
            } else {
                y[, i] <- FUN(as.character(y[, i]))  
            }
        }
    }
    return(y)
}
