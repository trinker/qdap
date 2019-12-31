#' Text Justification
#' 
#' \code{left_just} - Left justifies a text/character column.
#' 
#' @param dataframe A data.frame object with the text column.
#' @param column The column to be justified.  If \code{NULL} all columns are 
#' justified.
#' @param keep.class logical.  If \code{TRUE} will attempt to keep the original 
#' classes of the dataframe if the justification is not altered (i.e., numeric 
#' will not be honored but factor may be).
#' @return Returns a dataframe with selected text column left/right justified.
#' @rdname justification
#' @note \code{\link[qdap]{left_just}} inserts spaces to achieve the 
#' justification.  This could interfere with analysis and therefore the output 
#' from \code{\link[qdap]{left_just}} should only be used for visualization 
#' purposes, not analysis.
#' @export
#' @examples
#' \dontrun{
#' left_just(DATA)
#' left_just(DATA, "state")
#' left_just(CO2[1:15,])
#' right_just(left_just(CO2[1:15,]))
#' }
left_just <-
function(dataframe, column = NULL, keep.class = FALSE) {
    df.class <- function(dataframe) {
        sapply(1:ncol(dataframe), function(i) {
            x <- class(dataframe[, i])
            x[length(x)]
        })
    }
    CLASS <- df.class(dataframe)
    left.j <- function(x) {
        n <- max(nchar(x))
        return(sprintf(paste("%-", n, "s", sep = ""), x))
    }
    if (is.null(column)) column <- colnames(dataframe)
    lj <- function(DF2, column) {
        if (is.null(column)) column <- colnames(DF2)
        Q <- max(nchar(c(as.character(DF2[, column]), column)))
        DF2 <- data.frame(rbind(colnames(DF2), do.call(cbind,
            lapply(DF2, as.character))), check.names = FALSE)
        DF2[, column] <- left.j(as.character(DF2[, column]))     
        if (is.character(column)) {
            col <- names(DF2)[which(names(DF2) == column)]
                names(DF2)[which(names(DF2) == column)] <- sprintf(paste("%-", 
                Q, "s", sep = ""), col)
        } else {
            if (is.numeric(column)) {
                col <- names(DF2)[column]
                    names(DF2)[column] <- sprintf(paste("%-", Q, "s", 
                    sep = ""), col)
            }
        }
        DF2 <- data.frame(DF2[-1, , drop = FALSE], check.names = FALSE)
        rownames(DF2) <- NULL
        return(DF2)
    }
    if (length(column) < 2) {
        if (!is.data.frame(dataframe)) {
            y <- as.character(substitute(dataframe))
            dataframe <- data.frame(dataframe, check.names = FALSE)
            y <- if (y[1]%in%c("[", "$")) y[2] else y[1]
            names(dataframe) <- y
        }
        DF3 <- lj(DF2=dataframe, column=column)
    } else { 
        if (!is.numeric(column)) column <- match(column, names(dataframe))
        dat <- dataframe[, -c(column), drop=FALSE]
        ndf <- colnames(dataframe)
        LIST <- lapply(column, function(x) {
            lj(DF2=dataframe[, x, drop=FALSE], column = NULL)
        })
        dat2 <- data.frame(cbind(do.call('cbind', LIST), dat), checknames=FALSE)
        NAMES <- colnames(dat2)
        STrim <- function (x) gsub("^\\s+|\\s+$|\\.+$", "", x)
        newloc <- match(ndf, STrim(NAMES))
        DF3 <- dat2[, newloc]
    }
    if (keep.class) {
        colClasses <- function(d, colClasses) {
            colClasses <- rep(colClasses, len=length(d))
            d[] <- lapply(seq_along(d), function(i) switch(colClasses[i], 
                numeric=as.numeric(d[[i]]), 
                character=as.character(d[[i]]), 
                Date=as.Date(d[[i]], origin='1970-01-01'), 
                POSIXct=as.POSIXct(d[[i]], origin='1970-01-01'), 
                factor=as.factor(d[[i]]),
                methods::as(d[[i]], colClasses[i]) ))
            d
        }
        DF3 <- colClasses(DF3, CLASS)
    }
    colnames(DF3) <- gsub("\\.(?=\\.*$)", " ", colnames(DF3), perl=TRUE)
    return(DF3)
}

#' Right Justify Text
#' 
#' \code{right_just} - A means of undoing a left justification. 
#' 
#' @rdname justification
#' @export
right_just <-
function(dataframe){
    NAMES <- Trim(names(dataframe))
    x <- data.frame(sapply(dataframe, Trim))
    names(x) <- NAMES
    return(x)
}


