left.just <-
function(dataframe, column = NULL) {
    left.j <- function(x) {
        n <- max(nchar(x))
        return(sprintf(paste("%-", n, "s", sep = ""), x))
    }
    lj <- function(dataframe, column) {
        DF2 <- dataframe
        if (is.null(column)) column <- names(dataframe)
        Q <- max(nchar(as.character(DF2[, column])))
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
    return(DF2)
    }
    if (length(column)<2) {
        if (!is.data.frame(dataframe)) {
            y <- as.character(substitute(dataframe))
            dataframe <- data.frame(dataframe)
            y <- if (y[1]%in%c("[", "$")) y[2] else y[1]
            names(dataframe) <- y
        }
        DF3 <- lj(dataframe=dataframe, column=column)
    } else { 
        if (!is.numeric(column)) column <- match(column, names(dataframe))
        dat <- dataframe[, -c(column)]
        ndf <- colnames(dataframe)
        LIST <- lapply(column, function(x) lj(dataframe=
            dataframe[, x, drop=FALSE], column = NULL))
        dat2 <- cbind(do.call('cbind', LIST), dat, checknames=FALSE)
        NAMES <- colnames(dat2)
        newloc <- match(ndf, Trim(NAMES))
        
        DF3 <- dat2[, newloc]
    }
    return(DF3)
}
