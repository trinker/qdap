#' Read/Write Multiple csv Files at a Time
#' 
#' \code{mcsv_r} - Read and assign multiple csv files at the same time.
#' 
#' @param files csv file(s) to read.   
#' @param a.names object names to assign the csv file(s) to.  If \code{NULL} 
#' assigns the name(s) of the csv files in the directory, without the file 
#' extension, to the objects in the global environment.
#' @param l.name A single character string of a name to assign to the list if 
#' dataframes created by the csv files being read in.  Default (\code{NULL}) 
#' uses \code{L1}.
#' @param list logical.  If \code{TRUE} then a list of dataframes is crated in 
#' the global environment in addition to the individual dataframes.
#' @param pos where to do the removal. By default, uses the current environment. 
#' @param envir the environment to use. 
#' @param \dots data.frame object(s) to write to a file or a list of data.frame 
#' objects.  If the objects in a list are unnamed V + digit will be assigned.  
#' Lists of dataframes (e.g., the output from \code{\link[qdap]{termco}} or 
#' \code{\link[qdap]{polarity}}) can be passed as well.
#' @param dir optional directory names.  If \code{NULL} a directory will be 
#' created in the working directory with the data and time stamp as the folder 
#' name.
#' @param open logical.  If \code{TRUE} opens the directory upon completion.
#' @param sep A character string to separate the terms.
#' @param dataframes An optional character vector of dataframes in lieu of \dots 
#' argument.
#' @return \code{mcsv_r} - reads in multiple csv files at once.
#' @rdname multicsv
#' @note \code{\link[qdap]{mcsv_r}} is useful for reading in multiple csv files 
#' from \code{\link[qdap]{cm_df.temp}} for interaction with 
#' \code{\link[qdap]{cm_range2long}}.
#' @details mcsv is short for "multiple csv" and the suffix c(_r, _w) stands for 
#' "read" (r) or "write" (w).
#' @seealso \code{\link[qdap]{cm_range2long}},
#' \code{\link[qdap]{cm_df.temp}},
#' \code{\link[qdap]{condense}},
#' \code{\link[base]{assign}}
#' @export
#' @examples
#' \dontrun{
#' ## mcsv_r EXAMPLE:
#' mtcarsb <- mtcars[1:5, ]; CO2b <- CO2[1:5, ]
#' (a <- mcsv_w(mtcarsb, CO2b, dir="foo"))
#' rm("mtcarsb", "CO2b")  # gone from .GlobalEnv
#' (nms <- dir(a))
#' mcsv_r(file.path(a, nms))
#' mtcarsb; CO2b
#' rm("mtcarsb", "CO2b")  # gone from .GlobalEnv
#' mcsv_r(file.path(a, nms), paste0("foo.dat", 1:2))
#' foo.dat1; foo.dat2
#' rm("foo.dat1", "foo.dat2")  # gone from .GlobalEnv
#' delete("foo")
#' 
#' ## mcsv_w EXAMPLES:
#' (a <- mcsv_w(mtcars, CO2, dir="foo"))
#' delete("foo")
#' 
#' ## Write lists of dataframes as well
#' poldat <- with(DATA.SPLIT, polarity(state, person))
#' term <- c("the ", "she", " wh")
#' termdat <- with(raj.act.1,  termco(dialogue, person, term))
#' mcsv_w(poldat, termdat, mtcars, CO2, dir="foo2")
#' delete("foo2")
#' }
mcsv_r <-
function(files, a.names = NULL, l.name = NULL, list = TRUE, pos = 1,
    envir = as.environment(pos)){
    if (is.null(a.names)){
        a.names <- unlist(lapply(files, function(x){
            v <- unlist(strsplit(x, "/|\\\\"))
            v <- v[length(v)]
            gsub(".csv", "", v)
        }))
    }
    invisible(lapply(seq_along(files), function(i) {
        assign(a.names[i], utils::read.csv(files[i]), envir = envir)
    }))
    if (list) {
        L1 <- lapply(a.names, function(x){
            get(x)
        }) 
        names(L1) <- a.names
        if (is.null(l.name)){
            l.name <- "L1"
        }
        assign(l.name, L1, envir = envir)
    }
    assi <- paste(c(l.name, a.names), collapse=", ")
    message(paste0("objects assigned: ", assi))
    invisible(a.names)
}

#' Write Multiple csv Files at a Time
#' 
#' \code{mcsv_w} - Write multiple csv files into a file at the same time.
#' 
#' @return \code{mcsv_w} - creates a directory with multiple csv files.  
#' Silently returns the path of the directory.
#' @rdname multicsv
#' @export
mcsv_w <- 
function(..., dir = NULL, open = FALSE, sep = ", ", dataframes = NULL, pos = 1,
    envir = as.environment(pos)){
    x <- match.call(expand.dots = FALSE)
    z <- as.character(x[[2]])
    x2 <- list(...)
    if (identical(x2, list())) {
        z <- dataframes
        x2 <- lapply(z, get, envir = envir)
    }
    if (length(x2) == 1 && !sapply(x2, is.data.frame)) {
        x2 <- unlist(x2, recursive = FALSE)
        z <- names(x2)
        z[z == ""] <- paste0("V", which(z == ""))
    }
    names(x2) <- z
    y <- folder(folder.name = dir)
    Is.list <- function(x) is.list(x) & !is.data.frame(x)
    lists.x2 <- sapply(x2, Is.list)
    if (sum(lists.x2) > 0) {
        unlisted.x2 <- unlist(unclass(x2[lists.x2]), recursive = FALSE)
        x2 <- x2[!lists.x2]
        len1 <- length(x2)
        len2 <- length(unlisted.x2)
        x2[(len1 + 1):(len1 + len2)] <- unlisted.x2
        names(x2)[(len1 + 1):(len1 + len2)] <- names(unlisted.x2)
    }
    removes <- sapply(x2, function(x) {
        identical(x, integer(0)) | identical(x, character(0))
    })
    x2 <- x2[!removes]
    x2 <- lapply(x2, condense)
    files <- paste0(y, "/", names(x2), ".csv")
    which.df <- sapply(x2, function(x) {!is.data.frame(x) & !is.list(x)})
    x2[which.df] <- lapply(x2[which.df], data.frame)
    invisible(lapply(seq_along(x2), function(i){
        x3 <-x2[i]
        x3 <- x3[[1]]
        x3 <- condense(x3, sep = sep)
        names(x3) <- gsub(names(x2)[i], "", names(x3))
        utils::write.table(x3, file = files[i],  sep = ",", col.names = TRUE, 
            row.names=F, qmethod = "double")
        }
    ))
    if(open){
        if (.Platform['OS.type'] == "windows"){
            shell.exec(y)
        } else {
            system(paste(Sys.getenv("R_BROWSER"), y))
        }
    }
    message(paste("files written to:\n", y))
    invisible(y)
}
