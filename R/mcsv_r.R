#' Read/Write Multiple csv Files at a Time
#' 
#' \code{mcsv_w} - Read and assign multiple csv files at the same time.
#' 
#' @param files csv file(s) to read.   
#' @param a.names object names to assign the csv file(s) to.  If NULL assigns 
#' the csv to the name(s) of the csv file(s) in the global environment.
#' @param l.name A character vector of names to assign to the csv files 
#' (dataframes) being read in.  Default (\code{NULL}) uses the names of the 
#' files in the directory without the file extension.
#' @param list A character vector of length one to name the list being read in.  
#' Default is \code{"L1"}.
#' @param pos where to do the removal. By default, uses the current environment. 
#' @param envir the environment to use. 
#' @param \dots data.frame object(s) to write to a file or a list of data.frame 
#' objects.  If the objects in a list are unnamed V + digit will be assigned.
#' @param dir optional directory names.  If \code{NULL} a directory will be 
#' created in the working directory with the data and time stamp as the folder 
#' name.
#' @param open logical.  If \code{TRUE} opens the directory upon completion.
#' @param sep A character string to separate the terms.
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
#' #mcsv_r EXAMPLE:
#' mtcarsb <- mtcars[1:5, ]; CO2b <- CO2[1:5, ]
#' (a <- mcsv_w(mtcarsb, CO2b, dir="foo"))
#' rm("mtcarsb", "CO2b")  # gone from .GlobalEnv
#' (nms <- dir(a))
#' mcsv_r(paste(a, nms, sep="/"))
#' mtcarsb; CO2b
#' rm("mtcarsb", "CO2b")  # gone from .GlobalEnv
#' mcsv_r(paste(a, nms, sep="/"), paste0("foo.dat", 1:2))
#' foo.dat1; foo.dat2
#' rm("foo.dat1", "foo.dat2")  # gone from .GlobalEnv
#' delete("foo")
#' 
#' #mcsv_w EXAMPLE:
#' (a <- mcsv_w(mtcars, CO2, dir="foo"))
#' delete("foo")
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
        assign(a.names[i], read.csv(files[i]), envir = envir)
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
function(..., dir = NULL, open = FALSE, sep = ", "){
    x <- match.call(expand.dots = FALSE)
    z <- as.character(x[[2]])
    x2 <- list(...)
    if (length(x2) == 1 && !sapply(x2, is.data.frame)) {
        x2 <- unlist(x2, recursive = FALSE)
        z <- names(x2)
        z[z == ""] <- paste0("V", which(z == ""))
    }
    names(x2) <- z
    if (is.null(dir)) {
        
    }
    y <- folder(folder.name = dir)
    files <- paste0(y, "/", z, ".csv")
    invisible(lapply(seq_along(x2), function(i){
        x3 <-x2[i]
        x3 <- x3[[1]]
        x3 <- condense(x3, sep = sep)
        names(x3) <- gsub(names(x2)[i], "", names(x3))
        write.table(x3, file = files[i],  sep = ",", col.names = TRUE, 
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
