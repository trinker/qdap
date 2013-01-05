#' Read/Write Multiple csv Files at a Time
#' 
#' \code{mcsv_w} - Read and assign multiple csv files at the same time.
#' 
#' @param files csv file(s) to read.   
#' @param a.names object names to assign the csv file(s) to.  If NULL assigns 
#' the csv to the name(s) of the csv file(s) in the global enviroment.
#' @param l.name A character vector of names to assign to the csv files 
#' (dataframes) being read in.  Default (NULL) uses the names of the files in 
#' the directory without the file extension.
#' @param list A character vector of length one to name the list being read in.  
#' Default is \code{"L1"}.
#' @param \dots data.frame object(s) to write to a file
#' @param dir optional directory names.  If NULL a directory will be created in 
#' the working directory with the data and time stamp as the folder name.
#' @param open logical.  If TURE opens the directory upon completion.
#' @return \code{mcsv_r} reads in multiple csv files at once.
#' @rdname multicsv
#' @note \code{mcsv_r} is useful for reading in multiple csv files from 
#' \code{cm_csv.temp} for interaction with \code{cm_range2long}.
#' @details mcsv is short for "multiple csv" and the suffix c(_r, _w) stands for 
#' "read" (r) or "write" (w).
#' @seealso \code{\link[qdap]{cm_range2long}},
#' \code{\link[qdap]{cm_csv.temp}}
#' @export
#' @examples
#' \dontrun{
#' #mcsv_r EXAMPLE:
#' mtcarsb <- mtcars; CO2b <- CO2
#' a <- mcsv_w(mtcarsb, CO2b, dir="foo")
#' a
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
#' a <- mcsv_w(mtcars, CO2, dir="foo")
#' a
#' delete("foo")
#' }
mcsv_r <-
function(files, a.names = NULL, l.name = NULL, list = TRUE){
    if (is.null(a.names)){
        a.names <- unlist(lapply(files, function(x){
            v <- unlist(strsplit(x, "/|\\\\"))
            v <- v[length(v)]
            gsub(".csv", "", v)
        }))
    }
    invisible(lapply(seq_along(files), function(i) {
        assign(a.names[i], read.csv(files[i]), envir = .GlobalEnv)
    }))
    if (list) {
        L1 <- lapply(a.names, function(x){
            get(x)
        }) 
        names(L1) <- a.names
        if (is.null(l.name)){
            l.name <- "L1"
        }
        assign(l.name, L1, envir = .GlobalEnv)
    }
    assi <- paste(c(l.name, a.names), collapse=", ")
    message(paste0("objects assigned: ", assi))
    invisible(a.names)
}

#' Write Multiple csv Files at a Time
#' 
#' \code{mcsv_w} - Write multiple csv files into a file at the same time.
#' 
#' @return \code{mcsv_w} creates a directory with multiple csv files.  Silently returns the path of the directory.
#' @rdname multicsv
#' @export
mcsv_w <-
function(..., dir = NULL, open = FALSE){
    x <- match.call(expand.dots = FALSE)
    z <- as.character(x[[2]])
    x2 <- list(...)
    names(x2) <- z
    y <- folder(folder.name = dir)
    files <- paste0(y, "/", z, ".csv")
    invisible(lapply(seq_along(x2), function(i){
        x3 <-x2[i]
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
    invisible(y)
}
