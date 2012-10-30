#' Read Multiple csv Files at a Time
#' 
#' Read and assign multiple csv files at the same time.
#' 
#' @param files csv file(s) to read.   
#' @param a.names object names to assign the csv file(s) to.  If NULL assigns the csv to the name(s) of the csv file(s) in the global enviroment.
#' @param l.name 
#' @param list 
#' @return Creates a directory with multiple csv files.  Silently returns the path of the directory.
#' @note Useful for reading in multiple csv files from cm_csv.temp for interaction with cm_range2long.
#' @seealso \code{\link[qdap]{mcsv_w}},
#' \code{\link[qdap]{cm_range2long}}
#' @examples
#' \dontrun{
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
#' }
mcsv_r <-
function(files, a.names = NULL, l.name = NULL, list = TRUE){
    if (is.null(a.names)){
        a.names <- sapply(files, function(x){
            v <- unlist(strsplit(x, "/|\\\\"))
            gsub(".csv", "", v[length(v)])
        })
    }
    invisible(lapply(seq_along(files), function(i) {
        assign(a.names[i], read.csv(files[i]), envir = .GlobalEnv)
    }))
    if (list) {
        L1 <- lapply(a.names, function(x){
            get(a.names)
        }) 
        if (is.null(l.name)){
            l.name <- "L1"
        }
        assign(l.name, L1, envir = .GlobalEnv)
    }
    assi <- paste(c(l.name, a.names), collapse=", ")
    message(paste0("objects assigned: ", assi))
    invisible(a.names)
}
