#' Write Multiple csv Files at a Time
#' 
#' Write multiple csv files into a file at the same time.
#' 
#' @param \dots data.frame object(s) to write to a file
#' @param dir optional directory names.  If NULL a directory will be created in the working directory with the data and time stamp as the folder name.
#' @param open logical.  If TURE opens the directory upon completion.
#' @return Creates a directory with multiple csv files.  Silently returns the path of the directory.
#' @seealso \code{\link[qdap]{mcsv_r}}
#' @examples
#' \dontrun{
#' mcsv_w(mtcars, CO2, dir="foo", open=TRUE)
#' delete("foo")
#' }
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
