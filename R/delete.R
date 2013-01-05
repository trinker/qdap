#' Easy File Handling
#' 
#' \code{delete} - Deletes files and directories.
#' 
#' @param file The name of the file in the working directory or the path to the 
#' file to be deleted.  If NULL provides a menu of files from the working 
#' directory.
#' @param folder.name The name of the folder to be created.  Default NULL 
#' creates a file in the working directory with the creation data and time stamp.
#' @return \code{delete} permanently removes a file/directory.
#' @seealso  \code{\link[base]{unlink}}, 
#' \code{\link[base]{file.remove}}, 
#' \code{\link[base]{dir.create}}
#' @keywords file, delete, folder
#' @rdname file_handling
#' @export
#' @examples
#' \dontrun{
#' (x <- folder("DELETE.ME"))
#' which(dir() == "DELETE.ME")
#' delete("DELETE.ME")
#' which(dir() == "DELETE.ME")
#' }
delete <-
function(file = NULL) {
    x <- if (is.null(file)) {
        menu(dir())
    } else {
        file
    }
    unlink(x, recursive = TRUE, force = FALSE)
}

#' Create Folder
#' 
#' \code{folder} - Create a folder/directory.
#' 
#' @return \code{folder} creates a folder/directory.
#' @rdname file_handling
#' @export
folder <-
function(folder.name = NULL) {
    if (is.null(folder.name)) {
        SS <- gsub(":", ".", substr(Sys.time(), 1, 19))
        FN <-paste(substr(SS, 1, 10), "  Time", substr(SS, 11, 19), sep = "")
    } else {
        FN <-folder.name
    }
    if (length(unlist(strsplit(FN, "/"))) == 1) {
        x <- paste(getwd(), "/", FN, sep = "")
    } else {
        x <- FN
    }
    dir.create(x)
    return(x)
}