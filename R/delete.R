#' Easy File Handling
#' 
#' \code{delete} - Deletes files and directories.
#' 
#' @param file The name of the file in the working directory or the path to the 
#' file to be deleted.  If NULL provides a menu of files from the working 
#' directory.
#' @param \ldots The name(s) of the folder to be created.  If both \ldots and
#' \code{folder.name} are NULL creates a file in the working directory with the 
#' creation date and time stamp.
#' @param folder.name A character vector of the name(s) of the folder to be 
#' created.  Default NULL (if \ldots is NULL too) creates a file in the working 
#' directory with the creation date and time stamp.  Use this argument only if 
#' the directory names contain spaces.
#' @return \code{delete} permanently removes a file/directory.
#' @seealso  \code{\link[base]{unlink}}, 
#' \code{\link[base]{file.remove}}, 
#' \code{\link[base]{dir.create}}
#' @keywords file, delete, folder
#' @rdname file_handling
#' @export
#' @examples
#' \dontrun{
#' ##  (x <- folder("DELETE.ME"))
#' ##  which(dir() == "DELETE.ME")
#' ##  delete("DELETE.ME")
#' ##  which(dir() == "DELETE.ME")
#' ##  
#' ##  folder(cat, dog)
#' ##  lapply(c("cat", "dog"), delete)
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
folder <- function(..., folder.name = NULL) {
    if (!is.null(folder.name)) {
        x <- strsplit(terms, split = split)
    } else {
        x <- folder.name
    }
    x <- unblanker(scrubber(unlist(lapply(x, function(y) {
        as.character(y)}))))
    hfolder <- function(folder.name = NULL) {
        if (is.null(folder.name)) {
            FN <- mgsub(c(":", " "), c(".", "_"), 
                substr(Sys.time(), 1, 19))
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
    if (is.null(x)) {
        hfolder()
    } else {
        if (length(x) == 1) {
            hfolder(x)
        } else {
            lapply(x, function(z) {
                hfolder(z)
            })
        }
    }
}
