#' Add Title to Select qdap Plots
#' 
#' Add title to select qdap objects that store a plot.
#' 
#' @param object A select qdap object that stores a plot.
#' @param value The value to assign to title.
#' @rdname Title
#' @keywords title
#' @export
Title <- function(object) {
    attributes(object)[["title"]] 
}

#' @rdname Title
#' @export
"Title<-" <- function(object, value) {
    attributes(object)[["title"]] <- value
    object
}
