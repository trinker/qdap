function(file = NULL) {
    x <- if (is.null(file)) {
        menu(dir())
    } else {
        file
    }
    unlink(x, recursive = TRUE, force = FALSE)
}
