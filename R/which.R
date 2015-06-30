## Helper function to find out which cm type is used
which.cm <- function(x) {
    if(methods::is(x, "cmtime")) {
        return("cmtime")
    }
    if(methods::is(x, "cmrange")) {
        return("cmrange")
    }
    NULL
}

## helper function to determine unit span used
which.unit <- function(x) {
    x <- gsub("unit_", "", class(x)[grepl("unit_", class(x))])
    poss <- c("character", "syllable", "words", "sentence")
    if(any(x %in% poss)) {
        return(x)
    }
    NULL
}

## Helper function to find out which cm list type is used
which.lcm <- function(x) {
    if(methods::is(x, "l2d_cmtime")) {
        return("l2d_cmtime")
    }
    if(methods::is(x, "l2d_cmrange")) {
        return("l2d_cmrange")
    }
    NULL
}

## Helper function to find out which cm repeated emasures if there is one
which.cmrm <- function(x) {
    out <- class(x)[grepl("vname_", class(x))]
    if (identical(out, character(0))) {
        NULL
    } else {
        gsub("vname_", "", out)
    }
}

## generic which class
which.class <- function(x, y) gsub(y, "", class(x)[grepl(y, class(x))])
