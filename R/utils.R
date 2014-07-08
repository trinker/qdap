## rbinds and fills empties with zero
## http://stackoverflow.com/a/19804707/1000343
rbind_qdap <- function(x) {
    allnames <- unique(unlist(lapply(x, names)))
    data.frame(do.call(rbind, lapply(x, function(df) {
        not <- allnames[!allnames %in% names(df)]
        df[, not] <- 0
        df
    })), row.names = NULL)

}


## Chosing colour pallette that matches ggplot2's default colour pallette
## Compliments of John Colby
## http://stackoverflow.com/a/8197703/1000343
gg_color_hue <- function(n) {
    hues <- seq(15, 375, length=n+1)
    hcl(h=hues, l=65, c=100)[1:n]
}

## Pad number and character vectors
pad <- function (x, padding = max(nchar(as.character(x))), sort = TRUE, 
    type = "detect") {
    poss <- c("detect", "numeric", "character", "d", "s")
    if (!type %in% poss) 
        stop("type must be: \"detect\", \"numeric\"\\\"d\" or \"character\"\\\"s\"")
    Rel <- c(NA, "d", "s", "d", "s")
    type <- Rel[poss %in% type]
    if (is.na(type)) {
        type <- ifelse(is.numeric(x), "d", "s")
    }
    x <- sprintf(paste0("%0", padding, type), x)
    if (sort) {
        x <- sort(x)
    }
    x
}


## capture edges from an igraph object
## Needs igraph::E
edge_capture <- function(iobj) {

    data.frame(do.call(rbind, 
    strsplit(bracketX(capture.output(E(iobj)))[-c(1:2)], " -> ")))

}

## generate a from to based on vector of grouping variables
from_to <- function(x) {

    data.frame(from=as.character(x), to=c(as.character(x[-1]), 
        "end"), stringsAsFactors = FALSE)

}

## check if something inherits "qdap_hash"
is.hash <- function(x) inherits(x, "qdap_hash")

env_look <- function(terms, envir, missing = NA) {
    
    hits <- which(!is.na(match(terms, names(as.list(envir)))))
    x <- rep(ifelse(is.null(missing), NA, missing), length(terms))
    
    x[hits] <- recoder(terms[hits], envr = envir)

    if (is.null(missing)) { 
        x[is.na(x)] <- terms[is.na(x)]
        x
    }   
    x
	
}

## Helper function
recoder <- function(x, envr){                               
    x <- as.character(x) #turn the numbers to character                                                        
    unlist(lapply(x, get, envir = envr))                      
}  

## check if dplyr::tbl_df, wfm, tdm, dtm, missing punctuation, douple punctuation
is.tbl_df <- function(x) inherits(x, "tbl_df")
is.wfm <- function(x) inherits(x, "wfm")
is.tdm <- function(x) inherits(x, "TermDocumentMatrix")
is.dtm <- function(x) inherits(x, "DocumentTermMatrix")
## is missing punctiuation
is.mp <- function(x) any(suppressWarnings(na.omit(end_mark(x))) == "_")
is.empty <- function(x) any(na.omit(x) == "")
## is double punctuation
is.dp <- function(text.var) {
    punct <- c(".", "?", "!", "|")
    any(sapply(strsplit(text.var, NULL), function(x) {
        sum(x %in% punct) > 1
    }))
}
## is comma with no space
is.cns <- function(x) grepl("(,)([^ ])", x)
## x <- c("the, dog,went", "I,like,it", "where are you", NA, "why")
## is.cns(x)
is.empty.integer <- function(x) identical(integer(0), x)

is.dp2 <- function(text.var) {
    punct <- c(".", "?", "!", "|")
    sapply(strsplit(text.var, NULL), function(x) {
        sum(x %in% punct) > 1
    })
}

is.non.alpha <- function(x) {
    !is.na(x) & !grepl("[a-zA-Z]", x)
}

is.non.ascii <- function(x) {
    capture.output(nonascii <- tools::showNonASCII(x))
    x %in% unique(nonascii[!is.na(nonascii)])
}

which.incomplete <- function(x) {
    pat <- "\\?*\\?[.]+|[.?!]*\\? [.][.?!]+|[.?!]*\\. [.?!]+|[.?!]+\\. [.?!]*|[.?!]+\\.[.?!]*|[.?!]*\\.[.?!]+"
    out <- grep(pat, x)
    if(is.empty.integer(out)) return(NULL)
    out
}


which.escaped <- function(x) {
    out <- which(grepl("[\\\\]", x) & !grepl("\\\"|\\\'|\\\`", x))
    if(is.empty.integer(out)) return(NULL)
    out
}

which.mp <- function(x) {
    y <- suppressWarnings(end_mark(x))
    out <- which(!is.na(y) & y == "_")
    if(is.empty.integer(out)) return(NULL)
    out
}

which.empty <- function(x) {
    out <- which(!is.na(x) & x == "")
    if(is.empty.integer(out)) return(NULL)
    out
}

which.cns <- function(x) {
    out <- which(is.cns(x))
    if(is.empty.integer(out)) return(NULL)
    out
}

which.dp <- function(x){
    out <- which(is.dp2(x))
    if(is.empty.integer(out)) return(NULL)
    out
}

which.non.alpha <- function(x){
    out <- which(is.non.alpha(x))
    if(is.empty.integer(out)) return(NULL)
    out
}

which.non.ascii <- function(x){
    out <- which(is.non.ascii(x))
    if(is.empty.integer(out)) return(NULL)
    out
}

which.digit <- function(x) {
    out <- grep(paste(0:9, collapse="|"), x)    
    if(is.empty.integer(out)) return(NULL)
    out
}

