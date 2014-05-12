#' Search Columns of a Data Frame 
#' 
#' \code{Search} - Find terms located in columns of a data frame.
#' 
#' @param dataframe A dataframe object to search.
#' @param term A character string to search for.
#' @param column.name Optional column of the data frame to search (character 
#' name or integer index).
#' @param max.distance Maximum distance allowed for a match. Expressed either as 
#' integer, or as a fraction of the pattern length times the maximal 
#' transformation cost (will be replaced by the smallest integer not less than 
#' the corresponding fraction).
#' @param \ldots Other arguments passed to \code{agrep}.
#' @return \code{Search} - Returns the rows of the data frame that match the 
#' search term.
#' @rdname Search
#' @seealso \code{\link[qdap]{trans_context}}
#' @export
#' @examples
#' \dontrun{
#' ## Dataframe search:
#' (SampDF <- data.frame("islands"=names(islands)[1:32],mtcars, row.names=NULL))
#' 
#' Search(SampDF, "Cuba", "islands")
#' Search(SampDF, "New", "islands")
#' Search(SampDF, "Ho")
#' Search(SampDF, "Ho", max.distance = 0)
#' Search(SampDF, "Axel Heiberg")
#' Search(SampDF, 19) #too much tolerance in max.distance
#' Search(SampDF, 19, max.distance = 0)
#' Search(SampDF, 19, "qsec", max.distance = 0)
#' 
#' ##Boolean search:
#' boolean_search(DATA$state, " I ORliar&&stinks")
#' boolean_search(DATA$state, " I &&.", values=TRUE)
#' boolean_search(DATA$state, " I OR.", values=TRUE)
#' boolean_search(DATA$state, " I &&.")
#' 
#' ## Exclusion:
#' boolean_search(DATA$state, " I ||.", values=TRUE)
#' boolean_search(DATA$state, " I ||.", exclude = c("way", "truth"), values=TRUE)
#' 
#' ## From stackoverflow: http://stackoverflow.com/q/19640562/1000343
#' dat <- data.frame(x = c("Doggy", "Hello", "Hi Dog", "Zebra"), y = 1:4)
#' z <- data.frame(z =c("Hello", "Dog"))
#' 
#' dat[boolean_search(dat$x, paste(z$z, collapse = "OR")), ]
#' 
#' ## Binary operator version
#' dat[dat$x %bs% paste(z$z, collapse = "OR"), ]
#' 
#' ## Passing to `trans_context`
#' inds <- boolean_search(DATA.SPLIT$state, " I&&.|| I&&!", ignore.case = FALSE)
#' with(DATA.SPLIT, trans_context(state, person, inds=inds))
#' 
#' (inds2 <- boolean_search(raj$dialogue, spaste(paste(negation.words, 
#'     collapse = " || "))))
#' trans_context(raj$dialogue, raj$person, inds2)
#' }
Search <-
function(dataframe, term, column.name = NULL, max.distance = 0.02, ...) {
    cn <- column.name
    if (!is.null(column.name)) {
        HUNT <- agrep(term, dataframe[, cn], ignore.case = TRUE, 
            max.distance = max.distance, ...)
    } else {
        ser <- invisible(lapply(dataframe, function(x) {
            agrep(term, x, ignore.case = TRUE, max.distance = max.distance, ...)
        }))
        ser2 <- sort(unlist(ser))
        names(ser2) <- NULL
        HUNT <- unique(ser2)
    }
    dataframe[HUNT, ]
}


#' Boolean Term Search
#' 
#' \code{boolean_search} - Conducts a Boolean search for terms/strings within a 
#' character vector.
#' 
#' @param text.var The text variable.
#' @param terms A character string(s) to search for.  The terms are arranged in 
#' a single string with AND (use \code{AND} or \code{&&} to connect terms 
#' together) and OR (use \code{OR} or \code{||} to allow for searches of 
#' either set of terms.  Spaces may be used to control what is searched for.  
#' For example using \code{" I "} on \code{c("I'm", "I want", "in")} will result
#' in \code{FALSE TRUE FALSE} whereas \code{"I"} will match all three (if case 
#' is ignored).
#' @param ignore.case logical.  If \code{TRUE} case is ignored.
#' @param values logical.  Should the values be returned or the index of the 
#' values. 
#' @param exclude Terms to exclude from the search.  If one of these terms is 
#' found in the sentence it cannot be returned.  
#' @param apostrophe.remove logical.  If \code{TRUE} removes apostrophes from 
#' the text before examining.
#' @param char.keep A character vector of symbol character (i.e., punctuation) 
#' that strip should keep.  The default is to strip everything except 
#' apostrophes. \code{\link[qdap]{termco}} attempts to auto detect characters to 
#' keep based on the elements in \code{match.list}. 
#' @param digit.remove logical.  If \code{TRUE} strips digits from the text 
#' before counting. \code{\link[qdap]{termco}} attempts to auto detect if digits 
#' should be retained based on the elements in \code{match.list}. 
#' @return \code{boolean_search} - Returns the values (or indices) of a vector of strings that match
#' given terms.
#' @details The terms string is first split by the OR separators into a list.  
#' Next the list of vectors is split on the AND separator to produce a list of 
#' vectors of search terms.  Each sentence is matched against the terms.  For a 
#' sentence to be counted it must fit all of the terms in an AND Boolean or one 
#' of the conditions in an OR Boolean.
#' @seealso \code{\link[qdap]{termco}}
#' @rdname Search
#' @export
boolean_search <- function(text.var, terms, ignore.case = TRUE, values = FALSE, 
    exclude=NULL, apostrophe.remove = FALSE, char.keep = NULL, 
    digit.remove = FALSE) {

    terms <- splitting(terms)

    if (ignore.case) {
        terms <- lapply(terms, function(x) {
            tolower(x)
        })
        vect <- tolower(text.var) 
    } else {
        vect <- text.var
    }

    ## find special characters
    if (any(gsub("[0-9a-zA-Z[:space:]]", "", unlist(terms)) != "")) { 
        specials <- unique(unlist(strsplit(gsub("[0-9a-zA-Z[:space:]]", "", 
            paste(paste("$", unlist(terms)), collapse="")), NULL)))
        char.keep <- unique(c(char.keep, specials))
    }   

    ## add extra space before punctuation
    if (!is.null(char.keep)) {
        vect <- mgsub(char.keep, paste0("", char.keep), vect)
    }


    out <- lapply(terms, function(x) {
        locs <- term.find(vect, x, char.keep = char.keep, 
            apostrophe.remove = apostrophe.remove, digit.remove = digit.remove)
        if (length(x) == 1) {
            c(locs)
        } else {
            len <- length(locs)
            tabs <- table(unlist(locs))
            as.numeric(names(tabs)[tabs == len])
 
        }
    })
    rnumb <- sort(unique(unlist(out)))

    ## Exclusion terms
    if (!is.null(exclude)) {
        rnumb <-  rnumb[!rnumb %in% unlist(term.find(vect, exclude, char.keep = char.keep, 
            apostrophe.remove = apostrophe.remove, digit.remove = digit.remove))]
    }

    if (identical(rnumb, numeric(0)) | identical(rnumb, integer(0))) {
        message("No elements meet the search criteria")
        return(invisible(rnumb))
    }

    if (values) {
        out <- text.var[rnumb]
    } else { 
        out <- rnumb
    }
    
    class(out) <- c("boolean_qdap", class(out))
    out
} 

#' Prints a boolean_qdap object
#' 
#' Prints a boolean_qdap object
#' 
#' @param x The boolean_qdap object
#' @param \ldots ignored
#' @export
#' @method print boolean_qdap
print.boolean_qdap <-
function(x, ...) {
    class(x) <-  class(x)[!class(x) %in% "boolean_qdap"]
    message("The following elements meet the criteria:")
    print(x)
}


## Helper function to split terms
## splitting("3AND4AND5OR4OR6AND7")
## splitting("3&&4&&5||4||6&&7")
## splitting("3&&4&&5OR4||6&&7")
## splitting(".&&||?")
splitting <- function(x) {
      out <- lapply(strsplit(x, "OR|\\|\\|"), function(y){
          strsplit(y, "AND|\\&\\&")
      })
      unlist(out, recursive = FALSE)
}

#' Search Columns of a Data Frame 
#' 
#' \code{\%bs\%} - Binary operator version of \code{\link[qdap]{boolean_search}} .
#' 
#' @rdname Search
#' @export
`%bs%` <- function(text.var, terms) {
    boolean_search(text.var = text.var, terms = terms)
}
