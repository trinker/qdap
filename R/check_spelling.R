#' Check Spelling
#' 
#' \code{check_spelling} - Check the spelling for an vector of strings.  The 
#' function use the following technique:\cr
#' \itemize{
#'   \item Separate the words from a string into a bag of words.
#'   \item Look those words up in a dictionary to find words not recognized/found (considered possibly misspelled).
#'   \item These misses (possible misspellings) will be what is looked up for suggested replacements.
#'   \item Optionally, reduce dictionary by assuming the first letter of the misspelled word is correct (dictionary for this letter only).
#'   \item Reduce dictionary by eliminating words outside of the range of number of characters of the misspelled word.
#'   \item Reduce dictionary further by eliminating words with \code{\link[base]{agrep}} to within a preset tolerance (\code{max.distance}). 
#'   \item Use a binary lookup (that reverse engineers \code{\link[base]{agrep}}) to determine the word from dictionary that is closest to the misspelled term.
#' }
#'
#' @param text.var The text variable.
#' @param range An integer of length 1 to use as a range for number of 
#' characters, beyond the number of characters of a word not found in the 
#' \code{dictionary}, to initially limit \code{dictionary} size and thus time to 
#' find a suggested replacement term.  This may be expanded if no suitable 
#' suggestion is returned.
#' @param max.distance A maximum distance to use as a starting \code{dictionary} 
#' reduction as the \code{max.distance} passed to \code{\link[base]{agrep}}.  
#' This may be increased if no suitable suggestion is returned.
#' @param assume.first.correct logical.  If \code{TRUE} it is assumed that the 
#' first letter of the misspelled word is correct.  This reduces the dictionary 
#' size, thus speeding up computation.
#' @param dictionary A character vector of terms to search for.  To reduce 
#' overhead it is expected that this dictionary is lower case, unique terms.
#' @param parallel logical.  If \code{TRUE} attempts to run the function on 
#' multiple cores.  Note that this may not mean a speed boost if you have one 
#' core or if the data set is smaller as the cluster takes time to create.  
#' @param cores The number of cores to use if \code{parallel = TRUE}.  Default 
#' is half the number of available cores.  
#' @return \code{check_spelling} - Returns a \code{\link[base]{data.frame}} with 
#' \code{row} (row number), \code{not.found}  \code{word.no} (number of 
#' misspelled word), \code{not.found} (a word not found in the dictionary), 
#' \code{suggestion} (the most likely replacement for the word), and 
#' \code{more.suggestions} (A list of vectors of up to 10 most likely replacements).
#' @export
#' @rdname check_spelling
#' @references \url{http://stackoverflow.com/a/24454727/1000343}
#' @note A possible misspelled word is defined as not found in the 
#' \code{dictionary}.  Currently, the \code{dictionary} used is smaller and may 
#' not contain words with endings.  This may lead to false positives for 
#' misspelled words.
#' @examples
#' \dontrun{
#' x <- "Robots are evl creatres and deserv exterimanitation."
#' which_misspelled(x, suggest=FALSE)
#' which_misspelled(x, suggest=TRUE)
#'
#' check_spelling(DATA$state)
#' check_spelling(DATA$state, stem=FALSE)
#' 
#' ## browseURL("http://stackoverflow.com/a/24454727/1000343")
#' terms <- c("accounts", "account", "accounting", "acounting", "acount", "acounts", "accounnt")
#' 
#' set.seed(10)
#' (fake_text <- unlist(lapply(terms, function(x) {
#'     unbag(sample(c(x, sample(DICTIONARY[[1]], sample(1:5, 1)))))
#' })))
#' 
#' check_spelling(fake_text)
#' }
check_spelling <- function(text.var, range = 2, max.distance = .05,
    assume.first.correct = TRUE, 
    dictionary = qdapDictionaries::GradyAugmented, parallel = TRUE, 
    cores = parallel::detectCores()/2) {

    dictnchar <- nchar(dictionary)
    dictfirst <- substring(dictionary, 1, 1)

    if (parallel && cores > 1){

        cl <- parallel::makeCluster(mc <- getOption("cl.cores", cores))
        vars <- c("text.var", "Ldist", "which_misspelled", "dictnchar",
            "dictfirst", "bag_o_words", "range", "max.distance")
        
        parallel::clusterExport(cl=cl, varlist=vars, envir = environment())
        
        out <- setNames(parallel::parLapply(cl, text.var, which_misspelled, 
            range = range, suggest = TRUE, max.distance = max.distance, 
            assume.first.correct = assume.first.correct,
            dictionary = dictionary, nchar.dictionary = dictnchar, 
            first.char.dictionary=dictfirst), 1:length(text.var))
 
        parallel::stopCluster(cl)

    } else {

        out <- setNames(lapply(text.var, which_misspelled, range = range, 
            suggest = TRUE, max.distance = max.distance, 
            assume.first.correct = assume.first.correct,
            dictionary = dictionary, nchar.dictionary = dictnchar, 
            first.char.dictionary=dictfirst), 1:length(text.var))
    }


    out <- out[!sapply(out, is.null)]

    out <- data.frame(row = rep(as.numeric(names(out)), sapply(out, nrow)),
        do.call(rbind, out), stringsAsFactors = FALSE, row.names = NULL)

    class(out)  <- c("check_spelling", "which_misspelled", class(out))
    out
}

#' Check Spelling
#' 
#' \code{which_misspelled}  - Check the spelling for a string. 
#' 
#' @param x A character string.
#' @param suggest logical.  If \code{TRUE} returns a 
#' \code{\link[base]{data.frame}} with possible suggestions for misspelled words 
#' (words not found in the dictionary).
#' @return \code{which_misspelled} - Returns either a named vector (names are 
#' the word number) of possible misspelled words (if\code{suggestions = FALSE}) 
#' or a \code{\link[base]{data.frame}} with \code{word.no} (number of misspelled 
#' word), \code{not.found} (a word not found in the dictionary),
#' \code{suggestion} (the most likely replacement for the word), and 
#' \code{more.suggestions} (A list of vectors of up to 10 most likely replacements).
#' @rdname check_spelling
#' @export
which_misspelled <- function(x, suggest = FALSE, range = 2, max.distance = .05,
    assume.first.correct = TRUE, dictionary = qdapDictionaries::GradyAugmented,
    nchar.dictionary = nchar(dictionary), 
    first.char.dictionary = substring(dictionary, 1, 1)) {

    if (is.na(x)) return(NULL)

    ## break into a bag of words
    wrds <- bag_o_words(x)
    names(wrds) <- 1:length(wrds)

    ## determine if found in dictionary
    misses <- wrds[is.na(match(wrds, dictionary))]
    if (identical(character(0), unname(misses))) return(NULL)
    if (!suggest) return(misses)

    if(assume.first.correct) {
        dictionary <- split(data.frame(dictionary, nchar.dictionary, 
            stringsAsFactors = FALSE), first.char.dictionary)
    } else {
        ## compute nchar for dictionary for use in reducing possibilities
        chars <- nchar(dictionary)
    }

    replacements <- lapply(misses, function(x, wchar = nchar.dictionary, 
        therange = range, thedist = max.distance) {

        if(assume.first.correct) {
            dictionary <- dictionary[[substring(x, 1, 1)]]
            chars <- dictionary[[2]]
            dictionary <- dictionary[[1]]
        }

        wchar <- nchar(x)
        dict <- character(0)
        while(identical(character(0), dict)) {
            dict <- dictionary[chars >= max(1, wchar - therange) & chars <= (wchar + therange)]
            if (identical(character(0), dict)) {
                message(paste("Range was not large enough.  Increasing range to", 
                    therange + 1, "for: ", x))
                therange <- therange + 1
                flush.console()
            }
        }

        dict2 <- character(0)      
        while(identical(character(0), dict2)) {
            dict2 <- dict[agrep(x, dict, max.distance=thedist)]

            if (identical(character(0), dict2)) {
                message(paste("max.distance was not large enough.  Increasing max.distance to", 
                    thedist + .05, "for: ", x))
                thedist <- thedist + .05
                flush.console()
            } else {
                dict <- dict2
            }
        }
        names(sort(setNames(unlist(lapply(dict, Ldist, x)), 
            dict), decreasing =TRUE)[1:(min(10, length(dict)))])
    })

    out <- data.frame(word.no = names(misses), not.found=misses, 
        suggestion = unlist(lapply(replacements, "[", 1)), 
        stringsAsFactors = FALSE)

    out$more.suggestions <- lapply(replacements, function(x) x[-1])
    class(out)  <- c("which_misspelled", class(out))
    out
}

#' Prints a which_misspelled Object
#' 
#' Prints a which_misspelled object.
#' 
#' @param x The which_misspelled object.
#' @param \ldots ignored
#' @method print which_misspelled
#' @export
print.which_misspelled <- function(x, ...){

    WD <- options()[["width"]]
    options(width = 3000)
    class(x) <- "data.frame"
    cat("\n")
    print(x)
    options(width = WD)
}
