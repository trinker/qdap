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
#' 
#' ##============================##
#' ## INTERACTIVE SPELL CHECKING ##
#' ##============================##
#' 
#' ## No misspellings found
#' check_spelling_interactive(DATA$state)
#' 
#' ## minimal character method approach
#' dat <- DATA$state; dat[1] <- "I likedd the cokie icekream"
#' (o <- check_spelling_interactive(dat))
#' preprocessed(o)
#' fixit <- attributes(o)$correct
#' fixit(dat)
#' 
#' ## character method approach
#' m <- check_spelling_interactive(mraja1spl$dialogue[1:75])
#' preprocessed(m)
#' fixit <- attributes(m)$correct
#' fixit(mraja1spl$dialogue[1:75])
#' 
#' ## check_spelling method approach
#' out <- check_spelling(mraja1spl$dialogue[1:75], max.distance = .15)
#' (x <- check_spelling_interactive(out))
#' preprocessed(x)
#' correct(x)(mraja1spl$dialogue[1:75])
#' (y <- check_spelling_interactive(out, click=FALSE))
#' preprocessed(y)
#' }
check_spelling <- function(text.var, range = 2, max.distance = .15,
    assume.first.correct = TRUE, 
    dictionary = qdapDictionaries::GradyAugmented, parallel = TRUE, 
    cores = parallel::detectCores()/2) {

    dictnchar <- nchar(dictionary)
    dictfirst <- substring(dictionary, 1, 1)

    if (parallel && cores > 1){

        cl <- parallel::makeCluster(mc <- getOption("cl.cores", cores))
        vars <- c("text.var", "Ldist", "which_misspelled", "dictnchar",
            "dictfirst", "bag_o_words", "range", "max.distance", "mgsub")
        
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
    if (identical(unname(out), list())) {
        message("No spelling errors detected. Spell check complete.")
        return(invisible(NULL))
    }

    out <- data.frame(row = rep(as.numeric(names(out)), sapply(out, nrow)),
        do.call(rbind, out), stringsAsFactors = FALSE, row.names = NULL)

    class(out)  <- c("check_spelling", "which_misspelled", class(out))
    attributes(out)[["text.var"]] <- text.var
    out
}

#' Check Spelling
#' 
#' \code{which_misspelled}  - Check the spelling for a string. 
#' 
#' @param x If \code{which_misspelled} - A character string.  If \code{correct} -
#' An object from \code{check_spelling_interactive}.
#' @param suggest logical.  If \code{TRUE} returns a 
#' \code{\link[base]{data.frame}} with possible suggestions for misspelled words 
#' (words not found in the dictionary).
#' @param nchar.dictionary A vector that correponds in length and content to 
#' \code{dictionary} with elements that are the precalculated number of 
#' characters for each word in the dictionary.
#' @param first.char.dictionary A vector that corresponds in length ans content 
#' to \code{dictionary} with elements that are the pre-allotted first characters
#' of each word in the dictionary.
#' @return \code{which_misspelled} - Returns either a named vector (names are 
#' the word number) of possible misspelled words (if\code{suggestions = FALSE}) 
#' or a \code{\link[base]{data.frame}} with \code{word.no} (number of misspelled 
#' word), \code{not.found} (a word not found in the dictionary),
#' \code{suggestion} (the most likely replacement for the word), and 
#' \code{more.suggestions} (A list of vectors of up to 10 most likely replacements).
#' @rdname check_spelling
#' @export
which_misspelled <- function(x, suggest = FALSE, range = 2, max.distance = .15,
    assume.first.correct = TRUE, dictionary = qdapDictionaries::GradyAugmented,
    nchar.dictionary = nchar(dictionary), 
    first.char.dictionary = substring(dictionary, 1, 1)) {

    if (is.na(x)) return(NULL)

    ## break into a bag of words
    wrds <- bag_o_words(x)
    names(wrds) <- 1:length(wrds)

    ## determine if found in dictionary
    misses <- wrds[is.na(match(mgsub(c("'d", "'s"), "", wrds), dictionary))]
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
        stringsAsFactors = FALSE, row.names=NULL)

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

#' Generic Check Spelling Interactive Method
#' 
#' \code{check_spelling_interactive} - Interactively check spelling.
#' 
#' @param click logical.  If \code{TRUE} the interface is a point and click GUI.
#' If \code{FALSE} the interace is command line driven.
#' @note The user may go back (undo) pressing \code{"TYPE MY OWN"} entering 
#' either \code{"!"} (not) or \code{"0"} (similar to a phone system).  The 
#' second choice in the \code{"SELECT REPLACEMNT:"} will be the original word.
#' @export
#' @rdname check_spelling
#' @return \code{check_spelling_interactive} - Returns a character vector with 
#' the corrected text, the replacement list (via an \code{attribute} to the 
#' character vector), and a function to correct the same spelling errors in 
#' subsequent text character vectors.
check_spelling_interactive <- function(text.var, range = 2, 
    max.distance = .15, assume.first.correct = TRUE, click = TRUE, 
    dictionary = qdapDictionaries::GradyAugmented, parallel = TRUE, 
    cores = parallel::detectCores()/2, ...) {

    text.var
    click
    range
    max.distance
    assume.first.correct
    dictionary
    parallel
    cores
    
    UseMethod("check_spelling_interactive")
}

#' Prints a check_spelling_interactive Object
#' 
#' Prints a check_spelling_interactive object.
#' 
#' @param x The check_spelling_interactive object.
#' @param \ldots ignored
#' @method print check_spelling_interactive
#' @export
print.check_spelling_interactive <- function(x, ...) {

    print(as.character(x))

}

#' Check Spelling
#' 
#' View check_spelling_interactive preprocessed.
#' 
#' check_spelling_interactive Method for preprocessed
#' @param x The \code{\link[qdap]{check_spelling_interactive}} object.
#' @param \ldots ignored
#' @export
#' @method preprocessed check_spelling_interactive
preprocessed.check_spelling_interactive <- function(x, ...){
    attributes(x)[["replacements"]]
}

#' Check Spelling
#' 
#' \code{correct} - Access the spell corrector function from a 
#' \code{"check_spelling_interactive"} object for subsequent text character 
#' vector spelling corrections.
#' 
#' @param x An object of the class \code{"check_spelling_interactive"}.
#' @param \ldots ignored
#' @return \code{correct} - Returns a function for correcting spelling errors.
#' @export
#' @rdname check_spelling
correct <- function(x, ...){
    attributes(x)[["correct"]]
}

#' Check Spelling
#' 
#' View character check_spelling_interactive.
#' 
#' character Method for check_spelling_interactive
#' @param text.var A \code{\link[base]{character}} object, specifically a 
#' text vector of character strings.
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
#' @param click logical.  If \code{TRUE} the interface is a point and click GUI.
#' If \code{FALSE} the interace is command line driven.
#' @param dictionary A character vector of terms to search for.  To reduce 
#' overhead it is expected that this dictionary is lower case, unique terms.
#' @param parallel logical.  If \code{TRUE} attempts to run the function on 
#' multiple cores.  Note that this may not mean a speed boost if you have one 
#' core or if the data set is smaller as the cluster takes time to create.  
#' @param cores The number of cores to use if \code{parallel = TRUE}.  Default 
#' is half the number of available cores.  
#' @param \ldots ignored
#' @export
#' @method check_spelling_interactive character
check_spelling_interactive.character <- function(text.var, range = 2, 
    max.distance = .15, assume.first.correct = TRUE, click = TRUE, 
    dictionary = qdapDictionaries::GradyAugmented, parallel = TRUE, 
    cores = parallel::detectCores()/2, ...) {

    out <- check_spelling(text.var = text.var, range = range, 
        max.distance = max.distance, assume.first.correct = assume.first.correct, 
        dictionary = dictionary, parallel = parallel, cores = cores)

    if (is.null(out)) {
        return(invisible(NULL))
    }

    out <- split(out, out[["not.found"]])
    suggests <- lapply(out, function(x) unlist(x[1, 4:5], use.names = FALSE))
    out <- lapply(out, "[", ,1:3)

    output <- check_spelling_interactive_helper(out, suggests, click, text.var=text.var)

    out <- mgsub(output[, 1], output[, 2], text.var, ignore.case = TRUE, fixed=FALSE)

    class(out)  <- c("check_spelling_interactive", class(out))
    attributes(out)[["replacements"]] <- output
    attributes(out)[["correct"]] <- function(text.var) {
            mgsub(output[, 1], output[, 2], text.var, ignore.case = TRUE, fixed=FALSE)
        }
    out
}

#' Check Spelling
#' 
#' View factor check_spelling_interactive.
#' 
#' factor Method for check_spelling_interactive
#' @param text.var A \code{\link[base]{factor}} object, specifically a text vector 
#' of factor strings.  Note that this method is provided for factors for 
#' convenience, ideally the user should supply a character vector rather than 
#' factor.
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
#' @param click logical.  If \code{TRUE} the interface is a point and click GUI.
#' If \code{FALSE} the interace is command line driven.
#' @param dictionary A character vector of terms to search for.  To reduce 
#' overhead it is expected that this dictionary is lower case, unique terms.
#' @param parallel logical.  If \code{TRUE} attempts to run the function on 
#' multiple cores.  Note that this may not mean a speed boost if you have one 
#' core or if the data set is smaller as the cluster takes time to create.  
#' @param cores The number of cores to use if \code{parallel = TRUE}.  Default 
#' is half the number of available cores.  
#' @param \ldots ignored
#' @export
#' @method check_spelling_interactive factor
check_spelling_interactive.factor <- function(text.var, range = 2, 
    max.distance = .15, assume.first.correct = TRUE, click = TRUE, 
    dictionary = qdapDictionaries::GradyAugmented, parallel = TRUE, 
    cores = parallel::detectCores()/2, ...) {

    out <- check_spelling(text.var = text.var, range = range, 
        max.distance = max.distance, assume.first.correct = assume.first.correct, 
        dictionary = dictionary, parallel = parallel, cores = cores)

    if (is.null(out)) {
        return(invisible(NULL))
    }

    out <- split(out, out[["not.found"]])
    suggests <- lapply(out, function(x) unlist(x[1, 4:5], use.names = FALSE))
    out <- lapply(out, "[", ,1:3)

    output <- check_spelling_interactive_helper(out, suggests, click, text.var=text.var)
    out <- mgsub(output[, 1], output[, 2], text.var, ignore.case = TRUE, fixed=FALSE)

    class(out)  <- c("check_spelling_interactive", class(out))
    attributes(out)[["replacements"]] <- output
    attributes(out)[["correct"]] <- function(text.var) {
            mgsub(output[, 1], output[, 2], text.var, ignore.case = TRUE, fixed=FALSE)
        }
    out
}

#' Check Spelling
#' 
#' View check_spelling check_spelling_interactive.
#' 
#' check_spelling Method for check_spelling_interactive
#' @param text.var A \code{\link[qdap]{check_spelling}} object.
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
#' @param click logical.  If \code{TRUE} the interface is a point and click GUI.
#' If \code{FALSE} the interace is command line driven.
#' @param dictionary A character vector of terms to search for.  To reduce 
#' overhead it is expected that this dictionary is lower case, unique terms.
#' @param parallel logical.  If \code{TRUE} attempts to run the function on 
#' multiple cores.  Note that this may not mean a speed boost if you have one 
#' core or if the data set is smaller as the cluster takes time to create.  
#' @param cores The number of cores to use if \code{parallel = TRUE}.  Default 
#' is half the number of available cores.  
#' @param \ldots ignored
#' @export
#' @method check_spelling_interactive check_spelling
check_spelling_interactive.check_spelling <- function(text.var, range = 2, 
    max.distance = .15, assume.first.correct = TRUE, click = TRUE, 
    dictionary = qdapDictionaries::GradyAugmented, parallel = TRUE, 
    cores = parallel::detectCores()/2, ...) {

    out <- split(out, out[["not.found"]])
    suggests <- lapply(out, function(x) unlist(x[1, 4:5], use.names = FALSE))
    out <- lapply(out, "[", ,1:3)

    output <- check_spelling_interactive_helper(out, suggests, click, 
        text.var=attributes(text.var)[["text.var"]])

    out <- mgsub(output[, 1], output[, 2], attributes(text.var)[["text.var"]], 
        ignore.case = TRUE, fixed=FALSE)

    class(out)  <- c("check_spelling_interactive", class(out))
    attributes(out)[["replacements"]] <- output
    attributes(out)[["correct"]] <- function(text.var) {
            mgsub(output[, 1], output[, 2], text.var, ignore.case = TRUE, fixed=FALSE)
        }
    out
}

check_spelling_interactive_helper <- function(out, suggests, click, text.var) {

    text.var <- as.character(text.var)

    data.frame(do.call(rbind, Map(function(x, y) {
    
        Line <- gsub(x[["not.found"]][1], paste0("<<", x[["not.found"]][1], ">>"), 
            tolower(as.character(text.var[x[["row"]]])))

        under <- paste(rep("=", max(nchar(Line))), collapse="")
        invisible(lapply(Line, function(x) message("LINE: ", x)))
        comb <- c(x[["not.found"]], y)
        repl2 <- NULL

        if (click) {
            repl <- select.list(c("TYPE MY OWN", comb), title = "SELECT REPLACEMENT:")
            if (repl == "TYPE MY OWN") {
                ans <- "1" 
            } else { 
                ans <- "N"
            }
        } else {
            message(under, "\nSELECT REPLACEMENT:")
            ans <- menu(c("TYPE MY OWN", comb))
        }

        if (ans == "1") {
            message("\n","Enter Repalcement:","\n")  
            repl <- scan(n=1, what = character(0), quiet=TRUE)
            while (repl %in% c("0", "!")) {

                if (click) {
                    ans <- select.list(c("TYPE MY OWN", comb), 
                        title = "SELECT REPLACEMNT:")
                    if (ans == "TYPE MY OWN") {
                        ans <- "1" 
                        repl2 <- NULL
                    } else { 
                        repl <- ans
                        ans <- "N"
                        break
                    }
                } else {
                    message(under, "\nSELECT REPLACEMNT:")
                    ans <- menu(c("TYPE MY OWN", comb))
                }
                message("\n","ENTER REPLACEMENT:","\n")  
                repl <- scan(n=1, what = character(0), quiet=TRUE)
            }
            if (click && !is.null(repl2)) repl <- repl2
        } else {
            if (!click) {
                repl <- comb[ans - 1]
            }
        }

        data.frame(not.found = x[["not.found"]][1], 
            replacement = repl, stringsAsFactors = FALSE)

    }, out, suggests)), row.names=NULL, stringsAsFactors = FALSE)
}
