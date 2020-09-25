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
#'   \item Use \code{\link[stringdist]{stringdist}} to find string distances between possible replacements and the misspelled term.
#'   \item Select \emph{n} (\code{n.suggests}) terms from dictionary that are closest to the misspelled term.
#' }
#'
#' @param text.var The text variable.
#' @param range An integer of length 1 to use as a range for number of 
#' characters, beyond the number of characters of a word not found in the 
#' \code{dictionary}, to initially limit \code{dictionary} size and thus time to 
#' find a suggested replacement term.  This may be expanded if no suitable 
#' suggestion is returned.
#' @param assume.first.correct logical.  If \code{TRUE} it is assumed that the 
#' first letter of the misspelled word is correct.  This reduces the dictionary 
#' size, thus speeding up computation.
#' @param method Method for distance calculation. The default is "jaccard".  It 
#' is assumed that smaller measures indicate closer distance.  Measures that do 
#' not adhere to this assumption will result in incorrect output (see 
#' \code{\link[stringdist]{stringdist}} for details).
#' @param dictionary A character vector of terms to search for.  To reduce 
#' overhead it is expected that this dictionary is lower case, unique terms.
#' @param parallel logical.  If \code{TRUE} attempts to run the function on 
#' multiple cores.  Note that this may not mean a speed boost if you have one 
#' core or if the data set is smaller as the cluster takes time to create.  
#' @param cores The number of cores to use if \code{parallel = TRUE}.  Default 
#' is half the number of available cores. 
#' @param n.suggests The number of terms to suggest.  In the case of a tie 
#' (multiple terms have the same distance from misspelled word) all will be provided.  
#' Dictionary reduction may result in less than \code{n.suggests} suggested terms.
#' @return \code{check_spelling} - Returns a \code{\link[base]{data.frame}} with 
#' \code{row} (row number), \code{not.found}  \code{word.no} (number of 
#' misspelled word), \code{not.found} (a word not found in the dictionary), 
#' \code{suggestion} (the most likely replacement for the word), and 
#' \code{more.suggestions} (A list of vectors of up to 10 most likely replacements).
#' @export
#' @rdname check_spelling
#' @seealso \code{\link[stringdist]{stringdist}}
#' @references https://stackoverflow.com/a/24454727/1000343 \cr
#' https://journal.r-project.org/archive/2011-2/RJournal_2011-2_Hornik+Murdoch.pdf
#' @note A possible misspelled word is defined as not found in the 
#' \code{dictionary}.  
#' @examples
#' \dontrun{
#' x <- "Robots are evl creatres and deserv exterimanitation."
#' which_misspelled(x, suggest=FALSE)
#' which_misspelled(x, suggest=TRUE)
#'
#' check_spelling(DATA$state)
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
#' ## character method approach (minimal example)
#' dat <- DATA$state; dat[1] <- "I likedd the cokie icekream"
#' (o <- check_spelling_interactive(dat))
#' preprocessed(o)
#' fixit <- attributes(o)$correct
#' fixit(dat)
#' 
#' ## character method approach (larger example)
#' m <- check_spelling_interactive(mraja1spl$dialogue[1:75])
#' preprocessed(m)
#' fixit <- attributes(m)$correct
#' fixit(mraja1spl$dialogue[1:75])
#' 
#' ## check_spelling method approach
#' out <- check_spelling(mraja1spl$dialogue[1:75])
#' (x <- check_spelling_interactive(out))
#' preprocessed(x)
#' correct(x)(mraja1spl$dialogue[1:75])
#' (y <- check_spelling_interactive(out, click=FALSE))
#' preprocessed(y)
#' 
#' ## Examine Methods (?stringdist::stringdist)
#' strings <- c(
#'     "Robots are evl creatres and deserv exterimanitation kream.",
#'     "I gots me a biggert measrue, tommorrow"
#' )
#' 
#' meths <- c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw")
#' 
#' stats::setNames(lapply(meths, function(x) check_spelling(strings, method=x)), meths)
#' }
check_spelling <- function(text.var, range = 2,
    assume.first.correct = TRUE, method = "jw",
    dictionary = qdapDictionaries::GradyAugmented, parallel = TRUE, 
    cores = parallel::detectCores()/2, n.suggests = 8) {

    dictnchar <- nchar(dictionary)
    dictfirst <- substring(dictionary, 1, 1)

    if (parallel && cores > 1){

        cl <- parallel::makeCluster(mc <- getOption("cl.cores", cores))
        vars <- c("text.var", "which_misspelled", "dictnchar",
            "dictfirst", "bag_o_words", "range", "mgsub", "method", 
            "n.suggests")
        
        parallel::clusterExport(cl=cl, varlist=vars, envir = environment())
        
        out <- stats::setNames(parallel::parLapply(cl, text.var, which_misspelled, 
            range = range, suggest = TRUE, method = method, 
            assume.first.correct = assume.first.correct, n.suggests = n.suggests,
            dictionary = dictionary, nchar.dictionary = dictnchar, 
            first.char.dictionary=dictfirst), 1:length(text.var))
 
        parallel::stopCluster(cl)

    } else {

        out <- stats::setNames(lapply(text.var, which_misspelled, range = range, 
            suggest = TRUE,  method = method, n.suggests = n.suggests,
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
#' @param nchar.dictionary A vector that corresponds in length and content to 
#' \code{dictionary} with elements that are the precalculated number of 
#' characters for each word in the dictionary.
#' @param first.char.dictionary A vector that corresponds in length and content 
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
which_misspelled <- function(x, suggest = FALSE, range = 2, 
    assume.first.correct = TRUE, dictionary = qdapDictionaries::GradyAugmented,
    method = "jw", nchar.dictionary = nchar(dictionary), 
    first.char.dictionary = substring(dictionary, 1, 1), n.suggests = 8) {

    if (length(x) > 1) stop("`x` must be a single string")
    if (is.na(x)) return(NULL)
    if (is.non.alpha(x)) return(NULL)

    ## break into a bag of words
    wrds <- bag_o_words(x)
    names(wrds) <- 1:length(wrds)

    ## determine if found in dictionary
    misses <- wrds[is.na(match(mgsub(c("'d", "'s"), "", wrds), dictionary))]
    if (identical(character(0), unname(misses))) return(NULL)

    ## if no suggestions desired retruns misspelled words
    if (!suggest) return(misses)
    
    ## force eval of first.char.dictionary before
    ## possibly changing the dictionary
    first.char.dictionary <- force(first.char.dictionary)
 
    ## combine the words from dictionary with numb. characters
    dictionary <- data.frame(dictionary, nchar.dictionary, 
        stringsAsFactors = FALSE)
   
    ## if we assume the first letter is spelled correct we split
    ## the dictionary and take only those words begining with the at letter
    if(assume.first.correct && all(letters %in% sort(unique(first.char.dictionary)))) {
        dictionary <- split(dictionary, first.char.dictionary)
    } 

    replacements <- Map(function(x, y, dictionary2 = dictionary, 
        therange = range, n.sugg = n.suggests,
        meth = method) {

        ## if assume first grab the dictionary list that 
        ## starts with that letter
        if(assume.first.correct) {
            dictionary2 <- dictionary2[[substring(x, 1, 1)]]
        }

        ## grab words form dictionary within that range 
        ## of characters close to x
        dict <- character(0)
        while(identical(character(0), dict)) {
            dict <- dictionary2[dictionary2[[2]] >= max(1, y - therange) & dictionary2[[2]] <= (y + therange), 1]
            if (identical(character(0), dict)) {
                message(paste("Range was not large enough.  Increasing range to", 
                    therange + 1, "for: ", x))
                therange <- therange + 1
                utils::flush.console()
            }
        }

        ## find the disctionary word distance to the target ond sort
        ratings <- sort(stats::setNames(stringdist::stringdist(x, dict, method = meth), dict))
 
        ## return the top n terms in order of likliness
        names(ratings)[ratings <= ratings[min(n.sugg, length(ratings))]]

    }, misses, nchar(misses))

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
#' If \code{FALSE} the interface is command line driven.
#' @note \code{check_spelling_interactive} - The user may go back (undo) by 
#' pressing \code{"TYPE MY OWN"} entering either \code{"!"} (not) or \code{"0"} 
#' (similar to a phone system).  The second choice in the 
#' \code{"SELECT REPLACEMNT:"} will be the original word and is prefixed with 
#' \code{"IGNORE:"}.  Press this to keep the original word.
#' @export
#' @rdname check_spelling
#' @return \code{check_spelling_interactive} - Returns a character vector with 
#' the corrected text, the replacement list (via an \code{attribute} to the 
#' character vector), and a function to correct the same spelling errors in 
#' subsequent text character vectors.
check_spelling_interactive <- function(text.var, range = 2, 
    assume.first.correct = TRUE, click = TRUE, method = "jw",
    dictionary = qdapDictionaries::GradyAugmented, parallel = TRUE, 
    cores = parallel::detectCores()/2, n.suggests = 8, ...) {

    text.var
    click
    range
    assume.first.correct
    dictionary
    parallel
    cores
    n.suggests

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
#' @param assume.first.correct logical.  If \code{TRUE} it is assumed that the 
#' first letter of the misspelled word is correct.  This reduces the dictionary 
#' size, thus speeding up computation.
#' @param click logical.  If \code{TRUE} the interface is a point and click GUI.
#' If \code{FALSE} the interface is command line driven.
#' @param method Method for distance calculation. The default is "jaccard".  It 
#' is assumed that smaller measures indicate closer distance.  Measures that do 
#' not adhere to this assumption will result in incorrect output (see 
#' \code{\link[stringdist]{stringdist}} for details).
#' @param dictionary A character vector of terms to search for.  To reduce 
#' overhead it is expected that this dictionary is lower case, unique terms.
#' @param parallel logical.  If \code{TRUE} attempts to run the function on 
#' multiple cores.  Note that this may not mean a speed boost if you have one 
#' core or if the data set is smaller as the cluster takes time to create.  
#' @param cores The number of cores to use if \code{parallel = TRUE}.  Default 
#' is half the number of available cores.  
#' @param n.suggests The number of terms to suggest.  In the case of a tie 
#' (multiple terms have the same distance from misspelled word) all will be provided.  
#' Dictionary reduction may result in less than \code{n.suggests} suggested terms.
#' @param \ldots ignored
#' @export
#' @method check_spelling_interactive character
check_spelling_interactive.character <- function(text.var, range = 2, 
    assume.first.correct = TRUE, click = TRUE, method = "jw",
    dictionary = qdapDictionaries::GradyAugmented, parallel = TRUE, 
    cores = parallel::detectCores()/2, n.suggests = 8, ...) {

    out <- check_spelling(text.var = text.var, range = range, 
        assume.first.correct = assume.first.correct, 
        n.suggests = n.suggests, method = method, dictionary = dictionary, 
        parallel = parallel, cores = cores)

    if (is.null(out)) {
        return(invisible(NULL))
    }

    out <- split(out, out[["not.found"]])
    suggests <- lapply(out, function(x) unlist(x[1, 4:5], use.names = FALSE))
    out <- lapply(out, "[", ,1:3)

    output <- check_spelling_interactive_helper(out, suggests, click, text.var=text.var)
    
    output <- output[apply(output, 1, function(x) x[1] != x[2]), ]    

    out <- mgsub(paste0("\\b", output[[1]], "\\b"), output[[2]], text.var, ignore.case = TRUE, fixed=FALSE)

    class(out)  <- c("check_spelling_interactive", class(out))
    attributes(out)[["replacements"]] <- output
    attributes(out)[["correct"]] <- function(text.var) {
            mgsub(paste0("\\b", output[[1]], "\\b"), output[[2]], text.var, ignore.case = TRUE, fixed=FALSE)
        }
    message("\nSpelling Check Complete!\n")
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
#' @param assume.first.correct logical.  If \code{TRUE} it is assumed that the 
#' first letter of the misspelled word is correct.  This reduces the dictionary 
#' size, thus speeding up computation.
#' @param click logical.  If \code{TRUE} the interface is a point and click GUI.
#' If \code{FALSE} the interface is command line driven.
#' @param method Method for distance calculation. The default is "jaccard".  It 
#' is assumed that smaller measures indicate closer distance.  Measures that do 
#' not adhere to this assumption will result in incorrect output (see 
#' \code{\link[stringdist]{stringdist}} for details).
#' @param dictionary A character vector of terms to search for.  To reduce 
#' overhead it is expected that this dictionary is lower case, unique terms.
#' @param parallel logical.  If \code{TRUE} attempts to run the function on 
#' multiple cores.  Note that this may not mean a speed boost if you have one 
#' core or if the data set is smaller as the cluster takes time to create.  
#' @param cores The number of cores to use if \code{parallel = TRUE}.  Default 
#' is half the number of available cores.  
#' @param n.suggests The number of terms to suggest.  In the case of a tie 
#' (multiple terms have the same distance from misspelled word) all will be provided.  
#' Dictionary reduction may result in less than \code{n.suggests} suggested terms.
#' @param \ldots ignored
#' @export
#' @method check_spelling_interactive factor
check_spelling_interactive.factor <- function(text.var, range = 2, 
    assume.first.correct = TRUE, click = TRUE, method = "jw",
    dictionary = qdapDictionaries::GradyAugmented, parallel = TRUE, 
    cores = parallel::detectCores()/2, n.suggests = 8, ...) {

    out <- check_spelling(text.var = text.var, range = range, 
        assume.first.correct = assume.first.correct, 
        n.suggests = n.suggests, method = method, dictionary = dictionary, 
        parallel = parallel, cores = cores)


    if (is.null(out)) {
        return(invisible(NULL))
    }

    out <- split(out, out[["not.found"]])
    suggests <- lapply(out, function(x) unlist(x[1, 4:5], use.names = FALSE))
    out <- lapply(out, "[", ,1:3)

    output <- check_spelling_interactive_helper(out, suggests, click, text.var=text.var)

    output <- output[apply(output, 1, function(x) x[1] != x[2]), ]
    
    out <- mgsub(paste0("\\b", output[[1]], "\\b"), output[[2]], text.var, ignore.case = TRUE, fixed=FALSE)

    class(out)  <- c("check_spelling_interactive", class(out))
    attributes(out)[["replacements"]] <- output
    attributes(out)[["correct"]] <- function(text.var) {
            mgsub(paste0("\\b", output[[1]], "\\b"), output[[2]], text.var, ignore.case = TRUE, fixed=FALSE)
        }
    message("\nSpelling Check Complete!\n")    
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
#' @param assume.first.correct logical.  If \code{TRUE} it is assumed that the 
#' first letter of the misspelled word is correct.  This reduces the dictionary 
#' size, thus speeding up computation.
#' @param click logical.  If \code{TRUE} the interface is a point and click GUI.
#' If \code{FALSE} the interface is command line driven.
#' @param method Method for distance calculation. The default is "jaccard".  It 
#' is assumed that smaller measures indicate closer distance.  Measures that do 
#' not adhere to this assumption will result in incorrect output (see 
#' \code{\link[stringdist]{stringdist}} for details).
#' @param dictionary A character vector of terms to search for.  To reduce 
#' overhead it is expected that this dictionary is lower case, unique terms.
#' @param parallel logical.  If \code{TRUE} attempts to run the function on 
#' multiple cores.  Note that this may not mean a speed boost if you have one 
#' core or if the data set is smaller as the cluster takes time to create.  
#' @param cores The number of cores to use if \code{parallel = TRUE}.  Default 
#' is half the number of available cores.  
#' @param n.suggests The number of terms to suggest.  In the case of a tie 
#' (multiple terms have the same distance from misspelled word) all will be provided.  
#' Dictionary reduction may result in less than \code{n.suggests} suggested terms.
#' @param \ldots ignored
#' @export
#' @method check_spelling_interactive check_spelling
check_spelling_interactive.check_spelling <- function(text.var, range = 2, 
    assume.first.correct = TRUE, click = TRUE, method = "jw",
    dictionary = qdapDictionaries::GradyAugmented, parallel = TRUE, 
    cores = parallel::detectCores()/2, n.suggests = 8, ...) {

    out <- split(out, out[["not.found"]])
    suggests <- lapply(out, function(x) unlist(x[1, 4:5], use.names = FALSE))
    out <- lapply(out, "[", ,1:3)

    output <- check_spelling_interactive_helper(out, suggests, click, 
        text.var=attributes(text.var)[["text.var"]])

    output <- output[apply(output, 1, function(x) x[1] != x[2]), ]
    
    out <- mgsub(paste0("\\b", output[[1]], "\\b"), output[[2]], attributes(text.var)[["text.var"]], 
        ignore.case = TRUE, fixed=FALSE)

    class(out)  <- c("check_spelling_interactive", class(out))
    attributes(out)[["replacements"]] <- output
    attributes(out)[["correct"]] <- function(text.var) {
            mgsub(paste0("\\b", output[[1]], "\\b"), output[[2]], text.var, ignore.case = TRUE, fixed=FALSE)
        }
    message("\nSpelling Check Complete!\n")
    out
}

check_spelling_interactive_helper <- function(out, suggests, click, 
    text.var, n.suggests, method) {

    text.var <- as.character(text.var)

    data.frame(do.call(rbind, Map(function(x, y) {
    
        Line <- gsub(spaste(x[["not.found"]][1]), 
            spaste(paste0("<<", x[["not.found"]][1], ">>")), 
            spaste(tolower(as.character(text.var[x[["row"]]]))))

        under <- paste(rep("=", max(nchar(Line))), collapse="")
              
        invisible(lapply(Line, function(x) message("\nLINE: ", 
            paste(strwrap(x, 80), collapse="\n      "))))
        comb <- c(x[["not.found"]], y)
        repl2 <- NULL

        if (click) {
            repl <- utils::select.list(c("TYPE MY OWN", paste("IGNORE:", comb[1]), comb[-1]), 
                title = "SELECT REPLACEMENT:")
            if (repl == "TYPE MY OWN") {
                ans <- "1" 
            } else { 
                ans <- "N"
            }
        } else {
            message(under, "\nSELECT REPLACEMENT:")
            ans <- utils::menu(c("TYPE MY OWN", paste("IGNORE:", comb[1]), comb[-1]))
        }

        if (ans == "1") {
            message("\n","Enter Replacement:","\n")  
            repl <- readLines(n=1)

            while (repl %in% c("0", "!")) {

                if (click) {
                    ans <- utils::select.list(c("TYPE MY OWN", paste("IGNORE:", comb[1]), comb[-1]), 
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
                    ans <- utils::menu(c("TYPE MY OWN", comb))
                }
                message("\n","ENTER REPLACEMENT:","\n")  
                repl <- readLines(n=1)
            }
            if (click && !is.null(repl2)) repl <- repl2
        } else {
            if (!click) {
                repl <- comb[ans - 1]
            }
        }

        repl <- gsub("IGNORE: ", "", repl)

        data.frame(not.found = x[["not.found"]][1], 
            replacement = repl, stringsAsFactors = FALSE)

    }, out, suggests)), row.names=NULL, stringsAsFactors = FALSE)
}

#' Prints a check_spelling Object
#' 
#' Prints a check_spelling object.
#' 
#' @param x The check_spelling object.
#' @param \ldots ignored
#' @method print check_spelling
#' @export
print.check_spelling <- function(x, ...){

    WD <- options()[["width"]]
    options(width = 10000)
    class(x) <- "data.frame"
    x[["more.suggestions"]] <- sapply(x[["more.suggestions"]], paste, collapse=", ")
    print(left_just(x))
    options(width = WD)
}