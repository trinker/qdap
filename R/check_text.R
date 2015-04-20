#' Check Text For Potential Problems
#' 
#' Uncleaned text may result in errors, warnings, and incorrect results in 
#' subsequent analysis.  \code{check_text} checks text for potential problems 
#' and suggests possible fixes.  Potential text anomalies that are detected 
#' include: factors, missing ending punctuation, empty cells, double punctuation, 
#' non-space after comma, no alphabetic characters, non-ascii, missing value, 
#' and potentially misspelled words.
#' 
#' @param text.var The text variable.
#' @param file A connection, or a character string naming the file to print to.  
#' If \code{NULL} prints to the console.  Note that this is assigned as an 
#' attribute and passed to \code{print}.
#' @return Returns a list with the following potential text faults reports:\cr
#' \itemize{
#'   \item{non_character}{- Text that is non-character.}
#'   \item{missing_ending_punctuation}{- Text with no endmark at the end of the string.}
#'   \item{empty}{- Text that contains an empty element (i.e., \code{""}).}
#'   \item{double_punctuation}{- Text that contains two \pkg{qdap} punctuation marks in the same string.}
#'   \item{non_space_after_comma}{- Text that contains commas with no space after them.}
#'   \item{no_alpha}{- Text that contains string elements with no alphabetic characters.}
#'   \item{non_ascii}{- Text that contains non-ASCII characters.}
#'   \item{missing_value}{- Text that contains missing values (i.e., \code{NA}).}
#'   \item{containing_escaped}{- Text that contains escaped (see \code{?Quotes}).}
#'   \item{containing_digits}{- Text that contains digits.}
#'   \item{indicating_incomplete}{- Text that contains endmarks that are indicative of incomplete/trailing sentences (e.g., \code{...}).}
#'   \item{potentially_misspelled}{- Text that contains potentially misspelled words.}
#' }
#' @note The output is a list but prints as a pretty formatted output with 
#' potential problem elements, the accompanying text, and possible suggestions 
#' to fix the text.
#' @keywords check text spelling
#' @export
#' @seealso \code{\link[qdap]{check_spelling_interactive}}
#' @examples
#' \dontrun{
#' x <- c("i like", "i want. thet them .", "I am ! that|", "", NA, 
#'     "they,were there", ".", "   ", "?", "3;", "I like goud eggs!", 
#'     "i 4like...", "\\tgreat",  "She said \"yes\"")
#' check_text(x)
#' print(check_text(x), include.text=FALSE)
#'
#' y <- c("A valid sentence.", "yet another!")
#' check_text(y)
#' }
check_text <- function(text.var, file = NULL) {

    non_character <- is.factor(text.var) 
    text.var <- as.character(text.var)
    missing <- which(is.na(text.var))
    pot_spell <- lapply(text.var, which_misspelled)
    misspelled <- which(sapply(pot_spell, function(x) !is.null(x)))

    if (is.empty.integer(missing)) missing <- NULL
    if (!non_character) non_character <- NULL
    if (is.empty.integer(misspelled)) misspelled <- NULL

    out <- list(
        non_character = non_character,
        missing_ending_punctuation = which.mp(text.var),
        empty = which.empty(text.var),
        double_punctuation = which.dp(text.var),
        non_space_after_comma = which.cns(text.var),
        no_alpha = which.non.alpha(text.var),
        non_ascii = which.non.ascii(text.var),
        missing_value = missing, 
        containing_escaped = which.escaped(text.var),
        containing_digits = which.digit(text.var),
        indicating_incomplete = which.incomplete(text.var),
        potentially_misspelled = misspelled
    )
    class(out) <- "check_text"
    attributes(out)[["text.var"]] <- text.var
    attributes(out)[["file"]] <- file
    attributes(out)[["misspelled"]] <- unname(unlist(pot_spell))
    out
}

#' Prints a check_text Object
#' 
#' Prints a check_text object.
#' 
#' @param x The check_text object.
#' @param include.text logical.  If \code{TRUE} the offending text is printed as 
#' well.
#' @param file A connection, or a character string naming the file to print to.  
#' If \code{NULL} prints to the console.
#' @param \ldots ignored
#' @method print check_text
#' @export
print.check_text <- function(x, include.text = TRUE, file = NULL, ...) {

    if (is.null(file)) file <- force(attributes(x)[["file"]])
    spelling <- force(attributes(x)[["misspelled"]])
    file <- ifelse(is.null(file), "", file)

    txt.var <- force(attributes(x)[["text.var"]])
    out <- Map(function(x, y, z) {
    
            nm <- toupper(gsub("_", " ", y))
            lns <- paste(rep("=", nchar(nm)), collapse="")
       
            if (y == "non_character") {

                if(is.null(x)) {
                    mess <- "\n  --IS CHARACTER--\n"
                    mess3 <- NULL
                } else {
                    mess <- "\nText is a factor."
                    mess3 <- paste("\n*Suggestion: Consider", z)
                }         
                
                return(c(paste0("\n", lns), nm, lns, mess, mess3))
            }        
        
            mess <- sprintf("\nThe following observations were %s:\n", 
                tolower(nm))
            if(is.null(x)) {
                affected <- "  --NONE FOUND--"
            } else {
                affected <- paste(x, collapse=", ")
            }
            if(is.null(z) | is.null(x)) {
                mess3 <- NULL
            } else {
                mess3 <- paste("\n*Suggestion: Consider", z)
            }   
            if (include.text & !is.null(x) & y != "missing_value") {
                mess2 <- sprintf("\nThe following text is %s:\n", tolower(nm))

                if (y == "potentially_misspelled" && !is.null(spelling)) {
                    spelling <- unique(spelling)
                    txt.var[x] <- Trim(mgsub(spaste(spelling), 
                        spaste(paste0("<<", spelling, ">>")), 
                        spaste(strip(txt.var[x], apostrophe.remove = FALSE))))
                } 
                affected.text <- txt.var[x]

                c(paste0("\n", lns), nm, lns, mess, affected, mess2, 
                    paste0(x, ": ", affected.text), mess3, "")
            } else {
                c(paste0("\n", lns), nm, lns, mess, affected, mess3, "")
            }
        }, 
        unclass(x), names(x), .check_messages)
    cat(paste(unlist(out), collapse="\n"), file = file)
}


.check_messages <- list(
    non_character = "using `as.character`", 
    missing_ending_punctuation = "cleaning the raw text or running `add_incomplete`",
    empty = "running `blank2NA`",
    double_punctuation = "running `sentSplit`",
    non_space_after_comma = "running `comma_spacer`",
    no_alpha = "cleaning the raw text",
    non_ascii = "exploring `?Encoding`",
    missing_value = NULL, 
    containing_escaped = "using `clean` or `scrubber`",
    containing_digits = "using `replace_number`",
    indicating_incomplete = "using `incomplete_replace`",   
    potentially_misspelled = "running `check_spelling_interactive`"
)

