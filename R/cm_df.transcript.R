#' Transcript With Word Number
#' 
#' Output a transcript with word  number/index above for easy input back into 
#' qdap after coding.
#' 
#' @param text.var The text variable.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param file A connection, or a character string naming the file to print to 
#' (e.g., .doc, .txt).
#' @param indent Number of spaces to indent.
#' @param width Width to output the file (defaults to 70; this is generally a 
#' good width and indent for a .docx file).
#' @param space An integer value denoting the vertical spacing between the 
#' \code{grouping.var} and the numbered text (allow more space for more coding 
#' room) in the output of a text file.
#' @param \ldots Other arguments passed to strip.
#' @return Returns a transcript by grouping variable with word number above each 
#' word.  This makes use with \code{\link[qdap]{cm_df2long}} transfer/usage 
#' easier because the researcher has coded on a transcript with the numeric word 
#' index already.
#' @note It is recommended that the researcher actually codes on the output 
#' from this file.  The codes can then be transferred to via a list.  If a file 
#' already exists \code{cm_df.transcript} will append to that file.
#' @author BondedDust (stackoverflow.com), Gavin Simpson and Tyler Rinker <tyler.rinker@@gmail.com>
#' @seealso \code{\link[qdap]{cm_df2long}},
#' \code{\link[qdap]{cm_df.temp}}
#' @keywords transcript
#' @export
#' @examples
#' \dontrun{
#' with(DATA, cm_df.transcript(state, person))
#' with(DATA, cm_df.transcript(state, list(sex, adult)))
#' #use it with nested variables just to keep track of demographic info
#' with(DATA, cm_df.transcript(state, list(person, sex, adult)))
#' 
#' #use double tilde "~~" to keep word group as one word
#' DATA$state <- mgsub("be certain", "be~~certain", DATA$state, fixed = TRUE)
#' with(DATA, cm_df.transcript(state, person))
#' DATA <- qdap::DATA
#' 
#' ##  with(mraja1spl, cm_df.transcript(dialogue, list(person)))
#' ##  with(mraja1spl, cm_df.transcript(dialogue, list(sex, fam.aff, died)))
#' ##  with(mraja1spl, cm_df.transcript(dialogue, list(person), file="foo.doc"))
#' ##  delete("foo.doc")   #delete the file just created
#' }
cm_df.transcript <-
function (text.var, grouping.var, file = NULL, indent = 4, width = 70, 
    space = 2, ...) {
    if (is.list(grouping.var)) {
        grouping.var <- paste2(grouping.var)
    }
    DF <- sentCombine(text.var, grouping.var)
    DF[, "text.var"] <- clean(DF[, "text.var"])

    DF2 <- cm_df.temp(DF, "text.var", ...)
    y <- rle(as.character(DF2[, "grouping.var"]))
    lens <- y$lengths
    group <- y$values
    x <- cumsum(lens)
    L3 <- split(DF2, as.factor(rep(seq_along(x), lens)))
    invisible(lapply(seq_along(L3), function(i) {
        numbtext(L3[[i]][, "text"], width = width, lengths = L3[[i]][, 
            "word.num"], txt.file = file, name = group[i], 
            indent = indent, skip = i > 1, space = space)
    }))
}
