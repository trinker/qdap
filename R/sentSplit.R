#' Sentence Splitting
#' 
#' \code{sentSplit} - Splits turns of talk into individual sentences (provided 
#' proper punctuation is used).  This procedure is usually done as part of the 
#' data read in and cleaning process.
#' 
#' @param dataframe A dataframe that contains the person and text variable.
#' @param text.var The text variable.
#' @param rm.var An optional character vector of 1 or 2 naming the variables 
#' that are repeated measures (This will restart the \strong{"tot"} column).
#' @param endmarks A character vector of endmarks to split turns of talk into 
#' sentences.
#' @param incomplete.sub logical.  If \code{TRUE} detects incomplete sentences 
#' and replaces with \code{"|"}.
#' @param rm.bracket logical.  If \code{TRUE} removes brackets from the text.
#' @param stem.col logical.  If \code{TRUE} stems the text as a new column.
#' @param text.place A character string giving placement location of the text 
#' column. This must be one of the strings \code{"original"}, \code{"right"} or 
#' \code{"left"}.
#' @param verbose logical.  If \code{TRUE} select diagnostics from 
#' \code{\link[qdap]{check_text}} are reported. 
#' @param \ldots Additional options passed to \code{\link[qdap]{stem2df}}.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param tot A tot column from a \code{\link[qdap]{sentSplit}} output.
#' @return \code{sentSplit} - returns a dataframe with turn of talk broken apart 
#' into sentences.  Optionally a stemmed version of the text variable may be 
#' returned as well.
#' @section Warning: \code{\link[qdap]{sentSplit}} requires the dialogue (text) 
#' column to be cleaned in a particular way.  The data should contain qdap
#' punctuation marks (\code{c("?", ".", "!", "|")}) at the end of each sentence.
#' Additionally, extraneous punctuation such as abbreviations should be removed
#' (see \code{\link[qdap]{replace_abbreviation}}).
#' Trailing sentences such as \bold{I thought I...} will be treated as 
#' incomplete and marked with \code{"|"} to denote an incomplete/trailing 
#' sentence.
#' 
#' @section Suggestion: It is recommended that the user runs \code{\link[qdap]{check_text}} on the 
#' output of \code{sentSplit}'s text column.
#' @rdname sentSplit
#' @author Dason Kurkiewicz and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @seealso 
#' \code{\link[qdap]{bracketX}}, 
#' \code{\link[qdap]{incomplete_replace}},
#' \code{\link[qdap]{stem2df}} ,
#' \code{\link[qdap]{TOT}} 
#' @export
#' @examples
#' \dontrun{
#' ## `sentSplit` EXAMPLE:
#' (out <- sentSplit(DATA, "state"))
#' out %&% check_text()  ## check output text
#' sentSplit(DATA, "state", stem.col = TRUE)
#' sentSplit(DATA, "state", text.place = "left")
#' sentSplit(DATA, "state", text.place = "original")
#' sentSplit(raj, "dialogue")[1:20, ]
#' 
#' ## plotting
#' plot(out)
#' plot(out, grouping.var = "person")
#' 
#' out2 <- sentSplit(DATA2, "state", rm.var = c("class", "day"))
#' plot(out2)
#' plot(out2, grouping.var = "person")
#' plot(out2, grouping.var = "person", rm.var = "day")
#' plot(out2, grouping.var = "person", rm.var = c("day", "class"))
#'
#' ## `sentCombine` EXAMPLE:
#' dat <- sentSplit(DATA, "state") 
#' sentCombine(dat$state, dat$person)
#' truncdf(sentCombine(dat$state, dat$sex), 50)
#' 
#' ## `TOT` EXAMPLE:
#' dat <- sentSplit(DATA, "state") 
#' TOT(dat$tot)
#' 
#' ## `sent_detect`
#' sent_detect(DATA$state)
#' 
#' ## NLP based sentence splitting 
#' sent_detect_nlp(DATA$state)
#' }
sentSplit <-
function(dataframe, text.var, rm.var = NULL, endmarks = c("?", ".", "!", "|"), 
    incomplete.sub = TRUE, rm.bracket = TRUE, stem.col = FALSE, 
    text.place = "right", verbose = is.global(2),  ...) {

    if (verbose) {
        checks <- check_text(dataframe[[text.var]])
        checks <- checks[!names(checks) %in% c("double_punctuation", 
            "missing_value", "potentially_misspelled")]
        pot_probs <- !sapply(checks, is.null)
        if(sum(pot_probs) > 0) {
            probs <- gsub("_", " ", paste(names(pot_probs)[pot_probs], collapse=", "))
            warning("The following problems were detected:\n", probs, 
                "\n\n*Consider running `check_text`")
        }
    }

    if (is.null(rm.var)) {
        output <- sentSplit_helper(dataframe = dataframe, text.var = text.var, 
            endmarks = endmarks, incomplete.sub = incomplete.sub, 
            rm.bracket = rm.bracket, stem.col = stem.col, text.place = text.place
        )

    } else {

        if (length(rm.var) == 1) {
            rm_var_list <- dataframe[, rm.var[1]]
        } else {
            rm_var_list <- lapply(rm.var, function(x) dataframe[, x])
        }


        spdat <- split(dataframe, rm_var_list)
        output <- lapply(spdat, function(x){
            sentSplit(x, text.var = text.var, endmarks = endmarks, 
                incomplete.sub = incomplete.sub, rm.bracket = rm.bracket, 
                stem.col = stem.col, text.place = text.place)
        })

        if (length(rm.var) > 1) {
            rmvars <- lapply(dataframe[, rev(rm.var)], levels)
            rmvars <- expand.grid(rmvars)
            ord <- match(paste2(rmvars[, ncol(rmvars):1], sep = "."), names(output))
            output <- output[ord]
        }

        output <- data.frame(do.call(rbind, output), row.names = NULL)
    }
 
    if (!is.null(rm.var)) {
        rm.var <- paste0("rmvars_", paste(rm.var, collapse = ":"))
    } 
    class(output) <- unique(c("sent_split", "qdap_df", 
        paste0("sent_split_text_var:", text.var), rm.var, class(output)))
    attributes(output)[["text.var"]] <- text.var
    attributes(output)[["qdap_df_text.var"]] <- substitute(text.var)  
    
    output
}

sentSplit_helper <-
function(dataframe, text.var, endmarks = c("?", ".", "!", "|"), 
    incomplete.sub = TRUE, rm.bracket = TRUE, stem.col = FALSE, 
    text.place = "right", ...) {
    splitpoint <- paste0("[", paste("\\", endmarks, sep="", collapse=""), "]")
    if (is.numeric(text.var)) {
        text.var <- colnames(dataframe)[text.var]
    }
    if (incomplete.sub) {
        dataframe [, text.var] <- incomplete_replace(dataframe [, text.var])
    }
    if (rm.bracket) {
        dataframe [, text.var] <- bracketX(dataframe [, text.var])
    }
    if(length(dataframe) < 3) {
        dataframe $EXTRA1x2 <-  1:nrow(dataframe)
        dataframe $EXTRA2x2 <-  1:nrow(dataframe)
    } else {
        dataframe
    }
    input <- text.var
    breakinput <- function(input, splitpoint) {
        j <- gregexpr(splitpoint, input)
        lengths <- unlist(lapply(j, length))
        spots <- lapply(j, as.numeric)
        first <- unlist(lapply(spots, function(x) {
          c(1, (x + 1)[-length(x)])
        }))
        last <- unlist(spots)
        ans <- substring(rep(input, lengths), first, last)
        list(text = ans, lengths = lengths)
    }
    j <- breakinput(dataframe [, input], splitpoint)
    others <- dataframe [, -which(colnames(dataframe) %in% input), drop=FALSE]
    idx <- rep(1:dim(others)[1], j$lengths)
    ans <- data.frame(cbind(input = Trim(j$text), others[idx, ]))
    colnames(ans)[1] <- input
    vlen <- sapply(j$lengths, seq_len)
    ans$tot <- unlist(lapply(seq_along(vlen), function(i) paste0(i, ".", vlen[[i]])))
    if(any(stats::na.omit(ans[, input]==""))){
        NAdet <- ans[, input]==""
        NAdet[is.na(NAdet)] <- FALSE
        ans[NAdet, input] <- NA
    }
    if (text.place == "original") {
        ans <- ans[, c(colnames(dataframe), "tot")]
        if (stem.col) {
            ans <- stem2df(ans, which(colnames(ans) %in% text.var), ...)
        } 
    } else {
        if (text.place == "right") {
            ans <- data.frame(ans[, -1], ans[, 1])
            totn <- which(names(ans)=="tot")
            ans <- data.frame(ans[, 1, drop= FALSE], ans[, totn, drop = FALSE],
                ans[, -c(1, totn), drop = FALSE])
            colnames(ans) <- c(colnames(ans)[-ncol(ans)], input)
            if (stem.col) {
                ans <- stem2df(ans, ncol(ans), warn = FALSE, ...)
            }   
        } else {
            if (text.place == "left") {
                ans <- ans
                if (stem.col) {
                    ans <- stem2df(ans, 1, ...)
                }             
            } else {
                warning("incorrect text.place argument")
            }
        }
    }
    ans$EXTRA1x2 <- NULL
    ans$EXTRA2x2 <- NULL
    rownames(ans) <- NULL
    ans[, text.var] <- as.character(ans[, text.var])
    if (stem.col) {
        ans[, "stem.text"] <- as.character(ans[, "stem.text"])
    }
    ans
}

#' Combine Sentences 
#' 
#' \code{sentCombine} - Combines sentences by the same grouping variable together.
#' 
#' @param as.list logical.  If \code{TRUE} returns the output as a list. If 
#' \code{FALSE} the output is returned as a dataframe.
#' @return \code{sentCombine} - returns a list of vectors with the continuous 
#' sentences by grouping.var pasted together. 
#' returned as well.
#' @rdname sentSplit
#' @export
sentCombine <-
function(text.var, grouping.var = NULL, as.list = FALSE) {
    if(is.null(grouping.var)) {
        G <- "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            G <- paste(m, collapse="&")
        } else {
            G <- as.character(substitute(grouping.var))
            G <- G[length(G)]
        }
    }
    if(is.null(grouping.var)){
        grouping <- rep("all", length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- paste2(grouping.var)
        } else {
            grouping <- unlist(grouping.var)
        } 
    } 
    y <- rle(as.character(grouping))
    lens <- y$lengths
    group <- y$values
    x <- cumsum(lens)
    st <- c(1, x[-length(x)]+1)
    end <- c(x)
    L1 <- invisible(lapply(seq_along(st), function(i) {
        paste2(text.var[st[i]:end[i]], sep=" ")
    }))
    names(L1) <- group
    if (as.list) {
        return(L1)
    }
    DF <- data.frame(x=names(L1), text.var=unlist(L1), row.names=NULL)
    colnames(DF)[1] <- G
    DF
}


#' Convert the tot Column to Turn of Talk
#' 
#' \code{TOT} - Convert the tot column from \code{\link[qdap]{sentSplit}} to 
#' turn of talk index (no sub sentence).  Generally, for internal use.
#' 
#' @return \code{TOT} - returns a numeric vector of the turns of talk without 
#' sentence sub indexing (e.g. 3.2 become 3).
#' @rdname sentSplit
#' @export
TOT <-
function(tot){
    sapply(tot, function(x) {
        as.numeric(as.character(unlist(strsplit(as.character(x), ".", 
        fixed=TRUE))[[1]]))
    })
}


#' Prints a sent_split object
#' 
#' Prints a sent_split object
#' 
#' @param x The sent_split object
#' @param \ldots ignored
#' @export
#' @method print sent_split
print.sent_split <-
function(x, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    class(x) <- "data.frame"
    print(x)
    options(width=WD)
}

#' Plots a sent_split Object
#' 
#' Plots a sent_split object.
#' 
#' @param x The sent_split object.
#' @param text.var The text variable (character string).
#' @param rm.var An optional repeated measures character vector of 1 or 2 to 
#' facet by.  If \code{NULL} the \code{rm.var} from \code{sentSplit} is used.  To 
#' avoid this behavior use
#' \code{FALSE}.
#' @param \ldots Other arguments passed to \code{tot_plot}.
#' @method plot sent_split
#' @export
plot.sent_split <- function(x, text.var = NULL, rm.var = NULL, ...) {

    ## check for text var from class of x  
    if (is.null(text.var)) {
        dia <- "sent_split_text_var:"
        tv <- grepl(dia, class(x))
        text_var <- gsub(dia, "", class(x)[tv])
        if (!text_var %in% colnames(x)) {
            stop(paste("\nsentSplit object has been altered:",
                "please supply `text.var`"))
        }
    }

    ## check for repeated measure vars from class of x    
    if (is.null(rm.var)) {
        rmv <- "rmvars_"
        rv <- grepl(rmv, class(x))
        if (sum(rv) < 1){
            rm.var <- NULL
        } else {
            rm.var <- unlist(strsplit(gsub(rmv, "", class(x)[rv]), ":"))
            if (sum(rm.var %in% colnames(x)) != length(rm.var)) {
                warning("rm.var nit matched to column names: rm.var ignored")
                rm.var <- NULL
            }
        }
    } else {
        if (!isTRUE(rm.var) & is.logical(rm.var)) {
            rm.var <- NULL
        }
    }
    
    tot_plot(x, text.var = text_var, facet.vars = rm.var, ...)
    
}

#' Sentence Splitting
#' 
#' \code{sent_detect} - Detect and split sentences on endmark boundaries.
#' 
#' @return \code{sent_detect} - returns a character vector of sentences split on
#' endmark.
#' @rdname sentSplit
#' @export
sent_detect <- function(text.var, endmarks = c("?", ".", "!", "|"), 
    incomplete.sub = TRUE, rm.bracket = TRUE, ...) {

    splitpoint <- paste0("[", paste("\\", endmarks, sep="", collapse=""), "]")
    text.var <- as.character(text.var)
    text.var[is.na(text.var)] <- "DELETEME_QDAP_DELqdapDEL"
    text.var <- paste(text.var, collapse = " ")
    if (incomplete.sub) {
        text.var <- incomplete_replace(text.var)
    }
    if (rm.bracket) {
        text.var <- bracketX(text.var)
    }
    splits <- strsplit(text.var, sprintf("(?<=%s)|_DELqdapDEL", 
        splitpoint), perl=TRUE)
    out <-  Trim(unlist(splits))
    out[out == "DELETEME_QDAP"] <- NA
    out
}

#' Sentence Splitting
#' 
#' \code{sent_detect_nlp} - Detect and split sentences on endmark boundaries 
#' using \pkg{openNLP} & \pkg{NLP} utilities which matches the onld version of
#' the \pkg{openNLP} package's now removed \code{sentDetect} function.
#' 
#' @return \code{sent_detect} - returns a character vector of sentences split on
#' endmark.
#' @rdname sentSplit
#' @export
sent_detect_nlp <- function(text.var, ...){
    
    sent_token_annotator <- openNLP::Maxent_Sent_Token_Annotator(...)
    unlist(lapply(text.var, function(x) {
        if (is.na(x)) return(NA)
        tv <- NLP::as.String(unbag(x))
        out <- NLP::annotate(tv, sent_token_annotator)
        tv[out]
    }))
}


