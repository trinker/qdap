#' Print Context Around Indices
#' 
#' Print (or save to an external file) n text elements before and after indices.
#' 
#' @param text.var The text variable. 
#' @param grouping.var The grouping variables.  Also takes a single 
#' grouping variable or a list of 1 or more grouping variables.
#' @param inds A list of integer indices to print context for.
#' @param n.before The number of rows before the indexed occurrence.
#' @param tot logical.  If \code{TRUE} condenses sub-units (e.g., sentences) 
#' into turns of talk for that \code{grouping.var}.
#' @param n.after The number of rows after the indexed occurrence.
#' @param ord.inds logical.  If \code{TRUE} inds is ordered least to greatest.
#' @return Returns a dataframe of the class "qdap_context" that can be printed 
#' (i.e., saved) in flexible outputs.  The dataframe can be printed as a 
#' dataframe style or pretty text output.  The resulting file contains n rows 
#' before and after each index of a vector of indices.
#' @export
#' @seealso \code{\link[qdap]{boolean_search}},
#' \code{\link[qdap]{question_type}},
#' \code{\link[qdap]{end_mark}}
#' @examples
#' \dontrun{
#' (x <- with(DATA, trans_context(state, person, inds=c(1, 4, 7, 11))))
#' print(x, pretty=FALSE)
#' print(x, double_space = FALSE)
#' print(x, file="foo.xlsx")
#' print(x, file="foo.csv")
#' print(x, file="foo.txt")
#' print(x, file="foo.txt", pretty = FALSE)
#' print(x, file="foo.doc")
#'
#' ## With `end_mark`
#' inds1 <- which(end_mark(DATA.SPLIT[, "state"]) == "?")
#' with(DATA.SPLIT, trans_context(state, person, inds=inds1))
#' with(DATA.SPLIT, trans_context(state, person, n.before = 0, inds=inds1))
#' 
#' ## With `boolean_search`
#' inds2 <- boolean_search(DATA.SPLIT$state, " I &&.")
#' with(DATA.SPLIT, trans_context(state, person, inds=inds2))
#' 
#' inds3 <- boolean_search(DATA$state, " I ||.")
#' with(DATA.SPLIT, trans_context(state, person, inds=inds3))
#' with(DATA.SPLIT, trans_context(state, list(person, sex), inds=inds3))
#' with(DATA.SPLIT, trans_context(state, list(sex, adult), inds=inds3))
#' 
#' inds4 <- boolean_search(raj$dialogue, spaste(paste(negation.words, collapse = " || ")))
#' trans_context(raj$dialogue, raj$person, inds4)
#' 
#' ### With `question_type`
#' (x <- question_type(DATA.SPLIT$state, DATA.SPLIT$person))
#' 
#' ## All questions
#' with(DATA.SPLIT, trans_context(state, person, inds=x$inds))
#' 
#' ## Specific question types
#' y <- x[["raw"]]
#' inds5 <- y[y[, "q.type"] %in% qcv(what, how), "n.row"]
#' with(DATA.SPLIT, trans_context(state, person, inds=inds5))
#' with(DATA.SPLIT, trans_context(state, person, inds=inds5, tot=F))
#' }
trans_context <- function(text.var, grouping.var, inds, n.before = 3, 
    tot = TRUE, n.after = n.before, ord.inds = TRUE) {

    if (ord.inds) {
        inds <- sort(inds)
    }

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

    if (is.list(grouping.var) & length(grouping.var)>1) {
        grouping <- paste2(grouping.var)
    } else {
        grouping <- unlist(grouping.var)
    } 

    DF <- data.frame(grouping, text.var, check.names = FALSE, 
        stringsAsFactors = FALSE)
    DF$grouping <- factor(DF$grouping)

    ## Find original start and end indices    
    inds2 <- orig_inds <- inds2start_end(inds = inds, n.before = n.before, 
        n.after = n.after, nrow.trans = nrow(DF))

    ## If tot find where the indices (inds) will now be
    if (tot) {
        ID <- rle(as.character(DF[, "grouping"]))
        ID2 <- data.frame(id = rep(seq_along(ID[["lengths"]]), ID[["lengths"]]))
        ID2[, "INDS"] <- rep(FALSE, nrow(DF))
        ID2[, "INDS"][inds] <- TRUE
        inds <- ID2[ID2[, "INDS"], "id"]
        DF <- with(DF, sentCombine(text.var, grouping))
    }

    ## Add id var to DF for indexing later
    DF[, "id"] <- seq_len(nrow(DF))
    DF[, "event"] <- FALSE

    ## Find revised start and end indices if necessary
    if (tot) {   
        inds2 <- inds2start_end(inds = inds, n.before = n.before, 
            n.after = n.after, nrow.trans = nrow(DF))
    }

    ## Grab the specific events
    out1 <- lapply(seq_len(nrow(inds2)), function(i) {
        DF2 <- DF[inds2[i, "s"]:inds2[i, "e"], ]
        DF2[DF2[, "id"] == inds[i], "event"] <- TRUE
        DF2[, "context"] <- rep(i, nrow(DF2))
        if (n.before == 0 && n.after == 0) {
            DF2[, "indices"] <- as.character(orig_inds[i, "e"])  
        } else {
            DF2[, "indices"] <- paste0(orig_inds[i, "s"], "-", orig_inds[i, "e"])  
        }  
        DF2[, c("context", "indices", "event", "grouping", "text.var")]
    })

    ## bind it all together
    out2 <- do.call(rbind, out1)

    colnames(out2)[4:5] <- c(G, "text")
    class(out2) <- c("qdap_context", class(out2))
    out2
}

#' Prints a qdap_context object
#' 
#' Prints a qdap_context object
#' 
#' @param x The qdap_context object
#' @param file The name of the file (can print csv, xlsx, txt, doc and other 
#' text based files).  If \code{NULL} file prints to the console.
#' @param pretty logical.  If \code{TRUE} generates a prettier text version of 
#' the output (cannot be used with csv/xlsx file types).  If \code{FALSE} a
#' semi-structured dataframe is generated.
#' @param width A positive integer giving the target column for wrapping lines 
#' in the output.
#' @param sep.block logical.  If \code{TRUE} the blocked events are separated 
#' with text lines.
#' @param double_space logical.  If \code{TRUE} and \code{pretty = TRUE} 
#' double spacing between speech chunks (speakers) is used.
#' @param \ldots ignored
#' @export
#' @method print qdap_context
#' @importFrom tools file_ext
#' @importFrom openxlsx write.xlsx
print.qdap_context <- function(x, file = NULL, pretty = TRUE, width = 70, 
    sep.block = TRUE, double_space = TRUE, ...) {

    FE <- file
    if (!is.null(file)) {
        FE <- file_ext(file)
        if(any(FE %in% c("csv", "xlsx"))) {
            pretty <- FALSE
        }
    }

    if (pretty) {
        out <- pretty_form(x, sep.block = TRUE, width = 70, 
            indent = 4, file = file, double_space = double_space)
    } else {
        out <- df_form(x, sep.block = sep.block)
    }
    if(is.null(FE)) {
        if(pretty) {
            cat(out)
        } else {
            print(out, quote = FALSE)
        }
    } else {
        if(FE == "csv") {
            utils::write.csv(out, file = file, row.names = FALSE)
        } else {
            if(FE == "xlsx") {
                write.xlsx(out, file = file, row.names = FALSE)
            } else {
                if(!pretty) {
                    sink(file = file)
                    print(out, quote = FALSE)
                    sink()
                } else {
                    cat(out, file = file)
                }
            }
        }
    }
} 


inds2start_end <- function(inds, n.before, n.after, nrow.trans) {
    DF <- data.frame(s=inds - n.before, e=inds + n.after)
    DF$s <- ifelse(DF$s < 1, 1, DF$s)
    DF$e <- ifelse(DF$e > nrow.trans, nrow.trans, DF$e)
    DF
}

df_form <- function(x, sep.block = TRUE) {

    G <- colnames(x)[4]
    colnames(x)[4] <- "grouping"
    x[, 1:ncol(x)] <- lapply(x, as.character)

    ## determine separating line between event clusters
    if (sep.block) {
        ngroup <- max(nchar(c(G, x[, "grouping"])))
        sep1 <- paste(rep("=", ngroup), collapse = "")
        ntext <- max(nchar(x[, "text"]))
        sep2 <- paste(rep("=", ifelse(ntext < 30, ntext, 30)), collapse = "")  
    }

    ## Split apart into event clusters and format
    splitx <- split(x, x[, "context"])
    splitx <- splitx[as.character(sort(as.numeric(names(splitx))))]
    out <- lapply(splitx, function(y) {
        y[-1, c("context", "indices")] <- ""
        y[, "event"] <- ifelse(y[, "event"], "**", "")
        y[1, "indices"] <- sprintf("[lines %s]", y[1, "indices"])
        ## add separating line between event clusters
        if (sep.block) {
            y <- rbind(y, c("", "", "", sep1, sep2)  )
        }
        y
    })

    out2 <- do.call(rbind, out)
    rownames(out2) <- seq_len(nrow(out2))
    out2 <- as.matrix(out2)
    colnames(out2)[c(1:4)] <- c("", "", "", G)
    if (sep.block) {
        out2 <- rbind(c("", "", "", sep1, sep2), out2)
    }
    out2
}

pretty_form <- function(x, sep.block = TRUE, width = 70, indent = 4, 
    file = NULL, print = FALSE, double_space = TRUE) {

    if(sep.block) {
       sep <- paste(c("\n", rep("=", 35)), collapse = "")
    } else {
       sep <- NULL
    }
    
    ind <- indent - 5

    ds <- ifelse(double_space, "\n\n", "\n")

    G <- colnames(x)[3]
    colnames(x)[4] <- "grouping"
    x[, 1:ncol(x)] <- lapply(x, as.character)
    ngroup <- max(nchar(x[, "grouping"])) + 1
    x[, "grouping"] <- sprintf(paste0("%-", ngroup, "s"), 
       paste0(x[, "grouping"], ":"))
    spacing1 <- ind + 2
    spacing2 <- indent + ngroup
    wdth <- width - (1 + spacing2)

    if (is.null(file)) {
        file <- ""
    }

    ## Split apart into event clusters and format
    splitx <- split(x, x[, "context"])
    splitx <- splitx[as.character(sort(as.numeric(names(splitx))))]
    out <- lapply(splitx, function(y) {

        y[-1, c("context", "indices")] <- ""
        y[, "event"] <- paste0(paste(rep(" ", spacing1), collapse = ""), 
            ifelse(y[, "event"], "**", "  "))
        if (!is.na(suppressWarnings(as.numeric(y[1, "indices"])))) {
            plural <- ""
        } else {
            plural <- "s"
        }
        y[1, "indices"] <- sprintf("[line%s %s]", plural, y[1, "indices"])
        top <- sprintf("Event %s: %s\n", y[1, "context"], y[1, "indices"])
        y[, "new_group"] <- paste2(y[, c("event", "grouping")], sep = " ", 
            trim = FALSE)

        ## strwidth individual speaker's turn of talk
        out2 <- unlist(lapply(seq_len(nrow(y)), function(i) {
            dat2 <- data.frame(text = strwrap(y[i, c("text")], width = wdth))
             dat2[, "grp"] <- c(y[i, c("new_group")], rep(paste(rep(" ", spacing2), 
                collapse = ""), nrow(dat2) - 1))        
            paste(paste2(dat2[, 2:1], sep = " ", trim = FALSE), collapse = "\n")
        }))
        
        out3 <- paste(out2, collapse = ds)
        out4 <- paste(c(sep, top, out3), collapse = "\n")
        out4
    })

    final <- paste(out, coallspe="\n")
    if (sep.block) {
        final[1] <- sub("\n", "", final[1])
    }
    if (print) {
        cat(final, file = file)
    }
    invisible(final)
}
