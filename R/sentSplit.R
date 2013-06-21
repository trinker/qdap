#' Sentence Splitting
#' 
#' \code{sentSplit} - Splits turns of talk into individual sentences (provided 
#' proper punctuation is used).  This procedure is usually done as part of the 
#' data read in and cleaning process.
#' 
#' @param dataframe A dataframe that contains the person and text variable.
#' @param text.var The text variable.
#' @param endmarks A character vector of endmarks to split turns of talk into 
#' sentences.
#' @param incomplete.sub logical.  If \code{TRUE} detects incomplete sentences 
#' and replaces with \code{"|"}.
#' @param rm.bracket logical.  If \code{TRUE} removes brackets from the text.
#' @param stem.col logical.  If \code{TRUE} stems the text as a new column.
#' @param text.place A character string giving placement location of the text 
#' column. This must be one of the strings \code{"original"}, \code{"right"} or 
#' \code{"left"}.
#' @param \ldots Additional options passed to \code{\link[qdap]{stem2df}}.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param tot A tot column from a \code{\link[qdap]{sentSplit}} output.
#' @return \code{sentSplit} - returns a dataframe with turn of talk broken apart 
#' into sentences.  Optionally a stemmed version of the text variable may be 
#' returned as well.
#' @rdname sentSplit
#' @author Dason Kurkiewicz and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @seealso 
#' \code{\link[qdap]{bracketX}}, 
#' \code{\link[qdap]{incomplete.replace}},
#' \code{\link[qdap]{stem2df}} ,
#' \code{\link[qdap]{TOT}} 
#' @keywords sentence, split, turn-of-talk
#' @export
#' @examples
#' \dontrun{
#' #sentSplit EXAMPLE:
#' sentSplit(DATA, "state")
#' sentSplit(DATA, "state", stem.col = TRUE)
#' sentSplit(DATA, "state", text.place = "left")
#' sentSplit(DATA, "state", text.place = "original")
#' sentSplit(raj, "dialogue")[1:20, ]
#' 
#' #sentCombine EXAMPLE:
#' dat <- sentSplit(DATA, "state") 
#' sentCombine(dat$state, dat$person)
#' truncdf(sentCombine(dat$state, dat$sex), 50)
#' 
#' #TOT EXAMPLE:
#' dat <- sentSplit(DATA, "state") 
#' TOT(dat$tot)
#' }
sentSplit <-
function(dataframe, text.var, endmarks = c("?", ".", "!", "|"), 
    incomplete.sub = TRUE, rm.bracket = TRUE, stem.col = FALSE, 
    text.place = "right", ...) {
    splitpoint <- paste0("[", paste("\\", endmarks, sep="", collapse=""), "]")
    if (is.numeric(text.var)) {
      text.var <- colnames(dataframe)[text.var]
    }
    if (incomplete.sub) {
      dataframe [, text.var] <- incomplete.replace(dataframe [, text.var])
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
      }
      )
      )
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
    if(any(na.omit(ans[, input]==""))){
      NAdet <- ans[, input]==""
      NAdet[is.na(NAdet)] <- FALSE
      ans[NAdet, input] <- NA
    }
    if (text.place == "original") {
      ans <- ans[, colnames(dataframe)]
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
    DF <- data.frame(x=names(L1), text.var=unlist(L1))
    names(DF)[1] <- G
    DF
}


#' Convert the tot Column to Turn of Talk
#' 
#' \code{TOT} - Convert the tot column from \code{\link[qdap]{sentSplit}} to turn of talk 
#' index (no sub sentence).  Generally, for internal use.
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
