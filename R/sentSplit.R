#' Split Sentences Into Individual Lines
#' 
#' Splits turns of talk into individual sentences (provided proper punctuation is
#' used).  This procedure is usually done as part of the data read in and cleaing
#' process.
#' 
#' @param dataframe A dataframe that contains the person and text variable.
#' @param text.var The text variable.
#' @param endmarks A character vector of endmarks to split turns of talk into 
#' sentences.
#' @param incomplete.sub logical.  If TRUE detects incomplete sentences and 
#' replaces with \code{"|"}.
#' @param rm.bracket logical.  If TRUE removes brackets from the text.
#' @param stem.col logical.  If TRUE stems the text.
#' @param text.place A character string giving placement location of the text 
#' column. This must be one of the strings \code{"original", "right"} or 
#' \code{"left"}.
#' @param \ldots Additional options passed to \code{stem2df}.
#' @return Returns a dataframe with turn of talk broken apart into sentences.  
#' Optionally a stemmed version of the text variable may be returned as well.
#' @author Dason Kurkiewicz and Tyler Rinker <tyler.rinker@gmail.com>.
#' @seealso 
#' \code{\link[qdap]{bracketX}}, 
#' \code{\link[qdap]{incomplete.replace}},
#' \code{\link[qdap]{stem2df}} 
#' @keywords sentence split
#' @examples
#' #' sentSplit(DATA, "state")
#' sentSplit(DATA, "state", stem.col = FALSE)
#' sentSplit(DATA, "state", text.place = "left")
#' sentSplit(DATA, "state", text.place = "original")
#' \dontrun{
#' sentSplit(raj, "dialogue")
#' }
sentSplit <-
function(dataframe, text.var, endmarks = c("?", ".", "!", "|"), incomplete.sub = TRUE,  
           rm.bracket = TRUE, stem.col = TRUE, text.place = "right", ...) {
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
