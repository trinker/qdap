#' Count of Question Type
#' 
#' Transcript apply question counts.
#' 
#' @param text.var The text variable
#' @param grouping.var The grouping variables.  Default NULL generates one
#' output for all text.  Also takes a single grouping variable or a list of 1 or
#' more grouping variables.
#' @param proportional logical.  If TRUE outputs the table proportionally 
#' (see \code{\link[qdap]{prop}}).
#' @param prop.by.row logical.  If TRUE applies proportional to the row.  If 
#' FALSE applies by column. 
#' @param \ldots Other arguments passed to \code{\link[qdap]{prop}}.
#' @return Returns a table of total questions (\code{tot.quest}) and counts of 
#' question types (initial interagative word).
#' @details The algorith searchers for the following interrogative words: 
#'  
#' 1) whose 2) whom 3) who 4) where 5) what 6) which 7) why 8) when 9) were 
#' 10) was 11) does 12) did 13) do 14) is 15) are 16) will 17) how 18) should 
#' 19) could 20) would 21) shall 22) may 23) might 24) can
#' 
#' The interrogative word that is found first in the question determines the 
#' sentence type. 
#' @keywords question, question-count
#' @export 
#' @examples
#' \dontrun{
#' question_type(DATA$state, DATA$person)
#' question_type(DATA$state, DATA$person, proportional = TRUE)
#' with(DATA, question_type(state, list(sex, adult)))
#' with(mraja1spl, question_type(dialogue, list(sex, fam.aff)))
#' with(mraja1spl, question_type(dialogue, person))
#' }
question_type <- function(text.var, grouping.var = NULL, 
    proportional = FALSE, prop.by.row = TRUE, ...) {
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
    text.var <- as.character(text.var)
    DF <- data.frame(grouping, text.var, check.names = FALSE, 
        stringsAsFactors = FALSE)
    DF$grouping <- factor(DF$grouping)
    is.dp <- function(text.var) {
      punct <- c(".", "?", "!", "|")
      any(sapply(strsplit(text.var, NULL), function(x) {
        sum(x %in% punct) > 1
      }
      ))
    }
    if (is.dp(text.var=DF[, "text.var"])){
      warning(paste0("\n  Some rows contain double punctuation.",
          "  Suggested use of sentSplit function."))
    }
    DF[, "end.mark"] <- substring(DF[, "text.var"], nchar(DF[, "text.var"]))
    DF[, "stext.var"] <- strip(DF[, "text.var"])
    if (sum(DF$end.mark == "?", na.rm = TRUE) == 0) stop("No questions found") 
    DF <- DF[DF$end.mark == "?", ]
    L1 <- split(DF, DF[, "grouping"])
    missing <- names(L1)[sapply(L1, nrow) == 0]
    L1 <- L1[sapply(L1, nrow) != 0]
    key <- data.frame(x = c("whose", "whom", "who", "where", "what", 
            "which", "why", "when", "were", "was", "does", "did", "do", 
            "is", "are", "will", "how", "should", "could", "would", "shall",
            "may", "might", "can"),
        y = paste0("XXXXX", sprintf("%02d", 1:24)), 
        stringsAsFactors = FALSE)  
    L2 <- invisible(lapply(L1, function(x) {
        subtext <- mgsub(key[, "x"], key[, "y"], x[, "stext.var"])
        gsub("\\s+", " ", (Trim(gsub("[^XXX[:digit:]]", " ", subtext))))
    }))
    L2 <- invisible(lapply(L2, function(x) {
        sapply(stopwords(x, stopwords = NULL, ignore.case = FALSE), "[", 1) 
    }))   
    L2 <- lapply(L2, lookup, key.match = key[, 2:1], missing = "unknown")
    WFM <- t(wfm(unlist(L2), rep(names(L2), sapply(L2, length))))
    cols <- c(key[, "x"], "unknown")
    cols2 <- cols[cols %in% colnames(WFM)]
    WFM <- WFM[, cols2]
    grvar <- levels(DF[, "grouping"])
    grvarNA <- grvar[!grvar %in% rownames(WFM)]
    mat <- matrix(rep(0, length(grvarNA)*ncol(WFM)), ncol = ncol(WFM))     
    dimnames(mat) <- list(grvarNA, colnames(WFM))
    DF <- data.frame(rbind(WFM, mat))  
    tq <- rowSums(DF)
    if (proportional) {
        DF <- prop(DF, by.column = (1 - prop.by.row), ...)
        DF[is.nan(DF)] <- 0
    }
    DF <- data.frame(group=rownames(DF), tot.quest = tq, DF, row.names = NULL)
    DF <- DF[sort(DF[, "group"]), ]
    colnames(DF)[1] <- G  
    rownames(DF) <- NULL
    DF
}



