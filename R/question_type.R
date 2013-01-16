#' Count of Question Type
#' 
#' Transcript apply question counts.
#' 
#' @param text.var The text variable
#' @param grouping.var The grouping variables.  Default NULL generates one
#' output for all text.  Also takes a single grouping variable or a list of 1 or
#' more grouping variables.
#' @param neg.cont logical.  IF TRUE provides separate counts for the negative 
#' contraction forms of the interrogative words.
#' @param percent logical.  If TRUE output given as percent.  If FALSE the 
#' output is proption.
#' @param zero.replace Value to replace 0 values with.
#' @param prop.by.row logical.  If TRUE applies proportional to the row.  If 
#' FALSE applies by column. 
#' @param \ldots Other arguments passed to \code{\link[qdap]{prop}}.
#' @return Returns a list of:
#' \item{raw}{A dataframe of the questions used in the transcript and their 
#' type.}
#' \item{count}{A dataframe of total questions (\code{tot.quest}) and counts of 
#' question types (initial interrogative word) by grouping variable(s).}
#' \item{missing}{The row numbers of the missing data (excluded from analysis).}
#' @details The algorithm searchs for the following interrogative words (and 
#' optionally, their negative contraction form as well): 
#'  
#' 1) whose 2) whom 3) who 4) where 5) what 6) which 7) why 8) when 9) were 
#' 10) was 11) does 12) did 13) do 14) is 15) are 16) will 17) how 18) should 
#' 19) could 20) would 21) shall 22) may 23) might 24) must 25) can 26) has 
#' 27) have 28) had 29) ok 30) right 31) correct 
#' 
#' The interrogative word that is found first (with the exception of "ok", "right" 
#' and "correct") in the question determines the sentence type. "ok", "right" and 
#' "correct" sentence types are determined if the sentence is a question with no 
#' other interogative words found and "ok", "right" or "correct" is the last 
#' word of the sentence.  Those with undetermined sentence type are labeled 
#' unknown.
#' @keywords question, question-count
#' @export 
#' @examples
#' \dontrun{
#' (x <- question_type(DATA$state, DATA$person))
#' x$raw
#' x$count
#' plot(x)
#' plot(x, label = TRUE)
#' plot(x, label = TRUE, text.color = "red")
#' question_type(DATA$state, DATA$person, proportional = TRUE)
#' DATA[8, 4] <- "Won't I distrust you?"
#' question_type(DATA$state, DATA$person)
#' DATA <- qdap::DATA
#' with(DATA, question_type(state, list(sex, adult)))
#'
#' with(mraja1spl, question_type(dialogue, person))
#' with(mraja1spl, question_type(dialogue, list(sex, fam.aff)))
#' with(mraja1spl, question_type(dialogue, list(sex, fam.aff), 
#'     proportional = TRUE))
#' }
question_type <- function(text.var, grouping.var = NULL,
    neg.cont = FALSE, percent = TRUE, zero.replace = 0,
    prop.by.row = TRUE, ...) {
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
        stringsAsFactors = FALSE, orig.row.num = seq_len(length(text.var)))
    DF$grouping <- factor(DF$grouping)
    if (is.dp(text.var=DF[, "text.var"])){
        warning(paste0("\n  Some rows contain double punctuation.",
          "  Suggested use of sentSplit function."))
    }
    DF[, "end.mark"] <- substring(DF[, "text.var"], nchar(DF[, "text.var"]))
    DF[, "stext.var"] <- spaste(strip(DF[, "text.var"]))
    if (sum(DF$end.mark == "?", na.rm = TRUE) == 0) stop("No questions found") 
    rows.removed <- which(is.na(DF$end.mark))
    DF <- DF[!is.na(DF$end.mark), ]
    DF <- DF[DF$end.mark == "?", ]
    L1 <- split(DF, DF[, "grouping"])
    missing <- names(L1)[sapply(L1, nrow) == 0]
    L1 <- L1[sapply(L1, nrow) != 0]
    x <- c("whose", "whom", "who", "where", "what",  
            "which", "why", "when", "werent", "were", "wasnt", "was", "doesnt", 
            "does", "didnt", "did", "dont", "do", "isnt","is", "arent", "are",
            "will", "wont", "how", "shouldnt", "should", "couldnt", "could", 
            "wouldnt", "would", "shall", "may", "might", "must", "cant",  "can", 
            "hasnt", "has", "havent", "have", "hadnt", "had")
    y <- paste0(" XXXXX", sprintf("%02d", seq_along(x)), " ")
    key <- data.frame(x = spaste(x), y = spaste(y), stringsAsFactors = FALSE)  
    L1 <- lapply(L1, function(x){
        z <- x[, "stext.var"]
        y <- nchar(z)
        a1 <- (y-3) == sapply(gregexpr("okay", z), "[", 1) 
        a2 <- (y-1) == sapply(gregexpr("ok", z), "[", 1)
        x[, "ok"] <- a1 + a2   
        x[, "right"] <- (y-4) == sapply(gregexpr("right", z), "[", 1)
        x[, "correct"] <- (y-6) == sapply(gregexpr("correct", z), "[", 1)
        x
    })
    L2 <- invisible(lapply(L1, function(x) {
        subtext <- mgsub(key[, "x"], key[, "y"], x[, "stext.var"])
        gsub("\\s+", " ", (Trim(gsub("[^XXX[:digit:]]", " ", subtext))))
    }))
    L2 <- invisible(lapply(L2, function(x) {
        sapply(stopwords(x, stopwords = NULL, ignore.case = FALSE), "[", 1) 
    }))
    key <- apply(key, 2, Trim)
    L2 <- lapply(L2, lookup, key.match = key[, 2:1], missing = "unknown")
    L2 <- lapply(seq_along(L2), function(i) {
         unels <- L2[[i]] == "unknown"
         L2[[i]][unels & L1[[i]][, "ok"]] <- "ok"
         L2[[i]][unels & L1[[i]][, "right"]] <- "right"
         L2[[i]][unels & L1[[i]][, "correct"]] <- "correct"
         L2[[i]]
    })
    DF3 <- data.frame(DF, q.type = unlist(L2))
    names(DF3) <- c(G, "raw.text", "endmark", "strip.text", "q.type")
    WFM <- t(wfm(unlist(L2), rep(names(L1), sapply(L2, length))))
    cols <- c(key[, "x"], "ok", "right", "correct", "unknown")
    cols2 <- cols[cols %in% colnames(WFM)]
    WFM <- WFM[, cols2]
    grvar <- levels(DF[, "grouping"])
    grvarNA <- grvar[!grvar %in% rownames(WFM)]
    mat <- matrix(rep(0, length(grvarNA)*ncol(WFM)), ncol = ncol(WFM))     
    dimnames(mat) <- list(grvarNA, colnames(WFM))
    DF <- data.frame(rbind(WFM, mat))  
    tq <- rowSums(DF)
    if(!neg.cont) {
        ord <- c("whose", "whom", "who", "where", "what",  "which", 
            "why", "when", "were", "was", "does", "did", "do", 
            "is", "are", "will", "how", "should", "could", "would", 
            "shall", "may", "might", "must", "can", "has", "have", "had")  
        comdcol <- list(  
            were = c("werent", "were"), 
            was = c("wasnt", "was"), 
            does = c("doesnt", "does"), 
            did = c("didnt", "did"), 
            do = c("dont", "do"), 
            is = c("isnt","is"),
            are = c("arent", "are"),
            will = c("will", "wont"), 
            should = c("shouldnt", "should"), 
            could = c("couldnt", "could"), 
            would = c("wouldnt", "would"), 
            can = c("cant", "can"), 
            has = c("hasnt", "has"), 
            have = c("havent", "have"), 
            had = c("hadnt", "had")
        ) 
        DF <- qcombine(DF, comdcol)
        ord <- c(ord[ord %in% colnames(DF)], "unknown")
        DF <- DF[, ord[ord %in% colnames(DF)]] 
    }
    DF <- data.frame(group=rownames(DF), tot.quest = tq, DF, row.names = NULL) 
    DF <- DF[sort(DF[, "group"]), ]
    colnames(DF)[1] <-  G
    DF2 <- prop(DF[, -1], by.column = (1 - prop.by.row), ...)
    DF2[is.nan(DF2)] <- 0  
    DF2 <- data.frame(DF[, 1, drop = FALSE], DF2, check.names = FALSE)  
    rownames(DF) <- NULL
    rnp <- raw_pro_comb(DF[, -1], DF2[, -1], digitts = digits, 
        percent = percent, zero.replace = zero.replace)  
    rnp <- data.frame(DF2[, 1, drop = FALSE], rnp, check.names = FALSE) 
    o <- list(raw = DF3, count = DF, prop = DF2, rnp = rnp, 
        missing = rows.removed)
    class(o) <- "question_type"
    o
}

#' Prints a question_type object
#' 
#' Prints a question_type object
#' 
#' @param x The question_type object
#' @param \ldots ignored
#' @method print question_type
#' @S3method print question_type
print.question_type <-
function(x, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    print(x$rnp)
    options(width=WD)
}
#' Plots a question_type Object
#' 
#' Plots a question_type object.
#' 
#' @param x The question_type object.
#' @param label logical.  If TRUE the cells of the heat map plot will be labeled with 
#' raw and proportional values.
#' @param \ldots Other arguments passed to qheat.
#' @method plot question_type
#' @S3method plot question_type
plot.question_type <- function(x, label = FALSE, ...) {
    if (label) {
        qheat(x$prop, values=TRUE, mat2 = x$rnp, ...)
    } else {
        qheat(x$prop, ...)  
    }
}