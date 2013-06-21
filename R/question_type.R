#' Count of Question Type
#' 
#' Transcript apply question counts.
#' 
#' @param text.var The text variable
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param neg.cont logical.  IF \code{TRUE} provides separate counts for the 
#' negative contraction forms of the interrogative words.
#' @param percent logical.  If \code{TRUE} output given as percent.  If 
#' \code{FALSE} the output is proportion.
#' @param zero.replace Value to replace 0 values with.
#' @param digits Integer; number of decimal places to round when printing.   
#' @return Returns a list of:
#' \item{raw}{A dataframe of the questions used in the transcript and their 
#' type.}
#' \item{count}{A dataframe of total questions (\code{tot.quest}) and counts of 
#' question types (initial interrogative word) by grouping variable(s).}
#' \item{rnp}{Dataframe of the frequency and proportions of question types by 
#' grouping variable.} 
#' \item{missing}{The row numbers of the missing data (excluded from analysis).}
#' \item{percent}{The value of percent used for plotting purposes.}
#' \item{zero.replace}{The value of zero.replace used for plotting purposes.}
#' @details The algorithm searches for the following interrogative words (and 
#' optionally, their negative contraction form as well): 
#'  
#' 1) whose 2) whom 3) who 4) where 5) what 6) which 7) why 8) when 9) were 
#' 10) was 11) does 12) did 13) do 14) is 15) are 16) will 17) how 18) should 
#' 19) could 20) would 21) shall 22) may 23) might 24) must 25) can 26) has 
#' 27) have 28) had 29) ok 30) right 31) correct 32) implied do/does
#' 
#' The interrogative word that is found first (with the exception of "ok", "right" 
#' and "correct") in the question determines the sentence type. "ok", "right" and 
#' "correct" sentence types are determined if the sentence is a question with no 
#' other interrogative words found and "ok", "right" or "correct" is the last 
#' word of the sentence.  Those interrogative sentences beginning with the word 
#' "you" are categorized as implying do or does question type, though the use of 
#' do/does is not explicit.  Those with undetermined sentence type are labeled 
#' unknown.
#' @keywords question, question-count
#' @export 
#' @examples
#' \dontrun{
#' (x <- question_type(DATA$state, DATA$person))
#' truncdf(x$raw, 15)
#' x$count
#' plot(x)
#' plot(x, label = TRUE)
#' plot(x, label = TRUE, text.color = "red")
#' question_type(DATA$state, DATA$person, percent = FALSE)
#' DATA[8, 4] <- "Won't I distrust you?"
#' question_type(DATA$state, DATA$person)
#' DATA <- qdap::DATA
#' with(DATA, question_type(state, list(sex, adult)))
#' 
#' out1 <- with(mraja1spl, question_type(dialogue, person))
#' ## out1
#' out2 <- with(mraja1spl, question_type(dialogue, list(sex, fam.aff)))
#' ## out2
#' out3 <- with(mraja1spl, question_type(dialogue, list(sex, fam.aff),
#'    percent = FALSE))
#' plot(out3, label = TRUE, lab.digits = 3)
#' }
question_type <- function(text.var, grouping.var = NULL,
    neg.cont = FALSE, percent = TRUE, zero.replace = 0, digits = 2) {
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
    text.var <- replace_contraction(as.character(text.var), 
        qdap::contractions[grepl("you", qdap::contractions[, 1]), ])
    DF <- data.frame(grouping, text.var, check.names = FALSE, 
        stringsAsFactors = FALSE, orig.row.num = seq_len(length(text.var)))
    DF$grouping <- factor(DF$grouping)
    if (is.dp(text.var=DF[, "text.var"])){
        warning(paste0("\n  Some rows contain double punctuation.",
          "  Suggested use of sentSplit function."))
    }
    DF[, "end.mark"] <- substring(DF[, "text.var"], nchar(DF[, "text.var"]))
    DF[, "stext.var"] <- spaste(strip(gsub("'s ", " ", DF[, "text.var"])))
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
    key <- data.frame(x = spaste(x), y = y, stringsAsFactors = FALSE)  
    L1 <- lapply(L1, function(x){
        z <- x[, "stext.var"]
        y <- nchar(z)
        a1 <- (y-4) == sapply(gregexpr("okay", z), "[", 1) 
        a2 <- (y-2) == sapply(gregexpr("ok", z), "[", 1)
        x[, "ok"] <- a1 + a2  
        x[, "alright"] <- (y-7) == sapply(gregexpr("alright", z), "[", 1)
        x[, " right"] <- (y-6) == sapply(gregexpr("right", z), "[", 1)
        x[, "correct"] <- (y-7) == sapply(gregexpr("correct", z), "[", 1)
        x[, "huh"] <- (y-3) == sapply(gregexpr("huh", z), "[", 1)
        x[, "implied_do/does"] <- sapply(gregexpr("you", z), "[", 1) == 2 
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
         L2[[i]][unels & L1[[i]][, "alright"]] <- "alright"
         L2[[i]][unels & L1[[i]][, " right"]] <- "right"
         L2[[i]][unels & L1[[i]][, "correct"]] <- "correct"
         L2[[i]][unels & L1[[i]][, "huh"]] <- "huh"
         L2[[i]]
    })
    DF3a <- data.frame(ords = unlist(lapply(L1, "[", "orig.row.num")), 
        q.type = unlist(L2), stringsAsFactors = FALSE)
    DF3a[unlist(lapply(L1, "[", "implied_do/does")),  2] <- "implied_do/does"
    DF3 <- data.frame(DF, q.type = DF3a[order(DF3a[, "ords"]), 2])
    names(DF3) <- c(G, "raw.text", "n.row", "endmark", "strip.text", "q.type")
    unL2 <- unlist(L2)
    unL2[unlist(lapply(L1, "[", "implied_do/does"))] <- "idd"
    WFM <- t(wfm(unL2, rep(names(L1), sapply(L2, length))))
    cols <- c(key[, "x"], "ok", "alright", "right", "correct", "huh", "idd", "unknown")
    cols2 <- cols[cols %in% colnames(WFM)]
    WFM <- WFM[, cols2, drop = FALSE]
    if (all(grouping %in% "all")) {
        DF <- as.data.frame(matrix(WFM, nrow = 1))
        colnames(DF) <- colnames(WFM)
        rownames(DF) <- "all"          
    } else {
        grvar <- levels(DF[, "grouping"])
        grvarNA <- grvar[!grvar %in% rownames(WFM)]
        mat <- matrix(rep(0, length(grvarNA)*ncol(WFM)), ncol = ncol(WFM))   
        dimnames(mat) <- list(grvarNA, colnames(WFM))
        DF <- data.frame(rbind(WFM, mat))  
    }
    tq <- rowSums(DF)
    comdcol <- list(  
        were = c("weren't", "were"), 
        was = c("wasn't", "was"), 
        does = c("doesn't", "does"), 
        did = c("didn't", "did"), 
        do = c("don't", "do"), 
        is = c("isn't","is"),
        are = c("aren't", "are"),
        will = c("won't", "will"), 
        should = c("shouldn't", "should"), 
        could = c("couldn't", "could"), 
        would = c("wouldn't", "would"), 
        can = c("can't", "can"), 
        has = c("hasn't", "has"), 
        have = c("haven't", "have"), 
        had = c("hadn't", "had")
    ) 
    if(!neg.cont & ncol(DF) > 1) {
        ord <- c("whose", "whom", "who", "where", "what",  "which", 
            "why", "when", "were", "was", "does", "did", "do", 
            "is", "are", "will", "how", "should", "could", "would", 
            "shall", "may", "might", "must", "can", "has", "have", "had")  
        comdcol <- lapply(comdcol, function(x) gsub("'", "", x)) 
        DF <- qcombine(DF, comdcol)
        ord <- c(ord[ord %in% colnames(DF)], "ok", "alright", "right", 
            "correct", "huh", "idd", "unknown")
        DF <- DF[, ord[ord %in% colnames(DF)]] 
    }
    colnames(DF)[colnames(DF) == "idd"] <- "implied_do/does"
    DF <- data.frame(group=rownames(DF), tot.quest = tq, DF, row.names = NULL, 
        check.names = FALSE) 
    if(ncol(DF) == 3) {
        warning(paste0("Text does not contain enough questions to give", 
            "an output of the class \"question_type\":\n", 
            " ...only counts are returned"))
        return(DF)
    }
    DF <- DF[sort(DF[, "group"]), ]
    colnames(DF)[1] <-  G
    yesap <- sapply(comdcol, "[", 1)
    noap <- gsub("'", "", sapply(comdcol, "[", 1))
    colnames(DF) <- mgsub(noap, yesap, colnames(DF))
    DF2 <- as.matrix(DF[, -c(1:2)]/DF[, 2])
    DF2[is.nan(DF2)] <- 0 
    if (percent) {
        DF2 <- DF2*100
    } 
    DF2 <- data.frame(DF[, c(1:2)], as.data.frame(DF2), check.names = FALSE, 
        row.names = NULL) 
    rownames(DF) <- NULL
    rnp <- raw_pro_comb(DF[, -c(1:2)], DF2[, -c(1:2)], digits = digits, 
        percent = percent, zero.replace = zero.replace)  
    rnp <- data.frame(DF2[, 1:2], rnp, check.names = FALSE) 
    o <- list(raw = DF3, count = DF, prop = DF2, rnp = rnp, 
        missing = rows.removed, percent = percent, zero.replace = zero.replace)
    class(o) <- "question_type"
    o
}

#' Prints a question_type object
#' 
#' Prints a question_type object
#' 
#' @param x The question_type object
#' @param \ldots ignored
#' @S3method print question_type
#' @method print question_type
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
#' @param label logical.  If TRUE the cells of the heat map plot will be labeled 
#' with count and proportional values.
#' @param lab.digits Integer values specifying the number of digits to be 
#' printed if \code{label} is TRUE.
#' @param percent logical.  If TRUE output given as percent.  If FALSE the 
#' output is proportion.  If NULL uses the value from 
#' \code{\link[qdap]{question_type}}.  Only used if \code{label} is TRUE.
#' @param zero.replace Value to replace 0 values with.  If NULL uses the value 
#' from \code{\link[qdap]{question_type}}.  Only used if \code{label} is TRUE.
#' @param \ldots Other arguments passed to qheat.
#' @method plot question_type
#' @export
plot.question_type <- function(x, label = FALSE, lab.digits = 1, percent = NULL, 
    zero.replace = NULL, ...) {
    if (label) {
        if (!is.null(percent)) {
            if (percent != x$percent) {
                DF <- as.matrix(x$prop[, -c(1:2)])
                if (percent) {
                    DF <- DF*100    
                } else {
                    DF <-  DF/100
                }
                x$prop <- data.frame(x$prop[, 1:2], DF, check.names = FALSE) 
            }
        } else {
            percent <- x$percent 
        }
        if (is.null(zero.replace)) {
            zero.replace <- x$zero.replace
        }
        rnp <- raw_pro_comb(x$count[, -c(1:2), drop = FALSE], 
            x$prop[, -c(1:2), drop = FALSE], digits = lab.digits, 
            percent = percent, zero.replace = zero.replace)  
        rnp <- data.frame(x$count[, 1:2], rnp, check.names = FALSE) 
        qheat(x$prop, values=TRUE, mat2 = rnp, ...)
    } else {
        qheat(x$prop, ...)  
    }
}
