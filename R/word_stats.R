#' Descriptive Word Statistics
#' 
#' Transcript apply descriptive word statistics.
#' 
#' @param text.var The text variable or a  \code{"word_stats"} object (i.e., the 
#' output of a \code{word_stats} function).       
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.  
#' @param tot Optional turns of talk variable that yields turn of talk measures.    
#' @param parallel logical.  If \code{TRUE} attempts to run the function on 
#' multiple cores.  Note that this may not mean a speed boost if you have one 
#' core or if the data set is smaller as the cluster takes time to create 
#' (parallel is slower until approximately 10,000 rows).  To reduce run time 
#' pass a \code{"word_stats"} object to the \code{\link[qdap]{word_stats}} 
#' function.
#' @param rm.incomplete logical.  If \code{TRUE} incomplete statements are 
#' removed from calculations in the output.   
#' @param digit.remove logical.  If \code{TRUE} removes digits from calculating 
#' the output.       
#' @param apostrophe.remove logical.  If \code{TRUE} removes apostrophes from 
#' calculating the output.   
#' @param digits Integer; number of decimal places to round when printing.                    
#' @param \ldots Any other arguments passed to \code{\link[qdap]{end_inc}}.
#' @details Note that a sentence is classified with only one endmark.  An 
#' imperative sentence is classified only as imperative (not as a state, quest, 
#' or exclm as well).  If a sentence is both imperative and incomplete the 
#' sentence will be counted as incomplete rather than imperative.
#' labeled as both imperative 
#' @section Warning: It is assumed the user has run \code{sentSplit} on their 
#' data, otherwise some counts may not be accurate.     
#' @return Returns a list of three descriptive word statistics:
#' \item{ts}{A data frame of descriptive word statistics by row} 
#' \item{gts}{A data frame of word/sentence statistics per grouping variable:
#' \itemize{
#'     \item{n.tot}{ - number of turns of talk}
#'     \item{n.sent}{ - number of sentences}
#'     \item{n.words}{ - number of words}
#'     \item{n.char}{ - number of characters}
#'     \item{n.syl}{ - number of syllables}
#'     \item{n.poly}{ - number of polysyllables}
#'     \item{sptot}{ - syllables per turn of talk}
#'     \item{wptot}{ - words per turn of talk}
#'     \item{wps}{ - words per sentence}
#'     \item{cps}{ - characters per sentence}
#'     \item{sps}{ - syllables per sentence}
#'     \item{psps}{ - poly-syllables per sentence}
#'     \item{cpw}{ - characters per word}
#'     \item{spw}{ - syllables per word}
#'     \item{n.state}{ - number of statements}
#'     \item{n.quest}{ - number of questions}
#'     \item{n.exclm}{ - number of exclamations}
#'     \item{n.incom}{ - number of incomplete statements}
#'     \item{p.state}{ - proportion of statements}
#'     \item{p.quest}{ - proportion of questions}
#'     \item{p.exclm}{ - proportion of exclamations}
#'     \item{p.incom}{ - proportion of incomplete statements}
#'     \item{n.hapax}{ - number of hapax legomenon}
#'     \item{n.dis}{ - number of dis legomenon}
#'     \item{grow.rate}{ - proportion of hapax legomenon to words}
#'     \item{prop.dis}{ - proportion of dis legomenon to words}
#'     }
#' } 
#' \item{mpun}{An account of sentences with an improper/missing end mark} 
#' \item{word.elem}{A data frame with word element columns from gts}
#' \item{sent.elem}{A data frame with sentence element columns from gts} 
#' \item{omit}{Counter of omitted sentences for internal use (only included if 
#' some rows contained missing values)} 
#' \item{percent}{The value of percent used for plotting purposes.}
#' \item{zero.replace}{The value of zero.replace used for plotting purposes.}
#' \item{digits}{integer value od number of digits to display; mostly internal 
#' use}  
#' @keywords descriptive statistic
#' @export
#' @seealso 
#' \code{\link[qdap]{end_inc}}
#' @examples
#' \dontrun{
#' word_stats(mraja1spl$dialogue, mraja1spl$person)
#' (desc_wrds <- with(mraja1spl, word_stats(dialogue, person, tot = tot)))
#' with(mraja1spl, word_stats(desc_wrds, person, tot = tot)) #speed boost
#' names(desc_wrds)
#' htruncdf(desc_wrds$ts, 15, 5)
#' htruncdf(desc_wrds$gts, 15, 6)
#' desc_wrds$mpun 
#' desc_wrds$word.elem
#' desc_wrds$sent.elem 
#' plot(desc_wrds)
#' plot(desc_wrds, label=TRUE, lab.digits = 1)
#' with(mraja1spl, word_stats(dialogue, list(sex, died, fam.aff))) 
#' }
word_stats <-
function(text.var, grouping.var = NULL, tot = NULL, parallel = FALSE, 
    rm.incomplete = FALSE, digit.remove = FALSE, apostrophe.remove = FALSE, 
    digits = 3, ...) {
    totin <- tot
    n.sent <- n.tot <- n.words <- n.char <- n.syl <- n.poly <- NULL
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
    if (class(text.var) != "word_stats"){
        t.o.t. <- if(is.null(tot)){
            paste0(1:length(text.var), ".", 1)
        } else {
            as.character(tot)
        }
        Text <- as.character(text.var)
        is.dp <- function(text.var) {
          punct <- c(".", "?", "!", "|")
          any(sapply(strsplit(text.var, NULL), function(x) {
            sum(x %in% punct) > 1
          }
          ))
        }
        if (any(is.na(text.var))) {
            omit <- which(is.na(text.var))
        } else {
            omit <- NULL
        }
        DF <- na.omit(data.frame(group = grouping, tot.sen = t.o.t., 
            TOT = TOT(t.o.t.), text.var = Text, stringsAsFactors = FALSE)) 
        if (is.dp(text.var=DF[, "text.var"])){
            warning(paste0("\n  Some rows contain double punctuation.",
              "  Suggested use of sentSplit function."))
        }    
        if (rm.incomplete) {
            DF <- end_inc(dataframe = DF, text.var = text.var, ...)
        } 
        DF$group <- DF$group[ , drop = TRUE]
        DF$n.sent <- 1:nrow(DF)
        DF <- DF[with(DF, order(DF$group, DF$n.sent)), ]
        M <- DF_word_stats(text.var = DF$text.var, digit.remove = digit.remove, 
            apos_rm = apostrophe.remove, parallel = parallel)
        M <- M[, !names(M) %in% c("text.var", "n.sent")]
        DF <- data.frame(DF, M)
        DF$end.mark <- substring(DF$text.var, nchar(DF$text.var), 
            nchar(DF$text.var))
        DF$end.mark2 <- substring(DF$text.var, nchar(DF$text.var)-1, 
            nchar(DF$text.var))
        DF$sent.type <- ifelse(DF$end.mark2%in%c("*.", "*?", "*!"), "n.imper",
            ifelse(DF$end.mark==".", "n.state", 
            ifelse(DF$end.mark=="?", "n.quest",
            ifelse(DF$end.mark=="!", "n.exclm",
            ifelse(DF$end.mark=="|", "n.incom", NA))))) 
    } else {
        if(is.null(tot)){
            t.o.t. <- paste0(1:nrow(text.var[["ts"]]), ".", 1)
        } else {
            t.o.t. <- as.character(tot)
        }
        if (!is.null(text.var[["omit"]])){
            grouping <- grouping[-text.var[["omit"]]]
            if(!is.null(tot)){
                t.o.t. <- t.o.t.[-text.var[["omit"]]]
            }
            omit <- text.var[["omit"]]
        } else {
            omit <- NULL 
        }
        DF <- data.frame(group = grouping, tot.sen = t.o.t., 
            TOT = TOT(t.o.t.), text.var$ts[, -1])
        if (is.dp(text.var=DF[, "text.var"])){
            warning(paste0("\n  Some rows contain double punctuation.",
              "  Suggested use of sentSplit function."))
        } 
    }
    mpun <- which(!DF$end.mark %in% c("!", ".", "|", "?", "*"))
    comment(mpun) <- "These observations did not have a ! . | ? * endmark"
    if(any(is.na(DF$sent.type))) {
        warning("Some sentences do have standard qdap punctuation endmarks.",
            "\n  Use $mpun for a list of observations with missing endmarks.")
    }
    DF$end.mark2 <- NULL
    LIST <- split(DF, DF[, "group"])
    totter <- function(x) {length(unique(x))}
    stats <- function(x){
        st <- c(
            n.tot = totter(x[, "TOT"]),
            n.sent = nrow(x),
            n.words = sum(x[, "word.count"], na.rm = TRUE), 
            n.char = sum(x[, "character.count"], na.rm = TRUE),
            n.syl = sum(x[, "syllable.count"], na.rm = TRUE),
            n.poly = sum(x[, "polysyllable.count"], na.rm = TRUE)
        )
        return(st)
    }
    DF2 <- do.call("rbind", lapply(LIST, stats))
    row2col <- function(dataframe, new.col.name = NULL){
        x <- data.frame(NEW = rownames(dataframe), dataframe, 
            check.names=FALSE)
        if(!is.null(new.col.name)) names(x)[1] <- new.col.name
        rownames(x) <- 1:nrow(x)
        return(x)
    }
    DF2 <- row2col(DF2, "group")
    DF2 <- transform(DF2, 
        sptot = round(n.sent/n.tot, digits=digits),
        wptot = round(n.words/n.tot, digits=digits),
        wps = round(n.words/n.sent, digits=digits),
        cps = round(n.char/n.sent, digits=digits),
        sps = round(n.syl/n.sent, digits=digits),
        psps = round(n.poly/n.sent, digits=digits),
        cpw = round(n.char/n.words, digits=digits),
        spw = round(n.syl/n.words, digits=digits),
        pspw = round(n.poly/n.words, digits=digits)
    )
    typer <- function(df){
        types <- c("n.state", "n.quest", "n.exclm", "n.imper", "n.incom")
        sapply(types, function(x) sum(na.omit(df[, "sent.type"]==x)))
    }
    DF2 <- data.frame(DF2, do.call("rbind", lapply(LIST, typer)))   
    DF2 <- DF2[order(-DF2$n.words), ]
    rownames(DF2) <- NULL

    qdaMOD <- suppressWarnings(lapply(LIST, function(x) {
        A <- rm_stopwords(x[, "text.var"], stopwords="", strip=TRUE, unlist=TRUE)
            if (identical(A, character(0))) {
                return (c(DIS=0, HAPAX=0, ALL=0))
            } else {
                if (A=="") {
                    return (c(DIS=0, HAPAX=0, ALL=0))
                } else {
                    B <- data.frame(table(unblanker(A)))
                    HAPAX <- sum(B[,2]==1, na.rm = TRUE)
                    DIS <- sum(B[,2]==2, na.rm = TRUE)
                    ALL <- sum(B[,2], na.rm = TRUE)
                    return(c(DIS=DIS, HAPAX=HAPAX, ALL=ALL))
                }
            }
        }
    ))
    qdaMOD <- data.frame(do.call("rbind", qdaMOD))
    HAPAX <- qdaMOD[, "HAPAX"]
    DIS <- qdaMOD[, "DIS"]
    ALL <- qdaMOD[, "ALL"]
    rankDF <- data.frame(words=ALL, group=rownames(qdaMOD), 
        n.hapax=HAPAX, n.dis=DIS, 
        grow.rate=round(HAPAX/ALL, digits=digits), 
        prop.dis= round(DIS/ALL, digits=digits))
    rankDF <- rankDF[order(-rankDF$words),]
    rownames(rankDF) <- NULL
    DF2 <- data.frame(DF2, rankDF[, -c(1:2)])
    names(DF2)[1] <- G
    DF2 <- DF2[order(-DF2$n.tot, -DF2$n.sent), ]
    rownames(DF2) <- NULL
    DF3 <- DF
    if (class(text.var) != "word_stats") {
        DF3 <- DF3[order(DF3$n.sent), ]
    } else {
        DF3 <- DF3[order(DF3$sent.num), ]
    }
    rownames(DF3) <- NULL
    names(DF3) <- c(G, names(DF3)[c(2:3)], "text.var", 
        "sent.num", names(DF3)[-c(1:5)])
    DF3$tot.sen <- if(is.null(tot)){
        NULL
    } else {
        DF3$tot.sen 
    }
    DF3$TOT <- if (is.null(tot)){
        NULL
    } else {
        DF3$TOT
    }
    if(is.null(totin)){
        DF2$n.tot <- NULL
        DF2$sptot <- NULL
        DF2$wptot <- NULL
    } 
    sum2 <- function(x){
        if(is.numeric(x)){
            sum(x, na.rm = TRUE)
        } else {
            TRUE
        }
    }
    DF2 <- DF2[, unlist(lapply(DF2, sum2))!=0]
    rng <- which(colnames(DF2) %in% c("pspw", "n.hapax"))
    proDF2 <- lapply(DF2[, (rng[1]+1):(rng[2]-1)], function(x) {
        round(x/DF2[, "n.sent"], digits = digits)
    })
    proDF2 <- do.call(cbind, proDF2)
    class(proDF2) <- "matrix"
    colnames(proDF2) <- gsub("n.", "p.", colnames(proDF2), fixed = TRUE)
    word.elem <- DF2[, -c((rng[1]+1):(rng[2]-1))]
    sent.elem <- data.frame(DF2[, (rng[1]+1):(rng[2]-1)], proDF2)
    DF2 <- data.frame(DF2[, 1:(rng[2] - 1)], proDF2, DF2[, rng[2]:ncol(DF2)], 
        check.names = FALSE)
    o <- list(ts = DF3, gts = DF2, mpun = mpun, word.elem = word.elem, 
        sent.elem = sent.elem, omit = omit, digits = digits)
    class(o) <- "word_stats"
    return(o)
}


#' Prints a word_stats object
#' 
#' Prints a word_stats object.
#' 
#' @param x The word_stats object
#' @param digits Integer; number of decimal places to round in the display of 
#' the output. 
#' @param \ldots ignored
#' @method print word_stats
#' @S3method print word_stats
print.word_stats <-
function(x, digits = NULL, ...) {
    if (is.null(digits)) {
        digits <- x$digits
    }
    WD <- options()[["width"]]
    options(width=3000)
    print(left_just(dfnumfor(x$gts, digits = digits), 1))
    options(width=WD)
}


#' Plots a word_stats object
#' 
#' Plots a word_stats object.
#' 
#' @param x The word_stats object
#' @param label logical.  If \code{TRUE} the cells of the heat map plot will be 
#' labeled with count and proportional values.
#' @param lab.digits Integer values specifying the number of digits to be 
#' printed if \code{label} is \code{TRUE}.
#' @param \ldots Other arguments passed to qheat.
#' @method plot word_stats
#' @S3method plot word_stats
plot.word_stats <- function(x, label = FALSE, lab.digits = NULL, ...) {
    v <- x$gts
    if (is.null(lab.digits)) {
        lab.digits <- x$digits
    }
    if (!label) {
        qheat(v,  ...)
    } else {
        mat2 <- dfnumfor(x$gts, digits = lab.digits)
        qheat(v, values = label, mat2 = mat2, ...)    
    }
}

#a helper function used in word_stats (not exported)
DF_word_stats <-
function(text.var, digit.remove = FALSE, apos_rm = FALSE, 
    digits = 3, parallel = FALSE) {
    syllable.count <- character.count <- word.count <- NULL
    polysyllable.count <- NULL
    DF <- na.omit(data.frame(text.var = text.var, 
        stringsAsFactors = FALSE))
    DF$n.sent <- 1:nrow(DF)
    DF[, "word.count"] <- word_count(DF$text.var, missing = 0, 
        digit.remove = digit.remove)
    DF[, "character.count"] <- character_count(DF$text.var, 
        apostrophe.remove = apos_rm, digit.remove = digit.remove)
    DF <- data.frame(DF, combo_syllable_sum(DF$text.var, parallel = parallel))
    DF <- DF[, c("text.var", "n.sent", "word.count", "character.count",
        "syllable.count",  "polysyllable.count") ]
    DF <- transform(DF, char2word.ratio = 
        round(character.count/word.count, digits=digits),
        syl2word.ratio = round(syllable.count/word.count, digits=digits),
        polysyl2word.ratio = round(polysyllable.count/word.count, digits=digits))
    punctuation <- function(x) substr(x, nchar(x), nchar(x))
    DF$end.mark <- unlist(lapply(DF$text.var, punctuation))
    rownames(DF) <- 1:nrow(DF)
    DF <- DF[order(DF$n.sent),]  
    return(DF)
}
