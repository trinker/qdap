#' Descriptive Word Statistics
#' 
#' Transcript apply descriptive word statistics
#' 
#' @aliases word_stats DF_word_stats print.word.stats
#' @param text.var The text variable         
#' @param grouping.var The grouping variables.  Default NULL generates one 
#' output for all text.  Also takes a single grouping variable or a list of 1 
#' or more grouping variables.  
#' @param tot Optional toutn of talk variable.    
#' @param parallel logical.  If TRUE attempts to run the function on multiple 
#' cores.  Note that this may not mean a spead boost if you have one core or if 
#' the data set is smaller as the cluster takes time to create (parallel is 
#' slower until approximately 10,000 rows).       
#' @param rm.incomplete logical.  If TRUE incomplete statments are removed from 
#' calculating the output.   
#' @param digit.remove logical.  If TRUE removes digits from calculating the 
#' output.       
#' @param apostrophe.remove logical.  If TRUE removes apostophes from calculating 
#' the output.   
#' @param digits Integer; number of dicimal places to round.                     
#' @param \ldots Any other arguments passed to endf     
#' @return Returns a list of three descriptive word statistics:
#' \item{ts}{A data frame of descriptive word statistics by row} 
#' \item{gts}{A data frame of word statistics per grouping variable:
#' \itemize{
#'     \item{n.tot}{ - number of turns of talk}
#'     \item{n.sent}{ - number of sentences}
#'     \item{n.words}{ - number of words}
#'     \item{n.char}{ - number of characters}
#'     \item{n.syl}{ - number of syllables}
#'     \item{n.poly}{ - number of polysyllables}
#'     \item{sptot}{ - syllables per turn of talk}
#'     \item{wps}{ - words per sentence}
#'     \item{cps}{ - characters per sentemce}
#'     \item{sps}{ - syllables per sentence}
#'     \item{psps}{ - polly syllables per sentence}
#'     \item{cpw}{ - characters per word}
#'     \item{spw}{ - syllables per word}
#'     \item{n.state}{ - number of statements}
#'     \item{n.quest}{ - number of questions}
#'     \item{n.exclm}{ - vnumber of exclamations}
#'     \item{n.incom}{ - number of incomplete satetments}
#'     \item{n.hapax}{ - number of hapax legomenon}
#'     \item{n.dis}{ - number of dis legomenon}
#'     \item{grow.rate}{ - proportion of hapax legomenon to words}
#'     \item{prop.dis}{ - proportion of dis legomenon to words}
#'     }
#' } 
#' \item{mpun}{An account of sentences with improper end mark} 
#' @keywords descriptive statistic
#' @examples
#' \dontrun{
#' word_stats(mraja1spl$dialogue, mraja1spl$person)
#' (desc_wrds <- with(mraja1spl, word_stats(dialogue, person, tot = tot)))
#' names(desc_wrds)
#' desc_wrds$ts 
#' desc_wrds$gts
#' desc_wrds$pun 
#' with(mraja1spl, word_stats(dialogue, list(sex, died, fam.aff))) 
#' }
word_stats <-
function(text.var, grouping.var = NULL, tot = NULL, parallel = FALSE, 
    rm.incomplete = FALSE, digit.remove = FALSE, apostrophe.remove = FALSE, 
    digits = 3, ...) {
    G <- if(is.null(grouping.var)) {
        "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            paste(m, collapse="&")
        } else {
            G <- as.character(substitute(grouping.var))
            G[length(G)]
        }
    }
    grouping <- if(is.null(grouping.var)){
        rep("all", length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            apply(data.frame(grouping.var), 1, function(x){
                if (any(is.na(x))){
                        NA
                    } else {
                        paste(x, collapse = ".")
                    }
                }
            )
        } else {
            unlist(grouping.var)
        } 
    } 
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
    if (is.dp(text.var=Text)){
      warning(paste0("\n  Some rows contain double punctuation.",
          "  Suggested use of sentSplit function."))
    }
    DF <- na.omit(data.frame(group = grouping, tot.sen = t.o.t., 
        TOT = TOT(t.o.t.), text.var = Text, stringsAsFactors = FALSE))  
    if (rm.incomplete) {
        DF <- endf(dataframe = DF, text.var = text.var, ...)
    } 
    DF$group <- DF$group[ , drop = TRUE]
    DF$n.sent <- 1:nrow(DF)
    DF <- DF[with(DF, order(DF$group, DF$n.sent)), ]
    M <- DF_word_stats(text.var = DF$text.var, digit.remove = digit.remove, 
        apostrophe.remove = apostrophe.remove, parallel = parallel)
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
        st <- c(n.tot = totter(x[, "TOT"]),
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
    DF2 <- transform(DF2, sptot = round(n.sent/n.tot, digits=digits),
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
        A <- stopwords(x[, "text.var"], stopwords="", strip=TRUE, unlist=TRUE)
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
    DF3 <- DF3[order(DF3$n.sent), ]
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
    DF2$n.tot <- if(is.null(tot)){
        NULL
    } else {
        DF2$n.tot
    } 
    DF2$sptot <- if(is.null(tot)){
        NULL
    } else {
        DF2$sptot
    }
    sum2 <- function(x){
        if(is.numeric(x)){
            sum(x, na.rm = TRUE)
        } else {
            TRUE
        }
    }
    DF2 <- DF2[, unlist(lapply(DF2, sum2))!=0]
    o <- list(ts = DF3, gts = DF2, mpun = mpun )
    class(o) <- "word.stats"
    return(o)
}


#' Prints a word.stats object
#' 
#' Prints a word.stats object.
#' 
#' @param x The word.stats object
#' @param \ldots ignored
#' @method print word.stats
#' @S3method print word.stats
print.word.stats <-
function(x, ...) {
    print(x$gts)
}
