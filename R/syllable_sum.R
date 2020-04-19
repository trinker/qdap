#' Syllabication
#' 
#' \code{syllable_sum} - Count the number of syllables per row of text.
#' 
#' @param text.var The text variable
#' @param parallel logical.  If \code{TRUE} attempts to run the function on 
#' multiple cores.  Note that this may not mean a speed boost if you have one 
#' core or if the data set is smaller as the cluster takes time to create.
#' @param \ldots Other arguments passed to \code{syllable_count}.
#' @param text A single character vector of text.
#' @param remove.bracketed logical.  If \code{TRUE} brackets are removed from 
#' the analysis.
#' @param algorithm.report logical.  If \code{TRUE} generates a report of words 
#' not found in the dictionary (i.e., syllables were calculated with an 
#' algorithm).
#' @return \code{syllable_sum} - returns a vector of syllable counts per row. 
#' @rdname syllabication
#' @details The worker function of all the syllable functions is 
#' \code{\link[qdap]{syllable_count}}, though it is not intended for direct 
#' use on a transcript.  This function relies on a combined dictionary lookup 
#' (based on the Nettalk Corpus (Sejnowski & Rosenberg, 1987)) and backup 
#' algorithm method.
#' @references Sejnowski, T.J., and Rosenberg, C.R. (1987). "Parallel networks 
#' that learn to pronounce English text" in Complex Systems, 1, 145-168. 
#' @export
#' @importFrom parallel parLapply clusterExport makeCluster detectCores stopCluster
#' @importFrom qdapDictionaries view_data
#' @examples
#' \dontrun{
#' syllable_count("Robots like Dason lie.")
#' syllable_count("Robots like Dason lie.", algorithm.report = TRUE)
#' 
#' syllable_sum(DATA$state)
#' x1 <- syllable_sum(rajSPLIT$dialogue)
#' plot(x1)
#' cumulative(x1)
#' 
#' polysyllable_sum(DATA$state)
#' x2 <- polysyllable_sum(rajSPLIT$dialogue)
#' plot(x2)
#' cumulative(x2)
#' 
#' combo_syllable_sum(DATA$state)
#' x3 <- combo_syllable_sum(rajSPLIT$dialogue)
#' plot(x3) 
#' cumulative(x3)
#' }
syllable_sum <-
function(text.var, parallel = FALSE, ...) {
    if (!parallel) {
        out <- unlist(lapply(as.character(text.var), function(x) {
            sum(syllable_count(Trim(x))[['syllables']], ...)
        }))
    } else {
        cl <- makeCluster(mc <- getOption("cl.cores", detectCores()/2))
        clusterExport(cl=cl, varlist=c("text.var", "strip", "Trim",
            "syllable_count", "scrubber", "bracketX", "env.syl", 
            "reducer", "clean", "mgsub"), 
            envir = environment())
        m <- parLapply(cl, as.character(text.var), function(x) {
                sum(syllable_count(Trim(x))[['syllables']])
            }
        )
        stopCluster(cl)
        out <- unlist(m)
    }
    class(out) <- c("syllable_sum", "syllable_freq", class(out))
    attributes(out)[["wc"]] <- wc(text.var)
    attributes(out)[["type"]] <- "Syllable"   
    out
}

#' Prints an syllable_sum object
#' 
#' Prints an syllable_sum object
#' 
#' @param x The syllable_sum object
#' @param \ldots ignored
#' @export
#' @method print syllable_sum
print.syllable_sum <-
function(x, ...) {

    print(as.numeric(x))
}


#' Count the Number of Syllables Per Word in a Text String
#' 
#' \code{syllable_count} - Count the number of syllables in a single text string.
#' 
#' @param env A lookup environment to lookup the number of syllables in found 
#' words.
#' @return \code{syllable_count} - returns a dataframe of syllable counts and 
#' algorithm/dictionary uses and, optionally, a report of words not found in the dictionary. 
#' @rdname syllabication
#' @export
syllable_count <- 
function(text, remove.bracketed = TRUE, algorithm.report = FALSE, 
    env = qdap::env.syl) {
    if (length(text) > 1) stop("text must be length one")
    if (is.na(text)) {
        NA
    } else {
        q <- scrubber(text)
        if (remove.bracketed) {
            q <- bracketX(q) 
        } 
        q <- strip(q) 
        if (q=="") {
            return(NA)
        }
        y <- unlist(lapply(q, strsplit, "\\s+"))

        SYLL <- function(x) {
            if(exists(x, envir = env)){
                return(get(x, envir = env))   
            } else {  
                x2 <- as.character(substring(x, 1, nchar(x) - 1))
                if(substring(x, nchar(x)) == "s" &  
                    exists(x2, envir = env)){
                    return(get(x2, envir = env))
                } else {
                    m <- gsub("eeing", "XX", x)
                    m <- gsub("eing", "XX", m)
                    ended <- function(z) {
                        if (substring(z, nchar(z) - 1, nchar(z)) == "ed" & 
                            substring(z, nchar(z) - 2, nchar(z) - 2) %in% c("t", 
                            "d")) {
                            z
                        } else {
                            if (substring(z, nchar(z) - 1, nchar(z)) == "ed" & 
                                !substring(z, nchar(z) - 2, nchar(z) - 2) %in% 
                                c("t", "d")) {
                                substring(z, 1, nchar(z) - 2)
                            } else {
                                z
                            }                      
                        }
                    }
                    m <- ended(m)
                    conely <- function(z) {
                        if (substring(z, nchar(z) - 2, nchar(z)) == "ely"){
                            paste0(substring(z, 1, nchar(z) - 3), "ly")
                        } else {
                            z
                        }
                    }
                    m <- conely(m)
                    conle <- function(z) {
                        if (substring(z, nchar(z) - 1, nchar(z)) == "le" & 
                            !substring(z, nchar(z) - 2, nchar(z) - 2) %in% 
                            c("a", "e", "i", "o", "u", "y")) {
                            paste0(substring(z, 1, nchar(z) - 1), "X")
                        } else {
                            if (substring(z, nchar(z) - 1, nchar(z)) == "le" & 
                                substring(z, nchar(z) - 2, nchar(z) - 2) %in% 
                                c("a", "e", "i", "o", "u", "y")) {
                                substring(z, 1, nchar(z) - 1)
                            } else {
                                z
                            }
                        }
                    }
                    m <- conle(m)  
                    conles <- function(z) {
                        if (substring(z, nchar(z) - 2, nchar(z)) == "les" & 
                            !substring(z, nchar(z) - 3, nchar(z) - 3) %in% 
                            c("a", "e", "i", "o", "u", "y")) {
                            paste0(substring(z, 1, nchar(z) - 2), "X")
                        } else {
                            if (substring(z, nchar(z) - 2, nchar(z)) == "les" & 
                                substring(z, nchar(z) - 3, nchar(z) - 3) %in% 
                                c("a", "e", "i", "o", "u", "y")) {
                                substring(z, 1, nchar(z) - 2)
                            } else {
                                z
                            }
                        }
                    }
                    m <- conles(m)
                    magice <- function(z) {
                        if (substring(z, nchar(z), nchar(z)) == "e" & 
                            length(intersect(unlist(strsplit(z, NULL)), 
                            c("a", "e", "i", "o", "u", "y"))) > 1) {
                            substring(z, 1, nchar(z) - 1)
                        } else {
                            z
                        }
                    }
                    m <- magice(m)
                    nchar(gsub("[^X]", "", gsub("[aeiouy]+", "X", m)))
                }
            }
        }  
        n <- unlist(lapply(y, SYLL))
        InDic <- function(x) {
            ifelse(exists(x, envir = env), "-", 
                ifelse(substring(x, nchar(x), nchar(x)) == "s" &&  
                    exists(substring(x, nchar(x), nchar(x)), 
                    envir = env), "-", "NF"))
        }
        k <- sapply(y, InDic)

        DF <- data.frame(words = y, syllables = n, in.dictionary = k, 
            row.names = NULL, stringsAsFactors = FALSE)
        if (algorithm.report == TRUE){
            list("ALGORITHM REPORT" = DF[which(DF$in.dictionary == 'NF'), ], 
                "SYLLABLE DATAFRAME" = DF)
        } else {
            DF
        }
    }
}

## Function to check if ends in s
s_end <- function(x, n = 1){
    substr(x, nchar(x)-n+1, nchar(x)) == "s"
}

## Function to drop last letter
drop_s <- function(x, n){
  substr(x, 1, nchar(x) - 1)
}

#' Transcript Apply Summing of Polysyllables
#' 
#' \code{polysyllable_sum} - Count the number of polysyllables per row of text.
#' 
#' @return \code{polysyllable_sum} - returns a vector of polysyllable counts per row. 
#' @rdname syllabication
#' @export
polysyllable_sum <-
function(text.var, parallel = FALSE) {
    Var1 <- NULL
    counter <- function(x) {
        v <- table(syllable_count(Trim(x))[["syllables"]])
        if (identical(c(v), integer(0))){
            return(0)
        }
        y <- as.data.frame(v, stringsAsFactors = FALSE)
        z <- subset(y, as.numeric(as.character(Var1)) >= 3)
        j <- sum(z$Freq)
        return(j)
    }
    if (!parallel) {
        out <- unlist(lapply(as.character(text.var), function(x) counter(x)))
    } else {
        cl <- makeCluster(mc <- getOption("cl.cores", detectCores()/2))
        clusterExport(cl=cl, varlist=c("text.var", "counter", "strip",
            "Trim", "syllable_count", "scrubber", "bracketX", "env.syl"), 
            envir = environment())
        m <- parLapply(cl, as.character(text.var), function(x) {
                counter(x)
            }
        )
        stopCluster(cl)
        out <- unlist(m)
    }
    class(out) <- c("polysyllable_sum", "syllable_freq", class(out))
    attributes(out)[["wc"]] <- wc(text.var)  
    attributes(out)[["type"]] <- "Pollysyllable"       
    out    
}

#' Prints an polysyllable_sum object
#' 
#' Prints an polysyllable_sum object
#' 
#' @param x The polysyllable_sum object
#' @param \ldots ignored
#' @export
#' @method print polysyllable_sum
print.polysyllable_sum <-
function(x, ...) {
    print(as.numeric(x))
}

#' Transcript Apply Syllable and Polysyllable Count
#' 
#' \code{combo_syllable_sum} - Count the number of both syllables and 
#' polysyllables per row of text.
#' 
#' @return \code{combo_syllable_sum} - returns a dataframe of syllable and polysyllable 
#' counts per row. 
#' @rdname syllabication
#' @export
combo_syllable_sum <-
function(text.var, parallel = FALSE) {
    Trim <- function(x) gsub("^\\s+|\\s+$", "", x)
    counter <- function(x) {
        w <- syllable_count(Trim(x))[["syllables"]]
        if (length(w) ==1 && is.na(w)) return(c(0, 0))     
        y <- as.data.frame(table(w))
        z <- subset(y, as.numeric(as.character(w)) >= 3)
        j <- sum(z$Freq)
        k <- sum(w)
        return(c(k, j))
    }
    if (!parallel) {
        m <- unlist(lapply(as.character(text.var), function(x) counter(x)))
    } else {
        cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
        clusterExport(cl=cl, varlist=c("text.var", "counter", "strip",
            "syllable_count", "scrubber", "bracketX", "env.syl"), 
            envir = environment())
        m <- parLapply(cl, as.character(text.var), function(x) {
                counter(x)
            }
        )
        stopCluster(cl)
        m <- unlist(m)
    }
    n <- as.data.frame(t(matrix(m, 2, length(m)/2)))
    names(n) <- c("syllable.count", "polysyllable.count")
    class(n) <- c("combo_syllable_sum", class(n))
    attributes(n)[["text.var"]] <- text.var      
    attributes(n)[["wc"]] <- wc(text.var)     
    n
}

#' Prints an combo_syllable_sum object
#' 
#' Prints an combo_syllable_sum object
#' 
#' @param x The combo_syllable_sum object
#' @param \ldots ignored
#' @export
#' @method print combo_syllable_sum
print.combo_syllable_sum <-
function(x, ...) {
    class(x) <- "data.frame"
    print(x)
}


#' Plots a syllable_freq Object
#' 
#' Plots a syllable_freq object.
#' 
#' @param x The syllable_freq object.
#' @param \ldots ignored
#' @method plot syllable_freq 
#' @export
plot.syllable_freq <- function(x, ...){

    dat <- data.frame(x=x)
    ggplot2::ggplot(dat, ggplot2::aes_string(x="x")) +  
        ggplot2::geom_histogram()  +
        ggplot2::ylab("Count") + 
        ggplot2::xlab(sprintf("Number of %s Per Sentence", 
            attributes(x)[["type"]])) 

}

#' \code{cumulative.syllable_freq} - Generate syllable_freq over time (duration in 
#' sentences).
#' @rdname cumulative
#' @export
#' @method cumulative syllable_freq
cumulative.syllable_freq <- function(x, ...){

    keeps <- !is.na(attributes(x)[["wc"]])
    y <- x[keeps]
    syl_sent <- y/attributes(x)[["wc"]][keeps]
    out <- data.frame(cumave =cumsum(syl_sent)/1:length(y), Time = 1:length(y))
    class(out) <- c("cumulative_syllable_freq", class(out))
    attributes(out)[["type"]] <- attributes(x)[["type"]]
    attributes(out)[["ave"]] <- mean(syl_sent)    
    out
}

#' Plots a cumulative_syllable_freq Object
#' 
#' Plots a cumulative_syllable_freq object.
#' 
#' @param x The cumulative_syllable_freq object.
#' @param \ldots ignored
#' @method plot cumulative_syllable_freq
#' @export
plot.cumulative_syllable_freq <- function(x, ...) {

    ggplot2::ggplot(x, ggplot2::aes_string(x="Time", y="cumave")) + 
        ggplot2::theme_bw() +
        ggplot2::geom_line(size=1) + ggplot2::geom_smooth() + 
        ggplot2::ylab(sprintf("Cumulative Average %s Per Sentence", 
            attributes(x)[["type"]])) + 
        ggplot2::xlab("Duration") +
        ggplot2::geom_hline(y=attributes(x)[["ave"]], color="grey30", size=1, 
            alpha=.3, linetype=2) + 
        ggplot2::annotate("text", x = nrow(x)/3, y = attributes(x)[["ave"]],  
            label = sprintf("Average %s Per Sentence", attributes(x)[["type"]]), 
            vjust = .3, color="grey30", size=4) +        
        ggplot2::scale_x_continuous(expand=c(0, 0))
}    

#' Prints a cumulative_syllable_freqObject
#' 
#' Prints a cumulative_syllable_freq object.
#' 
#' @param x The cumulative_syllable_freqobject.
#' @param \ldots ignored
#' @method print cumulative_syllable_freq
#' @export
print.cumulative_syllable_freq <- function(x, ...) {
    print(plot.cumulative_syllable_freq(x, ...))
}

#' \code{cumulative.combo_syllable_sum} - Generate combo_syllable_sum over time (duration in 
#' sentences).
#' @rdname cumulative
#' @export
#' @method cumulative combo_syllable_sum
cumulative.combo_syllable_sum <- function(x, ...){

    keeps <- !is.na(attributes(x)[["wc"]])
    syl_sent <- y <- x[keeps, ]
    syl_sent[gsub(".count", "", paste0("ave.", names(y)))] <- lapply(y, function(v, den = 
        attributes(x)[["wc"]][keeps]) {
            v/den
    })
    len <- nrow(syl_sent)
    syl_sent[gsub(".count", "", paste0("cum.ave.", names(y)))] <- lapply(syl_sent[, 3:4], 
        function(v) {
            cumsum(v)/1:len
    })

    syl_sent[["Time"]] <- 1:len
    class(syl_sent) <- c("cumulative_combo_syllable_sum", "data.frame")
    attributes(syl_sent)[["ave.syl"]] <- sum(syl_sent[,
        "syllable.count"])/sum(attributes(x)[["wc"]][keeps])   
    attributes(syl_sent)[["ave.polysyl"]] <- sum(syl_sent[,
        "polysyllable.count"])/sum(attributes(x)[["wc"]][keeps])     
    syl_sent
}

#' Plots a cumulative_combo_syllable_sum Object
#' 
#' Plots a cumulative_combo_syllable_sum object.
#' 
#' @param x The cumulative_combo_syllable_sum object.
#' @param \ldots ignored
#' @method plot cumulative_combo_syllable_sum
#' @export
plot.cumulative_combo_syllable_sum <- function(x, ...) {

    dat <- reshape2::melt(x[, -c(1:4)], id="Time", variable.name="Type")
    dat[, "Type"] <- gsub("cum.ave.", "", dat[, "Type"])
    dat[, "Type"] <- paste0(toupper(substr(dat[, "Type"], 1, 1)), 
        substr(dat[, "Type"], 2, nchar(dat[, "Type"])))
    dat[, "Type"] <- factor(dat[, "Type"], levels=c("Syllable", "Polysyllable"))

    dat2 <- data.frame(Type=factor(c("Syllable", "Polysyllable"), 
        levels=c("Syllable", "Polysyllable")),
        Mean = c(attributes(x)[["ave.syl"]], attributes(x)[["ave.polysyl"]]),
        x = 1)

    dat2[, "labs"] <- sprintf("Average %s Per Sentence", dat2[["Type"]])

    ggplot2::ggplot(dat, ggplot2::aes_string(x="Time", group="Type", 
        color="Type", y="value")) + 
        ggplot2::theme_bw() +
        ggplot2::geom_smooth(color="black") + 
        ggplot2::geom_line(size=1) +
        ggplot2::ylab("Cummulative Average Per Sentence") + 
        ggplot2::xlab("Duration") +
        ggplot2::geom_hline(data=dat2, ggplot2::aes_string(yintercept="Mean", 
            group="Type"), color="grey30", size=1, alpha=.3, linetype=2) + 
        ggplot2::geom_text(data=dat2, ggplot2::aes_string(x = "x", 
            y = "Mean", label = "labs"), hjust = -.05, vjust = .3, 
            color="grey40", size=4) +        
        ggplot2::scale_x_continuous(expand=c(0, 0)) +
        ggplot2::facet_wrap(~Type, scales="free_y", ncol=1) + 
        ggplot2::guides(color=FALSE) 
}    


#' Prints a cumulative_combo_syllable_sum Object
#' 
#' Prints a cumulative_combo_syllable_sum object.
#' 
#' @param x The cumulative_combo_syllable_sum object.
#' @param \ldots ignored
#' @method print cumulative_combo_syllable_sum
#' @export
print.cumulative_combo_syllable_sum <- function(x, ...) {
    print(plot.cumulative_combo_syllable_sum(x, ...))
}

#' Plots a combo_syllable_sum Object
#' 
#' Plots a combo_syllable_sum object.
#' 
#' @param x The combo_syllable_sum object.
#' @param \ldots ignored
#' @method plot combo_syllable_sum 
#' @export
plot.combo_syllable_sum <- function(x, ...){
    x[["syllable.count"]] <- x[[1]] - x[[2]]
    wrdcnt <- attributes(x)[["wc"]]
    x <- x[!is.na(wrdcnt), ]
    wrdcnt <- wrdcnt[!is.na(wrdcnt)]
    x[] <- lapply(x, function(x) x/wrdcnt)
    
    x[["Time"]] <- 1:nrow(x) 
    x[["Time"]] <- factor(qdapTools::pad(x[["Time"]]))
    colnames(x) <- gsub("\\.count", "", colnames(x))
    dat <- reshape2::melt(x, id="Time", variable="Type")
    extra <- max(rowSums(x[, 1:2]))
    
    dat[["Type"]] <- factor(gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", 
        dat[["Type"]], perl=T) , levels=c("Polysyllable", "Syllable"))
    dat[["time"]] <- as.numeric(dat[["Time"]])
    
    ggplot2::ggplot(dat, aes_string(x="Time", fill="Type", weight="value")) +
        ggplot2::geom_bar() +
        ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE)) +
        ggplot2::xlab("Duration") +
        ggplot2::ylab("Syllable/Polysyllable Per Word") +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = "white", color="black")) + 
        ggplot2::scale_y_continuous(expand = c(0, 0), 
            limits = c(0, extra + extra*.05)) +
        ggplot2::scale_fill_manual(values=c("#000000", "#C1C1C1")) +
        ggplot2::geom_smooth(aes_string(y="value", x="time", group="Type"), 
            fill="#7070DB")
}
