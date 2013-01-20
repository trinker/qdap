#' Syllabication
#' 
#' \code{syllable.sum} - Count the number of syllables per row of text.
#' 
#' @param text.var The text variable
#' @param parallel logical.  If TRUE attempts to run the function on multiple 
#' cores.  Note that this may not mean a spead boost if you have one core or if 
#' the data set is smaller as the cluster takes time to create.
#' @param text A single character vector of text.
#' @param remove.bracketed logical.  If TRUE brackets are removed from the 
#' analysis.
#' @param algorithm.report logical.  If TRUE generates a report of words not 
#' found in the dictionary (i.e. syllables were calculated with an algorithm).
#' @return \code{syllable.sum} - returns a vector of syllable counts per row. 
#' @rdname syllabication
#' @note The worker of all the syllable functions is \code{syllable.count} 
#' though it is not intendeded for direct use on a transcript.  This function 
#' relies on a a combined dictionary lookup (based on the Nettalk Corpus (Sejnowski 
#' & Rosenberg, 1987)) and backup algorithm method.
#' @references Sejnowski, T.J., and Rosenberg, C.R. (1987). "Parallel networks 
#' that learn to pronounce English text" in Complex Systems, 1, 145-168. 
#' @keywords syllable, syllabication, polysyllable
#' @export
#' @import parallel
#' @examples
#' \dontrun{
#' syllable.count("Robots like Dason lie.")
#' syllable.count("Robots like Dason lie.", algorithm.report = TRUE)
#' syllable.sum(DATA$state)
#' polysyllable.sum(DATA$state)
#' combo_syllable.sum(DATA$state)
#' }
syllable.sum <-
function(text.var, parallel = FALSE) {
    if (!parallel) {
        unlist(lapply(as.character(text.var), function(x) {
            sum(syllable.count(Trim(x))['syllables'])
        }))
    } else {
        cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
        clusterExport(cl=cl, varlist=c("text.var", "strip", "Trim",
            "syllable.count", "scrubber", "bracketX", "env.syl"), 
            envir = environment())
        m <- parLapply(cl, as.character(text.var), function(x) {
                sum(syllable.count(Trim(x))['syllables'])
            }
        )
        stopCluster(cl)
        unlist(m)
    }
}


#' Count the Number of Syllables Per Word in a Text String
#' 
#' \code{syllable.count} - Count the number of syllables in a single text string.
#' 
#' @return \code{syllable.count} - returns a dataframe of syllable counts and 
#' algorithm/dictionary uses and, optionally, a report of words not found in the dictionary. 
#' @rdname syllabication
#' @export
syllable.count <- 
function(text, remove.bracketed = TRUE, algorithm.report = FALSE) {
    if (length(text) > 1) stop("text must be length one")
    if (is.na(text)) {
        NA
    } else {
        q <- scrubber(text)
        if (remove.bracketed) {
            q <- bracketX(q) 
        } 
        q <- strip(q) 
        q <- gsub("-", " ", q) 
        q <- gsub("\\?", " ", q) 
        q <- reducer(Trim(q))
        if (q=="") {
            return(NA)
        }
        q <- c(sapply(q, function(x) as.vector(unlist(strsplit(x, " ")))))
        y <- tolower(q)
        SYLL <- function(x) {
            if(exists(x, envir = env.syl)){
                return(get(x, envir = env.syl))   
            } else {  
                x2 <- as.character(substring(x, 1, nchar(x) - 1))
                if(substring(x, nchar(x), nchar(x)) == "s" &  
                    exists(x2, envir = env.syl)){
                    return(get(x2, envir = env.syl))
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
        n <- sapply(y, function(x) SYLL(x))
        InDic <- function(x) {
            ifelse(exists(x, envir = env.syl), "-", 
                ifelse(substring(x, nchar(x), nchar(x)) == "s" &&  
                    exists(substring(x, nchar(x), nchar(x)), 
                    envir = env.syl), "-", "NF"))
        }
        k <- sapply(y, InDic)
        DF <- data.frame(words = q, syllables = n, in.dictionary = k)
        row.names(DF) <- 1:nrow(DF)
        if (algorithm.report == TRUE){
            list("ALGORITHM REPORT" = DF[which(DF$in.dictionary == 'NF'), ], 
                "SYLLABLE DATAFRAME" = DF)
        } else {
            DF
        }
    }
}


#' Transcript Apply Summing of Polysyllables
#' 
#' \code{polysyllable.sum} - Count the number of polysyllables per row of text.
#' 
#' @return \code{polysyllable.sum} - returns a vector of polysyllable counts per row. 
#' @rdname syllabication
#' @export
polysyllable.sum <-
function(text.var, parallel = FALSE) {
    Var1 <- NULL
    counter <- function(x) {
        v <- table(syllable.count(Trim(x))["syllables"])
        if (identical(c(v), integer(0))){
            return(0)
        }
        y <- as.data.frame(v)
        z <- subset(y, as.numeric(as.character(Var1)) >= 3)
        j <- sum(z$Freq)
        return(j)
    }
    if (!parallel) {
        unlist(lapply(as.character(text.var), function(x) counter(x)))
    } else {
        cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
        clusterExport(cl=cl, varlist=c("text.var", "counter", "strip",
            "Trim", "syllable.count", "scrubber", "bracketX", "env.syl"), 
            envir = environment())
        m <- parLapply(cl, as.character(text.var), function(x) {
                counter(x)
            }
        )
        stopCluster(cl)
        unlist(m)
    }
}


#' Transcript Apply Syllable and Polysyllable Count
#' 
#' \code{combo_syllable.sum} - Count the number of both syllables and 
#' polysyllables per row of text.
#' 
#' @return \code{combo_syllable.sum} - returns a dataframe of syllable and polysyllable 
#' counts per row. 
#' @rdname syllabication
#' @export
combo_syllable.sum <-
function(text.var, parallel = FALSE) {
    Trim <- function(x) gsub("^\\s+|\\s+$", "", x)
    counter <- function(x) {
        w <- syllable.count(Trim(x))["syllables"]
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
            "syllable.count", "scrubber", "bracketX", "env.syl"), 
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
    return(n)
}
