abbreviation_replace <-
function(text.var, abbreviation = NULL, replace = NULL, ignore.case=TRUE) {
    if(is.null(abbreviation)) {
        abbreviation <- abbreviations
    } 
    if (!is.null(replace)) {
        ab <- data.frame(abv=abbreviation, repl=replace)
    } else {
        if (is.list(abbreviation)) {
        ab <-  data.frame(abv=c("Mr.", "Mrs.", "Ms.", "www.", ".com", "i.e.", 
            "A.D.", "B.C.", "A.M.", "P.M.", "et al."),
            rep=c("Mister", "Misses", "Miss", "dot com", "www dot", "ie", 
            "AD", "BC", "AM", "PM", "et al"))
            #ab <- data.frame(abv=abbreviation[[1]], repl=abbreviation[[2]])            
        } else {
            stop("must supply vector of abbreviations and vector of replacements")
        }
    }
    if (ignore.case) {
        ab[, 1] <- tolower(ab[, 1])    
        caps <- function(string, all = FALSE) {      
            capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))
            if (all) {
                x <- paste(unlist(lapply(strsplit(string, " "), capit)), collapse=" ")
                y <- paste(unlist(lapply(strsplit(x, NULL), capit)), collapse="")
                x <- c(x, y)
            } else {
                x <- capit(string)
            }
            return(x)
        }
        ab2 <- do.call(rbind, list(ab, ab))
        temp <- unlist(lapply(ab2[, 1], caps, TRUE))
        ab2[, 1] <- temp[1:(length(temp)/2)]
        ab <- data.frame(rbind(ab, ab2))
        ab[, 2] <- spaste(ab[, 2])
    }
    text.var <- Trim(text.var)
    pn <- which(substring(text.var, nchar(text.var)) == ".")
    text.var <- mgsub(ab[, 1], ab[, 2], text.var)
    x <- Trim(gsub("\\s+", " ", text.var))
    x[pn] <- sapply(x[pn], function(z) {
            if (substring(z, nchar(z)) != ".") {
                paste(z, ".", sep="")
            } else {
                z
            }
        }, USE.NAMES = FALSE)
    return(scrubber(x))
}
