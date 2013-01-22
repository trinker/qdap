#' Intuitively Remark Sentences as Imperative
#' 
#' Automatic imperative remarking.
#' 
#' @param dataframe A data.frame object.
#' @param person.var The person variable.
#' @param text.var The text variable.
#' @param lock.incomplete logical.  If TRUE locks incomplete sentences 
#' (sentences ending with "|") from being marked as imperative.
#' @param additional.names Additional names that may be used in a command 
#' (people in the context that do not speak).
#' @param parallel logical.  If TRUE attempts to run the function on multiple 
#' cores.  Note that this may not mean a speed boost if you have one core or if 
#' the data set is smaller as the cluster takes time to create.  With the 
#' \code{mraja1spl} data set, with an 8 core machine, \code{imperative} had
#' 1/3 the running time.
#' @param warning logical.  If TRUE provides comma warnings (sentences that 
#' contain numerous commas that may be handled incorrectly by the algorithm).
#' @return Returns a dataframe with a text variable indicating imperative 
#' sentences.  Imperative sentences are marked with * followed by the original 
#' end mark.
#' @section Warning: The algorithm used by \code{imperative} is sensitive to 
#' English language dialects and types.  Commas can indicate a choppy sentence 
#' and may indicate a false positive.
#' @export
#' @examples
#' \dontrun{
#' DATA3 <- data.frame(name=c('sue', rep(c('greg', 'tyler', 'phil', 'sue'), 2)),
#'     statement=c('go get it|', 'I hate to read.', 'Stop running!', 'I like it!',
#'     'You are terrible!', "Don't!", 'Greg, go to the red, brick office.',
#'     'Tyler go to the gym.', "Alex don't run."), stringsAsFactors = FALSE)
#' imperative(DATA3, 'name', 'statement', , c('Alex'))
#' imperative(DATA3, 'name', 'statement', lock.incomplete = TRUE, c('Alex'))
#' imperative(DATA3, 'name', 'statement', , c('Alex'), warning=TRUE)
#' imperative(mraja1spl, 'person', 'dialogue', warning=FALSE)
#' X <- imperative(mraja1spl, 'person', 'dialogue', warning=FALSE, parallel = TRUE)
#' truncdf(X[, -7], 60)
#' strwrap(X$dialogue)
#' }
imperative <-
function(dataframe, person.var, text.var, lock.incomplete = FALSE, 
    additional.names = NULL, parallel = FALSE, warning=FALSE){
    TV <- as.character(substitute(text.var))
    original <- names(dataframe)
    text <- as.character(dataframe[, c(text.var)])
    NAMES2 <- c(tolower(as.character(unlist(unique(dataframe[person.var])))))
    NAMES <- NAMES2
    NAMES <- if(is.null(additional.names)){
        NAMES 
    } else {
        c(NAMES, tolower(as.character(additional.names)))
    }
    '%p%' <- function(x, y) paste(x, y, sep="")
    vcheck <- function(x, place = 1) x[place]%in% action.verbs
    pcheck <- function(x, place = 1) x[place]%in% preposition
    acheck <- function(x, place = 1) x[place]%in% adverb
    ncheck <- function(x, place = 1, name.list=NAMES) x[place]%in% name.list
    ccheck <- function(x, place = 1) x[place]%in% ","
    dcheck <- function(x, place = 1) x[place]%in% "don't"
    DF2 <- if(length(dataframe) < 3) {
        DD <- data.frame(a = 1:nrow(dataframe), b = 1:nrow(dataframe)) 
        DD <- data.frame(DD, dataframe)
        DD[, !names(DD) %in% c(text.var)]
    } else {
        dataframe[, !names(dataframe) %in% c(text.var)]
    }
    IMP <- function(x){
        SL <- nchar(x)
        x <- gsub(" +", " ", x)
        y <- gsub("[^[:alnum:][:space:]'\ ,\"]", "", 
                  x, perl = T) #eliminate extra puntuatuion
        y <- gsub(",", " ,", y, perl=TRUE)      
        y <- gsub(" +", " ", y)
        y <- tolower(breaker(y))                   
        noP <- substring(x, 1, SL-1)
        P <- "*"%p%substring(x, SL, SL)
        z <- noP%p%P
        cp <- if(sum(unlist(lapply(y, ccheck))) > 0) which(unlist(lapply(y, 
            ccheck)))[1]+1 else length(y)+1
        cp2 <- if(sum(unlist(lapply(y, ccheck)))>1) which(unlist(lapply(y, 
            ccheck)))[2]+1 else length(y) +1
        ifelse(y[1]%in%c("sh", "shh", "shhh", "shhhh") & length(y)==1, z,
        ifelse(dcheck(y) & length(y)==1, z, 
        ifelse(y[1]=="duck" & length(y)==1, z, 
        ifelse(dcheck(y) & vcheck(y, 2), z, 
        ifelse(y[1]=="let's" & ((vcheck(y, 2) & !y[2]%in%c("see", "think", 
            "ponder"))|y[2]=="not" & vcheck(y, 3)), z,
        ifelse(ncheck(y) & dcheck(y, 2) & vcheck(y, 3), z, 
        ifelse(ncheck(y) & ccheck(y, 2) & dcheck(y, 3) & vcheck(y, 4), z, 
        ifelse(vcheck(y) & !y[1]%in%c("do", "cause", "last", "like", "come", 
            "sort") & !y[1]%in%NAMES , z,   
        ifelse(y[1]%in%"come" & !y[2]%in%"on", z, 
        ifelse(y[1]%in%"come" & y[2]%in%"on" & 
            (!ccheck(y, 3)|(y[3]%in%"now" & !ccheck(y, 4))), z,
        ifelse(acheck(y) & !y[1]%in%c("to") & !y[2]%in%c("last") & 
            vcheck(y, 2), z,                   
        ifelse(acheck(y) & ccheck(y, 2) & vcheck(y, 3), z, 
        ifelse(y[1]=="sort" & !y[2]%in%"of", z, 
        ifelse(ncheck(y) & vcheck(y, 2), z,  
        ifelse(y[1]=="do" & !y[2]%in%c("i", "you", "not", "they"), z,
        ifelse(y[1]=="do" & y[2]=="not" & vcheck(y, 3), z,
        ifelse(ncheck(y) & ccheck(y, 2) & vcheck(y, 3), z,
        ifelse(ncheck(y) & acheck(y, 2) & vcheck(y, 3), z,  
        ifelse(ncheck(y) & ccheck(y, 2) & acheck(y, 3) & vcheck(y, 4), z,
        ifelse(y[1]=="and" & ncheck(y, 2)& ccheck(y, 3) & 
            acheck(y, 4) & vcheck(y, 5), z,
        ifelse(y[1]=="and" & ncheck(y, 2) & ccheck(y, 3) & vcheck(y, 4), z,
        ifelse(y[1]=="and" & ncheck(y, 2) & acheck(y, 3) & vcheck(y, 4), z,
        ifelse(y[1]=="and" & ncheck(y, 2) & vcheck(y, 3), z,
        ifelse(y[1]=="and" & vcheck(y, 2) & ccheck(y, 3) & vcheck(y,4), z,
        ifelse(pcheck(y) & (vcheck(y[cp:length(y)])| acheck(y[cp:length(y)]) &
           vcheck(y[cp:length(y)], 2)), z, 
        ifelse(pcheck(y) & ncheck(y[cp:length(y)]) & 
           (vcheck(y[cp:length(y)], 2)| ncheck(y[cp:length(y)]) &
           acheck(y[cp:length(y)], 2) & vcheck(y[cp:length(y)], 3)), z,  
        ifelse(pcheck(y) & ncheck(y[cp:length(y)]) & 
           ccheck(y[cp:length(y)], 2) & (vcheck(y[cp:length(y)], 
           3)| ncheck(y[cp:length(y)]) & ccheck(y[cp:length(y)], 2) & 
           acheck(y[cp:length(y)], 3) & vcheck(y[cp:length(y)], 4)), z, 
        ifelse(acheck(y) & ccheck(y, 2) & pcheck(y, 3) & 
           vcheck(y[cp2:length(y)]), z,
        ifelse(ncheck(y) & pcheck(y, 2) & 
           (vcheck(y[cp:length(y)])| acheck(y[cp:length(y)]) & 
            vcheck(y[cp:length(y)], 2)), z,  
        ifelse(ncheck(y) & ccheck(y, 2) & pcheck(y, 3) & 
           (vcheck(y[cp2:length(y)])| acheck(y[cp2:length(y)]) &
           vcheck(y[cp2:length(y)], 2)), z, x)
        )))))))))))))))))))))))))))))
    }
    WARN <- function(x){
        SL <- nchar(x)
        y <- gsub("[^[:alnum:][:space:]'\ ,\"]", "", x, perl = T) 
        y <- gsub(",", " ,", y, perl = TRUE) 
        y <- gsub(" +", " ", y)
        y <- tolower(breaker(y))
        a <- ifelse(ncheck(y) & dcheck(y, 2) & vcheck(y, 3), TRUE, FALSE)
        b <- ifelse(ncheck(y) & ccheck(y, 2) & dcheck(y, 3) & 
            vcheck(y, 4) , TRUE, FALSE)
        i <- if(sum(a,b)>0) "ebonics"
        j <- if(sum(y%in%",")>2) {
            "+3 commas"
        } else {
            if(sum(y%in%",")>1){
                "2 commas"
            } else {
                NULL
            }
        }
        k <- if(sum(y%in%"read")>0) "read"
        d <- c(i, j, k)
        d <- ifelse(length(d)==0, "-", paste(d, collapse=" & "))
        d <- ifelse(substring(x, SL, SL)=="?", "-", d)
        return(d)
    } 
    endf2 <- function(tx){
        nc <- nchar(tx)
        y <- substring(tx, nc, nc) == "|"
        if (is.na(y)) {
            y <- FALSE
        }
        return(y)
    }
    if (parallel){
        cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
        clusterExport(cl=cl, varlist=c("text", "lock.incomplete", "endf2", 
            "IMP", "DF2", "breaker", "action.verbs", "preposition",
            "adverb"), envir = environment())
        m <- parLapply(cl, text, function(x) {
                if (lock.incomplete & endf2(x)){
                    x
                } else {
                    IMP(x)
                }
            }
        )
        stopCluster(cl)
        DF2$text <- unlist(m)
    } else {
        DF2$text <- invisible(unlist(lapply(as.character(text), function(x) {
            if (lock.incomplete & endf2(x)){
                x
            } else {
                IMP(x)
            }
        })))
    }    
    names(DF2)[which(names(DF2)%in%"text")] <- TV
    DF2$a <- NULL; DF2$b <- NULL
    DF2 <- DF2[, original]
    if(warning) {
        DF2$warnings <- unlist(lapply(text, WARN))
    }
    return(DF2)
}
