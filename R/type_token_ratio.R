#' Type-Token Ratio
#' 
#' Calculate type-token ratio by grouping variable.
#' 
#' @param text.var The text variable
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param n.words An integer specifying the number of words in each chunk.
#' @param \ldots ignored.
#' @return Returns a list of class \code{type_text_ratio}.  This object 
#' contains a type-token ratio for the overall text and a data frame
#' type-token ratios per grouping vriable.
#' @references Baker, P. (2006) Using Corpora in Discourse Analysis. London: Continuum.
#' @export
#' @examples
#' with(raj, type_token_ratio(dialogue, person))
#' plot(with(raj, type_token_ratio(dialogue, person)))
type_token_ratio <-  function(text.var, grouping.var = NULL, n.words = 1000, ...) {

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

    DF <- data.frame(grouping, text.var, wc = wc(text.var),
        check.names = FALSE, stringsAsFactors = FALSE)
    DF[["grouping"]] <- factor(DF[["grouping"]])

    ## Split into chuks of 2000 words
    text2000 <- chunker(DF[["text.var"]], DF[["grouping"]], n.words = n.words)
    
    ## word counts per grouping
    key <- qdapTools::matrix2df(
        data.frame(wc = sapply(split(DF[["wc"]], DF[["grouping"]]), sum)), 
        "group.var"
    )

    ## calculate type-token ration per group
    ## mean of 2000 word chunks
    out <- qdapTools::vect2df(sapply(text2000, function(x){
        mean(sapply(unlist(x), ttr))
    }), "group.var", "ttr") 

    out <- data.frame(
        out[, "group.var", drop =FALSE], 
        wc = qdapTools::lookup(out[["group.var"]], key),
        out[, "ttr", drop =FALSE]
    )
    names(out)[1] <- G
    all_ttr <- mean(sapply(unlist(chunker(DF[["text.var"]], n.words = n.words)), ttr))

    o <- list(all = all_ttr, ttr = out)
    class(o) <- "type_token_ratio"
    attributes(o)[["group.name"]] <- G
    text.env <- new.env(FALSE)
    text.env[["text.var"]] <- DF[["text.var"]]
    attributes(o)[["text.var"]] <- text.env 
    group.env <- new.env(FALSE)
    group.env[["grouping.var"]] <- DF[["grouping"]]
    attributes(o)[["grouping.var"]] <- group.env   
    attributes(o)[["n.words"]] <- n.words
    o
}



#' Prints a type_token_ratio Object
#' 
#' Prints a type_token_ratio  object.
#' 
#' @param x The type_token_ratio object.
#' @param digits The number of type-token ratio digits to print.
#' @param \ldots ignored
#' @method print type_token_ratio
#' @export
print.type_token_ratio <-
function(x, digits = 3, ...) {
  
    WD <- options()[["width"]]
    options(width=3000)

    y <- x[["ttr"]]
    y[["ttr"]] <- round(y[["ttr"]], digits)
    print(y)
    cat(sprintf("\nType-token ratio for entire text: %s\n", round(x[["all"]], digits)))
    options(width=WD)
}


#' Plots a type_token_ratio Object
#' 
#' Plots a type_token_ratio object.
#' 
#' @param x The type_token_ratio object.
#' @param \ldots ignored.
#' @importFrom scales alpha
#' @method plot type_token_ratio
#' @export
plot.type_token_ratio <- function(x, ...){

    nms <- paste(sapply(strsplit(attributes(x)[["group.name"]], "&")[[1]], Caps), collapse = " & ")

    ggplot2::ggplot(data = x[["ttr"]], ggplot2::aes_string(x = "ttr", 
        y = attributes(x)[["group.name"]])) +
        ggplot2::geom_vline(xintercept = x[["all"]], size = .7, linetype = "longdash", alpha = .4) +
        ggplot2::geom_point(ggplot2::aes_string(size="wc"), alpha = .3) +
        ggplot2::geom_point(color="red") +
        ggplot2::xlab("Type-Token Ratio") +
        ggplot2::ylab(nms) + 
        ggplot2::theme_bw() +
        ggplot2::annotate("text", x = x[["all"]], y = ceiling(nrow(x[["ttr"]])/2), 
            size =2.3, alpha = .3, label = "Type-Token\nRatio\nAll Text") + 
        ggplot2::scale_size_continuous(name="Word\nCount")

}


ttr <- function(x){
    y <- table(bag_o_words(x))
    length(unlist(y))/sum(unlist(y))    
}
