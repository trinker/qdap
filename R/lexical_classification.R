#' Lexical Classification Score
#' 
#' Transcript apply lexical classification score (content to functional word 
#' proportion) by grouping variable(s) and optionally plot 
#' the breakdown of the model.  
#' 
#' @param text.var The text variable.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param order.by.lexical_classification logical.  If \code{TRUE} orders the 
#' results by #' lexical_classification score.
#' @param function.words A vector of function words.  Default is 
#' \code{\link[qdapDictionaries]{function.words}}.
#' @param bracket The bracket type to remove.  Use \code{NULL} to not remove 
#' bracketed substrings.  See \code{bracket} argument in 
#' \code{\link[qdap]{bracketX}} for bracket types.
#' @param \ldots Other arguments passed to \code{\link[qdap]{bracketX}}.
#' @details Content words (i.e., nouns, verbs, adjectives, and adverbs) tend to 
#' be the words speakers stresses in language use.  Whereas, functional words 
#' are the "glue" that holds the content together.  Speakers devote much less 
#' time and stress to these words (i.e., pronouns, articles, conjunctions, 
#' quantifiers, and prepositions).
#' @return A list containing at the following components: 
#' \item{content}{A \code{data.frame} of all content words used and corresponding frequencies} 
#' \item{functional}{A \code{data.frame} of all content words used and corresponding frequencies} 
#' \item{raw}{Sentence level descriptive statistics on content vs. functional word use (ave.content.rate is also nown as lexical density} 
#' \item{lexical_classification}{Summarized (grouping variable level) descriptive statistics for content vs. functional word use} 
#' @references Chung, C. & Pennebaker, J. (2007). The Psychological Functions of Function Words. In K. Fiedler (Ed.) Social Communication (pp. 343-359). New York: Psychology Press.
#'
#' Pulvermuller, F. (1999). Words in the brain's language. Behavioral and Brain Sciences, 22, pp. 253-279. doi:10.1017/S0140525X9900182X
#'
#'  Segalowitz, S. J. & Lane, K. (2004). Perceptual fluency and lexical access for function versus content words. Behavioral and Brain Sciences, 27, 307-308. doi:10.1017/S0140525X04310071 
#'
#' Bell, A., Brenier, J. M., Gregory, M., Girand, C. & Jurafsky, D. (2009).  Predictability Effects on Durations of Content and Function Words in Conversational English.  Journal of Memory and Language, 60(1), 92-111. doi:10.1016/j.jml.2008.06.003
#' @export
#' @rdname lexical_classification
#' @examples
#' \dontrun{
#' lexical_classification("I did not like the dog.")
#' lexical_classification(DATA.SPLIT$state, DATA.SPLIT$person)
#' 
#' (out <- with(pres_debates2012, lexical_classification(dialogue, list(person, time))))
#' plot(out)
#' 
#' scores(out)
#' 
#' out2 <- preprocessed(out)
#' htruncdf(out2)
#' plot(out2)
#' 
#' plot(out[["content"]])
#' dev.new()
#' plot(out[["functional"]])
#' 
#' ## cloud of functional vs. content
#' ## Highlight Content Words
#' set.seed(10)
#' par(mar = c(0,0,0,0))
#' list(
#'         content = out[["content"]],
#'         functional = out[["functional"]]
#'     ) %>%
#'     list_df2df("type") %>%
#'     dplyr::mutate(colors = ifelse(type == "functional", "gray80", "blue")) %>%
#'     with(., wordcloud::wordcloud(
#'         word, 
#'         freq, 
#'         min.freq = 8, 
#'         random.order=FALSE,
#'         ordered.colors = TRUE,
#'         colors = colors
#'     )) 
#' mtext("2012 Presidential Debates:\nFunctional vs. Content Word Use", padj=1.25)
#' legend(
#'     .05, .12, bty = "n",
#'     legend = c("functional", "content"), 
#'     fill = c("gray80", "blue"),  
#'     cex = .7
#' )
#' 
#' ## Highlight Functional Words
#' set.seed(10)
#' par(mar = c(0,0,0,0))
#' list(
#'         content = out[["content"]],
#'         functional = out[["functional"]]
#'     ) %>%
#'     list_df2df("type") %>%
#'     dplyr::mutate(colors = ifelse(type == "functional", "red", "gray80")) %>%
#'     with(., wordcloud::wordcloud(
#'         word, 
#'         freq, 
#'         min.freq = 8, 
#'         random.order=FALSE,
#'         ordered.colors = TRUE,
#'         colors = colors
#'     )) 
#' mtext("2012 Presidential Debates:\nFunctional vs. Content Word Use", padj=1.25)
#' legend(
#'     .05, .12, bty = "n",
#'     legend = c("functional", "content"), 
#'     fill = c("red", "gray80"),  
#'     cex = .7
#' )
#' 
#' #=============#
#' ## ANIMATION ##
#' #=============#
#' ## EXAMPLE 1
#' lex_ani <- lexical_classification(DATA.SPLIT$state, DATA.SPLIT$person)
#' lexa <- Animate(lex_ani, content="white", functional="blue",
#'     current.color = "yellow", current.speaker.color="grey70")
#' 
#' bgb <- vertex_apply(lexa, label.color="grey80", size=20, color="grey40")
#' bgb <- edge_apply(bgb, label.color="yellow")
#' 
#' print(bgb, bg="black", net.legend.color ="white", pause=1)
#' 
#' ## EXAMPLE 2
#' lex_ani2 <- lexical_classification(mraja1spl$dialogue, mraja1spl$person)
#' lexa2 <- Animate(lex_ani2, content="white", functional="blue",
#'     current.color = "yellow", current.speaker.color="grey70")
#' 
#' bgb2 <- vertex_apply(lexa2, label.color="grey80", size=17, color="grey40")
#' bgb2 <- edge_apply(bgb2, label.color="yellow")
#' print(bgb2, bg="black", pause=.75, net.legend.color = "white")
#' 
#' ## EXAMPLE 3 (bar plot)
#' Animate(lex_ani2, type="bar")
#' 
#' ## EXAMPLE 4 (text plot)
#' Animate(lex_ani2, type="text")
#' 
#' #======================#
#' ## Complex Animations ##
#' #======================#
#' ## EXAMPLE 1: Network + Text + Bar
#'  
#' library(animation)
#' library(grid)
#' library(gridBase)
#' library(qdap)
#' library(igraph)
#' library(plotrix)
#' 
#' lex_ani2 <- lexical_classification(mraja1spl$dialogue, mraja1spl$person)
#' 
#' ## Set up the network version
#' lex_net <- Animate(lex_ani2, contextual="white", lexal="blue",
#'     current.color = "yellow", current.speaker.color="grey70")
#' bgb <- vertex_apply(lex_net, label.color="grey80", size=17, color="grey40")
#' bgb <- edge_apply(bgb, label.color="yellow")
#' 
#' 
#' ## Set up the bar version
#' lex_bar <- Animate(lex_ani2, type="bar")
#' 
#' ## Set up the text
#' lex_text <- Animate(lex_ani2, type="text", size = 3, width=125, color="white")
#' 
#' ## Generate a folder
#' loc <- folder(animation_lexical_classification)
#' setwd(loc)
#' 
#' ## Set up the plotting function
#' oopt <- animation::ani.options(interval = 0.1)
#' 
#' 
#' lex_text_bar <- Map(function(x, y){
#' 
#'     uns <- unit(c(-1.6,.5,-.2,.25), "cm")
#' 
#'     x <- x +
#'         theme(plot.margin = uns,
#'             text=element_text(color="white"),
#'             legend.text=element_text(color="white"),
#'             legend.background = element_rect(fill = "black"),
#'             panel.border = element_rect(color = "black"),
#'             panel.background = element_rect(fill = "black"),
#'             plot.background = element_rect(fill = "black",
#'                 color="black"))
#' 
#'     uns2 <- unit(c(-.5,.5,-.45,.25), "cm")
#' 
#'     y <- y +
#'         theme(plot.margin = uns2,
#'             text=element_text(color="white"),
#'             legend.text=element_text(color="white"),
#'             legend.background = element_rect(fill = "black"),
#'             plot.background = element_rect(fill = "black",
#'                 color="black"))
#' 
#'     gA <- ggplotGrob(x)
#'     gB <- ggplotGrob(y)
#'     maxWidth <- grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
#'     gA$widths[2:5] <- as.list(maxWidth)
#'     gB$widths[2:5] <- as.list(maxWidth)
#'     out <- arrangeGrob(gA, gB, ncol=1, heights = grid::unit(c(.3, .7), "native"))
#'     ## grid.draw(out)
#'     invisible(out)
#' 
#' }, lex_text, lex_bar)
#' 
#' 
#' FUN <- function(follow=FALSE, theseq = seq_along(bgb)) {
#' 
#'     Title <- "Animated Content Rate: Romeo and Juliet Act 1"
#'     Legend <- c(.2, -1, 1.5, -.95)
#'     Legend.cex <- 1
#' 
#'     lapply(theseq, function(i) {
#'         if (follow) {
#'             png(file=sprintf("%s/images/Rplot%s.png", loc, i),
#'                 width=750, height=875)
#'         }
#'         ## Set up the layout
#'         layout(matrix(c(rep(1, 7), rep(2, 6)), 13, 1, byrow = TRUE))
#' 
#'         ## Plot 1
#'         par(mar=c(2, 0, 2, 0), bg="black")
#'         #par(mar=c(2, 0, 2, 0))
#'         set.seed(22)
#'         plot.igraph(bgb[[i]], edge.curved=TRUE)
#'         mtext(Title, side=3, col="white")
#'         color.legend(Legend[1], Legend[2], Legend[3], Legend[4],
#'               c("Functional", "Content"), attributes(bgb)[["legend"]],
#'               cex = Legend.cex, col="white")
#' 
#'         ## Plot2
#'         plot.new()
#'         vps <- baseViewports()
#' 
#'         print(lex_text_bar[[i]], vp = vpStack(vps$figure,vps$plot))
#' 
#'         animation::ani.pause()
#' 
#'         if (follow) {
#'             dev.off()
#'         }
#'     })
#' 
#' }
#' 
#' FUN()
#' 
#' ## Detect OS
#' type <- if(.Platform$OS.type == "windows") shell else system
#' 
#' 
#' saveHTML(FUN(), autoplay = FALSE, loop = TRUE, verbose = FALSE,
#'     ani.height = 1000, ani.width=750,
#'     outdir = loc, single.opts =
#'     "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0")
#' 
#' FUN(TRUE)
#' 
#' ## EXAMPLE 2: Line + Text + Bar
#' ## Generate a folder
#' loc2 <- folder(animation_lexical_classification2)
#' setwd(loc2)
#' 
#' lex_ani2 <- lexical_classification(mraja1spl$dialogue, mraja1spl$person)
#' 
#' ## Set up the bar version
#' lex_bar <- Animate(lex_ani2, type="bar")
#' cumline <- cumulative(lex_bar)
#' lex_line <- plot(cumline)
#' ylims <- range(cumline[[1]][-c(1:100)]) + c(-.1, .1)
#' 
#' ## Set up the text
#' lex_text <- Animate(lex_ani2, type="text", size = 4, width = 80)
#' 
#' 
#' lex_line_text_bar <- Map(function(x, y, z){
#' 
#'     mar <- theme(plot.margin = unit(c(0, .5, 0, .25), "cm"))
#' 
#'     gA <- ggplotGrob(x + mar + 
#'         theme(panel.background = element_rect(fill = NA, colour = NA), 
#'             panel.border = element_rect(fill = NA, colour = NA),
#'             plot.background = element_rect(fill = NA, colour = NA)))
#'     gB <- ggplotGrob(y + mar)
#'     gC <- ggplotGrob(z + mar + ylab("Average Content Rate") + 
#'         coord_cartesian(ylim = ylims) +
#'         ggtitle("Average Content Rate: Romeo & Juliet Act 1"))
#' 
#'     maxWidth <- grid::unit.pmax(gA$widths[2:5], gB$widths[2:5], gC$widths[2:5])
#'     gA$widths[2:5] <- as.list(maxWidth)
#'     gB$widths[2:5] <- as.list(maxWidth)
#'     gC$widths[2:5] <- as.list(maxWidth)
#'     out <- arrangeGrob(gC, gA, gB, ncol=1, heights = grid::unit(c(.38, .25, .37), "native"))
#'     ## grid.draw(out)
#'     invisible(out)
#' 
#' }, lex_text, lex_bar, lex_line)
#' 
#' 
#' FUN2 <- function(follow=FALSE, theseq = seq_along(lex_line_text_bar)) {
#' 
#' 
#'     lapply(theseq, function(i) {
#'         if (follow) {
#'             png(file=sprintf("%s/images/Rplot%s.png", loc2, i),
#'                 width=750, height=875)
#'         }
#'  
#'         print(lex_line_text_bar[[i]])
#'         animation::ani.pause()
#' 
#'         if (follow) {
#'             dev.off()
#'         }
#'     })
#' 
#' }
#' 
#' FUN2()
#' 
#' ## Detect OS
#' type <- if(.Platform$OS.type == "windows") shell else system
#' 
#' library(animation)
#' saveHTML(FUN2(), autoplay = FALSE, loop = TRUE, verbose = FALSE,
#'     ani.height = 1000, ani.width=750,
#'     outdir = loc2, single.opts =
#'     "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0")
#' 
#' FUN2(TRUE)
#' 
#' #==================#
#' ## Static Network ##
#' #==================#
#' (lexdat <- with(sentSplit(DATA, 4), lexical_classification(state, person)))
#' m <- Network(lexdat)
#' m
#' print(m, bg="grey97", vertex.color="grey75")
#' 
#' print(m, title="Lexical Content Discourse Map", title.color="white", 
#'     bg="black", legend.text.color="white", vertex.label.color = "grey70",
#'     edge.label.color="yellow")
#' 
#' ## or use themes:
#' dev.off()
#' m + qtheme()
#' m + theme_nightheat
#' dev.off()
#' m + theme_nightheat(title="Lexical Content Discourse Map",
#'     vertex.label.color = "grey50")
#'     
#' #==================================#
#' ## Content Rate Over Time Example ##
#' #==================================#
#' lexpres <- lapply(with( pres_debates2012, split(dialogue, time)), function(x) {
#'     lexical_classification(x)
#' })
#' lexplots <- lapply(seq_along(lexpres), function(i) {
#'     dat <- cumulative(lexpres[[i]])
#'     m <- plot(dat)
#'     if (i != 2) m <- m + ylab("")  
#'     if (i == 2) m <- m + ylab("Average Content Rate") 
#'     if (i != 3) m <- m + xlab(NULL)
#'     if (i != 1) m <- m + theme(plot.margin=unit(c(0, 1, 0, .5) + .1, "lines"))
#'     m + ggtitle(paste("Debate", i)) + 
#'         coord_cartesian(xlim = c(300, length(dat[[1]])),
#'             ylim = unlist(range(dat[[1]][-c(1:300)]) + c(-.25, .25)))
#' })
#' 
#' library(grid)
#' library(gridExtra)
#' do.call(grid.arrange, lexplots)
#' }
lexical_classification <- function(text.var, grouping.var = NULL,
    order.by.lexical_classification = TRUE,
    function.words = qdapDictionaries::function.words, 
    bracket = "all", ...) {

    lexical <- group <- word.count <- n.content <- n.functional <- freq <- ave.content.rate <- NULL

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

    DF <- data.frame(grouping, text.var = as.character(text.var), check.names = FALSE, 
        stringsAsFactors = FALSE, orig.row.num = seq_len(length(text.var)))

    DF[, "grouping"] <- factor(DF[, "grouping"])
    if (is.dp(text.var=DF[, "text.var"])){
        warning(paste0("\n  Some rows contain double punctuation.",
          "  Suggested use of sentSplit function."))
    }

    ## remove brackets
    if (!is.null(bracket)) {
        DF[["text.var"]] <- bracketX(DF[, "text.var"], bracket = bracket, ...)
    } 

    ## assign and count content vs. functional
    char02NA <- function(x){ 
        x[sapply(x, function(x) identical(character(0), x))] <- NA
        x
    }
    words <- lapply(DF[["text.var"]], bag_o_words)
    is_content <- lapply(words, function(x) !x %in% function.words)
    nan2na <- function(x) {
        sapply(x, function(y){
            if (length(y) > 1) return(y)
            if (is.nan(y)) return(NA)
            y
        })
    }

    DF2 <- data.frame(
        group = DF[, "grouping"], 
        markup = sapply(Map(function(x, y) paste(x, y, sep="/"), 
            words, lapply(is_content, as.numeric)), paste, collapse=" "),
        word.count=wc(DF[["text.var"]]), 
        content.rate = nan2na(100*sapply(is_content, mean, na.rm=TRUE)),
        n.content = sapply(is_content, sum, na.rm=TRUE),
        n.functional = sapply(is_content, function(x) sum(!x, na.rm=TRUE)),
        stringsAsFactors = FALSE
    )

    DF2[["words"]] <- words
    DF2[["lexical"]] <- lapply(is_content, as.numeric)

    DF2[DF2[["markup"]] == "", 2:8] <- NA
    DF2[["content"]] <- char02NA(Map(function(x, y) x[as.logical(y)], 
        DF2[["words"]], DF2[["lexical"]]))
    DF2[["functional"]] <- char02NA(Map(function(x, y) x[as.logical(y)], 
        DF2[["words"]], lapply(DF2[["lexical"]], function(x) !x)))


    ## calculate summary scores
    DF3 <- DF2 %>% 
        dplyr::group_by(group) %>%
        dplyr::summarise(
            word.count = sum(word.count, na.rm = TRUE),
            ave.content.rate = 100*mean(unlist(lexical), na.rm = TRUE),
            SE = SE(100*stats::na.omit(unlist(lexical))),
            n.content = sum(n.content, na.rm = TRUE),
            n.functional = sum(n.functional, na.rm = TRUE),
            content = paste(stats::na.omit(unlist(content)), collapse="_"),
            functional = paste(stats::na.omit(unlist(functional)), collapse="_")
        ) 

    DF3[["content"]] <- char02NA(lapply(DF3[["content"]], function(x) strsplit(x, "_")[[1]]))
    DF3[["functional"]] <- char02NA(lapply(DF3[["functional"]], function(x) strsplit(x, "_")[[1]]))
    if (order.by.lexical_classification) {
        DF3 <- DF3 %>% 
            dplyr::arrange(-ave.content.rate)
        DF3[["group"]] <- factor(DF3[["group"]], levels = DF3[["group"]])
    }
    class(DF3) <- c("lexical_classification_by", "data.frame")
    colnames(DF3)[1] <- colnames(DF2)[1] <- G

    class(DF2) <- c("lexical_classification_sent", "data.frame")    

    content <- NA

    if (!all(is.na(unlist(DF2[["content"]])))) {
        content <- DF2[["content"]] %>%
            unlist() %>%
            stats::na.omit() %>%
            table() %>%
            as.matrix() %>%
            matrix2df() %>%
            stats::setNames(c("word", "freq")) %>% 
            dplyr::arrange(-freq)

        class(content) <- c("lexical", class(content))
        attributes(content)[["type"]] <- "content"
    }

    functional <- NA
    if (!all(is.na(unlist(DF2[["functional"]])))) {
        functional <- DF2[["functional"]] %>%
            unlist() %>%
            stats::na.omit() %>%
            table() %>%
            as.matrix() %>%
            matrix2df() %>%
            stats::setNames(c("word", "freq")) %>% 
            dplyr::arrange(-freq)

        class(functional) <- c("lexical", class(functional) )
        attributes(functional)[["type"]] <- "functional"

    }

    o <- list(content = content, functional = functional, 
        raw = DF2, lexical_classification = DF3)
    class(o) <- "lexical_classification"
    attributes(o)[["lexical_classification"]] <- lexical_classification

    text.env <- new.env(FALSE)
    text.env[["text.var"]] <- DF[["text.var"]]
    attributes(o)[["text.var"]] <- text.env
    group.env <- new.env(FALSE)
    group.env[["grouping.var"]] <- DF[["grouping"]]
    attributes(o)[["grouping.var"]] <- group.env
    o  
}

#' Prints a lexical_classification Object
#' 
#' Prints a lexical_classification_by  object.
#' 
#' @param x The lexical_classification_by object.
#' @param ave.digits The number of average lexical distribution proportion 
#' digits to print.
#' @param se.digits The number of standard error of the lexical distribution 
#' proportion digits to print.
#' @param trunc The width to truncate content/function word lists.
#' @param \ldots ignored
#' @method print lexical_classification_by
#' @export
print.lexical_classification_by <-
function(x, ave.digits = 1, se.digits = 2, trunc = 25, ...) {
  
    WD <- options()[["width"]]
    options(width=3000)

    class(x) <- "data.frame"

    if ("ave.content.rate" %in% colnames(x)) {
        x[["ave.content.rate"]] <- gsub("^\\.0+%$", "0%", 
            paste0(numformat(x[["ave.content.rate"]], 
            digits = ave.digits), "%"))
        x[["SE"]] <- numformat(x[["SE"]], digits = se.digits)
    }

    x[["content"]] <- sapply(x[["content"]], function(w) {
        if (length(w) == 1 && is.na(w)) return(w)
        paste(unlist(w), collapse=", ")
    })
    x[["functional"]] <- sapply(x[["functional"]], function(w) {
        if (length(w) == 1 && is.na(w)) return(w)
        paste(unlist(w), collapse=", ")
    })

    x[c("content", "functional")] <- lapply(c("content", "functional"), function(y) {
        sapply(x[[y]], function(z){
            if (is.na(z)) return("")
            if (nchar(z) < 1000) return(z)
            substring(z, 1, 1000)
        })
    })

    if (all(c("functional", "content") %in% colnames(x))) {
        x <- left_just(x, 7)
        x <- left_just(x, 8)
    }

    maxchar <- max(nchar(unlist(lapply(x[, 1:6], as.character))))
    trunc <- ifelse(maxchar > trunc, maxchar, trunc)
    x <- truncdf(x, trunc)

    x[[7]] <- sapply(as.character(x[[7]]), function(x) { 
        if (grepl("^NA\\s+$|\\s{2,}$", x)) {
            x
        } else {
            gsub(".{3}$", "\\.\\.\\.", x)
        }
    })
    x[[8]] <- sapply(as.character(x[[8]]), function(x) { 
        if (grepl("^NA\\s+$|\\s{2,}$", x)) {
            x
        } else {
            gsub(".{3}$", "\\.\\.\\.", x)
        }
    })

    print(x)
    options(width=WD)
}

#' Prints an lexical_classification Object
#' 
#' Prints an lexical_classification object.
#' 
#' @param x The lexical_classification object.
#' @param \ldots Other arguments passed to 
#' \code{\link[qdap]{print.lexical_classification_by}}.
#' @method print lexical_classification
#' @export
print.lexical_classification <- function(x, ...) {
    print(scores(x), ...)
}



#' Plots a lexical Object
#' 
#' Plots a lexical object.
#' 
#' @param x The lexical object.
#' @param min.freq Words with frequency below \code{min.freq} will not be plotted.
#' @param rot.per Proportion words with 90 degree rotation.
#' @param random.order logical.  If code{TRUE} plot words in random order. If \code{FALSE}, they will be plotted in decreasing frequency.
#' @param title The title of the plot.  Use \code{NULL} to eliminate.
#' @param title.color The color of the title.
#' @param \ldots Other arguments passed to \code{\link[wordcloud]{wordcloud}}.
#' @importFrom scales alpha
#' @method plot lexical
#' @export
plot.lexical <- function(x, min.freq=1, rot.per=0, random.order = FALSE, 
    title = TRUE, title.color = "blue", ...){

    wordcloud::wordcloud(x[["word"]], x[["freq"]], random.order = random.order,
        min.freq = min.freq, rot.per = rot.per, ...)

    if (!is.null(title)) {
        graphics::par(mar = c(0, 0, 2, 0))
        if (isTRUE(title)) title <- Caps(attributes(x)[["type"]])
        graphics::mtext(title, side = 3, col = title.color, padj=-1)
    }
}

#' Lexical Classification
#' 
#' \code{scores.lexical_classification} - View scores from \code{\link[qdap]{lexical_classification}}.
#' 
#' lexical_classification Method for scores
#' @param x The lexical_classification object.
#' @param \ldots ignored
#' @export
#' @method scores lexical_classification
scores.lexical_classification <- function(x, ...) {

    out <- x[["lexical_classification"]]
    attributes(out) <- list(
            class = c("lexical_classification_score", class(out)),
            type = "lexical_classification_scores",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}



#' Plots a lexical_classification Object
#' 
#' Plots a lexical_classification object as a heat map Gantt plot with lexical_classification over 
#' time (measured in words) and lexical_classification scores per sentence.  In the dotplot 
#' plot the black dots are the average lexical_classification per grouping variable.
#' 
#' @param x The lexical_classification object.
#' @param bar.size The size of the bars used in the Gantt plot.
#' @param low The color to be used for lower values.
#' @param mid The color to be used for mid-range values (default is a less 
#' striking color).
#' @param high The color to be used for higher values.
#' @param ave.lexical_classification.shape The shape of the average lexical_classification score used in the 
#' dot plot.
#' @param alpha Transparency level of points (ranges between 0 and 1).
#' @param shape The shape of the points used in the dot plot.
#' @param point.size The size of the points used in the dot plot.
#' @param jitter Amount of vertical jitter to add to the points.
#' @param nrow The number of rows in the dotplot legend (used when the number of 
#' grouping variables makes the legend too wide).  If \code{NULL} no legend if 
#' plotted.
#' @param na.rm logical. Should missing values be removed?
#' @param order.by.lexical_classification logical.  If \code{TRUE} the group lexical_classification plot 
#' will be ordered by average lexical_classification score, otherwise alphabetical order is 
#' assumed.
#' @param plot logical.  If \code{TRUE} the plot will automatically plot.  
#' The user may wish to set to \code{FALSE} for use in knitr, sweave, etc.
#' to add additional plot layers.
#' @param error.bars logical.  If \code{TRUE} error bars are added to the 
#' lexical_classification dot plot using the standard error of the mean lexical_classification score.
#' @param error.bar.height The height of the error bar ends.
#' @param error.bar.size The size/thickness of the error bars.
#' @param error.bar.color The color of the error bars.  If \code{NULL} each 
#' bar will be colored by grouping variable.
#' @param error.bar.alpha The alpha level of the error bars.
#' @param \ldots ignored
#' @return Invisibly returns the \code{ggplot2} objects that form the larger 
#' plot.  
#' @method plot lexical_classification
#' @importFrom gridExtra grid.arrange
#' @importFrom scales alpha
#' @importFrom qdapTools lookup
#' @importFrom ggplot2 ggplot aes geom_segment xlab ylab scale_colour_gradientn theme_bw guides geom_point guide_colorbar scale_color_discrete guide_legend
#' @export
plot.lexical_classification <- function(x, bar.size = 5, low = "blue", mid = "grey99", 
    high = "red", ave.lexical_classification.shape = "+", alpha = 1/4, shape = 19, 
    point.size = 2.5,  jitter = .1, nrow = NULL, na.rm = TRUE, 
    order.by.lexical_classification = TRUE, plot = TRUE, error.bars =TRUE, 
    error.bar.height = .5, error.bar.size = .5, error.bar.color = "black",
    error.bar.alpha = .6, ...){

    start <- end <- ave <- content.rate <- Lexical_classification <- group <- content.rate <- ave.content.rate <- unit <- NULL

    dat2 <- x[["lexical_classification"]] 
    dat <- as.data.frame(stats::setNames(lapply(1:6, function(i) dat2[, i]), 
        colnames(dat2)[1:6]))
    dat2 <- x[["raw"]][, c(1, 4)]
    dat2[["dialogue"]] <- sapply(x[["raw"]][, 7], unbag)
    dat2[dat2[["dialogue"]] == "", "dialogue"] <- NA
    if (na.rm) {
       dat <- stats::na.omit(dat)
       dat2 <- stats::na.omit(dat2)
    }
    G <- names(dat)[1]
  
    colnames(dat2)[1] <- colnames(dat)[1] <-  "group"
    names(dat)[3] <- "content.rate"

    dat2 <- data.frame(dat2, with(dat2, 
        gantt(dialogue, list(group, seq_along(group)))))
    if (is.null(nrow)) {
        leg <- FALSE
        nrow <- 1
    } else {
        leg <- TRUE
    }

    ## reverse the levels so first factor level is on top
    dat2[["group"]] <- factor(dat2[["group"]], 
        levels = rev(dat[["group"]]))

    ## the filled lexical_classification Gantt plot
    nms <- paste(sapply(strsplit(G, "&")[[1]], Caps), collapse = " & ")
    XX <- ggplot(dat2, aes(color = content.rate)) + 
        geom_segment(aes(x=start, xend=end, y=group, yend=group), 
            size=bar.size) +
        xlab("Duration (sentences)") + ylab(nms) +
        scale_colour_gradientn(colours = c(low, mid, high), name="Content\nRate") +
        theme_bw() + 
        theme(
            plot.margin = unit(c(1, 1, 1, 1), "lines"),
            legend.position="bottom"
        ) + 
        guides(colour = guide_colorbar(barwidth = 9, barheight = .75, nbin=1000))

    ## order the ave. poalrity dotplot by ave. lexical_classification or factor level
    if (order.by.lexical_classification) {
        dat[["group"]] <- factor(dat[["group"]], levels = dat[order(dat[["content.rate"]]), 
            "group"])
        dat2[["group"]] <- factor(dat2[["group"]], 
            levels = dat[order(dat[["content.rate"]]), "group"])
    } else {
        ## reverse the levels so first factor level is on top
        dat2[["group"]] <- factor(dat2[["group"]], 
            levels = sort(unique(dat2[["group"]]), decreasing = TRUE))
        dat[["group"]] <- factor(dat[["group"]], 
            levels = sort(unique(dat[["group"]]), decreasing = TRUE))     
    }
    if (na.rm) {
       dat2 <- stats::na.omit(dat2)
       dat <- stats::na.omit(dat)
    }
    
    ## Plot the lexical_classification dotplot with optional error bars
    YY <- ggplot(dat2, aes(y=group, x=content.rate, colour = group)) + 
        geom_point(data = dat, aes(x=content.rate), shape = ave.lexical_classification.shape, 
            size = 6, show_guide=FALSE) +
        geom_point(alpha = alpha, shape = shape, 
            size = point.size, position = position_jitter(height = jitter)) 

    ## Optional Error Bars
    if (error.bars) {
        ## optional error.bar single color; if NULL colored by group
        if (!is.null(error.bar.color)) {
            YY <- YY + geom_errorbarh(data=dat, height = error.bar.height, alpha = error.bar.alpha,
                size = error.bar.size, color = error.bar.color, aes(x=content.rate, 
                    xmax = content.rate + SE, xmin = content.rate- SE))
        } else {
            YY <- YY + geom_errorbarh(data=dat, height = error.bar.height, alpha = error.bar.alpha, 
                size = error.bar.size, aes(x=content.rate, 
                    xmax = content.rate + SE, xmin = content.rate - SE))
        }
    }

    ## Add the black average lexical_classification point
    YY <- YY + geom_point(data = dat, aes(x=content.rate), shape = 19, 
            size = 1.5, colour = "black", show_guide=FALSE) +
        ylab(nms) + xlab("Average Content Rate") +
        scale_color_discrete(name= nms) 

    ## Legend for dotplot
    if (leg) {
        YY <- YY + theme(plot.margin = unit(c(-.25, 1, 1, 1), "lines"), 
            legend.position="bottom")  +
            guides(col = guide_legend(nrow = nrow, byrow = TRUE, 
                override.aes = list(shape = shape, alpha = 1)))
    } else {
        YY <- YY + theme(plot.margin = unit(c(-.25, 1, 1, 1), "lines"), 
            legend.position="none")       
    } 

    ## Logical plotting argument for use in knitr
    if (plot) {
        grid.arrange(XX, YY, nrow = 2)
    }
    invisible(list(p1 = XX, p2 = YY))
}

        
#' Prints a lexical_classification_score Object
#' 
#' Prints a lexical_classification_score object.
#' 
#' @param x The lexical_classification_score object.
#' @param digits The number of digits displayed if \code{values} is \code{TRUE}.
#' @param \ldots ignored
#' @method print lexical_classification_score
#' @export
print.lexical_classification_score <-
    function(x, digits = 3, ...) {

    class(x) <- c("lexical_classification_by", "data.frame")
    print(x, ...)

}


#' Lexical Classification
#' 
#' \code{preprocessed.lexical_classification} - View preprocessed from \code{\link[qdap]{lexical_classification}}.
#' 
#' lexical_classification Method for preprocessed.
#' @param x The lexical_classification object.
#' @param \ldots ignored
#' @export
#' @method preprocessed lexical_classification
preprocessed.lexical_classification <- function(x, ...) {
    out <- x[["raw"]]
    attributes(out) <- list(
            class = c("lexical_classification_preprocessed", class(out)),
            type = "lexical_classification_preprocessed",
            names = colnames(out),
            row.names = rownames(out),
            text.var = attributes(x)[["text.var"]]
    )
    out
}


#' Prints a lexical_classification_preprocessed Object
#' 
#' Prints a lexical_classification_preprocessed object.
#' 
#' @param x The lexical_classification_preprocessed object.
#' @param \ldots ignored
#' @method print lexical_classification_preprocessed
#' @export
print.lexical_classification_preprocessed <-
    function(x, ...) {

    class(x) <- "data.frame"
        
    WD <- options()[["width"]]
    options(width=3000)
    print(x)
    options(width=WD)
}


#' Plots a lexical_classification_score Object
#' 
#' Plots a lexical_classification_score object.
#' 
#' @param x The lexical_classification_score object.
#' @param error.bar.height The height of the error bar ends.
#' @param error.bar.size The size/thickness of the error bars.
#' @param error.bar.alpha The alpha level of the error bars.
#' @param \ldots ignored
#' @importFrom gridExtra grid.arrange
#' @importFrom scales alpha
#' @method plot lexical_classification_score
#' @export
plot.lexical_classification_score <- function(x, error.bar.height = .35, 
    error.bar.size = .5, error.bar.alpha = .3, ...){ 

    ave.content.rate <- n.content <- n.functional <- NULL
    
    character.count <- sentence.count <- word.count <- grvar <- 
        SE <- ave.polarity <- sd.polarity <- total.sentences <- NULL

    x  <- x[order(x[, "ave.content.rate"]), ]
    x[, 1] <- factor(x[, 1], levels = x[, 1])
    nms <- paste(sapply(strsplit(names(x)[1], "&")[[1]], Caps), collapse = " & ")
    names(x)[1] <- "grvar"

    dat <- as.data.frame(stats::setNames(lapply(1:6, function(i) x[[i]]), colnames(x)[1:6])) 

    dat2 <- dat %>% 
        tidyr::gather(class, counts, c(n.content, n.functional)) %>%
        dplyr::mutate(prop = counts/word.count) %>%
        dplyr::group_by(grvar) %>%
        dplyr::mutate(
            position = Reduce('+', list(prop/2, cumsum(c(0, utils::head(prop, -1))))),
            labs = paste0(numformat(100*round(prop, 4), 2), "%"),
            class = sapply(gsub("n\\.", "", class), Caps)
         )

    ##     plot1 <- ggplot2::ggplot(dat2, ggplot2::aes_string(x = "grvar", 
    ##             fill = "class", weight ="counts")) +
    ##         ggplot2::geom_bar(position = "fill") + 
    ##         ggplot2::scale_y_continuous(expand = c(0,0)) +
    ##         ggplot2::theme(
    ##             legend.position = "bottom", 
    ##             axis.ticks.x=ggplot2::element_blank(), 
    ##             axis.text.x=ggplot2::element_blank(),
    ##             legend.title=ggplot2::element_blank()
    ##         ) +
    ##         ggplot2::xlab(nms) +
    ##         ggplot2::ylab(NULL) + 
    ##         ggplot2::geom_text(aes_string(y="position", label="labs"), size=3, 
    ##             color = "grey30") + 
    ##         ggplot2::coord_flip() 

    plot2 <- ggplot2::ggplot(dat, ggplot2::aes_string(y = "grvar", x = "ave.content.rate")) +
        ggplot2::geom_point(ggplot2::aes_string(size="word.count"), color="grey40", alpha=.3) + 
        ggplot2::geom_errorbarh(ggplot2::aes(xmax = ave.content.rate + SE, 
                xmin = ave.content.rate - SE), 
                height = error.bar.height, size = error.bar.size, 
                alpha = error.bar.alpha) +
        ggplot2::geom_point(size=2) + 
        ggplot2::ylab(nms) +
        ggplot2::xlab("Average Sentence Content Rate") + 
        ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%")) +
        ggplot2::scale_size_continuous(name="Word\nCount") +
        ggplot2::theme(legend.key = element_rect(fill = NA)) +
        ggplot2::guides(size=guide_legend(title.hjust =0.5))

    print(plot2)
    invisible(plot2)
}


#' Plots a lexical_classification_preprocessed Object
#' 
#' Plots a lexical_classification_preprocessed object.
#' 
#' @param x The lexical_classification_preprocessed object.
#' @param jitter The amount to jitter the points by in the bocplots.
#' @param text.size The text size to use for plotting the mean in the boxplots.
#' @param alpha The alpha level to use for points.
#' @param ncol The number of columns to use for \code{\link[ggplot2]{facet_wrap}}.
#' @param \ldots ignored
#' @importFrom ggplot2 ggplot aes geom_point theme theme_minimal ylab xlab scale_size_continuous element_blank guides 
#' @importFrom scales alpha
#' @method plot lexical_classification_preprocessed
#' @export
plot.lexical_classification_preprocessed <- function(x, jitter=.1, 
    text.size=3.5, alpha = .3, ncol = 3, ...){ 

    ave <- content.rate <- NULL
    
    nms <- paste(sapply(strsplit(names(x)[1], "&")[[1]], 
        Caps), collapse = " & ")

    dat <- data.frame(group = x[[1]],
        word.count = x[["word.count"]], 
        content.rate = x[["content.rate"]],
        time = as.numeric(attributes(x)[["row.names"]]), 
        stringsAsFactors = FALSE
    )

    dat2 <- dat %>%
        dplyr::group_by_("group") %>%
        dplyr::summarise(ave=mean(content.rate, na.rm=TRUE)) %>%
        dplyr::arrange_("ave") %>%
        dplyr::mutate(labs=numformat(ave, 2))

    dat[["group"]] <- factor(dat[["group"]], levels=dat2[["group"]])
    dat2[["group"]] <- factor(dat2[["group"]], levels=dat2[["group"]])

    plot1 <- ggplot2::ggplot(dat, ggplot2::aes_string(x="group", y = "content.rate",
            group="group", color="group")) + 
        ggplot2::geom_jitter(alpha=alpha, h = 0, w = jitter) +
        ggplot2::geom_boxplot(fill=NA, outlier.shape=NA, size=.7) +
        ggplot2::geom_text(data=dat2, ggplot2::aes_string(y="ave", x="group", 
            label="labs"), vjust=1.2, color="grey40", size=text.size) +
        ggplot2::geom_point(data=dat2, ggplot2::aes_string(y="ave", x="group"), 
            shape=3, color="black")  + 
        ggplot2::coord_flip() + 
        ggplot2::xlab(nms) + 
        ggplot2::ylab("Content Rate") +
        ggplot2::guides(color=FALSE, alpha=FALSE) +
        theme_minimal()

    dat[["group"]] <- factor(dat[["group"]], levels=rev(dat2[["group"]]))

    plot2 <- ggplot2::ggplot(dat, ggplot2::aes_string(y = "content.rate",
            x="word.count", color="group")) +
        ggplot2::geom_hline(data=dat2, aes_string(yintercept = "ave"), 
            linetype=2, size=.7, alpha=.7) +
        ggplot2::geom_point(alpha=alpha)  + 
        ggplot2::geom_smooth() +
        ggplot2::facet_wrap(~group, ncol=ncol) +
        ggplot2::theme_minimal() + 
        ggplot2::theme(panel.grid = ggplot2::element_blank(),
            panel.spacing = grid::unit(1, "lines")) +
        ggplot2::annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
        ggplot2::annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
        ggplot2::ylab("Content Rate") + 
        ggplot2::xlab("Word Count") +
        ggplot2::guides(color=FALSE, alpha=FALSE) +
        ggplot2::scale_color_manual(values = 
            rev(gg_color_hue(length(levels(dat[["group"]])))))

    gridExtra::grid.arrange(plot1, plot2, ncol=2)
}


## heper to animate a networkplot
Animate_lexical_classification_net <- function(x, functional = "yellow", 
    content = "red", edge.constant, wc.time = TRUE, time.constant = 1, 
    title = NULL, digits = 1, current.color = "black", missing.color="purple", 
    current.speaker.color, non.speaker.color = NA, ...){

    content.rate <- word.count <- id <- NULL
    
    qsep <- "|-|qdap|-|"

    brks <- seq(0, 1, by=.001)
    max.color.breaks <- length(brks)

    y <- preprocessed(x) 

    nms <- names(y)[1]
    names(y)[1] <- "group"
    y <- y %>%
        dplyr::select_("group", "word.count", "content.rate") %>%
        dplyr::mutate(content.rate=content.rate/100)

    condlens <- rle(as.character(y[["group"]]))
    y[["temp"]] <- rep(paste0("X", pad(1:length(condlens[[2]]))),
        condlens[[1]])

    ## Add to  and from columns
    y <- cbind(y, from_to_End(y[["group"]]))

    ## repeat last to column to match with split sentence (i.e.
    ## we don't want an edge to return to the node it leaves
    tos <- split(y[["to"]], y[["temp"]])
    tos_lens <- sapply(tos, length)
    y[["to"]] <- rep(sapply(tos, utils::tail, 1), tos_lens)
  
    ## make a combined from|to column
    y[["fromQDAPQDAPto"]] <- paste2(y[, c("from", "to")], sep=qsep)

    ## add id column
    y[["id"]] <- 1:nrow(y)

    nrows <- 1:nrow(y)
    inds <- unlist(lapply(nrows, function(i) nrows[1:i]))
    bigy <- y[inds, c("word.count", "content.rate", "fromQDAPQDAPto", "id")]
    bigy[["times"]] <- rep(nrows, nrows)

    df_lexical_classification <- bigy %>%
        dplyr::group_by_("times", "fromQDAPQDAPto") %>%
            dplyr::summarise(
                content.rate=replace_nan(mean(content.rate, na.rm = TRUE)),
                wc=sum(word.count, na.rm = TRUE), 
                id=max(id, na.rm = TRUE)
         ) %>%
         dplyr::group_by_("times") %>%
         dplyr::mutate(
             prop_wc = wc/(sum(wc, rm.na=TRUE) - 1)
         )

    ## set up color gradients
    colfunc <- grDevices::colorRampPalette(c(functional, content))
    cols <- colfunc(max.color.breaks)
   
    ## add colors to df_lexical_classification based on agrgegated 
    ## average lexical_classification per edge
    cuts <- cut(df_lexical_classification[["content.rate"]], brks)

    df_lexical_classification[["color"]] <- cuts %l% data.frame(cut(brks, brks), cols, 
        stringsAsFactors = FALSE)

    ## split it back into the iterative per row 
    ## dataframes of aggregated values
    list_lexical_classification <- lapply(split(as.data.frame(df_lexical_classification)[, -1], df_lexical_classification[[1]]), 
        function(x) {
            y <- colsplit2df(x, sep=qsep)
            colnames(y)[1:2] <- c("from", "to")
            y
    })

    ## create a single network plot with all values
    dat <- sentCombine(attributes(x)[["text.var"]][["text.var"]], y[["from"]])
    theplot <- discourse_map(dat[, "text.var"], dat[, "from"], 
        ...)[["plot"]]

    ## generate edge constant if needed
    if (missing(edge.constant)) {
        edge.constant <- length(unique(y[, 1])) * 2.5
    }

    ## Add colors from the aggregated list of average content rate
    ## and output a corresponding list of network plots
    new_lex_nets <- lapply(list_lexical_classification, colorize, theplot)

    names(y)[7] <- sub("QDAPQDAP", "|", names(y)[7])
    missing <- which(is.na(y[["word.count"]]))

    ## Add edge weights etc to each graph
    igraph_objs <- stats::setNames(lapply(seq_along(new_lex_nets), 
        function(i, grp =new_lex_nets, len=length(unique(y[[1]])), sep=qsep){

        ## limit the edge weights (widths) of first 5 plots)
        if (i %in% 1:5) {
            edge.constant <- edge.constant/(len/i)
        }

        ## calculate edge widths
        cur <- list_lexical_classification[[i]]
        cur[["width"]] <- edge.constant*cur[["prop_wc"]]

        ## get current edge
        cur_edge <- which.max(cur[["id"]])
        cur_edge2 <- max(cur[["id"]])

        ## create current edge label and lexical_classification sign
        cur_lex <- y[y[["id"]] == cur_edge2, "content.rate"]
        lab <- ifelse(is.na(cur_lex), "-", numbformat(cur_lex, digits))
        E(grp[[i]])$label <- NA
        curkey <- data.frame(paste2(cur[cur_edge, 1:2], sep="|-|qdap|-|"), lab, 
            stringsAsFactors = FALSE)

        if (i %in% missing) current.speaker.color <- missing.color

        ## Set up widths and colors
        tcols <- cur[, c("from", "to", "color"), drop=FALSE]
        widths <- cur[, c("from", "to", "width"), drop=FALSE]
        widths[["width"]] <- ceiling(widths[["width"]])
        ekey <- paste2(edge_capture(grp[[i]]), sep=sep)
        ckey <- colpaste2df(tcols, 1:2, sep = sep, keep.orig=FALSE)[, 2:1]
        wkey <- colpaste2df(widths, 1:2, sep = sep, keep.orig=FALSE)[, 2:1]
        E(grp[[i]])$width <- NAer(ekey %l% wkey, 1)
        #plot(grp[[i]], edge.curved=TRUE)
        E(grp[[i]])$color <- ekey %l% ckey
        E(grp[[i]])$label <- ekey %l% curkey
        V(grp[[i]])$frame.color <- NA
        if (!is.null(current.speaker.color)) {
            spkkey <- data.frame(as.character(cur[cur_edge, 1]), current.speaker.color, 
                stringsAsFactors = FALSE)
            V(grp[[i]])$frame.color <- V(grp[[i]])$name %l% spkkey
        }
        V(grp[[i]])$frame.color[is.na(V(grp[[i]])$frame.color)] <- non.speaker.color

        ## change edge label color
        E(grp[[i]])$label.color <- current.color
        ##ekey %l% data.frame(curkey[1, 1], current.color)
            
        grp[[i]]
    }), paste0("Turn_", pad(1:nrow(y))))

    timings <- round(exp(y[["word.count"]]/(max(y[["word.count"]], na.rm=TRUE)/time.constant)))


    if(wc.time) {
        igraph_objs <- rep(igraph_objs, replace_nan(timings, is.na, 1))
    }

    ## starts with a blank object
    igraph_objs <- rep(igraph_objs, c(2, rep(1, length(igraph_objs) - 1)))
    len <- nchar(char2end(names(igraph_objs)[1], "_"))
    names(igraph_objs)[1] <- sprintf("turn_%s", paste(rep(0, len), collapse=""))

    uncol <- E(igraph_objs[[1]])$color
    E(igraph_objs[[1]])$color <- NA
    E(igraph_objs[[1]])$label.color <- NA
    E(igraph_objs[[1]])$label <- NA
    V(igraph_objs[[1]])$frame.color <- non.speaker.color    

    ## end with no label or frame color
    igraph_objs <- rep(igraph_objs, c(rep(1, length(igraph_objs) - 1), 2))
    E(igraph_objs[[length(igraph_objs)]])$label.color <- NA
    E(igraph_objs[[length(igraph_objs)]])$label <- NA
    V(igraph_objs[[length(igraph_objs)]])$frame.color <- non.speaker.color
    
    ## add class info
    class(igraph_objs) <- "animated_lexical_classification"
    attributes(igraph_objs)[["title"]] <- title
    attributes(igraph_objs)[["timings"]] <- timings
    attributes(igraph_objs)[["type"]] <- "network"
    attributes(igraph_objs)[["legend"]] <- cols
    attributes(igraph_objs)[["data"]] <- list_lexical_classification
    igraph_objs
}

## Hlper to animate bar graph
Animate_lexical_classification_bar <- function(x, wc.time = TRUE, time.constant = 2, 
    digits = 2, all.color.line = "red", ...) {

    content.rate <- NULL
    
    y <- preprocessed(x) 

    isna <- which(is.na(y[["word.count"]]))
    if (!identical(integer(0), isna)){
        y[isna, 3:6] <- 0
    }

    nms <- names(y)[1]
    names(y)[1] <- "group"
    y <- y %>%
        dplyr::select_("group", "word.count", "content.rate")

    nrows <- 1:nrow(y)
    inds <- unlist(lapply(nrows, function(i) nrows[1:i]))
    bigy <- y[inds, ]
    bigy[["times"]] <- rep(nrows, nrows)

    thedat <- bigy %>%
        dplyr::group_by_("times", "group") %>%
            dplyr::summarise(
                lexical_classification = replace_nan(mean(content.rate, na.rm = TRUE))
         ) 

    thebardat <- bigy %>%
        dplyr::group_by_("times") %>%
        dplyr::summarise(
           ave.lex = mean(content.rate, na.rm = TRUE)
        ) %>% `[`(, 2) %>% unlist

    ## Order factor levels greatest to least
    ord <- levels(scores(x)[[1]])
    thedat[["group"]] <- factor(thedat[["group"]], levels = ord)

    rng <- max(thedat[["lexical_classification"]], na.rm=TRUE)

    listdat <- split(thedat, thedat[["times"]])

    theplot <- ggbar_lex(listdat[[length(listdat)]], grp = nms, rng = rng)

    ggplots <- stats::setNames(lapply(seq_along(listdat), function(i, aplot=theplot) {

        listdat[[i]][["group"]] <- factor(listdat[[i]][["group"]], levels=ord)
        titlepol <- numbformat(thebardat[i], digits)

        aplot[["labels"]][["title"]] <- paste(
            paste0(sprintf("Average Discourse Content Rate:  %s",titlepol), "%"), 
            sprintf("%sCurrent Speaker:   %s", paste(rep(" ", 15), 
            collapse=""), y[i, 1]))

        aplot[["data"]] <- listdat[[i]]
        aplot + geom_hline(yintercept=unlist(thebardat[i]), size=1, color=all.color.line) 
        }), paste0("turn_", pad(1:length(listdat))))

    timings <- round(exp(y[["word.count"]]/(max(y[["word.count"]], na.rm=TRUE)/time.constant)))

    if(wc.time) {
        ggplots <- rep(ggplots, replace_nan(timings, is.na, 1))
    }

    ## starts with a blank object and end match the network Animate
    theplot[["data"]][, "lexical_classification"] <- NaN
    ggplots <- unlist(list(list(theplot), ggplots, 
        ggplots[length(ggplots)]), recursive=FALSE)

    len <- nchar(char2end(names(ggplots)[2], "_"))
    names(ggplots)[1] <- sprintf("turn_%s", paste(rep(0, len), collapse=""))

    ## add class info
    class(ggplots) <- "animated_lexical_classification"
    attributes(ggplots)[["timings"]] <- timings
    attributes(ggplots)[["type"]] <- "bar"
    attributes(ggplots)[["legend"]] <- NULL
    attributes(ggplots)[["data"]] <- listdat
    ggplots
}

replace_nan <- function(x, fun = is.nan, repl = NA) {x[fun(x)] <- repl; x}


## Helper to make intial plot
ggbar_lex <- function(dat, grp = grp, rng = rng) {

    padding <- rng*.05
  
    ggplot2::ggplot(dat, aes_string(x="group"))  +
        ggplot2::geom_bar(aes_string(weight="lexical_classification")) +
        ggplot2::ylab("Average Content Rate") + 
        ggplot2::xlab(paste(sapply(unlist(strsplit(grp, "&")), Caps), collapse = " ")) +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(sprintf("Average Discourse Content Rate:  %s", "")) +
        ggplot2::theme(axis.text.x=element_text(angle = 90, vjust = .4, hjust = 1, size=11),
            plot.title=element_text(hjust=0, size=11, color="grey60")) + 
        ggplot2::scale_x_discrete(drop=FALSE) + 
        ggplot2::scale_y_continuous(expand = c(0,0), limits=c(0, rng + padding),
            labels = function(x) paste0(x, "%")) 
}


## Helper for animated text

Animate_lexical_classification_text <- function(x, wc.time = TRUE, time.constant = 2, 
    width, function.words, left, right, coord, just, ...) {
    
    y <- preprocessed(x) 

    txt <- gsub("/0", "", gsub("/1", right, gsub("(?<=\\b)(['a-z09]+)(?=/1)", 
        paste0(left, "\\1"), y[["markup"]], perl=TRUE))) 
    
    txt <- lapply(txt, function(x){
            paste(strwrap(x, width), collapse="\n")
        }) %>% unlist

    theplot <- ggplot2::ggplot(data.frame(x=0:1, y=0:1), ggplot2::aes(x, x, y=y)) + 
        ggplot2::geom_blank() + ggplot2::theme_bw() +
        ggplot2::theme( 
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            axis.text = ggplot2::element_blank()
        ) + 
        ggplot2::ylab(NULL) + 
        ggplot2::xlab(NULL) 

    ggplots <- lapply(txt, function(z){
        theplot + ggplot2::annotate("text", x = coord[1], 
            y = coord[2], label = z, vjust = just[2], hjust = just[1], ...)
    })

    timings <- round(exp(y[["word.count"]]/(max(y[["word.count"]], na.rm=TRUE)/time.constant)))

    if(wc.time) {
        ggplots <- rep(ggplots, replace_nan(timings, is.na, 1))
    }

    ## starts with a blank object and end match the network Animate
    ggplots <- unlist(list(list(theplot), ggplots, 
        list(theplot)), recursive=FALSE)

    ## add class info
    class(ggplots) <- "animated_lexical_classification"
    attributes(ggplots)[["timings"]] <- timings
    attributes(ggplots)[["type"]] <- "text"
    attributes(ggplots)[["legend"]] <- NULL
    attributes(ggplots)[["data"]] <- NULL
    ggplots
}



#' Animate Formality
#' 
#' \code{Animate.lexical_classification} - Animate a 
#' \code{\link[qdap]{lexical_classification}} object.
#' 
#' lexical_classification Method for Animate
#' @param x A \code{\link[qdap]{lexical_classification}} object.
#' @param type  Character string of either \code{"network"} (as a network 
#' plot), \code{"bar"} (as a bar plot), or \code{"text"} (as a simple 
#' colored text plot).
#' @param content The color to use for 100\% lexical_classification (purely 
#' content).
#' @param functional The color to use for 0\% lexical_classification (purely 
#' functional).
#' @param edge.constant A constant to multiple edge width by.
#' @param wc.time logical.  If \code{TRUE} weights duration of frame by word 
#' count.
#' @param time.constant A constant to divide the maximum word count by.  Time
#' is calculated by `round(exp(WORD COUNT/(max(WORD COUNT)/time.constant)))`.  
#' Therefore a larger constant will make the difference between the large and 
#' small word counts greater.
#' @param title The title to apply to the animated image(s).
#' @param digits The number of digits to use in the current turn of talk's
#' content rate.
#' @param current.color The color to use for the current turn of talk's 
#' content rate.
#' @param current.speaker.color The color for the current speaker.
#' @param non.speaker.color The color for the speakers not currently speaking.
#' @param missing.color The color to use in a network plot for edges 
#' corresponding to missing text data.  Use \code{\link[stats]{na.omit}} before 
#' hand to remove the missing values all together.
#' @param all.color.line The color to use for the total average discourse 
#' content rate.
#' @param width The width to break text at if \code{type = "text"}.
#' @param function.words A vector of function words.  Default is 
#' \code{\link[qdapDictionaries]{function.words}}.
#' @param left A left bound to wrap content words with if \code{type = "text"}.
#' @param right A right bound to wrap content words with  if \code{type = "text"}.
#' @param coord The x/y coordinate to plot the test if \code{type = "text"}.
#' @param just The \code{hjust} and \code{vjust} values to use for the text if 
#' \code{type = "text"}.
#' @param \ldots Other arguments passed to \code{\link[qdap]{discourse_map}} or
#' \code{\link[ggplot2]{annotate}} if \code{type = "text"}.
#' @note The width of edges is based on words counts on that edge until that 
#' moment divided by total number of words used until that moment.  Thicker 
#' edges tend to thin as time passes.  The actual duration the current edge 
#' stays as the \code{current.color} is based on word counts for that particular 
#' flow of dialogue divided by total dialogue (words) used.  The edge label is
#' the current content rate for that turn of talk (an aggregation of 
#' the sub sentences of the current turn of talk).  The coloring of the current 
#' edge content rate is produced at th sentence level, therefor a label may 
#' indicate a more content laden current turn of talk, while the coloring may 
#' indicate a functional laden average of sentences.  Coloring is based on 
#' percentage of conent words.
#' @import igraph
#' @export
#' @method Animate lexical_classification
Animate.lexical_classification <- function(x, type = "network", content = "red", 
    functional = "yellow", edge.constant, wc.time = TRUE, time.constant = 2, 
    title = NULL, digits = 2, current.color = "black", 
    current.speaker.color = NULL, non.speaker.color = NA,
    missing.color = "purple", all.color.line = "red", width = 65,
    function.words = qdapDictionaries::function.words, left = "<<", right = ">>", 
    coord = c(.0, .5), just = c(.0, .5), ...){

    switch(type,
        network = {
            Animate_lexical_classification_net(x = x, content = content, 
                functional = functional, edge.constant = edge.constant, 
                wc.time = wc.time, time.constant = time.constant, title = title, 
                digits = digits, current.color = current.color, 
                current.speaker.color = current.speaker.color, 
                non.speaker.color = non.speaker.color, missing.color = missing.color , 
                ...)
        },
        bar = {
            Animate_lexical_classification_bar(x = x, wc.time = wc.time, 
                time.constant = time.constant, digits = digits, 
                all.color.line = all.color.line, ...)         
        },
        text = {
           Animate_lexical_classification_text(x = x, wc.time = wc.time, 
               time.constant = time.constant, width = width, 
               function.words = function.words, left = left, right = right, 
               coord = coord, just = just, ...)
        }, stop("`type` must be \"network\", \"bar\", or \"text\"")
    )

}

#' Prints an animated_lexical_classification  Object
#' 
#' Prints an animated_lexical_classification  object.
#' 
#' @param x The animated_lexical_classification  object.
#' @param title The title of the plot.
#' @param layout \pkg{igraph} \code{layout} to use.
#' @param seed The seed to use in plotting the graph.
#' @param pause The length of time to pause between plots.
#' @param legend The coordinates of the legend. See 
#' \code{\link[plotrix]{color.legend}} for more information.
#' @param legend.cex character expansion factor. \code{NULL} and \code{NA} are 
#' equivalent to 1.0. See \code{\link[graphics]{mtext}} for more information.
#' @param bg The color to be used for the background of the device region. See
#' \code{\link[graphics]{par}} for more information. 
#' @param net.legend.color The text legend color for the network plot.
#' @param \ldots Other Arguments passed to \code{\link[igraph]{plot.igraph}}.
#' @import igraph
#' @method print animated_lexical_classification 
#' @export
print.animated_lexical_classification <- function(x, title = NULL, 
    seed = sample(1:10000, 1), layout=layout.auto, pause = 0, 
    legend = c(-.5, -1.5, .5, -1.45), legend.cex=1, bg=NULL, 
    net.legend.color = "black", ...){
    
    if (is.null(title)) {
        title <- attributes(x)[["title"]]
    }

    switch(attributes(x)[["type"]],
        network = {
            invisible(lapply(x, function(y) {
                set.seed(seed)
                graphics::par(bg = bg)
                plot.igraph(y, edge.curved=TRUE, layout=layout)
                if (!is.null(title)) {
                    graphics::mtext(title, side=3)
                }
                if (!is.null(legend)) {
                    plotrix::color.legend(legend[1], legend[2], legend[3], legend[4], 
                        c("Functional", "Content"), attributes(x)[["legend"]], 
                        cex = legend.cex, col=net.legend.color, ...)
                }
                if (pause > 0) Sys.sleep(pause)
            })) 
        },
        bar = {
            invisible(lapply(x, print))
        },
        text = {
            invisible(lapply(x, print))
        }, stop("`type` must be \"network\", \"bar\", or \"text\"")
    )  
}


#' Plots an animated_lexical_classification  Object
#' 
#' Plots an animated_lexical_classification  object.
#' 
#' @param x The animated_lexical_classification  object.
#' @param \ldots Other arguments passed to \code{print.animated_lexical_classification }.
#' @method plot animated_lexical_classification 
#' @export
plot.animated_lexical_classification  <- function(x, ...){ 

    print(x, ...)

}


#' Network Lexical Classification
#' 
#' \code{Network.lexical_classification} - Network a 
#' \code{\link[qdap]{lexical_classification}} object.
#' 
#' lexical_classification Method for Network
#' @param x A \code{\link[qdap]{lexical_classification}} object.
#' @param content The color to use for 100\% lexical_classification (purely 
#' content).
#' @param functional The color to use for 0\% lexical_classification (purely 
#' functional).
#' @param edge.constant A constant to multiple edge width by.
#' @param title The title to apply to the Networked image(s).
#' @param digits The number of digits to use in the current turn of talk 
#' lexical_classification.
#' @param \ldots Other arguments passed to \code{\link[qdap]{discourse_map}}.
#' @import igraph
#' @importFrom qdapTools %l% 
#' @export
#' @method Network lexical_classification
Network.lexical_classification <- function(x, functional = "yellow", content = "red", 
    edge.constant, title = NULL, digits = 2, ...){

    content.rate <- word.count <- id <- NULL
    
    qsep <- "|-|qdap|-|"

    brks <- seq(0, 1, by=.001)
    max.color.breaks <- length(brks)

    y <- preprocessed(x) 

    nms <- names(y)[1]
    names(y)[1] <- "group"
    y <- y %>%
        dplyr::select_("group", "word.count", "content.rate") %>%
        dplyr::mutate(content.rate=content.rate/100)

    condlens <- rle(as.character(y[["group"]]))
    y[["temp"]] <- rep(paste0("X", pad(1:length(condlens[[2]]))),
        condlens[[1]])

    ## Add to  and from columns
    y <- cbind(y, from_to_End(y[["group"]]))

    ## repeat last to column to match with split sentence (i.e.
    ## we don't want an edge to return to the node it leaves
    tos <- split(y[["to"]], y[["temp"]])
    tos_lens <- sapply(tos, length)
    y[["to"]] <- rep(sapply(tos, utils::tail, 1), tos_lens)
  
    ## make a combined from|to column
    y[["fromQDAPQDAPto"]] <- paste2(y[, c("from", "to")], sep=qsep)

    df_lexical_classification <- y %>%
        dplyr::group_by_("fromQDAPQDAPto") %>%
            dplyr::summarise(
                content.rate=replace_nan(mean(content.rate, na.rm = TRUE)),
                wc=sum(word.count, na.rm = TRUE)
         ) %>%
         dplyr::mutate(
             prop_wc = wc/(sum(wc, rm.na=TRUE) - 1)
         )

    ## set up color gradients
    colfunc <- grDevices::colorRampPalette(c(functional, content))
    cols <- colfunc(max.color.breaks)
   
    ## add colors to df_lexical_classification based on agrgegated 
    ## average lexical_classification per edge
    cuts <- cut(df_lexical_classification[["content.rate"]], brks)

    df_lexical_classification[["color"]] <- cuts %l% data.frame(cut(brks, brks), cols, 
        stringsAsFactors = FALSE)

    ## split it back into the iterative per row 
    ## dataframes of aggregated values
    df_lexical_classification <- colsplit2df(as.data.frame(df_lexical_classification), 
        sep=qsep, keep.orig = TRUE)
    colnames(df_lexical_classification)[2:3] <- c("from", "to")

    ## create a single network plot with all values
    dat <- sentCombine(attributes(x)[["text.var"]][["text.var"]], y[["from"]])
    theplot <- discourse_map(dat[, "text.var"], dat[, "from"], 
        ...)[["plot"]]

    ## generate edge constant if needed
    if (missing(edge.constant)) {
        edge.constant <- length(unique(y[, 1])) * 2.5
    }

    ## Add colors from the aggregated list of average content rate
    ## and output a corresponding list of network plots
    theplot <- colorize(df_lexical_classification[, -1], theplot)

    names(df_lexical_classification)[1] <- names(y)[7] <- sub("QDAPQDAP", "|", names(y)[7])
    missing <- which(is.na(y[["word.count"]]))
   
    theedges <- paste2(edge_capture(theplot), sep=qsep)
    E(theplot)$label <- qdapTools::lookup(theedges, df_lexical_classification[, "from|to"], 
        numbformat(df_lexical_classification[, "content.rate"], digits))

    ## Set up widths and colors
    df_lexical_classification[, "width"] <- edge.constant*df_lexical_classification[, "prop_wc"]
    tcols <- df_lexical_classification[, c("from", "to", "color"), drop=FALSE]
    widths <- df_lexical_classification[, c("from", "to", "width"), drop=FALSE]
    widths[, "width"] <- ceiling(widths[, "width"])
    ekey <- paste2(edge_capture(theplot), sep=qsep)
    ckey <- colpaste2df(tcols, 1:2, sep = qsep, keep.orig=FALSE)[, 2:1]
    wkey <- colpaste2df(widths, 1:2, sep = qsep, keep.orig=FALSE)[, 2:1]
    E(theplot)$width <- NAer(ekey %l% wkey, 1)
    
    ## add class info
    class(theplot) <- c("Network", class(theplot))
    attributes(theplot)[["title"]] <- title
    attributes(theplot)[["legend.gradient"]] <- cols
    attributes(theplot)[["network.type"]] <- "lexical_classification"
    attributes(theplot)[["legend.label"]] <- c("Functional", "Content")
    attributes(theplot)[["n.color.breaks"]] <- max.color.breaks
    attributes(theplot)[["color.locs"]] <- as.numeric(cuts)    
    theplot
}


#' \code{cumulative.lexical_classification} - Generate lexical_classification over time (duration in 
#' sentences).
#' @rdname cumulative
#' @export
#' @method cumulative lexical_classification
cumulative.lexical_classification <- function (x, ...) {
    keeps <- !is.na(preprocessed(x)[["content.rate"]])
    y <- preprocessed(x)[["content.rate"]][keeps]
    out <- list(cumulative_average_content_rate = cummean(y))
    class(out) <- "cumulative_lexical_classification"
    out
}

#' Plots a cumulative_lexical_classification Object
#' 
#' Plots a cumulative_lexical_classification object.
#' 
#' @param x The cumulative_lexical_classification object.
#' @param \ldots ignored
#' @method plot cumulative_lexical_classification 
#' @export
plot.cumulative_lexical_classification <- function(x, ...){

    len <- length(x[[1]])
    cumlexical_classification <- data.frame(cum_mean = x[[1]], Time = 1:len) 

    ggplot2::ggplot() + ggplot2::theme_bw() +
        ggplot2::geom_smooth(data = cumlexical_classification, ggplot2::aes_string(y="cum_mean", 
            x = "Time")) +
        ggplot2::geom_hline(y=mean(x[[1]]), color="grey30", size=1, alpha=.3, linetype=2) + 
        ggplot2::annotate("text", x = len/2, y = mean(x[[1]]), color="grey30", 
            label = "Average Content Rate", vjust = .3, size=4) +
        ggplot2::geom_line(data = cumlexical_classification, ggplot2::aes_string(y="cum_mean", 
            x = "Time"), size=1) +
        ggplot2::ylab("Cumulative Average Content Rate") + 
        ggplot2::xlab("Duration") +
        ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, len)) +
        ggplot2::scale_y_continuous(labels=function(x) paste0(x, "%"))

}

#' Prints a cumulative_lexical_classification Object
#' 
#' Prints a cumulative_lexical_classification  object.
#' 
#' @param x The cumulative_lexical_classification object.
#' @param \ldots ignored
#' @method print cumulative_lexical_classification
#' @export
print.cumulative_lexical_classification <- function(x, ...) {
    print(plot.cumulative_lexical_classification(x, ...))
}

#' \code{cumulative.animated_lexical_classification} - Generate animated lexical_classification over time 
#' (duration in sentences).
#' @rdname cumulative
#' @export
#' @method cumulative animated_lexical_classification
cumulative.animated_lexical_classification <- function(x, ...) {

    if(attributes(x)[["type"]] != "bar") {
        stop("Output must be from an `Animate.lexical_classification` when `type =\"bar\"`")
    }

    out <- c(0, unlist(lapply(x, grab_ave_lexical_classification), use.names = FALSE))
    avelex <- utils::tail(out, 1)
    len <- length(out)
    
    output <- data.frame(cum_mean = out, Time = 1:len, drop=TRUE) 

    class(output) <- c("cumulative_animated_lexical_classification", class(output))
    attributes(output)[["length"]] <- len
    attributes(output)[["average.lexical_classification"]] <- avelex   
    attributes(output)[["range"]] <- x[[1]][["scales"]][["scales"]][[1]][["limits"]]  
    output
}

#' Plots a cumulative_animated_lexical_classification Object
#' 
#' Plots a cumulative_animated_lexical_classification object.
#' 
#' @param x The cumulative_animated_lexical_classification object.
#' @param \ldots ignored
#' @method plot cumulative_animated_lexical_classification 
#' @export
plot.cumulative_animated_lexical_classification <- function(x, ...){
   
    output <- lapply(1:nrow(x), function(i) {

        ggplot2::ggplot() + ggplot2::theme_bw() +
            ggplot2::geom_line(data = x[1:i, ,drop=FALSE], ggplot2::aes_string(y="cum_mean", 
                x = "Time"), size=1) +
            ggplot2::geom_hline(yintercept=50, size=1, alpha=.4, color="grey50", linetype="dashed") + 
            ggplot2::geom_hline(y=attributes(x)[["average.lexical_classification"]], 
                color="grey30", size=1, alpha=.3) + 
            ggplot2::ylab("Cumulative Average Content Rate") + 
            ggplot2::xlab("Duration") +
            ggplot2::scale_x_continuous(expand = c(0, 0), 
                limits = c(0, attributes(x)[["length"]])) +
            ggplot2::ylim(range(x[["cum_mean"]])) +
            ggplot2::annotate("point", y = x[i, "cum_mean"], 
                x =x[i, "Time"], colour = "red", size = 1.5) +
            ggplot2::scale_y_continuous(labels=function(x) paste0(x, "%"))
    })

    output[[1]][["layers"]][[4]][["geom_params"]][["colour"]] <- NA
    output[[length(output)]] <- output[[length(output)]] + 
        ggplot2::geom_smooth(data = x, 
            ggplot2::aes_string(y="cum_mean", x = "Time")) 

    output
}

#' Prints a cumulative_animated_lexical_classification Object
#' 
#' Prints a cumulative_animated_lexical_classification  object.
#' 
#' @param x The cumulative_animated_lexical_classification object.
#' @param \ldots ignored
#' @method print cumulative_animated_lexical_classification
#' @export
print.cumulative_animated_lexical_classification <- function(x, ...) {
    print(plot.cumulative_animated_lexical_classification(x, ...))
}

grab_ave_lexical_classification <- function(x, left="Average Discourse Content Rate:", 
    right = "%") {
  
    genXtract(x[["labels"]][["title"]], left, right) %>% 
    Trim() %>% 
    as.numeric() 
}


