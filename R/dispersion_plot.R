#' Lexical Dispersion Plot
#' 
#' Generate a lexical dispersion plot of terms.
#' 
#' @param text.var The text variable.
#' @param match.terms  A vector of quoted terms or a named list of quoted terms.  
#' If the latter terms will be combined into a single unified theme named 
#' according to the list names.  Note that terms within the vectors of the list 
#' cannot be duplicated.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param rm.vars The repeated measures variables.  Default \code{NULL} generates 
#' one facet for all text.  Also takes a single repeated measures variable or 
#' a list of 1 or more grouping variables.
#' @param color The color of the word symbols.
#' @param bg.color The background color.
#' @param horiz.color The color of the horizontal tracking stripe.  Use 
#' \code{horiz.color = bg.color} to eliminate.
#' @param total.color The color to use for summary `all` group.  If \code{NULL}
#' totals are dropped.
#' @param symbol The word symbol.  Default is \code{"|"}.
#' @param title Title of the plot
#' @param rev.factor logical.  If \code{TRUE} reverses the plot order of the 
#' factors.
#' @param wrap a character to wrap around the words (enables the reader to 
#' visualize spaces).  Default is \code{"'"}, use \code{""} to remove.
#' @param xlab The x label.
#' @param ylab The y label.
#' @param size The size of the plotting symbol.
#' @param plot logical.  If \code{TRUE} the plot will automatically plot.  
#' The user may wish to set to \code{FALSE} for use in knitr, sweave, etc.
#' to add additional plot layers.
#' @param char2space A vector of characters to be turned into spaces.  
#' @param apostrophe.remove logical.  If \code{TRUE} removes apostrophes from 
#' the output.
#' @param scales Should scales be fixed (\code{"fixed"}, the default), free 
#' (\code{"free"}), or free in one dimension (\code{"free_x"}, \code{"free_y"})
#' @param space If \code{"fixed"}, the default, all panels have the same size. 
#' If \code{"free_y"} their height will be proportional to the length of the y 
#' scale; if \code{"free_x"} their width will be proportional to the length of 
#' the x scale; or if \code{"free"} both height and width will vary. 
#' @param \ldots Other argument supplied to \code{\link[qdap]{strip}}.
#' @return Plots a dispersion plot and invisibly returns the ggplot2 object.
#' @keywords dispersion
#' @export
#' @importFrom ggplot2 ggplot scale_colour_manual aes geom_point scale_x_continuous element_rect element_line ggtitle theme theme_bw element_blank facet_grid ylab xlab
#' @note The match.terms is character sensitive.  Spacing is an important way 
#' to grab specific words and requires careful thought.  Using "read" will find 
#' the words "bread", "read" "reading", and "ready".  If you want to search 
#' for just the word "read" you'd supply a vector of c(" read ", " reads", 
#' " reading", " reader").  
#' @seealso \code{\link[qdap]{term_match}}
#' @examples 
#' \dontrun{
#' term_match(raj$dialogue, c(" love ", "love", " night ", "night"))
#' dispersion_plot(raj$dialogue, c(" love ", "love", " night ", "night"))
#' dispersion_plot(raj$dialogue, c("love", "night"), rm.vars = raj$act)
#' with(rajSPLIT , dispersion_plot(dialogue, c("love", "night"), 
#'     grouping.var = list(fam.aff, sex), rm.vars = act))
#' 
#' ## With grouping variables
#' with(rajSPLIT , dispersion_plot(dialogue, c("love", "night"), 
#'      grouping.var = sex, rm.vars = act))
#' 
#' ## Drop total with `total.color = NULL`
#' with(rajSPLIT , dispersion_plot(dialogue, c("love", "night"), 
#'      grouping.var = sex, rm.vars = act, total.color = NULL))
#'
#' ## Change color scheme
#' with(rajSPLIT, dispersion_plot(dialogue, c("love", "night"), 
#'     bg.color = "black", grouping.var = list(fam.aff, sex), 
#'     color = "yellow", total.color = "white", horiz.color="grey20"))
#'     
#' ## Use `word_list`
#' ## Presidential debates by all
#' wrds <- word_list(pres_debates2012$dialogue, stopwords = Top200Words)
#' wrds2 <- spaste(wrds[["rfswl"]][["all"]][, "WORD"])
#' wrds2 <- c(" governor~~romney ", wrds2[-c(3, 12)])
#' with(pres_debates2012 , dispersion_plot(dialogue, wrds2, rm.vars = time))
#'
#' ## Presidential debates by person
#' dat <- pres_debates2012
#' dat <- dat[dat$person %in% qcv(ROMNEY, OBAMA), ]
#' 
#' wordlist <- c(" tax", " health", " rich ", "america", " truth", 
#'     " money", "cost", " governnor", " president", " we ", 
#'     " job", " i ", " you ", " because ", " our ", " years ")
#' 
#' with(dat, dispersion_plot(dialogue, wordlist, total.color = NULL, 
#'     bg.color = "white", grouping.var = person, rm.vars = time,
#'     color = "black", horiz.color="grey80"))
#' 
#' wordlist2 <- c(" i'd ", " i'll ", " i'm ", " i've ", " i ", 
#'     " we'd ", " we'll ", " we're ", " we've ", " we ", 
#'     " you'd ",  " you'll ", " you're ", " you've ", " you ", " your ",
#'     " he'd ", " he'll ", " he's ", " he ")
#' 
#' with(dat, dispersion_plot(dialogue, wordlist2, 
#'     bg.color = "black", grouping.var = person, rm.vars = time,
#'     color = "yellow", total.color = NULL, horiz.color="grey20"))
#'    
#' with(dat, dispersion_plot(dialogue, wordlist2, 
#'     bg.color = "black", grouping.var = person, rm.vars = time,
#'     color = "red", total.color = "white", horiz.color="grey20"))
#' 
#' ## `match.terms` as a named list        
#' wordlist3 <- list(
#'     I = c(" i'd ", " i'll ", " i'm ", " i've ", " i "),
#'     we = c(" we'd ", " we'll ", " we're ", " we've ", " we "),
#'     you = c(" you'd ",  " you'll ", " you're ", " you've ", " you ", " your "),
#'     he = c(" he'd ", " he'll ", " he's ", " he ")
#' )
#' 
#' with(dat, dispersion_plot(dialogue, wordlist3,
#'     bg.color = "grey60", grouping.var = person, rm.vars = time,
#'     color = "blue", total.color = "grey40", horiz.color="grey20"))
#' 
#' colsplit2df(scores(with(dat, termco(dialogue, list(time, person), wordlist3))))
#' 
#' ## Extras:
#' ## Reverse facets
#' 
#' x <- with(pres_debates2012 , dispersion_plot(dialogue, wrds2, rm.vars = time))
#' 
#' ## function to reverse ggplot2 facets
#' rev_facet <- function(x) {
#'     names(x$facet)[1:2] <- names(x$facet)[2:1]
#'     print(x)
#' }
#' 
#' rev_facet(x)
#' 
#' ## Discourse Markers: See...
#' ## Schiffrin, D. (2001). Discourse markers: Language, meaning, and context. 
#' ##    In D. Schiffrin, D. Tannen, & H. E. Hamilton (Eds.), The handbook of 
#' ##    discourse analysis (pp. 54-75). Malden, MA: Blackwell Publishing.
#' 
#' discoure_markers <- list(
#'     response_cries = c(" oh ", " ah ", " aha ", " ouch ", " yuk "),
#'     back_channels = c(" uh-huh ", " uhuh ", " yeah "), 
#'     summons = " hey ", 
#'     justification = " because "
#' )
#' 
#' (markers <- with(pres_debates2012, 
#'     termco(dialogue, list(person, time), discoure_markers)
#' ))
#' plot(markers, high="red")
#' 
#' with(pres_debates2012, 
#'     termco(dialogue, list(person, time), discoure_markers, elim.old = FALSE)
#' )
#' 
#' with(pres_debates2012, 
#'     dispersion_plot(dialogue, unlist(discoure_markers), person, time)
#' )
#' }
dispersion_plot <- function(text.var, match.terms, grouping.var = NULL,  
    rm.vars =NULL, color = "blue", bg.color = "grey90", horiz.color = "grey85", 
    total.color = "black", symbol = "|", title = "Lexical Dispersion Plot", 
    rev.factor = TRUE, wrap = "'", xlab = "Dialogue (Words)", ylab = NULL, 
    size = 4, plot = TRUE, char2space = "~~", apostrophe.remove = FALSE, 
    scales="free", space="free", ...) {

    match.list <- NULL
    if (is.list_o_vectors(match.terms)) {
        match.terms <- list_namer(match.terms)
        match.list <- match.terms
        match.terms <- unlist(match.terms)
        if (any(duplicated(match.terms))) stop("`match.terms` cannot contain duplicates")
    }

    word.num <- NULL
    GV <- ifelse(!is.null(grouping.var), TRUE, FALSE)

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

    if(is.null(rm.vars)){
        time <- rep("time1", length(text.var))
    } else {
        if (is.list(rm.vars) & length(rm.vars)>1) {
            time <- paste2(rm.vars)
        } else {
            time<- unlist(rm.vars)
        } 
    } 

    ## Put the char2space into the text
    text.var <- tolower(text.var)
    reps <- Trim(match.terms)
    text.var <- mgsub(gsub(char2space, " ", reps), reps, text.var)

    ## combine text, rm.var, group vars together in one DF
    DF <- data.frame(grouping, text.var, check.names = FALSE, time = time,
        stringsAsFactors = FALSE, orig.row.num = seq_len(length(text.var)))
    DF[, "grouping"] <- factor(DF[, "grouping"])

    ## reverse grouping variable order
    if (rev.factor && !is.null(grouping.var)) {
        DF[, "grouping"] <- factor(DF[, "grouping"], 
            levels = rev(levels(DF[, "grouping"])))    
    }

    DF[, "time"] <- factor(DF[, "time"])
    DF <- DF[!is.na(DF[["text.var"]]), ]
    
    ## split DF into a list by rm.vars
    LDF <- split(DF, DF[, "time"])

    ## split it out by rep. measures var.
    rmout <- lapply(LDF, function(x) {
        x[, "text.var"] <- strip(x[, "text.var"], 
            apostrophe.remove = apostrophe.remove, ...)
        out <- cm_df.temp(x, text.var = "text.var", strip = FALSE, 
            char.keep = char2space)
        out[, "text"] <- spaste(gsub(char2space, " ", out[, "text"]))

        ## cycle through the words list and match rows of the out
        out3 <- lapply(match.terms, function(x){
            xx <- gsub(char2space, " ", x)
            out2 <- out[hits(xx, out[, "text"]), ]
            out2[, "word"] <- rep(xx, nrow(out2))
            out2
        })

        ## bind all the hits together
        do.call(rbind, out3) 
    })

    if (is.null(ylab)) {
        ylab <- simpleCap(gsub("&", " & ", G))
    }

    ## Bind it together
    dat2 <- do.call(rbind, rmout)
    dat2[, "word"] <- factor(paste0(" ", wrap, dat2[, "word"], wrap), 
       levels = paste0(" ", wrap, gsub(char2space, " ", match.terms), wrap))

    ## used for color fill when total.color != NULL
    dat2[, "summary"] <- rep("sub", nrow(dat2))
    
    ## Add totals if total.color != NULL
    if (!is.null(total.color) && GV) {
        dat2b <- dat2
        lvls <- c("all", levels(dat2b[, "grouping"]))
        dat2b[, "grouping"] <- rep("all", nrow(dat2))
        dat2b[, "summary"] <- rep("all", nrow(dat2))
        dat2 <- rbind(dat2, dat2b)
        dat2[, "grouping"] <- factor(dat2[, "grouping"], levels=lvls)    
        cols <- c(total.color, color) 
    } else {
        cols <- color
    }

    if(!is.null(match.list)) {

        word2 <- rep(NA, nrow(dat2))
        for (i in seq_along(match.list)) {
            word2[dat2[["text"]] %in% match.list[[i]]] <- paste(" ", names(match.list)[i])
        }
        dat2[["word"]] <- factor(word2, levels=paste(" ", names(match.list)))
    }

    ## remove NA values
    dat2 <- dat2[!is.na(dat2[["word"]]), ]

    the_plot <- ggplot(data = dat2, aes(x = word.num, y = grouping)) + 
        geom_point(aes(color = summary), 
            shape = symbol, size = size) + 
        theme_bw() + 
        theme(panel.background = element_rect(fill = bg.color), 
            panel.grid.minor.x = element_blank(), 
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_line(color = horiz.color),
            strip.text.y = element_text(angle=0, hjust = 0), 
            strip.background = element_blank()) +
        ylab(ylab) + xlab(xlab) + ggtitle(title) + 
        scale_colour_manual(values = cols, guide=FALSE)

    if(is.null(rm.vars)) {
        the_plot <- the_plot + facet_grid(word~., scales=scales, space=space)
    } else {
        the_plot <- the_plot + facet_grid(word~time, scales=scales, space=space)
    }
    if (is.null(grouping.var)){
        the_plot <- the_plot + 
            theme(axis.ticks.y = element_blank(), 
                axis.text.y = element_blank())
    }

    if (plot) {
        print(the_plot)
    }
    attributes(the_plot)[["qdap_data"]] <- dat2
    invisible(the_plot)
}

## function to search for matches
hits <- function(a, b) {
    which(grepl(a, b))
}

## Helper function to capitalize
simpleCap <- function(x) { 
    x <- gsub("(\\w)(\\w*)","\\U\\1\\L\\2", x, perl=T)
    mgsub(c("And", "Of"), c("and", "of"), x)
}
