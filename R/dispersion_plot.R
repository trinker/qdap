#' Lexical Dispersion Plot
#' 
#' Generate a lexical dispersion plot of terms.
#' 
#' @param text.var The text variable.
#' @param match.terms  A vector of quoted terms.
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
#' @param symbol The word symbol.  Defualt is \code{"|"}.
#' @param title Title of the plot
#' @param rev.factor logical.  If \code{TRUE} reverses the plot order of the 
#' factors.
#' @param wrap a character to wrap around the words (enables the reader to 
#' visualize spaces).  Defualt is \code{"'"}, use \code{""} to remove.
#' @param xlab The x label.
#' @param ylab The y label.
#' @param size The size of the plotting symbol.
#' @param plot logical.  If \code{TRUE} the plot will automatically plot.  
#' The user may wish to set to \code{FALSE} for use in knitr, sweave, etc.
#' to add additional plot layers.
#' @param char2space A vector of characters to be turned into spaces.  
#' @param scales Should scales be fixed (\code{"fixed"}, the default), free 
#' (\code{"free"}), or free in one dimension (\code{"free_x"}, \code{"free_y"})
#' @param space If \code{"fixed"}, the default, all panels have the same size. 
#' If \code{"free_y"} their height will be proportional to the length of the y 
#' scale; if \code{"free_x"} their width will be proportional to the length of 
#' the x scale; or if \code{"free"} both height and width will vary. 
#' @return Plots a dispersion plot and invisibly returns the ggplot2 object.
#' @keywords dispersion
#' @export
#' @importFrom ggplot2 ggplot aes geom_point scale_x_continuous element_rect element_line ggtitle theme theme_bw element_blank facet_grid ylab xlab
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
#' ## Change color scheme
#' with(rajSPLIT, dispersion_plot(dialogue, c("love", "night"), 
#'     bg.color = "black", grouping.var = list(fam.aff, sex), 
#'     color = "yellow", horiz.color="grey20"))
#'     
#' ## Use word list
#' wrds <- word_list(pres_debates2012$dialogue, stopwords = Top200Words)
#' wrds2 <- spaste(wrds[["rfswl"]][["all"]][, "WORD"])
#' wrds2 <- c(" governor~~romney ", wrds2[-c(3, 12)])
#' with(pres_debates2012 , dispersion_plot(dialogue, wrds2, rm.vars = time))
#' }
dispersion_plot <- function(text.var, match.terms, grouping.var = NULL,  
    rm.vars =NULL, color = "blue", bg.color = "grey90", horiz.color = "grey85", 
    symbol = "|", title = "Lexical Dispersion Plot", rev.factor = TRUE, 
    wrap = "'", xlab = "Dialogue (Words)", ylab = NULL, size = 4, 
    plot = TRUE, char2space = "~~", scales="free", space="free") {

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

    ## split DF into a list by rm.vars
    LDF <- split(DF, DF[, "time"])

    ## split it out by rep. measures var.
    rmout <- lapply(LDF, function(x) {

        out <- cm_df.temp(x, text.var = "text.var", strip = TRUE, 
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

    dat2 <- do.call(rbind, rmout)
    dat2[, "word"] <- factor(paste0(" ", wrap, dat2[, "word"], wrap), 
       levels = paste0(" ", wrap, gsub(char2space, " ", match.terms), wrap))

    the_plot <- ggplot(data = dat2, aes(x = word.num, y = grouping)) + 
        geom_point(color = color, aes(position="dodge"), 
            shape = symbol, size = size) + 
        theme_bw() + 
        theme(panel.background = element_rect(fill = bg.color), 
            panel.grid.minor.x = element_blank(), 
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_line(color = horiz.color),
            strip.text.y = element_text(angle=0, hjust = 0), 
            strip.background = element_blank()) +
        scale_x_continuous(expand = c(0, 0)) + 
        ylab(ylab) + xlab(xlab) + ggtitle(title)

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


