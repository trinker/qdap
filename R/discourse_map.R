#' Discourse Mapping
#' 
#' View the flow of discourse from social actors.
#' 
#' @param text.var The text variable or a  \code{"word_stats"} object (i.e., the 
#' output of a \code{word_stats} function).       
#' @param grouping.var The grouping variables.  Also takes a single grouping 
#' variable or a list of 1 or more grouping variables.  
#' @param edge.constant A constant to multiple the edges by.  Defaults (if 
#' \code{missing}) to 2.5 times the number of social actors.
#' @param \ldots ignored
#' @param condense logical.  If \code{TRUE} \code{sentCombine} is used to 
#' condense text by grouping variable.
#' @param sep The separator character to use between grouping variables.
#' @return Returns a list:
#' \item{raw}{The dataframe with to and from columns (the edges) + word counts}
#' \item{edge_word_count}{A dataframe of edges and word counts + proportional 
#' word count}
#' \item{vertex_word_count}{A dataframe of vertices and word counts + 
#' proportional word count}
#' \item{plot}{An \pkg{igraph} object}
#' @export
#' @importFrom qdapTools %l% matrix2df list2df list_df2df
#' @details For an example of the video generated from the \code{Animate} 
#' output of \code{discourse_map} see: 
#' https://www.youtube.com/watch?v=7LcqFZODXNo&feature=youtu.be.  An HTML
#' output can be viewed: 
#' http://trinker.github.io/qdap_examples/animation_dialogue/.
#' @import igraph
#' @examples
#' \dontrun{
#' discourse_map(DATA$state, list(DATA$person, DATA$sex))
#' x <- with(mraja1, discourse_map(dialogue, person))
#' x
#' lview(x)
#' library(igraph)
#' plot(visual(x), edge.curved=FALSE)
#' 
#' ## Quickly add/remove a title
#' Title(x) <- "Act 1"
#' x
#' Title(x) <- NULL
#' x
#' 
#' ## Augmenting the plot
#' library(qdapTools)
#' mygraph <- visual(x)
#' 
#' plot(mygraph, edge.curved=TRUE)
#' 
#' V(mygraph)$sex <- V(mygraph)$name %lc% raj.demographics[, 1:2]
#' V(mygraph)$color <- ifelse(V(mygraph)$sex=="f", "pink", "lightblue")
#' 
#' plot(mygraph, edge.curved=TRUE)
#' 
#' V(mygraph)$family <- V(mygraph)$name %l+% raj.demographics[, c(1, 3)]
#' cols <- qcv(blue, red, brown, darkgreen, grey10)
#' V(mygraph)$label.color <- lookup(V(mygraph)$family, 
#'     unique(V(mygraph)$family), cols)
#' 
#' plot(mygraph, edge.curved=TRUE)
#' 
#' ## Community detection
#' x <- with(mraja1, discourse_map(dialogue, person))
#' wc <- walktrap.community(visual(x))
#' colors <- grDevices::rainbow(max(membership(wc)))
#' plot(x, vertex.color=colors[membership(wc)])
#' 
#' ## Repeated Measures (BASIC EXAMPLE)
#' ##------------------------------
#' 
#' ## First merge data and map to discourse per act 
#' ## to separate networks
#' 
#' dat <- key_merge(raj, raj.demographics)
#' list_dat <- split(dat, dat$act)
#' plot_dat <- lapply(list_dat, function(x) with(x, discourse_map(dialogue, person)))
#' 
#' opar <- par()$mar
#' par(mfrow=c(3, 2), mar=c(0, 0, 3, 0))
#' 
#' lapply(seq_along(plot_dat), function(i){
#'     plot(plot_dat[[i]])
#'     graphics::mtext(paste("Act", names(plot_dat)[i]), side=3)
#' })
#' 
#' 
#' ## Repeated Measures (EXTENDED EXAMPLE)
#' ##------------------------------
#' fam_key <- data.frame(fam=unique(raj.demographics$fam.aff),
#'     cols=qcv(blue, grey10, red, orange), 
#'     stringsAsFactors = FALSE)
#' 
#' par(mfrow=c(3, 2), mar=c(0, 1, 3, 1))
#' lapply(seq_along(plot_dat), function(i){
#' 
#'     THE_PLOT <- visual(plot_dat[[i]])
#' 
#'     V(THE_PLOT)$sex <- V(THE_PLOT)$name %l% raj.demographics[, 1:2]
#'     V(THE_PLOT)$color <- ifelse(V(THE_PLOT)$sex=="f", "pink", "lightblue")
#'     V(THE_PLOT)$family <- V(THE_PLOT)$name %lc+% raj.demographics[, c(1, 3)]
#'     V(THE_PLOT)$label.color <- lookup(V(THE_PLOT)$family, fam_key)
#' 
#'     plot(THE_PLOT, edge.curved=TRUE)
#'     graphics::mtext(paste("Act", names(plot_dat)[i]), side=3)
#' })
#' frame()
#' bords <- rep("black", 7)
#' bords[3] <- "white"
#' legend(.29, .95, c("Female", "Male", NA, as.character(fam_key[, 1])), 
#'     fill=c("pink", "lightblue", NA, fam_key[, 2]), border=bords, cex=1.5) 
#' 
#' ## Reset graphics margins
#' par(mar=opar)
#' 
#' ## ANIMATION
#' #===========
#' test <- discourse_map(DATA$state, list(DATA$person))
#' 
#' ## Very quick, hard to see
#' Animate(test)
#' 
#' pdf("test.pdf")
#'     par(mar=c(0, 0, 1, 0))
#'     Animate(test, title="Test Plot")
#' dev.off()
#' 
#' ## Animate it
#' ##-----------
#' library(animation)
#' library(igraph)
#' 
#' loc <- folder(animation_dialogue)
#' ans <- Animate(test)
#' 
#' ## Set up the plotting function
#' oopt <- animation::ani.options(interval = 0.1)
#' 
#' FUN <- function() {
#'     lapply(seq_along(ans), function(i) {
#'         par(mar=c(0, 0, 1, 0))
#'         set.seed(10)
#'         plot.igraph(ans[[i]], edge.curved=TRUE, layout=layout.circle)
#'         graphics::mtext("Discourse Map", side=3)
#'         animation::ani.pause()
#'     })
#' }
#' 
#' ## Detect OS
#' type <- if(.Platform$OS.type == "windows") shell else system
#' saveGIF(FUN(), interval = 0.1, outdir = loc, cmd.fun = type)
#' 
#' saveVideo(FUN(), video.name = "discourse_map.avi", interval = 0.1, outdir = loc)
#' 
#' saveLatex(FUN(), autoplay = TRUE, loop = FALSE, latex.filename = "tester.tex", 
#'     caption = "animated dialogue", outdir = loc, ani.type = "pdf", 
#'     ani.dev = "pdf", ani.width = 5, ani.height = 5.5, interval = 0.1)
#' 
#' saveHTML(FUN(), autoplay = FALSE, loop = TRUE, verbose = FALSE, 
#'     outdir = file.path(loc, "new"), single.opts = 
#'     "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0")
#'     
#'     
#' ## More Elaborate Layout
#' test2 <- with(mraja1, discourse_map(dialogue, person))
#' 
#' loc2 <- folder(animation_dialogue2)
#' ans2 <- Animate(test2)
#' ## Set up the plotting function
#' oopt <- animation::ani.options(interval = 0.1)
#' 
#' FUN3 <- function() {
#'     lapply(seq_along(ans2), function(i) {
#'         par(mar=c(0, 0, 1, 0))
#'         set.seed(10)
#'         plot.igraph(ans2[[i]], edge.curved=TRUE, layout=layout.auto)
#'         graphics::mtext("Discourse Map\nRomeo and Juliet: Act 1", side=3)
#'         animation::ani.pause()
#'     })
#' }
#' 
#' saveHTML(FUN3(), autoplay = FALSE, loop = FALSE, verbose = FALSE,
#'     outdir = file.path(loc2, "new"), single.opts =
#'     "'controls': ['first', 'play', 'loop', 'speed'], 'delayMin': 0")
#'     
#' saveVideo(FUN3(), video.name = "discourse_map.avi", interval = 0.2, 
#'     outdir = loc2)    
#' }
discourse_map <- function(text.var, grouping.var, edge.constant, sep = "_", 
    condense = TRUE, ...) {

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

    if (is.list(grouping.var) & length(grouping.var)>1) {
        grouping <- paste2(grouping.var, sep = sep, ...)
    } else {
        grouping <- unlist(grouping.var)
    } 

    qsep <- "|-|qdap|-|"

    if (missing(edge.constant)) {
        edge.constant <- length(unique(grouping)) * 2.5
    }

    ## check if any grouping vars repeat and condense
    if(condense) {
        if (sum(rle(as.character(grouping.var))[["lengths"]] > 1) > 0){
            dat <- sentCombine(text.var, grouping.var)
            grouping <- dat[, 1]
            text.var <- dat[, 2]
        }
    }

    DF <- map_df1(grouping, text.var) 

    DF2 <- map_df2(DF, qsep)
    DF3 <- map_df3(DF)
    g <- map_graph_qdap(DF2, edge.constant)

    V(g)$wc <- V(g)$name %l% DF3[, -3]
    V(g)$prop_wc<- V(g)$name %l% DF3[, -2]
   
    o <- list(raw = DF, edge_word_count=DF2, 
        vertex_word_count=DF3, plot = g)
    class(o) <- "discourse_map"
    o
}


map_df1 <- function(grouping, text.var){
    DF <- data.frame(from=grouping, wc=wc(text.var), 
        check.names = FALSE, stringsAsFactors = FALSE)
    DF[, "from"] <- factor(DF[, "from"])
    DF[, "to"] <- factor(c(as.character(DF[-1, "from"]), "End"))
    DF <- DF[, c(1, 3, 2)] 
    DF
}

map_df2 <- function(DF, qsep){
    DF2 <- colpaste2df(DF, 1:2, keep.orig=FALSE, sep=qsep, name.sep ="|")
    DF2 <- colsplit2df(list2df(lapply(split(DF2[, "wc"], 
        DF2[, "from|to"]) , sum), "wc", "from&to")[, 2:1], sep=qsep)
    DF2[, "prop_wc"] <- DF2["wc"]/sum(DF2[, "wc"])
   DF2
}


map_df3 <- function(DF){
    DF3 <- matrix2df(do.call(rbind, lapply(split(DF[, "wc"], 
        DF[, "from"]), sum)), "from")
    names(DF3)[2] <- "wc"
    DF3[, "prop_wc"] <- DF3["wc"]/sum(DF3[, "wc"])
    DF3
}


map_graph_qdap <- function(DF2, edgeconstant){
    g <- graph.data.frame(DF2, directed=TRUE)
    V(g)$size <- 10 
    E(g)$width <- edgeconstant*DF2[, "prop_wc"]
    g
}

#' Prints a discourse_map Object
#' 
#' Prints a discourse_map object.
#' 
#' @param x The discourse_map object.
#' @param edge.curved logical.  If \code{TRUE} edges are plotted with curves.
#' @param title The title of the plot.
#' @param \ldots Other Arguments passed to \code{\link[igraph]{plot.igraph}}.
#' @import igraph
#' @method print discourse_map
#' @export
print.discourse_map <- function(x, edge.curved = TRUE, title = NULL, ...) {
    plot.igraph(x[["plot"]], edge.curved = edge.curved, ...)
    if (!is.null(title)) {
        graphics::mtext(title, side=3)
    } else { 
        if (!is.null(attributes(x)[["title"]])){
            graphics::mtext(Title(x), side=3)
        }
    }
}


#'Discourse Map
#' 
#' \code{visual.discourse_map} - View visual from \code{\link[qdap]{discourse_map}}.
#' 
#' discourse_map Method for visual
#' @param x The discourse_map object.
#' @param \ldots ignored
#' @export
#' @method visual discourse_map
visual.discourse_map <- function(x, ...) {
    x[["plot"]]
}


#' Plots a discourse_map Object
#' 
#' Plots a discourse_map object.
#' 
#' @param x The discourse_map object.
#' @param \ldots Other arguments passed to \code{print.discourse_map}.
#' @method plot discourse_map
#' @export
plot.discourse_map <- function(x, ...){ 

    print(x, ...)

}

#' Plots an animated_discourse_map  Object
#' 
#' Plots an animated_discourse_map  object.
#' 
#' @param x The animated_discourse_map  object.
#' @param \ldots Other arguments passed to \code{print.animated_discourse_map }.
#' @method plot animated_discourse_map 
#' @export
plot.animated_discourse_map  <- function(x, ...){ 

    print(x, ...)

}


animated_discourse_map <- function(DF, edge.constant, sep = "_", 
    current.color = "red", previous.color = "grey50", 
    wc.time = TRUE, time.constant = 2, title = NULL, ...) {

    qsep <- "|-|qdap|-|"
    nms <- as.character(unique(unlist(DF[, 1:2])))
    if (missing(edge.constant)) {
        edge.constant <- length(nms) * 2.5
    }

    g <- graph.data.frame(map_df2b(DF, qsep))
    E(g)$width <- 0
    E(g)$color <- NA

    igraph_weights <- stats::setNames(lapply(1:nrow(DF), function(i) {
        DF2 <- map_df2b(DF[1:i, ], qsep)
        DF2[, "color"] <- previous.color
        DF2[nrow(DF2), "color"] <- current.color
        DF2
    }), paste0("Turn_", pad(1:nrow(DF))))

    igraph_objs <- stats::setNames(lapply(seq_along(igraph_weights), 
        function(i, grp =g, len=length(nms), sep=qsep){

        if (i %in% 1:5) {
            edge.constant <- edge.constant/(len/i)
        }

        weight <- igraph_weights[[i]][, c("from", "to", "prop_wc"), drop=FALSE]
        weight[, "prop_wc"] <- edge.constant*weight[, "prop_wc"]
        cols <- igraph_weights[[i]][, c("from", "to", "color"), drop=FALSE]
        wkey <- colpaste2df(weight, 1:2, sep = sep, keep.orig=FALSE)[, 2:1]
        el <- ends(grp, E(grp), names=TRUE)
        ekey <- paste(sep=sep, el[,1], el[,2])
        ckey <- colpaste2df(cols, 1:2, sep = sep, keep.orig=FALSE)[, 2:1]

        E(grp)$width <- NAer(ekey %l% wkey)
        E(grp)$color <- ekey %l% ckey
        grp
    }), paste0("Turn_", pad(1:nrow(DF))))

    timings <- round(exp(DF[, "wc"]/(max(DF[, "wc"])/time.constant)))
    if(wc.time) {
        igraph_objs <- rep(igraph_objs, timings)
    }

    ## starts with a blank object
    igraph_objs <- rep(igraph_objs, c(2, rep(1, length(igraph_objs) - 1)))
    len <- nchar(char2end(names(igraph_objs)[1], "_"))
    names(igraph_objs)[1] <- sprintf("turn_%s", paste(rep(0, len), collapse=""))
    uncol <- E(igraph_objs[[1]])$color
    E(igraph_objs[[1]])$color[!is.na(uncol)] <- NA

    class(igraph_objs) <- "animated_discourse_map"
    attributes(igraph_objs)[["title"]] <- title
    attributes(igraph_objs)[["timings"]] <- timings
    igraph_objs
}

#' Prints an animated_discourse_map  Object
#' 
#' Prints an animated_discourse_map  object.
#' 
#' @param x The animated_discourse_map  object.
#' @param title The title of the plot.
#' @param seed The seed to use in plotting the graph.
#' @param layout \pkg{igraph} \code{layout} to use.
#' @param pause The length of time to pause between plots.
#' @param \ldots Other Arguments passed to \code{\link[igraph]{plot.igraph}}.
#' @import igraph
#' @method print animated_discourse_map 
#' @export
print.animated_discourse_map <- function(x, title = NULL, 
    seed = sample(1:10000, 1), layout=layout.auto, pause = 0, ...){
    
    if (is.null(title)) {
        title <- attributes(x)[["title"]]
    }

    invisible(lapply(x, function(y) {
        set.seed(seed)        
        plot.igraph(y, edge.curved=TRUE, layout=layout)
        if (!is.null(title)) {
            graphics::mtext(title, side=3)
        }
        if (pause > 0) Sys.sleep(pause)
    }))  
   
}

map_df2b <- function(DF, qsep){
    DF2 <- colpaste2df(DF, 1:2, keep.orig=FALSE, sep=qsep, name.sep ="|")
    DF2[, "id"] <- seq_len(nrow(DF2))
    DF2 <- colsplit2df(list_df2df(lapply(split(DF2[, c("wc", "id")], 
        DF2[, "from|to"]), function(x) {
            data.frame(wc=sum(x[, 1]), id = max(x[, 2]))
        }), "from&to"), sep=qsep)
    DF2[, "prop_wc"] <- DF2["wc"]/sum(DF2[, "wc"])
    DF2[order(DF2[, "id"]), ]
}


#' Discourse Map
#' 
#' \code{Animate.discourse_map} - Animate a discourse 
#' \code{\link[qdap]{discourse_map}}.
#' 
#' discourse_map Method for Animate
#' @param x The discourse_map object.
#' @param edge.constant A constant to multiple edge width by.
#' @param sep The separator character to use between grouping variables.
#' @param current.color The color to make the vector edge as it moves.
#' @param previous.color The color to make the already plotted edges.
#' @param wc.time logical.  If \code{TRUE} weights duration of frame by word 
#' count.
#' @param time.constant A constant to divide the maximum word count by.  Time
#' is calculated by `round(exp(WORD COUNT/(max(WORD COUNT)/time.constant)))`.  
#' Therefore a larger constant will make the difference between the large and 
#' small word counts greater.
#' @param title The title to apply to the animated image(s).
#' @param \ldots ignored
#' @note The width of edges is based on words counts on that edge until that 
#' moment divided by total number of words used until that moment.  Thicker 
#' edges tend to thin as time passes.  The actual duration the current edge 
#' stays as the \code{current.color} is based on word counts for that particular 
#' flow of dialogue divided by total dialogue (words) used.
#' @import igraph
#' @export
#' @method Animate discourse_map
Animate.discourse_map <- function(x, edge.constant, sep = "_", 
    current.color = "red", previous.color = "grey50", 
    wc.time = TRUE, time.constant = 2, title = NULL, ...) {

    animated_discourse_map(x[["raw"]], edge.constant = edge.constant, 
        sep = sep, current.color = current.color, 
        previous.color = previous.color, wc.time = wc.time, 
        title = title, ...)

}
