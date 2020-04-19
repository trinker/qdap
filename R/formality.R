#' Formality Score
#' 
#' Transcript apply formality score by grouping variable(s) and optionally plot 
#' the breakdown of the model.
#' 
#' @param text.var The text variable (or an object from \code{\link[qdap]{pos}},
#' \code{\link[qdap]{pos_by}} or \code{\link[qdap]{formality}}.  Passing the 
#' later three object will greatly reduce run time.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param order.by.formality logical.  If \code{TRUE} orders the results by 
#' formality score.
#' @param digits The number of digits displayed.
#' @param \ldots Other arguments passed to \code{\link[qdap]{pos_by}}.
#' @section Warning: Heylighen & Dewaele (2002) state, "At present, a sample would 
#' probably need to contain a few hundred words for the measure to be minimally 
#' reliable. For single sentences, the F-value should only be computed for 
#' purposes of illustration" (p. 24).
#' @details Heylighen & Dewaele(2002)'s formality score is calculated as:
#' \deqn{F = 50(\frac{n_{f}-n_{c}}{N} + 1)}{F = 50(n_f-n_c/N + 1)}
#'
#' Where:
#' \deqn{f = \left \{noun, \;adjective, \;preposition, \;article\right \}}{f = {noun,adjective, preposition, article}}
#' \deqn{c = \left \{pronoun, \;verb, \;adverb, \;interjection\right \}}{c = {pronoun, verb, adverb, interjection}}
#' \deqn{N = \sum{(f \;+ \;c \;+ \;conjunctions)}}{N = \sum(f + c + conjunctions)}
#' @return A list containing at the following components: 
#' \item{text}{The text variable} 
#' \item{POStagged}{Raw part of speech for every word of the text variable} 
#' \item{POSprop}{Part of speech proportion for every word of the text variable} 
#' \item{POSfreq}{Part of speech count for every word of the text variable} 
#' \item{pos.by.freq}{The part of speech count for every word of the text 
#' variable by grouping variable(s)} 
#' \item{pos.by.prop}{The part of speech proportion for every word of the text 
#' variable by grouping variable(s)} 
#' \item{form.freq.by}{The nine broad part of speech categories count for every 
#' word of the text variable by grouping variable(s)} 
#' \item{form.prop.by}{The nine broad part of speech categories proportion for 
#' every word of the text variable by grouping variable(s)} 
#' \item{formality}{Formality scores by grouping variable(s)} 
#' \item{pos.reshaped}{An expanded formality scores output (grouping, 
#' word.count, pos & form.class) by word}
#' @references Heylighen, F., & Dewaele, J.M. (2002). Variation in the 
#' contextuality of language: An empirical measure. Context in Context, Special 
#' issue of Foundations of Science, 7 (3), 293-340.
#' @export
#' @rdname formality
#' @examples
#' \dontrun{
#' with(DATA, formality(state, person))
#' (x1 <- with(DATA, formality(state, list(sex, adult))))
#' plot(x1)
#' plot(x1, short.names = FALSE)
#' 
#' scores(x1)
#' counts(x1)
#' proportions(x1)
#' preprocessed(x1)
#' 
#' plot(scores(x1))
#' plot(counts(x1))
#' plot(proportions(x1), high="darkgreen")
#' plot(preprocessed(x1))
#' 
#' data(rajPOS) #A data set consisting of a pos list object
#' x2 <- with(raj, formality(rajPOS, act))
#' plot(x2)
#' cumulative(x2)
#' x3 <- with(raj, formality(rajPOS, person))
#' plot(x3, bar.colors="Dark2")
#' plot(x3, bar.colors=c("Dark2", "Set1"))
#' x4 <- with(raj, formality(rajPOS, list(person, act)))
#' plot(x4, bar.colors=c("Dark2", "Set1"))
#' 
#' rajDEM <- key_merge(raj, raj.demographics) #merge demographics with transcript.
#' x5 <- with(rajDEM, formality(rajPOS, sex))
#' plot(x5, bar.colors="RdBu")
#' x6 <- with(rajDEM, formality(rajPOS, list(fam.aff, sex)))
#' plot(x6, bar.colors="RdBu")
#' x7 <- with(rajDEM, formality(rajPOS, list(died, fam.aff)))
#' plot(x7, bar.colors="RdBu",  point.cex=2, point.pch = 3)
#' x8 <- with(rajDEM, formality(rajPOS, list(died, sex)))
#' plot(x8, bar.colors="RdBu",  point.cex=2, point.pch = "|")
#' 
#' names(x8)
#' colsplit2df(x8$formality)
#' 
#' #pass an object from pos or pos_by
#' ltruncdf(with(raj, formality(x8 , list(act, person))), 6, 4)
#' 
#' #=============#
#' ## ANIMATION ##
#' #=============#
#' ## EXAMPLE 1
#' form_ani <- formality(DATA.SPLIT$state, DATA.SPLIT$person)
#' forma <- Animate(form_ani, contextual="white", formal="blue", 
#'     current.color = "yellow", current.speaker.color="grey70")
#' 
#' bgb <- vertex_apply(forma, label.color="grey80", size=20, color="grey40")
#' bgb <- edge_apply(bgb, label.color="yellow")
#' 
#' print(bgb, bg="black", net.legend.color ="white", pause=1)
#' 
#' ## EXAMPLE 2
#' form_ani2 <- formality(raj.act.1POS, mraja1spl$person)
#' forma2 <- Animate(form_ani2, contextual="white", formal="blue",
#'     current.color = "yellow", current.speaker.color="grey70")
#' 
#' bgb2 <- vertex_apply(forma2, label.color="grey80", size=17, color="grey40")
#' bgb2 <- edge_apply(bgb2, label.color="yellow")
#' print(bgb2, bg="black", pause=.75, net.legend.color = "white")
#' 
#' ## EXAMPLE 3 (bar plot)
#' Animate(form_ani2, as.network=FALSE)
#' 
#' #=====================#
#' ## Complex Animation ##
#' #=====================#
#' library(animation)
#' library(grid)
#' library(gridBase)
#' library(qdap)
#' library(igraph)
#' library(plotrix)
#' 
#' form_ani2 <- formality(raj.act.1POS, mraja1spl$person)
#' 
#' ## Set up the network version
#' form_net <- Animate(form_ani2, contextual="white", formal="blue",
#'     current.color = "yellow", current.speaker.color="grey70")
#' bgb <- vertex_apply(form_net, label.color="grey80", size=17, color="grey40")
#' bgb <- edge_apply(bgb, label.color="yellow")
#' 
#' 
#' ## Set up the bar version
#' form_bar <- Animate(form_ani2, as.network=FALSE)
#' 
#' ## Generate a folder
#' loc <- folder(animation_formality)
#' 
#' ## Set up the plotting function
#' oopt <- animation::ani.options(interval = 0.1)
#' 
#' 
#' FUN <- function(follow=FALSE, theseq = seq_along(bgb)) {
#' 
#'     Title <- "Animated Formality: Romeo and Juliet Act 1"
#'     Legend <- c(.2, -1, 1.5, -.95)
#'     Legend.cex <- 1
#' 
#'     lapply(theseq, function(i) {
#'         if (follow) {
#'             png(file=sprintf("%s/images/Rplot%s.png", loc, i),
#'                 width=650, height=725)
#'         }
#'         ## Set up the layout
#'         layout(matrix(c(rep(1, 9), rep(2, 4)), 13, 1, byrow = TRUE))
#' 
#'         ## Plot 1
#'         par(mar=c(2, 0, 2, 0), bg="black")
#'         #par(mar=c(2, 0, 2, 0))
#'         set.seed(22)
#'         plot.igraph(bgb[[i]], edge.curved=TRUE)
#'         graphics::mtext(Title, side=3, col="white")
#'         color.legend(Legend[1], Legend[2], Legend[3], Legend[4],
#'               c("Contextual", "Formal"), attributes(bgb)[["legend"]],
#'               cex = Legend.cex, col="white")
#' 
#'         ## Plot2
#'         plot.new()
#'         vps <- baseViewports()
#' 
#'         uns <- unit(c(-1.3,.5,-.75,.25), "cm")
#'         p <- form_bar[[i]] +
#'             theme(plot.margin = uns,
#'                 text=element_text(color="white"),
#'                 legend.text=element_text(color="white"),
#'                 legend.background = element_rect(fill = "black"),
#'                 plot.background = element_rect(fill = "black",
#'                     color="black"))
#'         print(p,vp = vpStack(vps$figure,vps$plot))
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
#' saveHTML(FUN(, 1:20), autoplay = FALSE, loop = TRUE, verbose = FALSE,
#'     ani.height = 1000, ani.width=650,
#'     outdir = loc, single.opts =
#'     "'controls': ['first', 'play', 'loop', 'speed'], 'delayMin': 0")
#' 
#' FUN(TRUE)
#' 
#' #==================#
#' ## Static Network ##
#' #==================#
#' (formdat <- with(sentSplit(DATA, 4), formality(state, person)))
#' m <- Network(formdat)
#' m
#' print(m, bg="grey97", vertex.color="grey75")
#' 
#' print(m, title="Formality Discourse Map", title.color="white", bg="black",
#'     legend.text.color="white", vertex.label.color = "grey70", 
#'     edge.label.color="yellow")
#'     
#' ## or use themes:
#' dev.off()
#' m + qtheme()
#' m + theme_nightheat
#' dev.off()
#' m + theme_nightheat(title="Formality Discourse Map", 
#'     vertex.label.color = "grey50")
#'     
#' #===============================#
#' ## Formality Over Time Example ##
#' #===============================#
#' formpres <- lapply(with( pres_debates2012, split(dialogue, time)), function(x) {
#'     formality(x)
#' })
#' formplots <- lapply(seq_along(formpres), function(i) {
#'     m <- plot(cumulative(formpres[[i]]))
#'     if (i != 2) m <- m + ylab("")
#'     if (i != 3) m <- m + xlab(NULL)
#'     m + ggtitle(paste("Debate", i))
#' })
#' 
#' library(grid)
#' library(gridExtra)
#' do.call(grid.arrange, formplots)
#' }
formality <- function(text.var, grouping.var = NULL,                    
    order.by.formality = TRUE, digits = 2, ...){  
  
    if(is.null(grouping.var)) {
        gv <- TRUE 
        G <- "all"
    } else {
        gv <- FALSE
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
        grouping.var <- rep("all", length(text.var))                                                 
    } else {                                                                         
        if(is.list(grouping.var) & length(grouping.var)>1) {                             
            grouping.var <- apply(data.frame(grouping.var), 1, function(x){                             
                     if (any(is.na(x))){                                             
                         NA                                                          
                     }else{                                                          
                         paste(x, collapse = ".")                                    
                     }                                                               
                 }                                                                   
             )                                                                       
        } else {                                                                     
            grouping.var <-  unlist(grouping.var)                                                     
        }                                                                            
    }                                                                          
    if (!gv) {                                                                       
        pos.list <- pos_by(text.var = text.var,                                      
            grouping.var = grouping.var, digits = digits, ...)                            
    } else {                                                                         
        pos.list <- suppressWarnings(pos_by(text.var = text.var,                     
            grouping.var = NULL, digits = digits, ...))                                   
    } 
                                                                             
    text.var <- pos.list$text        
    WOR <- pos.list[["POStagged"]][["word.count"]]
##   WOR <- word_count(text.var)                                                      
    X <- pos.list[["pos.by.freq"]]                                                   
    nameX <- rownames(X)                                                             
    X <- data.frame(X, stringsAsFactors = FALSE)                                                               
    xn <- nrow(X)                                                                    
    X$JI <- rep(0, xn)                                                               
    X$JK <- rep(0, xn)                                                               
                                                                             
    if (!gv){                                                                        
        stv <- split(text.var, grouping.var)                                         
        stv <- stv[sapply(stv, function(x) !identical(x, character(0)))]             
        articles <- unlist(lapply(stv, function(x){                                  
                sum(article(x))                                                      
            }                                                                        
        ))                                                                           
    } else {                                                                         
        articles <- sum(article(text.var))                                           
    }                                                                                
    if (!is.null(X$DT)) {                                                            
        PD <- X$DT-articles                                                          
    }  else {
        PD <- rep(0, nrow(X))
    }                                                                                
    DF1 <- DF2 <- data.frame(                                                        
        noun = rowSums(X[, names(X) %in% c("NN", "NNS", "NNP", "NNPS",               
            "POS", "JI", "JK")]),                                                    
        adj = rowSums(cbind(X[, names(X) %in% c("CD", "JJ", "JJR", "JJS",            
            "JI", "JK")], PD)),                                                      
        prep = rowSums(X[, names(X) %in% c("IN", "RP", "TO", "JI", "JK")]),          
        articles = articles,                                                         
        pronoun = rowSums(X[, names(X) %in% c("PRP", "PRP$", "PRP.", "WDT",          
            "WP", "WP$", "WP.", "JI", "JK", "EX")]),                                 
        verb = rowSums(X[, names(X) %in% c("MD", "VB", "VBD", "VBG",                 
            "VBN", "VBP", "VBZ", "JI", "JK")]),                                      
        adverb = rowSums(X[, names(X) %in% c("RB", "RBR", "RBS", "WRB",              
            "JI", "JK")]),                                                           
        interj = rowSums(X[, names(X) %in% c("UH", "JI", "JK")]), stringsAsFactors = FALSE)                    
    DF1RS <- rowSums(DF1)       
    if (!gv) {                                                                       
        WOR <- sapply(split(WOR, grouping.var), sum, na.rm = TRUE)                   
    } else {                                                                         
        WOR <- sum(WOR, na.rm=TRUE)                                                  
    } 
    
    DF2$other <- DF1$other <- WOR - DF1RS    
                                             
    DF1 <- do.call(rbind, lapply(1:nrow(DF1), function(i) 100*(DF1[i, ]/WOR[i])))  
    FOR <- (rowSums(cbind(DF1[, "noun"], DF1[, "articles"], DF1[, "adj"], DF1[, "prep"])) -               
        rowSums(cbind(DF1[, "pronoun"], DF1[, "verb"], DF1[, "adverb"], DF1[, "interj"])) + 100)/2                                                                                     
    if(!gv) {                                                                        
        FOR <- data.frame(replace = X[, 1], word.count = WOR, formality = FOR, stringsAsFactors = FALSE)       
        colnames(FOR)[1] <- G                                                        
    } else {                                                                         
        FOR <- data.frame(replace = X[, 1], word.count = WOR,                        
            formality = FOR, stringsAsFactors = FALSE)                                                         
        colnames(FOR)[1] <- G                                                        
    }                                                                                
                
    if (!gv & order.by.formality) {                                                   
        FOR <- FOR[order(-FOR$formality), ]                                          
        rownames(FOR) <- NULL                                                        
    }                                                                                
    if (!gv) {                                                                       
        prop.by <- data.frame(var=names(WOR),                                        
            word.count = WOR,                                                        
            apply(DF1, 2, round, digits = digits), stringsAsFactors = FALSE)                                   
        freq.by <- data.frame(var=names(WOR),                                        
            word.count = WOR, DF2, stringsAsFactors = FALSE)                                                   
    } else {                                                                         
        prop.by <- data.frame(var="all",                                             
            word.count = sum(WOR, na.rm = TRUE), DF1, stringsAsFactors = FALSE)                                
        freq.by <- data.frame(var="all",                                             
            word.count = sum(WOR, na.rm = TRUE), DF2, stringsAsFactors = FALSE)                                
    }                                                                                
    colnames(prop.by)[1] <- colnames(freq.by)[1] <- colnames(FOR)[1]                 
    rownames(prop.by) <- rownames(freq.by) <- NULL                                   
    o <- unclass(pos.list)                                                           
    o$form.freq.by <- freq.by                                                        
    o$form.prop.by <- prop.by                                                        
    o$formality <- FOR                                                               
    dat <- stats::reshape(freq.by,                                                          
        direction="long",                                                            
        varying=list(c(3:11)),                                                       
        idvar= names(freq.by)[c(1:2)],                                                  
        timevar="pos",                                                               
        v.names=c("freq"),                                                           
        times =names(freq.by)[-c(1:2)])                                              
    colnames(dat)[1] <- "grouping"   
    ON <- sum(dat[, "pos"] == "other")
    dat[, "form.class"] <- c(rep(c("formal", "contectual"), 
        each = (nrow(dat) - ON)/2), rep("other", ON)) 

     
    dat <- dat[rep(seq_len(dim(dat)[1]), dat[, 4]), -4]                              
    dat[, "pos"] <- factor(dat[, "pos"], levels=unique(dat[, "pos"]))                
    dat[, "form.class"] <- factor(dat[, "form.class"],                               
        levels=unique(dat[, "form.class"]))                                          
    row.names(dat) <- NULL                                                           
    o$pos.reshaped <- dat  
    o$group <- G                                                                                                                                                                                                      
    class(o) <- "formality"  
    attributes(o)[["digits"]] <- digits
    return(o)                                                                        
}

## Helper function to find articles
article <- function(x) {                                                         
    if (identical(x, character(0))) {                                            
        return(0)                                                                
    } else {                                                                     
        WORDS <- rm_stopwords(x, stopwords = NULL,                                  
            unlist = FALSE, strip = TRUE)                                        
        sapply(WORDS, function(x) sum(x %in% c("the", "an", "a"),                
            na.rm = TRUE ))                                                      
    }                                                                            
}  


#' Plots a formality Object
#' 
#' Plots a formality object including the parts of speech used to 
#' calculate contextual/formal speech.
#' 
#' @param x The formality object.
#' @param point.pch The plotting symbol.
#' @param point.cex  The plotting symbol size.
#' @param point.colors A vector of colors (length of two) to plot word count and 
#' formality score.
#' @param bar.colors A palette of colors to supply to the bars in the 
#' visualization.  If two palettes are provided to the two bar plots 
#' respectively.
#' @param short.names logical.  If TRUE shortens the length of legend and label 
#' names for more compact plot width.
#' @param min.wrdcnt A minimum word count threshold that must be achieved to be 
#' considered in the results.  Default includes all subgroups.
#' @param order.by.formality logical.  If \code{TRUE} the group formality plot 
#' will be ordered by average formality score, otherwise alphabetical order is 
#' assumed.
#' @param plot logical.  If \code{TRUE} the plot will automatically plot.  
#' The user may wish to set to \code{FALSE} for use in knitr, sweave, etc.
#' to add additional plot layers.
#' @param \ldots ignored
#' @return Invisibly returns the \code{ggplot2} objects that form the larger 
#' plot.
#' @method plot formality
#' @import RColorBrewer
#' @importFrom gridExtra grid.arrange
#' @importFrom qdapTools lookup
#' @importFrom ggplot2 ggplot geom_bar coord_flip aes ylab xlab theme ggtitle scale_y_continuous scale_fill_brewer facet_grid scale_x_discrete scale_fill_discrete geom_point geom_text labs scale_size_continuous 
#' @export
plot.formality <- function(x, point.pch = 20, point.cex = .5,            
    point.colors = c("gray65", "red"), bar.colors = NULL, 
    short.names = TRUE, min.wrdcnt = NULL, order.by.formality = TRUE, 
    plot = TRUE, ...) {
    word.count <- NULL
    grouping <- form.class <- NULL
    dat <- x$pos.reshaped   
    FOR <- x$formality
    G <- x$group
    if (!is.null(min.wrdcnt)){                                                       
        dat <- dat[dat[, "word.count"] > min.wrdcnt, ,drop = TRUE]                   
        dat[, 1] <- factor(dat[, 1])                                                 
        FOR <- FOR[FOR[, "word.count"] > min.wrdcnt, ,drop = TRUE]                   
    }   
    if(short.names){
        dat[, "form.class"] <- lookup(dat[, "form.class"], 
            c("formal", "contectual", "other"),
            c("form", "cont", "other"))
    }       

    if (order.by.formality) {
        dat[, "grouping"] <- factor(dat[, "grouping"], levels=rev(FOR[, 1]))
        FOR[, 1] <- factor(FOR[, 1], levels=rev(FOR[, 1]))
    }
                                                                                                         
    YY <- ggplot(dat, aes(grouping,  fill=form.class)) +                         
        geom_bar(position='fill') +                                              
        coord_flip() +  labs(fill=NULL) +                                        
        ylab("proportion") + xlab(G)  +                                          
        theme(legend.position = 'bottom') +
        ggtitle("Percent Contextual-Formal") +
        scale_y_continuous(breaks = c(0, .25, .5, .75, 1),
            labels=c("0", ".25", ".5", ".75", "1"))  
        if (!is.null(bar.colors)) { 
 
            YY <- YY + suppressWarnings(scale_fill_brewer(palette = 
                utils::head(bar.colors, 1)))

    }        
    dat2 <- dat[dat[, "pos"] != "other", ] 
    dat2[, "pos"] <- factor(dat2[, "pos"])
    dat2[, "form.class"] <- factor(dat2[, "form.class"])
    if(short.names) {
        LAB <- c("noun", "adj", "prep", "art", "pro", "verb", 
            "adverb", "interj")
    } else {
        LAB <- c("noun", "adjective", "preposition",                         
            "articles", "pronoun", "verb", "adverb", "interjection")  
    }

    LAB2 <- LAB[substring(LAB, 1, 3) %in% substring(levels(dat2$pos), 1, 3)]
    XX <- ggplot(data=dat2, aes(grouping,  fill=pos)) +                           
        geom_bar(position='fill') + coord_flip() +                               
        facet_grid(~form.class, scales="free", margins = TRUE) +                 
        scale_x_discrete(drop=F) +  labs(fill=NULL) +   
        scale_y_continuous(breaks = c(0, .25, .5, .75, 1),
            labels=c("0", ".25", ".5", ".75", "1")) +
        ylab("proportion") + xlab(G)  +                                              
        theme(legend.position = 'bottom') +
        ggtitle("Percent Parts of Speech By Contextual-Formal")                                         
    if (!is.null(bar.colors)) {  

        XX <- XX + scale_fill_brewer(palette=utils::tail(bar.colors, 1),                     
            name = "", breaks=levels(dat2$pos), labels = LAB2)  

    } else {
             XX <- XX + scale_fill_discrete(name = "", 
                 breaks=levels(dat2$pos), labels = LAB2)
    }
    names(FOR)[1] <- "grouping"                                                  
    buffer <- diff(range(FOR$formality))*.05                                   
    ZZ <- ggplot(data=FOR, aes(grouping,  formality, size=word.count)) +         
        geom_point(colour=point.colors[1]) + coord_flip()+                       
        geom_text(aes(label = word.count), vjust = 1.2, size = 3,                
            position = "identity",colour = "grey30") +                           
        labs(size="word count") +                                                
        theme(legend.position = 'bottom') +      
        ggtitle("F Measure (Formality)") +
        scale_y_continuous(limits=c(min(FOR$formality)-buffer,                   
            max(FOR$formality) + buffer)) +                                      
        scale_size_continuous(range = c(1, 8)) + xlab(G)  +                      
    if (point.pch == "|") {                                                  
        geom_text(aes(label = "|"), colour=point.colors[2], 
            size=point.cex, position = "identity", hjust = .25, 
            vjust = .25)                 
    } else {                                                                 
        geom_point(colour=point.colors[2], shape=point.pch, 
            size=point.cex)  
    }   
    if (plot) {
        suppressWarnings(grid.arrange(YY, XX,                         
            ZZ, widths= grid::unit(c(.24, .47, .29), "native"), ncol=3))   
    }
    invisible(list(f1 = XX, f2 = YY, f3 = ZZ))
}


#' Prints a formality Object
#' 
#' Prints a formality  object.
#' 
#' @param x The formality object.
#' @param digits The number of digits to print.
#' @param \ldots ignored
#' @method print formality
#' @export
print.formality <-
function(x, digits, ...) {
    
    y <- x[["formality"]]

    if ("formality" %in% colnames(y)) {
        if (missing(digits)) {
             if(!is.null(attributes(x)[["digits"]])) {
                 digits <- attributes(x)[["digits"]]
             } else {
                 digits <- 2   
             }
        }
        y[, "formality"] <- round(y[, "formality"], digits = digits)
    }
    print(y)
}

#' Formality
#' 
#' View formality scores.
#' 
#' formality Method for scores
#' @param x The \code{\link[qdap]{formality}} object.
#' @param \ldots ignored
#' @export
#' @method scores formality
scores.formality <- function(x, ...) {

    out <- x[["formality"]]
    attributes(out) <- list(
            class = c("formality_scores", class(out)),
            type = "formality_scores",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


#' Formality
#' 
#' View formality counts.
#' 
#' formality Method for counts
#' @param x The \code{\link[qdap]{formality}} object.
#' @param \ldots ignored
#' @export
#' @method counts formality
counts.formality <- function(x, ...) {

    out <- x[["form.freq.by"]]
    attributes(out) <- list(
            class = c("table_count", class(out)),
            type = "formality_counts",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}

#' Formality
#' 
#' View \code{\link[qdap]{formality}} proportions.
#' 
#' formality Method for proportions
#' @param x The formality object.
#' @param \ldots ignored
#' @export
#' @method proportions formality
proportions.formality <- function(x, ...) {

    out <- x[["form.freq.by"]]
    out[, -c(1:2)] <- out[, -c(1:2)]/out[, 2]

    attributes(out) <- list(
            class = c("table_proportion", class(out)),
            type = "formality_proportions",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}

#' Formality
#' 
#' View formality preprocessed.
#' 
#' formality Method for preprocessed
#' @param x The \code{\link[qdap]{formality}} object.
#' @param \ldots ignored
#' @export
#' @method preprocessed formality
preprocessed.formality <- function(x, ...) {

    out <- x[["POStagged"]]
    attributes(out) <- list(
            class = c("pos_preprocessed", class(out)),
            type = "formality_preprocessed",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}

#' Prints a formality_scores object
#' 
#' Prints a formality_scores object
#' 
#' @param x The formality_scores object
#' @param \ldots ignored
#' @export
#' @method print formality_scores
print.formality_scores <-
function(x, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    class(x) <- "data.frame"
    print(x)
    options(width=WD)
}


#' Prints a pos_preprocessed object
#' 
#' Prints a pos_preprocessed object
#' 
#' @param x The pos_preprocessed object
#' @param \ldots ignored
#' @export
#' @method print pos_preprocessed
print.pos_preprocessed <-
function(x, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    class(x) <- "data.frame"
    print(x)
    options(width=WD)
}


#' Plots a formality_scores Object
#' 
#' Plots a formality_scores object.
#' 
#' @param x The formality_scores object.
#' @param \ldots ignored
#' @importFrom ggplot2 ggplot aes geom_point scale_y_continuous ggtitle theme labs geom_text xlab ylab scale_size_continuous
#' @method plot formality_scores
#' @export
plot.formality_scores <- function(x, ...){ 

    group <- formality <- word.count <- NULL

    point.colors <- c("gray65", "red")
    buffer <- diff(range(x$formality))*.05
    nms1 <- colnames(x)[1]
    colnames(x)[1] <- "group"

    x <- x[order(x["formality"]), ]
    x[, "group"] <- factor(x[, "group"], levels = x[, "group"])

    ggplot(data=x, aes(group, formality, size=word.count)) +         
        geom_point(colour=point.colors[1]) + coord_flip()+                       
        geom_text(aes(label = word.count), vjust = 1.2, size = 3,                
            position = "identity",colour = "grey30") +                           
        labs(size="word count") +                                                
        theme(legend.position = 'bottom') +      
        ggtitle("F Measure (Formality)") +
        scale_y_continuous(limits=c(min(x$formality)-buffer,                   
            max(x$formality) + buffer)) +  
        geom_point(color="red", size=.4) +
        ylab("Formality") + xlab(plot_namer(nms1)) +
        scale_size_continuous(name="Word Count")
                                   
}



Animate_formality_net <- function(x, contextual, formal, 
    edge.constant, wc.time = TRUE, time.constant = 2, title = NULL, digits = 3, 
    current.color = "black", missing.color="purple", current.speaker.color, 
    non.speaker.color = NA, ...){

    v <- cbind.data.frame(group=attributes(x)[["grouping.var"]], 
        x[["POSfreq"]], stringsAsFactors = FALSE)

    nas <- which(is.na(x[["text"]]))
    x[["text"]][nas] <- ""

    if(!identical(nas, integer(0))) {
        x[["POSfreq"]][nas,] <- 0
        x[["POSfreq"]][nas, 1] <- 1
    }

    
#    v <- v[!is.na(x[["text"]]), ]
#    x[["text"]] <- x[["text"]][!is.na(x[["text"]])]
    
    ## Count the articles per row
    articles <- unlist(lapply(x[["text"]], function(x){  
            if(identical(x, character(0)) ) return(0)                               
            sum(article(x))                                                      
    })) 

    if (!is.null(v$DT)) {                                                            
        PD <- v$DT-articles                                                          
    } else {
        PD <- rep(0, nrow(v))
    }    

    ## tally parts of speech for formality stat
    z <- data.frame(v[, 1:2, drop=FALSE],                                    
        noun = rowSums(v[, names(v) %in% c("NN", "NNS", "NNP", "NNPS",               
            "POS", "JI", "JK"), drop=FALSE]),                                                    
        adj = rowSums(cbind(v[, names(v) %in% c("CD", "JJ", "JJR", "JJS",            
            "JI", "JK"), drop=FALSE], PD)),                                                      
        prep = rowSums(v[, names(v) %in% c("IN", "RP", "TO", "JI", "JK"), 
            drop=FALSE]),          
        articles = articles,                                                         
        pronoun = rowSums(v[, names(v) %in% c("PRP", "PRP$", "PRP.", "WDT",          
            "WP", "WP$", "WP.", "JI", "JK", "EX"), drop=FALSE]),                                 
        verb = rowSums(v[, names(v) %in% c("MD", "VB", "VBD", "VBG",                 
            "VBN", "VBP", "VBZ", "JI", "JK"), drop=FALSE]),                                      
        adverb = rowSums(v[, names(v) %in% c("RB", "RBR", "RBS", "WRB",              
            "JI", "JK"), drop=FALSE]),                                                           
        interj = rowSums(v[, names(v) %in% c("UH", "JI", "JK"), drop=FALSE]))

    qsep <- "|-|qdap|-|"

    brks <- seq(0, 1, by=.001)
    max.color.breaks <- length(brks)

    y <- form_fun(z)

    y[is.na(y[, 3]), -c(1:2)] <- c(1, rep(0, 5))

    condlens <- rle(as.character(y[, 1]))
    temp <- rep(paste0("X", pad(1:length(condlens[[2]]))),
        condlens[[1]])

    y <- list_df2df(lapply(split(y, temp), function(x) {
        x[, 2] <- utils::tail(x[, 2], 1)
        x
    }))[, -1]

    y <- colpaste2df(y, 1:2, keep.orig =FALSE, sep=qsep, name.sep="|")
    y <- data.frame(y[, 7, drop=FALSE], y[, -7], check.names=FALSE, stringsAsFactors = FALSE) 
    y[, "id"] <- 1:nrow(y) 

    ## get aggregated values iterating through rows
    ## sum wc, max(id),  prop_wc
    list_formality <- lapply(1:nrow(y), function(i) col_meaner(y[1:i, ]))

    ## combine into a dataframe by turn of talk
    df_formality <- list_df2df(list_formality, "turn")

    ## set up color gradients
    colfunc <- grDevices::colorRampPalette(c(contextual, formal))
    cols <- colfunc(max.color.breaks)
   
    ## add colors to df_formality based on agrgegated 
    ## average formality per edge
    cuts <- cut(df_formality[, "prop_formal"], brks)

    df_formality[, "color"] <- cuts %l% data.frame(cut(brks, brks), cols, 
        stringsAsFactors = FALSE)

    ## Handle missing data colors
    missing <- df_formality[, "wc"] == "1" & 
        df_formality[, "formal"] =="0" & 
        df_formality[, "contextual"] == "0"
    df_formality[missing , "color"] <- missing.color

    ## split it back into the iterative per row 
    ## dataframes of aggregated values
    list_formality <- lapply(split(df_formality[, -1], df_formality[, 1]), 
        function(x) {
            y <- colsplit2df(x, sep=qsep)
            colnames(y)[1:2] <- c("from", "to")
            y
    })

    ## create a single network plot with all values
    dat <- sentCombine(x[["text"]], attributes(x)[["grouping.var"]])
    theplot <- discourse_map(dat[, "text.var"], dat[, "grouping.var"], 
        ...)[["plot"]]

    ## generate edge constant of needed
    if (missing(edge.constant)) {
        edge.constant <- length(unique(dat[, "grouping.var"])) * 2.5
    }

    ## Add colors from the aggregated list of average polarities
    ## and output a corresponding list of network plots
    new_form_nets <- lapply(list_formality, colorize, theplot)

    ## Add edge weights etc to each graph
    igraph_objs <- stats::setNames(lapply(seq_along(new_form_nets), 
        function(i, grp =new_form_nets, len=length(unique(y[, 1])), sep=qsep){

        ## limit the edge weights (widths) of first 5 plots)
        if (i %in% 1:5) {
            edge.constant <- edge.constant/(len/i)
        }

        ## calculate edge widths
        cur <- list_formality[[i]]
        cur[, "width"] <- edge.constant*cur[, "prop_wc"]

        ## get current edge
        cur_edge <- which.max(cur[, "id"])
        cur_edge2 <- max(cur[, "id"])

        ## create current edge label and formality sign
        cur_form <- y[y[, "id"] == cur_edge2, "prop_formal"]
        lab <- numbformat(cur_form, digits)
        if(cur[cur[, "id"] == cur_edge2, "color"] == missing.color) {
           lab <- "NA"
        }

        E(grp[[i]])$label <- NA
        curkey <- data.frame(paste2(cur[cur_edge, 1:2], sep="|-|qdap|-|"), lab, 
            stringsAsFactors = FALSE)

        ## Set up widths and colors
        tcols <- cur[, c("from", "to", "color"), drop=FALSE]
        widths <- cur[, c("from", "to", "width"), drop=FALSE]
        widths[, "width"] <- ceiling(widths[, "width"])
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

            
        grp[[i]]
    }), paste0("Turn_", pad(1:nrow(y))))

    timings <- round(exp(y[, "wc"]/(max(y[, "wc"])/time.constant)))
    if(wc.time) {
        igraph_objs <- rep(igraph_objs, timings)
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
    class(igraph_objs) <- "animated_formality"
    attributes(igraph_objs)[["title"]] <- title
    attributes(igraph_objs)[["timings"]] <- timings
    attributes(igraph_objs)[["type"]] <- "network"
    attributes(igraph_objs)[["legend"]] <- cols
    attributes(igraph_objs)[["data"]] <- list_formality
    igraph_objs
}

form_fun <- function(z) {
    out <- data.frame(formal=rowSums(z[, c("noun", "articles", "adj", "prep")]),        
        contextual=rowSums(z[, c("pronoun", "verb", "adverb", "interj")])) 
    out[, "total"] <- rowSums(out)
    out[, paste0("prop_", names(out)[1:2])] <- out[, 1:2]/out[, 3]
    data.frame(from=z[, 1], to=c(as.character(z[-1, 1]), "End"), 
        wc=z[, "wrd.cnt"], out)
}

col_meaner <- function(y) {
    m <- utils::head(y, -1)
    if (nrow(m) == "0") {
        out <- utils::tail(y, 1)
        out[, "prop_wc"] <- 1
        return(out)
    }
    n <- matrix2df(do.call(rbind, lapply(split(m[, c(2:4, 8)], m[, 1]), function(x) {
        if (nrow(x) == "0") return(NULL)
        out <- data.frame(t(colSums(x[, c("wc", "formal", "contextual")])))
        out[, "id"] <- max(x[, "id"])
        out
    })), "from|to")
    n[, "total"] <- rowSums(n[3:4])
    n[, paste0("prop_", names(n)[3:4])] <- n[, 3:4]/n[, 6]
    n <- n[!n[, 1] %in% utils::tail(y, 1)[, 1], ]
    out <- data.frame(rbind(n, utils::tail(y, 1)), row.names=NULL, check.names=FALSE, 
        stringsAsFactors = FALSE)
    out[, "prop_wc"] <- out[, "wc"]/sum(out[, "wc"], na.rm=TRUE)
    out
}

Animate_formality_bar <- function(x, wc.time = TRUE, time.constant = 2, 
    digits = 2, all.color.line = "red", plus.300.color = "grey40",
    under.300.color = "grey88", ...) {

    v <- cbind.data.frame(group=attributes(x)[["grouping.var"]], 
        x[["POSfreq"]])

    colnms1 <- colnames(scores(x))[1]
    ord <- levels(scores(x)[, 1])
    nas <- which(is.na(x[["text"]]))

    if(!identical(nas, integer(0))) {
        x[["POSfreq"]][nas,] <- 0
        x[["POSfreq"]][nas, 1] <- 1
    }
    
    ## Count the articles per row
    articles <- unlist(lapply(x[["text"]], function(x){  
            if(identical(x, character(0)) ) return(0)                               
            sum(article(x))                                                      
    })) 

    if (!is.null(v$DT)) {                                                            
        PD <- v$DT-articles                                                          
    } else {
        PD <- rep(0, nrow(v))
    }    

    ## tally parts of speech for formality stat
    z <- data.frame(v[, 1:2, drop=FALSE],                                    
        noun = rowSums(v[, names(v) %in% c("NN", "NNS", "NNP", "NNPS",               
            "POS", "JI", "JK"), drop=FALSE]),                                                    
        adj = rowSums(cbind(v[, names(v) %in% c("CD", "JJ", "JJR", "JJS",            
            "JI", "JK"), drop=FALSE], PD)),                                                      
        prep = rowSums(v[, names(v) %in% c("IN", "RP", "TO", "JI", "JK"), 
            drop=FALSE]),          
        articles = articles,                                                         
        pronoun = rowSums(v[, names(v) %in% c("PRP", "PRP$", "PRP.", "WDT",          
            "WP", "WP$", "WP.", "JI", "JK", "EX"), drop=FALSE]),                                 
        verb = rowSums(v[, names(v) %in% c("MD", "VB", "VBD", "VBG",                 
            "VBN", "VBP", "VBZ", "JI", "JK"), drop=FALSE]),                                      
        adverb = rowSums(v[, names(v) %in% c("RB", "RBR", "RBS", "WRB",              
            "JI", "JK"), drop=FALSE]),                                                           
        interj = rowSums(v[, names(v) %in% c("UH", "JI", "JK"), drop=FALSE]))

    qsep <- "|-|qdap|-|"
    z_form <- z[, 1:2]
    z_form[, "formal"] <- rowSums(z[, c("noun", "articles", "adj", "prep")])
    z_form[, "contextual"] <- rowSums(z[, c("pronoun", "verb", "adverb", "interj")])

    listdat <- lapply(1:nrow(z_form), function(i) {
        dat <- z_form[1:i, ]
        out <- agg_form(dat)
        attributes(out)[["formality"]] <- form_stats_total(dat)
        out
    })

    form_all <- sapply(listdat, function(x) attributes(x)[["formality"]])
    thedat <- list_df2df(listdat, "row")
    rng <- max(thedat[, "formality"], na.rm=TRUE)

    theplot <- ggbar_form(listdat[[length(listdat)]], grp = colnms1, rng = rng, 
        colors=c(plus.300.color, under.300.color))

    ggplots <- stats::setNames(lapply(seq_along(listdat), function(i, aplot=theplot) {
        listdat[[i]][, "group"] <- factor(listdat[[i]][, "group"], levels=ord)
        titlepol <- numbformat(form_all[i], digits)

        aplot[["labels"]][["title"]] <- paste(sprintf("Total Discourse Formality:  %s", 
            titlepol), sprintf("%sCurrent Speaker:   %s", paste(rep(" ", 15), 
            collapse=""), z_form[i, 1]))

        aplot[["data"]] <- listdat[[i]]
        aplot + geom_hline(yintercept=form_all[i], size=1, color=all.color.line) 
        }), paste0("turn_", pad(1:length(listdat))))

    wrds <- z_form[, "wrd.cnt"]
    wrds[is.na(wrds)] <- 1
    timings <- round(exp(wrds/(max(wrds)/time.constant)))
    
    if(wc.time) {
        ggplots <- rep(ggplots, timings)
    }

    ## starts with a blank object and end match the network Animate
    theplot[["data"]][, "formality"] <- NaN
    ggplots <- unlist(list(list(theplot), ggplots, 
        ggplots[length(ggplots)]), recursive=FALSE)

    len <- nchar(char2end(names(ggplots)[1], "_"))
    names(ggplots)[1] <- sprintf("turn_%s", paste(rep(0, len), collapse=""))

    ## add class info
    class(ggplots) <- "animated_formality"
    attributes(ggplots)[["timings"]] <- timings
    attributes(ggplots)[["type"]] <- "bar"
    attributes(ggplots)[["legend"]] <- NULL
    attributes(ggplots)[["data"]] <- listdat
    ggplots
}


form_stats <- function(x) {
    grp <- x[1, 1]
    x[, 1] <- NULL
    x <- colSums(x, na.rm=TRUE)
    x[paste0("prop_", names(x)[2:3])] <- x[2:3]/x[1]

    formality <- 50*(1 +(x["prop_formal"] - x["prop_contextual"])/x["wrd.cnt"]) 

    data.frame(group=grp, wc=x["wrd.cnt"], prop_contextual=x["prop_contextual"], 
        prop_formal=x["prop_formal"], formality=formality, row.names=NULL, 
        Words = ifelse(x["wrd.cnt"] > 299, "300 Plus", "Less Than 300"), 
        stringsAsFactors = FALSE)

}

form_stats_total <- function(x) {
 
    x[, 1] <- NULL
    x <- colSums(x, na.rm=TRUE)
    x[paste0("prop_", names(x)[2:3])] <- x[2:3]/x[1]

    stats::setNames(50*(1 +(x["prop_formal"] - x["prop_contextual"])/x["wrd.cnt"]), "formality") 

}

agg_form <- function(x) {
    ldat <- split(x, x[, 1])
    ldat <- ldat[sapply(ldat, nrow) > 0]
    data.frame(do.call(rbind, lapply(ldat, form_stats)), row.names=NULL, 
        stringsAsFactors = FALSE)
 
}

ggbar_form <- function(dat, grp = grp, rng = rng, colors) {

    padding <- rng*.05
    levels(dat[, "Words"]) <- c("Less Than 300", "300 Plus")
  
    ggplot2::ggplot(dat, aes_string(x="group"))  +
        ggplot2::geom_bar(aes_string(weight="formality", fill="Words")) +
        ggplot2::ylab("Average Formality") + 
        ggplot2::xlab(paste(sapply(unlist(strsplit(grp, "&")), Caps), collapse = " ")) +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(sprintf("Average Discourse Formality:  %s", "")) +
        ggplot2::theme(axis.text.x=element_text(angle = 90, vjust = .4, hjust = 1, size=11),
            plot.title=element_text(hjust=0, size=11, color="grey60")) + 
        ggplot2::scale_x_discrete(drop=FALSE) + 
        ggplot2::scale_fill_manual(values=rev(colors), name="Number of Words", drop=FALSE) +
        ggplot2::scale_y_continuous(expand = c(0,0), limits=c(0, rng + padding)) +
        ggplot2::guides(fill=guide_legend(reverse=TRUE))

}

Animate_formality_text <- function(x, wc.time = TRUE, time.constant = 2, 
    width,  just, coord, ...) {

    txt <- lapply(x[["text"]], function(x){
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
    
    y <- preprocessed(x)

    timings <- round(exp(y[["word.count"]]/(max(y[["word.count"]], na.rm=TRUE)/time.constant)))

    if(wc.time) {
        ggplots <- rep(ggplots, replace_nan(timings, is.na, 1))
    }

    ## starts with a blank object and end match the network Animate
    ggplots <- unlist(list(list(theplot), ggplots, 
        list(theplot)), recursive=FALSE)

    ## add class info
    class(ggplots) <- "animated_formality"
    attributes(ggplots)[["timings"]] <- timings
    attributes(ggplots)[["type"]] <- "text"
    attributes(ggplots)[["legend"]] <- NULL
    attributes(ggplots)[["data"]] <- NULL
    ggplots
}

#' Animate Formality
#' 
#' \code{Animate.formality} - Animate a \code{\link[qdap]{formality}} object.
#' 
#' formality Method for Animate
#' @param x A \code{\link[qdap]{formality}} object.
#' @param contextual The color to use for 0\% formality (purely contextual).
#' @param formal The color to use for 100\% formality (purely formal).
#' @param edge.constant A constant to multiple edge width by.
#' @param wc.time logical.  If \code{TRUE} weights duration of frame by word 
#' count.
#' @param time.constant A constant to divide the maximum word count by.  Time
#' is calculated by `round(exp(WORD COUNT/(max(WORD COUNT)/time.constant)))`.  
#' Therefore a larger constant will make the difference between the large and 
#' small word counts greater.
#' @param title The title to apply to the animated image(s).
#' @param digits The number of digits to use in the current turn of talk 
#' formality.
#' @param current.color The color to use for the current turn of talk formality.
#' @param current.speaker.color The color for the current speaker.
#' @param non.speaker.color The color for the speakers not currently speaking.
#' @param missing.color The color to use in a network plot for edges 
#' corresponding to missing text data.  Use \code{\link[stats]{na.omit}} before 
#' hand to remove the missing values all together.
#' @param all.color.line The color to use for the total discourse formality 
#' color line if \code{network = FALSE}.
#' @param plus.300.color The bar color to use for grouping variables exceeding 
#' 299 words per Heylighen & Dewaele's (2002) minimum word recommendations.
#' @param under.300.color The bar color to use for grouping variables less 
#' than 300 words per Heylighen & Dewaele's (2002) minimum word recommendations.
#' @param type  Character string of either \code{"network"} (as a network 
#' plot), \code{"bar"} (as a bar plot), or \code{"text"} (as a simple 
#' colored text plot).
#' @param width The width to break text at if \code{type = "text"}.
#' @param coord The x/y coordinate to plot the text if \code{type = "text"}.
#' @param just The \code{hjust} and \code{vjust} values to use for the text if 
#' \code{type = "text"}.
#' @param \ldots Other arguments passed to \code{\link[qdap]{discourse_map}} or
#' \code{\link[ggplot2]{annotate}} if \code{type = "text"}.
#' @note The width of edges is based on words counts on that edge until that 
#' moment divided by total number of words used until that moment.  Thicker 
#' edges tend to thin as time passes.  The actual duration the current edge 
#' stays as the \code{current.color} is based on word counts for that particular 
#' flow of dialogue divided by total dialogue (words) used.  The edge label is
#' the current formality for that turn of talk (an aggregation of the sub 
#' sentences of the current turn of talk).  The coloring of the current edge 
#' formality is produced at th sentence level, therefor a label may indicate a 
#' positive current turn of talk, while the coloring may indicate a negative 
#' sentences.  Coloring is based on percentage of formal parts of speech (i.e.,
#' noun, adjective, preposition, article).
#' @import igraph
#' @importFrom qdapTools %l% matrix2df list_df2df
#' @importFrom ggplot2 ggplot geom_hline geom_bar ylab xlab theme ggtitle theme_bw ylim element_text scale_x_discrete scale_fill_manual
#' @export
#' @method Animate formality
Animate.formality <- function(x, contextual = "yellow", formal = "red",
    edge.constant, wc.time = TRUE, time.constant = 2, title = NULL, digits = 3, 
    current.color = "black", current.speaker.color = NULL, non.speaker.color = NA,
    missing.color = "purple", all.color.line = "red", plus.300.color = "grey40", 
    under.300.color = "grey88", type = "network", width = 65, coord = c(.0, .5), 
    just = c(.0, .5), ...){
    
    switch(type,
        network = {
            Animate_formality_net(x = x, contextual = contextual, formal = formal, 
                edge.constant = edge.constant, wc.time = wc.time, time.constant = time.constant, 
                title = title, digits = digits, current.color = current.color, 
                current.speaker.color = current.speaker.color, 
                non.speaker.color = non.speaker.color, missing.color = missing.color , 
                ...)
        },
        bar = {
            Animate_formality_bar(x = x, wc.time = wc.time, 
                time.constant = time.constant, digits = digits, 
                all.color.line = all.color.line, plus.300.color = plus.300.color, 
                under.300.color = under.300.color, ...)         
            },
        text = {
           Animate_formality_text(x = x, wc.time = wc.time, 
               coord = coord, just = just, width=width, ...)
        }, stop("`type` must be \"network\", \"bar\", or \"text\"")
    )

}


#' Prints a animated_formality  Object
#' 
#' Prints a animated_formality  object.
#' 
#' @param x The animated_formality  object.
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
#' @importFrom plotrix color.legend
#' @method print animated_formality 
#' @export
print.animated_formality <- function(x, title = NULL, 
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
                    color.legend(legend[1], legend[2], legend[3], legend[4], 
                        c("Contextual", "Formal"), attributes(x)[["legend"]], 
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



#' Plots a animated_formality  Object
#' 
#' Plots a animated_formality  object.
#' 
#' @param x The animated_formality  object.
#' @param \ldots Other arguments passed to \code{print.animated_formality }.
#' @method plot animated_formality 
#' @export
plot.animated_formality  <- function(x, ...){ 

    print(x, ...)

}

#' Network Formality
#' 
#' \code{Network.formality} - Network a \code{\link[qdap]{formality}} object.
#' 
#' formality Method for Network
#' @param x A \code{\link[qdap]{formality}} object.
#' @param contextual The color to use for 0\% formality (purely contextual).
#' @param formal The color to use for 100\% formality (purely formal).
#' @param edge.constant A constant to multiple edge width by.
#' @param title The title to apply to the \code{Network}ed image(s).
#' @param digits The number of digits to use in the current turn of talk 
#' formality.
#' @param plus.300.color The bar color to use for grouping variables exceeding 
#' 299 words per Heylighen & Dewaele's (2002) minimum word recommendations.
#' @param under.300.color The bar color to use for grouping variables less 
#' than 300 words per Heylighen & Dewaele's (2002) minimum word recommendations.
#' @param missing.color The color to use in a network plot for edges 
#' corresponding to missing text data.  Use \code{\link[stats]{na.omit}} before 
#' hand to remove the missing values all together.
#' @param \ldots Other arguments passed to \code{\link[qdap]{discourse_map}}.
#' @import igraph
#' @importFrom qdapTools %l% 
#' @export
#' @method Network formality
Network.formality <- function(x, contextual = "yellow", formal = "red", 
    edge.constant, title = NULL, digits = 3, plus.300.color = "grey40", 
    under.300.color = "grey88", missing.color = "purple", ...){

    v <- cbind.data.frame(group=attributes(x)[["grouping.var"]], 
        x[["POSfreq"]])

    nas <- which(is.na(x[["text"]]))
    x[["text"]][nas] <- ""

    if(!identical(nas, integer(0))) {
        x[["POSfreq"]][nas,] <- 0
        x[["POSfreq"]][nas, 1] <- 1
    }

    ## Count the articles per row
    articles <- unlist(lapply(x[["text"]], function(x){  
            if(identical(x, character(0)) ) return(0)                               
            sum(article(x))                                                      
    })) 

    if (!is.null(v$DT)) {                                                            
        PD <- v$DT-articles                                                          
    } else {
        PD <- rep(0, nrow(v))
    }    

    ## tally parts of speech for formality stat
    z <- data.frame(v[, 1:2, drop=FALSE],                                    
        noun = rowSums(v[, names(v) %in% c("NN", "NNS", "NNP", "NNPS",               
            "POS", "JI", "JK"), drop=FALSE]),                                                    
        adj = rowSums(cbind(v[, names(v) %in% c("CD", "JJ", "JJR", "JJS",            
            "JI", "JK"), drop=FALSE], PD)),                                                      
        prep = rowSums(v[, names(v) %in% c("IN", "RP", "TO", "JI", "JK"), 
            drop=FALSE]),          
        articles = articles,                                                         
        pronoun = rowSums(v[, names(v) %in% c("PRP", "PRP$", "PRP.", "WDT",          
            "WP", "WP$", "WP.", "JI", "JK", "EX"), drop=FALSE]),                                 
        verb = rowSums(v[, names(v) %in% c("MD", "VB", "VBD", "VBG",                 
            "VBN", "VBP", "VBZ", "JI", "JK"), drop=FALSE]),                                      
        adverb = rowSums(v[, names(v) %in% c("RB", "RBR", "RBS", "WRB",              
            "JI", "JK"), drop=FALSE]),                                                           
        interj = rowSums(v[, names(v) %in% c("UH", "JI", "JK"), drop=FALSE]))

    qsep <- "|-|qdap|-|"

    brks <- seq(0, 1, by=.001)
    max.color.breaks <- length(brks)

    condlens <- rle(as.character(z[, 1]))
    temp <- rep(paste0("X", pad(1:length(condlens[[2]]))),
        condlens[[1]])

    z <- list_df2df(lapply(split(z, temp), function(x) {
        data.frame(x[1, 1, drop=FALSE], t(colSums(x[, -1, drop=FALSE])))
    }))[, -1]

    z[, "from"] <- as.character(z[, "group"])
    z[, "to"] <- c(z[-1, "from"], "end")
    nc <- ncol(z[, -1])
    z <- colpaste2df(z[, -1], (nc-1):nc, keep.orig =FALSE, sep=qsep, name.sep="|")
    
    nc <- ncol(z)

    z <- list_df2df(lapply(split(z[, -nc], z[, nc]), function(x) {
        data.frame(t(colSums(x)))
    }), "group")

    y <- form_fun2(z)

    df_formality <- colsplit2df(y, sep=qsep, name.sep="|")
    df_formality[, "to"] <- gsub("end", "End", df_formality[, "to"])

    ## set up color gradients
    colfunc <- grDevices::colorRampPalette(c(contextual, formal))
    cols <- colfunc(max.color.breaks)

    ## add colors to df_formality based on agrgegated 
    ## average formality per edge
    cuts <- cut(df_formality[, "prop_formal"], c(-1, brks, 2) )

    df_formality[, "color"] <- cuts %l% data.frame(cut(brks, brks), cols, 
        stringsAsFactors = FALSE)

    ## Handle missing data colors
    missing <- df_formality[, "wc"] == "1" & 
        df_formality[, "formal"] =="0" & 
        df_formality[, "contextual"] == "0"
    df_formality[missing , "color"] <- missing.color

    ## create a single network plot with all values
    dat <- sentCombine(x[["text"]], attributes(x)[["grouping.var"]])
    theplot <- discourse_map(dat[, "text.var"], dat[, "grouping.var"], 
        ...)[["plot"]]

    ## generate edge constant of needed
    if (missing(edge.constant)) {
        edge.constant <- length(unique(dat[, "grouping.var"])) * 2.5
    }

    ## Add colors from 
    theplot <- colorize(df_formality, theplot)

    theedges <- paste2(edge_capture(theplot), sep=qsep)
    df_formality <- colpaste2df(df_formality, 1:2, sep=qsep, name.sep="|")

    counts <- stats::aggregate(wc~from, df_formality, sum)
    counts[, "vcol"] <- ifelse(counts[, "wc"] > 299, 
        plus.300.color, under.300.color)

    V(theplot)$color <- lookup(V(theplot)$name, counts[, -2], missing = under.300.color)
    E(theplot)$label <-lookup(theedges, df_formality[, "from|to"], 
        numbformat(df_formality[, "prop_formal"], digits))

    ## Set up widths and colors
    df_formality[, "prop_wc"] <- df_formality[, "wc"]/sum(df_formality[, "wc"])
    df_formality[, "width"] <- edge.constant*df_formality[, "prop_wc"]
    tcols <- df_formality[, c("from", "to", "color"), drop=FALSE]
    widths <- df_formality[, c("from", "to", "width"), drop=FALSE]
    widths[, "width"] <- ceiling(widths[, "width"])
    ekey <- paste2(edge_capture(theplot), sep=qsep)
    ckey <- colpaste2df(tcols, 1:2, sep = qsep, keep.orig=FALSE)[, 2:1]
    wkey <- colpaste2df(widths, 1:2, sep = qsep, keep.orig=FALSE)[, 2:1]
    E(theplot)$width <- NAer(ekey %l% wkey, 1)    
    
    ## add class info
    class(theplot) <- c("Network", class(theplot))
    attributes(theplot)[["title"]] <- title
    attributes(theplot)[["legend.gradient"]] <- cols
    attributes(theplot)[["network.type"]] <- "formality"
    attributes(theplot)[["legend.label"]] <- c("Contextual", "Formal")  
    attributes(theplot)[["n.color.breaks"]] <- max.color.breaks
    attributes(theplot)[["color.locs"]] <- as.numeric(cuts)
    theplot
}



form_fun2 <- function (z) {
    out <- data.frame(formal = rowSums(z[, c("noun", "articles", 
        "adj", "prep")]), contextual = rowSums(z[, c("pronoun", 
        "verb", "adverb", "interj")]))
    out[, "total"] <- rowSums(out)
    out[, paste0("prop_", names(out)[1:2])] <- out[, 1:2]/out[, 3]
    data.frame(`from|to` = z[, 1], wc = z[, "wrd.cnt"], out, check.names=FALSE)
}



#' \code{cumulative.formality} - Generate formality over time (duration in 
#' sentences).
#' @rdname cumulative
#' @export
#' @method cumulative formality
cumulative.formality <- function(x, ...){
    
    v <- x[["POSfreq"]]

    colnms1 <- colnames(scores(x))[1]
    ord <- levels(scores(x)[, 1])
    nas <- which(is.na(x[["text"]]))

    if(!identical(nas, integer(0))) {
        x[["POSfreq"]][nas,] <- 0
        x[["POSfreq"]][nas, 1] <- 1
    }
    
    ## Count the articles per row
    articles <- unlist(lapply(x[["text"]], function(x){  
            if(identical(x, character(0)) ) return(0)                               
            sum(article(x))                                                      
    })) 

    if (!is.null(v$DT)) {                                                            
        PD <- v$DT-articles                                                          
    } else {
        PD <- rep(0, nrow(v))
    }    

    ## tally parts of speech for formality stat
    z <- data.frame(                                    
        noun = rowSums(v[, names(v) %in% c("NN", "NNS", "NNP", "NNPS",               
            "POS", "JI", "JK"), drop=FALSE]),                                                    
        adj = rowSums(cbind(v[, names(v) %in% c("CD", "JJ", "JJR", "JJS",            
            "JI", "JK"), drop=FALSE], PD)),                                                      
        prep = rowSums(v[, names(v) %in% c("IN", "RP", "TO", "JI", "JK"), 
            drop=FALSE]),          
        articles = articles,                                                         
        pronoun = rowSums(v[, names(v) %in% c("PRP", "PRP$", "PRP.", "WDT",          
            "WP", "WP$", "WP.", "JI", "JK", "EX"), drop=FALSE]),                                 
        verb = rowSums(v[, names(v) %in% c("MD", "VB", "VBD", "VBG",                 
            "VBN", "VBP", "VBZ", "JI", "JK"), drop=FALSE]),                                      
        adverb = rowSums(v[, names(v) %in% c("RB", "RBR", "RBS", "WRB",              
            "JI", "JK"), drop=FALSE]),                                                           
        interj = rowSums(v[, names(v) %in% c("UH", "JI", "JK"), drop=FALSE]))

    z_form <- v[, 1, drop=FALSE]
    z_form[, "formal"] <- rowSums(z[, c("noun", "articles", "adj", "prep")])
    z_form[, "contextual"] <- rowSums(z[, c("pronoun", "verb", "adverb", "interj")])

    n.obs <- nrow(z_form)

    out <- list(cumulative_formality = sapply(1:n.obs, function(i) {
        x <- colSums(z_form[1:i, ], na.rm=TRUE)
        x[paste0("prop_", names(x)[2:3])] <- x[2:3]/x[1]

        unname(50*(1 +(x["prop_formal"] - x["prop_contextual"])/x["wrd.cnt"]))

    }), greater_than_300 = which(cumsum(z_form[, 1]) >= 300)[1])

    class(out) <- "cumulative_formality"
    out

}

#' \code{cumulative.pos} - Generate formality over time (duration in 
#' sentences).
#' @rdname cumulative
#' @export
#' @method cumulative pos
cumulative.pos <- cumulative.formality

#' \code{cumulative.pos_by} - Generate formality over time (duration in 
#' sentences).
#' @rdname cumulative
#' @export
#' @method cumulative pos_by
cumulative.pos_by <- cumulative.formality

#' Plots a cumulative_formality Object
#' 
#' Plots a cumulative_formality object.
#' 
#' @param x The cumulative_formality object.
#' @param \ldots ignored
#' @method plot cumulative_formality 
#' @export
plot.cumulative_formality <- function(x, ...){

    g300 <- x[[2]]
    len <- length(x[[1]])
    form_range <- range(x[[1]][g300:len])
    cumformality <- data.frame(cum_mean = x[[1]], Time = 1:len, drop=TRUE)
    cumformality <- cumformality[g300:length(x[[1]]), ]    
    note <- sprintf("*Note: After 300 Words (Begins With Statement %s)", g300)
    note_coord <- c(x[[2]], x[[1]][g300] + (diff(form_range) * .01))


    ggplot2::ggplot() + ggplot2::theme_bw() +
        ggplot2::geom_smooth(data = cumformality, ggplot2::aes_string(y="cum_mean", 
            x = "Time")) +
        ggplot2::geom_hline(y=mean(x[[1]]), color="grey30", size=1, alpha=.3, linetype=2) + 
        ggplot2::annotate("text", x = len/2, y = mean(x[[1]]), color="grey30", 
            label = "Average Formality", vjust = .3, size=4) +
        ggplot2::geom_line(data = cumformality, ggplot2::aes_string(y="cum_mean", 
            x = "Time"), size=1) +
        ggplot2::ylab("Cumulative Average Formality") + 
        ggplot2::xlab("Duration") +
        ggplot2::scale_x_continuous(expand = c(.01,.01)) +
        ggplot2::annotate("text", x = note_coord[1], y = note_coord[2], 
            color="grey40", label = note, size=2.5, fontface = 3, hjust=0, alpha=.4) 

}

#' Prints a cumulative_formality Object
#' 
#' Prints a cumulative_formality  object.
#' 
#' @param x The cumulative_formality object.
#' @param \ldots ignored
#' @method print cumulative_formality
#' @export
print.cumulative_formality <- function(x, ...) {
    print(plot.cumulative_formality(x, ...))
}

#' \code{cumulative.animated_formality} - Generate animated formality over time 
#' (duration in sentences).
#' @rdname cumulative
#' @export
#' @method cumulative animated_formality
cumulative.animated_formality <- function(x, ...) {

    if(attributes(x)[["network"]]) {
        stop("Output must be from an `Animate.formality` when `network = FALSE`")
    }

    out <- c(NA, unlist(lapply(x, grab_ave_formality), use.names = FALSE))
    out[1] <- out[2]
    avepol <- utils::tail(out, 1)
    len <- length(out)
    
    output <- data.frame(cum_mean = out, Time = 1:len, drop=TRUE) 

    class(output) <- c("cumulative_animated_formality", class(output))
    attributes(output)[["length"]] <- len
    attributes(output)[["average.formality"]] <- avepol    
    attributes(output)[["range"]] <- x[[1]][["scales"]][["scales"]][[1]][["limits"]]  
    output
}


#' Plots a cumulative_animated_formality Object
#' 
#' Plots a cumulative_animated_formality object.
#' 
#' @param x The cumulative_animated_formality object.
#' @param \ldots ignored
#' @method plot cumulative_animated_formality 
#' @export
plot.cumulative_animated_formality <- function(x, ...){
   
    output <- lapply(1:nrow(x), function(i) {

        ggplot2::ggplot() + ggplot2::theme_bw() +
            ggplot2::geom_line(data = x[1:i, ,drop=FALSE], ggplot2::aes_string(y="cum_mean", 
                x = "Time"), size=1) +
            ggplot2::geom_hline(yintercept=0, size=1.5, color="grey50", linetype="dashed") + 
            ggplot2::geom_hline(y=attributes(x)[["average.formality"]], 
                color="grey30", size=1, alpha=.3) + 
            ggplot2::ylab("Cumulative Average formality") + 
            ggplot2::xlab("Duration") +
            ggplot2::scale_x_continuous(expand = c(0, 0), 
                limits = c(0, attributes(x)[["length"]])) +
            ggplot2::ylim(range(x[["cum_mean"]])) +
            ggplot2::annotate("point", y = x[i, "cum_mean"], 
                x =x[i, "Time"], colour = "red", size = 1.5) 
    })

    output[[1]][["layers"]][[4]][["geom_params"]][["colour"]] <- NA
    output[[length(output)]] <- output[[length(output)]] + 
        ggplot2::geom_smooth(data = x, 
            ggplot2::aes_string(y="cum_mean", x = "Time")) 

    output
}

#' Prints a cumulative_animated_formality Object
#' 
#' Prints a cumulative_animated_formality  object.
#' 
#' @param x The cumulative_animated_formality object.
#' @param \ldots ignored
#' @method print cumulative_animated_formality
#' @export
print.cumulative_animated_formality <- function(x, ...) {
    print(plot.cumulative_animated_formality(x, ...))
}

grab_ave_formality <- function(x, left="Total Discourse Formality:", 
    right = "Current Speaker:") {

    genXtract(x[["labels"]][["title"]], left, right) %>% 
    Trim() %>% 
    as.numeric() 
}
