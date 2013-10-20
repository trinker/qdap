#' Quick Heatmap
#' 
#' A quick heatmap function for visualizing typical qdap dataframe/matrix 
#' outputs.
#' 
#' @param mat A matrix or dataframe produced by many qdap functions in 
#' which the first column is the grouping variable and the rest of the matrix 
#' is numeric.  Also accepts objects directly from \code{\link[qdap]{word_stats}} 
#' and \code{\link[qdap]{question_type}}.
#' @param low The color to be used for lower values.
#' @param high The color to be used for higher values.
#' @param values logical.  If \code{TRUE} the cell values will be included on 
#' the heatmap.
#' @param digits The number of digits displayed if \code{values} is \code{TRUE}.
#' @param text.size A integer size to plot the text if \code{values} is 
#' \code{TRUE}.
#' @param text.color A character vector to plot the text if \code{values} 
#' is \code{TRUE}.
#' @param xaxis.col A single character vector color choice for the high values.
#' @param yaxis.col  A single character vector color choice for the low values.
#' @param order.by An optional character vector of a variable name to order the 
#' columns by.  To reverse use a negative (\code{-}) before the column name.
#' @param grid The color of the grid (Use \code{NULL} to remove the grid).  
#' @param by.column logical.  If \code{TRUE} applies scaling to the column.  If 
#' \code{FALSE}  applies scaling by row (use \code{NULL} to turn off scaling).
#' @param auto.size logical.  If \code{TRUE} the visual will be resized to 
#' create square cells.
#' @param mat2 A second matrix equal in dimensions to \code{mat} that will be 
#' used for cell labels if \code{values} is \code{TRUE}.
#' @param plot logical.  If \code{TRUE} the plot will automatically plot.  
#' The user may wish to set to \code{FALSE} for use in knitr, sweave, etc.
#' to add additional plot layers.
#' @param facet.vars A character vector of of 1 or 2 column names to facet by.
#' @param facet.flip logical If \code{TRUE} the direction of the faceting 
#' is reversed.
#' @param diag.na logical.  If \code{TRUE} and \code{mat} is a symetrical 
#' matrix the diagonals are set to \code{NA}.  This is useful with correlation 
#' matrices because the diagonal of ones do not effect the scaling of the 
#' heatmap.
#' @param diag.values The string to be used for the diagonal labels (values) 
#' if \code{diag.na} is set to \code{TRUE}.  Defualt is to not print a value.
#' @details \code{qheat} is useful for finding patterns and anomalies in large
#' qdap generated dataframes and matrices.
#' @note \code{\link[qdap]{qheat}} is a fast way of working with data formats 
#' produced by qdap.  The function isn't designed to be extended beyond 
#' exploratory qdap usage.
#' @keywords heatmap
#' @export
#' @import RColorBrewer 
#' @importFrom gridExtra grid.arrange 
#' @importFrom reshape2 melt 
#' @importFrom scales alpha 
#' @importFrom ggplot2 ggplot facet_grid geom_tile geom_text aes scale_fill_gradient theme_grey scale_x_discrete scale_y_discrete theme element_text coord_equal
#' @examples
#' \dontrun{
#' dat <- sentSplit(DATA, "state")
#' ws.ob <- with(dat, word_stats(state, list(sex, adult), tot=tot))
#' qheat(ws.ob)
#' qheat(ws.ob) + coord_flip()
#' qheat(ws.ob, order.by = "sptot", 
#'     xaxis.col = c("red", "black", "green", "blue"))
#' qheat(ws.ob, order.by = "sptot")
#' qheat(ws.ob, order.by = "-sptot")
#' qheat(ws.ob, values = TRUE)
#' qheat(ws.ob, values = TRUE, text.color = "red")
#' qheat(ws.ob, "yellow", "red", grid = FALSE)
#' qheat(mtcars, facet.vars = "cyl")
#' qheat(mtcars, facet.vars = c("gear", "cyl"))
#' qheat(t(mtcars), by.column=FALSE)
#' qheat(cor(mtcars), diag.na=TRUE, diag.value="", by.column=NULL, values = TRUE)
#' 
#' dat1 <- data.frame(G=LETTERS[1:5], matrix(rnorm(20), ncol = 4))
#' dat2 <- data.frame(matrix(LETTERS[1:25], ncol=5))
#' qheat(dat1, values=TRUE)
#' qheat(dat1, values=TRUE, mat2=dat2)
#' }
qheat <-
function(mat, low = "white", high ="darkblue", values = FALSE,
    digits = 1, text.size = 3, text.color = "grey40", xaxis.col = "black",
    yaxis.col = "black", order.by = NULL, grid = "white", by.column = TRUE, 
    auto.size = FALSE, mat2 = NULL, plot = TRUE, facet.vars = NULL, 
    facet.flip = FALSE, diag.na = FALSE, diag.values = "") {
    group <- value <- values2 <- NULL

    ## set diagonal to NA
    if (diag.na) {
        if (diff(dim(mat)) == 0 && is.matrix(mat)) {
            diag(mat) <- NA
        } else {
            warning ("`mat` is either not a matrix or not symetrical; `diag.na` ignore.")
        }
    }

    ## convert all numeric matrices
    if (all(sapply(mat, is.numeric))) {
        if (is.matrix(mat)) {
            mat <- data.frame(mat, check.names = FALSE) 
        }
        nms <- rownames(mat)        
        mat <- data.frame(group=factor(nms), mat, row.names = NULL)
        mat[, "group"] <- factor(mat[, "group"], levels = nms)
    }
    
    if (!is.null(mat2) & !values) {
        values <- TRUE 
    }

    if (!is.null(facet.vars)) {
        f.vars <- data.frame(mat)[, facet.vars, drop = FALSE]
        keeps <- colnames(mat)[!colnames(mat) %in% colnames(f.vars)]
        mat <- mat[, keeps]
    }
    numformat <- function(val, digits) { 
        sub("^(-?)0.", "\\1.", sprintf(paste0("%.", digits, "f"), val)) 
    }
    classRdf <- c("diversity")
    if (any(class(mat) %in% classRdf)) {
        class(mat) <- "data.frame"
    }     
    CLS <- class(mat)[1]
    if (CLS == "word_stats") {
        mat <- mat[["gts"]]
        class(mat) <- "data.frame"
    }
    if (CLS %in% c("character_table", "question_type", "pos_by")) {
        mat <- mat[["prop"]]
    }
    if (CLS == "termco") {
        mat2 <- mat[["rnp"]]
        mat <- data.frame(mat[["prop"]])
        class(mat2) <- "data.frame"
    }      
    dat2 <- as.matrix(mat[, -1, drop = FALSE])
    NMS <- colnames(dat2)
    if (!is.null(by.column)){
        by.column <- by.column + 1
        dat2 <- apply(dat2, by.column, scale)
        if (by.column == 1) {
            dat2 <- t(dat2)
            colnames(dat2) <- NMS
        }
    }
    if (!is.null(order.by)) {
        if(substring(order.by, 1, 1) != "-") {
            ord <- as.character(mat[order(mat[, order.by]), 1])
        } else {
            ord <- rev(as.character(mat[order(mat[, gsub("-", "", 
                order.by)]), 1]))
        }
        mat[, 1] <- factor(mat[, 1], levels = ord)
    }
    ws4 <- data.frame(group = mat[, 1], dat2, check.names = FALSE)
    colnames(ws4)[1] <- "group"

    if (!is.null(facet.vars)) {
        rmnames <- colnames(f.vars)
        ws4 <- data.frame(f.vars, ws4)
        LRM <- seq_along(rmnames)
        colnames(ws4)[LRM] <- paste0("facet_", LRM)
        ws4 <- melt(ws4, id.var = c(colnames(ws4)[LRM], "group"))
    } else {
        ws4 <- melt(ws4, id.var = "group")
        colnames(ws4)[1:2] <- c("group", "var")
    }

    ws4[, "var"] <- factor(ws4$var, levels=rev(levels(ws4$var)))

    if (values) {
        if (is.null(mat2)) {
            mat2 <- mat
        }      
        ws5 <- data.frame(group = mat2[, 1], mat2[, -1, drop = FALSE])

        ws5 <- melt(ws5, id.var = "group")

        if(is.numeric(ws5$value)) {
            ws4$values2 <- numformat(ws5$value, digits = digits)
        } else {
            ws4$values2 <- ws5$value
        }

        ## what to print the na values on the diagonal as
        if (!is.null(diag.values)) {
            ws4[ws4[, "values2"] == "NA", "values2"] <- diag.values
        }
    }
    if (length(xaxis.col) == 1) {
        ws4[, "xaxis.col"] <- rep(xaxis.col, nrow(ws4))
    } else {
        if (length(xaxis.col) != nrow(mat)) {
            warning("Length of colors not equal to number of grouping variables")
        }
        ws4[, "xaxis.col"] <- lookup(ws4[, "group"], mat[, 1], xaxis.col)         
    }
    if (length(yaxis.col) == 1) {
        ws4[, "yaxis.col"] <- rep(yaxis.col, nrow(ws4))
    } else {
        if (length(yaxis.col) != (ncol(mat) - 1)) {
            warning("Length of colors not equal to number of grouping variables")
        }      
        ws4[, "yaxis.col"] <- lookup(ws4[, "group"], mat[, 1], yaxis.col)         
    }
    if (is.null(grid)) {
        if (values) {
            GP <- ggplot(ws4, aes(group, var, group=var)) +
                geom_tile(aes(fill = value)) +
                geom_text(aes(fill = value, label = values2), 
                    size = text.size, color = text.color)
        } else {
            GP <- ggplot(ws4, aes(group, var, group=var)) +
                geom_tile(aes(fill = value))
        }
    } else {
        if (values) {
            GP <- ggplot(ws4, aes(group, var, group=var)) +
                geom_tile(aes(fill = value), color = grid) +
                geom_text(aes(fill = value, label = values2), 
                    size = text.size, color = text.color)
        } else {
            GP <- ggplot(ws4, aes(group, var, group=var)) +
                geom_tile(aes(fill = value), color = grid)
        }
    }
    GP <- GP + scale_fill_gradient(low = low, high = high) + 
        theme_grey() + 
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0)) + 
        theme(axis.ticks = element_blank(), 
            axis.text.x = element_text(angle = -90, 
            hjust = -.1, vjust=.6, colour=xaxis.col), 
            axis.text.y =element_text(colour=yaxis.col)) +
    xlab(gsub("\\&", " & ", colnames(mat)[1])) +
    ylab("")
    if (auto.size) {
        GP <- GP + coord_equal()
    }
    if(!is.null(facet.vars)) {
        if(length(LRM) == 1) {
            if (!facet.flip) {
                GP <- GP + facet_grid(facet_1~.)
            } else {        
                GP <- GP + facet_grid(.~facet_1)
            }
        } else {
            if (!facet.flip) {
                GP <- GP + facet_grid(facet_1~facet_2)
            } else {        
                GP <- GP + facet_grid(facet_2~facet_1)
            }
        }
    }
    
    if (plot) {
        print(GP)
    }
    invisible(GP)
}