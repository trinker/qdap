## ggplot2 Theme with No Background or Gridlines. 
## 
## A ggplot2 theme with no background and no gridlines.
## 
## @param x logical.  If \code{TRUE} vertical gridlines are added.
## @param y logical.  If \code{TRUE} horizontal gridlines are added.
## @note Both x and y can not be \code{TRUE}.  Use 
## \code{\link[ggplot2]{theme_bw}} instead.
## @export
## @seealso \code{\link[ggplot2]{theme}}
## importFrom ggplot2 theme_bw theme element_blank
## @examples
## ggplot(mtcars, aes(factor(cyl))) + geom_bar() + theme_qdap()
## ggplot(mtcars, aes(factor(cyl))) + geom_bar() + theme_qdap(x = TRUE)
## ggplot(mtcars, aes(factor(cyl))) + geom_bar() + theme_qdap(y = TRUE)
## ggplot(mtcars, aes(factor(cyl))) + geom_bar() + theme_qdap(x = TRUE, y = TRUE)
## NOT EXPORTED AT THIS POINT
theme_qdap <- function(x = FALSE, y = FALSE) {
    a <- theme_bw() 
    if (!x && !y) {
        a + theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    } else {
        if (!x && y) {
            a + theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank())
        } else {
            if (x && !y) {
                a + theme(panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())
            } else {
                return(message("`x` and `y` set to TRUE; use `theme_bw()` instead"))
            }    
        }
    }
}
