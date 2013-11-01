#Internal number formatting functions
is.Integer <- function(x, tol = .Machine$double.eps^0.5)  {
    abs(x - round(x)) < tol
}
#is.integer(3)
#is.Integer(3)

#@export
## numfor <- function(val, digits, override = FALSE) {
##     if (!is.numeric(val) | is.factor(val)) {
##         return(as.character(val))
##     }
##     if (all(is.Integer(val)) & !override){
##         sub("^(-?)0.", "\\1.", sprintf(paste0("%.0f"), val)) 
##     } else {
##         sub("^(-?)0.", "\\1.", sprintf(paste0("%.", digits, "f"), val))
##     } 
## }

## New version 10-32-13 has na.rm = TRUE in all(
numfor <- 
function(val, digits, override = FALSE) {
    if (!is.numeric(val) | is.factor(val)) {
        return(as.character(val))
    }
    if (all(is.Integer(val), na.rm = TRUE) & !override){
        sub("^(-?)0.", "\\1.", sprintf(paste0("%.0f"), val)) 
    } else {
        sub("^(-?)0.", "\\1.", sprintf(paste0("%.", digits, "f"), val))
    } 
}
#numfor(mtcars$mpg)
#numfor(mtcars$carb)
#numfor(CO2$Plant)


#@export
dfnumfor <- function(dataframe, digits = 2, override = FALSE) {
    data.frame(do.call(cbind, lapply(dataframe, numfor, digits = digits, 
        override = override)), check.names = FALSE)
}

#dfnumfor(mtcars, 3)
#@export
paster <- function(x, digits = 2, symbol = "", override = FALSE) {
    x <- ifelse(x == 0, "0", numfor(x, digits = digits, override = override))
    paste0("(", x, symbol, ")")
}
