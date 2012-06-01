symbol_change <-
function(text.var, dollar = TRUE, percent = TRUE, 
    pound = TRUE, at = TRUE, and = TRUE) {
    
    sch <- function(text, dollar = TRUE, percent = TRUE, pound = TRUE, 
        at = TRUE, and = TRUE) {
            x <- ifelse(percent == TRUE, gsub("%", " percent ", 
                text), text)
            x <- ifelse(dollar == TRUE, gsub("$", " dollars ", x, 
                fixed = TRUE), x)
            x <- ifelse(pound == TRUE, gsub("#", " number ", x, 
                fixed = TRUE), x)
            x <- ifelse(and == TRUE, gsub("&", " and ", x, fixed = TRUE), x)
            x <- ifelse(at == TRUE, gsub("@", " at ", x, fixed = TRUE), x)
            gsub(" +", " ", x)
    }
    unlist(lapply(text.var, function(text) sch(text, dollar = dollar, 
        percent = percent, pound = pound, at = at, and = and)))
}
