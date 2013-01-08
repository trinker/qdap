#Internal function (not exported)
xnoy <-
function(x, y) {
    x[!x %in% y]
}
