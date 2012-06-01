breaker <-
function(x) {
    unlist(strsplit(x, "[[:space:]]|(?=[.!?*-])", perl=TRUE))
}
