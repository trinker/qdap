#helper function for word_list/trans_cloud
freqTab2words <-
function(word.list){
    if(is.data.frame(word.list)){
        rep(word.list[, 1], word.list[, 2])
    } else {
        lapply(word.list, function(x) rep(x[, 1], x[, 2]))
    }
}
