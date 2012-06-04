clean <-
function(text) gsub("\\s+", " ", gsub("\n|\t", " ", text))
