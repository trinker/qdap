#internal not exported
unblanker <-
function(x)subset(x, nchar(x)>0)
