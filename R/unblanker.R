#internal not exported
#@export
unblanker <-
function(x)subset(x, nchar(x)>0)
