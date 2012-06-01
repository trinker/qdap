affixFind <-
function(affix.string, char.string, affix='suffix'){
    n.char<-nchar(affix.string)
    AFF<-affix(char.string=char.string, n.char=n.char, affix=affix)
    AFF[which(AFF==affix.string)]
}
