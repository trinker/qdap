x <- "http://esl.about.com/library/vocabulary/bl1000_list%s.htm"

library(XML)

the_list <- do.call(rbind, lapply(1:4, function(i) {
   
    out <- lapply(if(i == 1) 1:2 else 1, function(j) {
        y <- readHTMLTable(sprintf(x, i), which=j, header=FALSE) 
        rbind(setNames(y[, 1:2], nm=c("Rank", "Word")), setNames(y[, 4:5], nm=c("Rank", "Word")))
    })

    out <- do.call(rbind, out)

    out <- data.frame(out[out[, 1] != "Rank", ], 
        row.names=NULL, stringsAsFactors = FALSE)
    out[, 1] <- as.numeric(as.character(out[, 1]))
    out[order(out[, 1]), ]
}))[, 2:1]

Fry_1000 <- as.character(the_list[, 1])



dolch_list <- list(
    pre_primer = qcv(terms="a, and, away, big, blue, can, come, down, find, for, funny, go, help, here, I, in, is, it, 
        jump, little, look, make, me, my, not, one, play, red, run, said, see, the, three, to, two, up, we, where, yellow, 
        you", split=","), 
    primer = qcv(terms="all, am, are, at, ate, be, black, brown, but, came, did, do, eat, four, get, good, have, he, 
        into, like, must, new, no, now, on, our, out, please, pretty, ran, ride, saw, say, she, so, soon, that, there, 
        they, this, too, under, want, was, well, went, what, white, who, will, with, yes", split=","), 
    first_grade = qcv(terms="after, again, an, any, as, ask, by, could, every, fly, from, give, giving, had, has, her, him, his, 
        how, just, know, let, live, may, of, old, once, open, over, put, round, some, stop, take, thank, them, then, 
        think, walk, were, when", split=","),
    second_grade = qcv(terms="always, around, because, been, before, best, both, buy, call, cold, does, don't, fast, first, five, 
        found, gave, goes, green, its, made, many, off, or, pull, read, right, sing, sit, sleep, tell, their, these, 
        those, upon, us, use, very, wash, which, why, wish, work, would, write, your", split=","),
    third_grade = qcv(terms="about, better, bring, carry, clean, cut, done, draw, drink, eight, fall, far, full, got, grow, 
        hold, hot, hurt, if, keep, kind, laugh, light, long, much, myself, never, only, own, pick, seven, shall, show, 
        six, small, start, ten, today, together, try, warm", split=",")
)

Leveled_Dolch <- list2df(dolch_list, "Word", "Level")

Dolch <- list2df(dolch_list, "Word", "Level")[, 1]

pack.skel(Fry_1000, Leveled_Dolch, Dolch)






