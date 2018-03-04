library(qdap)
neg <- readLines("negative.txt")
pos <- readLines("positive.txt")
neg <- tail(neg, -35)
pos <- tail(pos, -36)

pos[grep("[^a-zA-Z]", pos)]
neg[grep("[^a-zA-Z]", neg)]


pos <- mgsub("-", " ", pos)
neg <- mgsub(c("-", "f**k", "d*mn", "bull----", "bull****", "sh*t"), 
    c(" ", "fuck", "damn", "bullshit", "bullshit", "shit"), neg, fixed=TRUE)
neg[neg == "naïve"] <- "naive"

neg <- replace_number(neg)
pos <- replace_number(pos)

negation.words <- c("ain't", "aren't", "can't", "couldn't", "didn't", "doesn't", 
  "don't", "hasn't", "isn't", "mightn't", "mustn't", "shan't", "shouldn't", 
  "wasn't", "weren't", "won't", "wouldn't", "neither", "never", "no", 
  "nobody", "nor", "not")

deamplification.words <- c("faintly", "barely", "very few", "very little", "little", "only", 
  "few", "little", "slightly", "sparsely", "sporadically", "rarely", "seldom", "hardly")

amplification.words <- c("incalculably", "acutely", "awfully", "badly", "certainly", "colossally",
    "desperately", "enormously", "exceedingly", "exceptionally", "extraordinarily", "certainly", 
    "extremely", "greatly", "hugely", "immensely", "incredibly", "massively", "definitely", 
    "more", "particularly", "purposely", "really", "remarkably", "seriously", "miraculously",
    "severely", "significantly", "surely", "terrifically", "tremendously", "truly",
    "admirably", "amazingly", "beautifully", "extraordinarily", "magnificently",
    "marvelously", "remarkably", "spectacularly", "strikingly", "stunningly",
    "excellently", "unusually", "vastly", "very", "highly", "quite")


negation.words <- sort(negation.words)
deamplification.words <- sort(deamplification.words)
amplification.words <- sort(amplification.words)

c(neg, pos)[c(neg, pos) %in% negation.words]
c(neg, pos)[c(neg, pos) %in% deamplification.words]
c(neg, pos)[c(neg, pos) %in% amplification.words]

negative.words <- unique(sort(neg))
positive.words <- unique(sort(c("a plus", pos)))
positive.words <- positive.words[!positive.words %in% negative.words]
length(negative.words)
length(positive.words)

env.pol <- polarity_frame(positive.words, negative.words)


amplification.words <- amplification.words[!amplification.words %in% c(neg, pos)]


pack.skel(env.pol, deamplification.words, amplification.words, negation.words, negative.words, positive.words)
