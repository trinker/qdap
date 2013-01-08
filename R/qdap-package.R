#' qdap: Quantitative Discourse Analysis Package
#'
#' This package automates many of the tasks associated with quantitative 
#' discourse analysis of transcripts containing discourse.  The package 
#' provides parsing tools for preparing transcript data, coding tools and 
#' anlalysis tools for richer understanding of the data.  Many functions 
#' allow the user to aggregate data by any number of grouping variables, 
#' providing analysis and seamless integration with other R packages which 
#' enable higher level analysis and visualization of text.  This empowers 
#' the researcher with more flexible, efficient and targeted methods and tools.
#'
#' @docType package
#' @name qdap
#' @aliases qdap package-qdap
NULL



#' Buckley & Salton Stopword List
#' 
#' A stopword list containing a character vector of stopwords.
#' 
#' @details \href{http://www.lextek.com/manuals/onix/stopwords2.html}{From Onix Text Retrieval Toolkit API Reference}:
#' "This stopword list was built by Gerard Salton and Chris Buckley for the
#' experimental SMART information retrieval system at Cornell University.
#' This stopword list is generally considered to be on the larger side and so
#' when it is used, some implementations edit it so that it is better suited
#' for a given domain and audience while others use this stopword list as it
#' stands."
#' 
#' @note Reduced from the original 571 words to 546.
#' 
#' @docType data 
#' @keywords datasets 
#' @name BuckleySaltonSWL 
#' @usage data(BuckleySaltonSWL) 
#' @format A character vector with 546 elements 
#' @references \url{http://www.lextek.com/manuals/onix/stopwords2.html}
NULL
 
#' Fictitious Classroom Dialogue
#' 
#' A fictitious dataset useful for small demonstrations.
#' 
#' @details 
#' \itemize{ 
#'   \item person. Speaker
#'   \item sex. Gender
#'   \item adult. Dummy coded adult (0-no; 1-yes)
#'   \item state. Statement (dialogue)
#'   \item code. Dialogue coding scheme  
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name DATA 
#' @usage data(DATA) 
#' @format A data frame with 11 rows and 5 variables 
NULL
 
#' Fictitious Repeated Measures Classroom Dialogue
#' 
#' A repeated measures version of the \code{\link[qdap]{DATA}} dataset.
#' 
#' @details 
#' \itemize{ 
#'   \item day. Day of observation
#'   \item class. Class period/subject of observation
#'   \item person. Speaker
#'   \item sex. Gender
#'   \item adult. Dummy coded adult (0-no; 1-yes)
#'   \item state. Statement (dialogue)
#'   \item code. Dialogue coding scheme 
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name DATA2 
#' @usage data(DATA2) 
#' @format A data frame with 74 rows and 7 variables 
NULL
 
#' Nettalk Corpus Syllable Data Set
#' 
#' A dataset containing syllable counts.
#' 
#' @note This data set is based on the Nettalk Corpus but has some researcher 
#' word deletions and additions based on the needs of the 
#' \code{\link[qdap]{syllable.sum}} algorithm.
#' 
#' @details 
#' \itemize{ 
#'   \item word. The word
#'   \item syllables. Number of syllables
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name DICTIONARY 
#' @usage data(DICTIONARY) 
#' @format A data frame with 20137 rows and 2 variables 
#' @references Sejnowski, T.J., and Rosenberg, C.R. (1987). "Parallel networks 
#' that learn to pronounce English text" in Complex Systems, 1, 145-168. 
#' Retrieved from: \url{http://archive.ics.uci.edu/ml/datasets/Connectionist+Bench+(Nettalk+Corpus)}
#' 
#' \href{http://archive.ics.uci.edu/ml/machine-learning-databases/undocumented/connectionist-bench/nettalk/}{UCI Machine Learning Repository website}
NULL
 
#' Onix Text Retrieval Toolkit Stopword List 1
#' 
#' A stopword list containing a character vector of stopwords. 
#' 
#' @details \href{http://www.lextek.com/manuals/onix/stopwords1.html}{From Onix Text Retrieval Toolkit API Reference}:
#' "This stopword list is probably the most widely used stopword list. It
#' covers a wide number of stopwords without getting too aggressive and
#' including too many words which a user might search upon."
#' 
#' @note Reduced from the original 429 words to 404.
#' 
#' @docType data 
#' @keywords datasets 
#' @name OnixTxtRetToolkitSWL1 
#' @usage data(OnixTxtRetToolkitSWL1) 
#' @format A character vector with 404 elements 
#' @references \url{http://www.lextek.com/manuals/onix/stopwords1.html}
NULL
 
#' Fry's  100 Most Commonly Used English Words
#' 
#' A stopword list containing a character vector of stopwords. 
#' 
#' @details Fry's Word List: The first 25 make up about one-third of all printed 
#' material in English. The first 100 makem up about one-half of all printed 
#' material in English. The first 300 makem up about 65\% of all printed 
#' material in English."
#' 
#' 
#' @docType data 
#' @keywords datasets 
#' @name Top100Words 
#' @usage data(Top100Words) 
#' @format A character vector with 100 elements 
#' @references Fry, E. B. (1997). Fry 1000 instant words. Lincolnwood, IL: 
#' Contemporary Books.
NULL
 
#' Fry's 200 Most Commonly Used English Words
#' 
#' A stopword list containing a character vector of stopwords. 
#' 
#' @details Fry's Word List: The first 25 make up about one-third of all printed 
#' material in English. The first 100 makem up about one-half of all printed 
#' material in English. The first 300 makem up about 65\% of all printed 
#' material in English."
#' 
#' 
#' @docType data 
#' @keywords datasets 
#' @name Top200Words 
#' @usage data(Top200Words) 
#' @format A character vector with 200 elements 
#' @references Fry, E. B. (1997). Fry 1000 instant words. Lincolnwood, IL: 
#' Contemporary Books.
NULL
 
#' Fry's 25 Most Commonly Used English Words
#' 
#' A stopword list containing a character vector of stopwords. 
#' 
#' @details Fry's Word List: The first 25 make up about one-third of all printed 
#' material in English. The first 100 makem up about one-half of all printed 
#' material in English. The first 300 makem up about 65\% of all printed 
#' material in English."
#' 
#' @docType data 
#' @keywords datasets 
#' @name Top25Words 
#' @usage data(Top25Words) 
#' @format A character vector with 25 elements 
#' @references Fry, E. B. (1997). Fry 1000 instant words. Lincolnwood, IL: 
#' Contemporary Books.
NULL
 
#' Small Abrreviations Data Set
#' 
#' A dataset containing abbreviations and their qdap friendly form.
#' 
#' @details 
#' \itemize{ 
#'   \item abv. Common transcript abbreviations
#'   \item rep. qdap representation of those abbraviations
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name abbreviations 
#' @usage data(abbreviations) 
#' @format A data frame with 14 rows and 2 variables 
NULL
 
#' Action Word List
#' 
#' A dataset containing a vector of action words.  This is a subset of the 
#' \href{http://icon.shef.ac.uk/Moby/}{Moby project: Moby Part-of-Speech}.
#' 
#' @details 
#' \href{http://icon.shef.ac.uk/Moby/}{From Grady Ward's Moby project:}
#' "This second edition is a particularly thorough revision of the original Moby
#' Part-of-Speech. Beyond the fifteen thousand new entries, many thousand more
#' entries have been scrutinized for correctness and modernity. This is
#' unquestionably the largest P-O-S list in the world. Note that the many included
#' phrases means that parsing algorithms can now tokenize in units larger than a
#' single word, increasing both speed and accuracy."
#' 
#' @docType data 
#' @keywords datasets 
#' @name action.verbs 
#' @usage data(action.verbs) 
#' @format A vector with 1569 elements 
#' @references 
#' \url{http://icon.shef.ac.uk/Moby/mpos.html}
NULL
 
#' Adverb Word List
#' 
#' A dataset containing a vector of adverbs words.  This is a subset of the 
#' \href{http://icon.shef.ac.uk/Moby/}{Moby project: Moby Part-of-Speech}.
#' 
#' @details 
#' \href{http://icon.shef.ac.uk/Moby/}{From Grady Ward's Moby project:}
#' "This second edition is a particularly thorough revision of the original Moby
#' Part-of-Speech. Beyond the fifteen thousand new entries, many thousand more
#' entries have been scrutinized for correctness and modernity. This is
#' unquestionably the largest P-O-S list in the world. Note that the many included
#' phrases means that parsing algorithms can now tokenize in units larger than a
#' single word, increasing both speed and accuracy."
#' 
#' @docType data 
#' @keywords datasets 
#' @name adverb 
#' @usage data(adverb) 
#' @format A vector with 13398 elements 
#' @references 
#' \url{http://icon.shef.ac.uk/Moby/mpos.html}
NULL
 
#' Emoticons Data Set
#' 
#' A dataset containing common emoticons (adapted from 
#' \href{http://www.lingo2word.com/lists/emoticon_listH.html}{Popular Emoticon List}).
#' 
#' @details 
#' \itemize{ 
#'   \item meaning. The meaning of the emoticon
#'   \item emoticon. The graphic representation of the emoticon
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name emoticon 
#' @usage data(emoticon) 
#' @format A data frame with 81 rows and 2 variables 
#' @references 
#' \url{http://www.lingo2word.com/lists/emoticon_listH.html}
NULL
 
#' Syllable Lookup Environment
#' 
#' A dataset containing a syllable lookup environment (see \code{link[qdap]{DICTIONARY}}).
#' 
#' @details For internal use.
#' 
#' @docType data 
#' @keywords datasets 
#' @name env.syl 
#' @usage data(env.syl) 
#' @format A environment with  
#' @references 
#' \href{http://archive.ics.uci.edu/ml/machine-learning-databases/undocumented/connectionist-bench/nettalk/}{UCI Machine Learning Repository website}
NULL
 
#' Amplifying Words
#' 
#' A dataset containing a vector of words that amplifly word meaning.
#' 
#' @details 
#' Valence shifters are words that alter or intensify the meaning of the polarized
#' words and include negators and amplifiers. Negators are, generally, adverbs
#' that negate sentence meaning; for example the word like in the sentence, "I do
#' like pie.", is given the opposite meaning in the sentence, "I do not like
#' pie.", now containing the negator not. Amplifiers are, generally, adverbs or
#' adjectives that intensify sentence meaning. Using our previous example, the
#' sentiment of the negator altered sentence, "I seriously do not like pie.", is
#' heightened with addition of the amplifier seriously.
#' 
#' @docType data 
#' @keywords datasets 
#' @name increase.amplification.words 
#' @usage data(increase.amplification.words) 
#' @format A vector with 32 elements 
NULL
 

#' Interjections
#' 
#' A dataset containing a character vector of common interjections.
#' 
#' @docType data 
#' @keywords datasets 
#' @name interjections
#' @usage data(interjections) 
#' @format A character vector with 139 elements 
#' @references
#' \url{http://www.vidarholen.net/contents/interjections/}
NULL
 
#' Romeo and Juliet: Act 1 Dialogue Merged with Demographics
#' 
#' A dataset containing act 1 of Romeo and Juliet with demographic information.
#' 
#' @details 
#' \itemize{ 
#'   \item person. Character in the play
#'   \item sex. Gender
#'   \item fam.aff. Family affiliation of character
#'   \item died. Dummy coded death variable (0-no; 1-yes);  if yes the character 
#'   dies in the play
#'   \item dialogue. The spoken dialogue
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name mraja1 
#' @usage data(mraja1) 
#' @format A data frame with 235 rows and 5 variables 
#' @references 
#' \url{http://shakespeare.mit.edu/romeo_juliet/full.html}
NULL
 
#' Romeo and Juliet: Act 1 Dialogue Merged with Demographics and Split
#' 
#' A dataset containing act 1 of Romeo and Juliet with demographic information 
#' and turns of talk split into sentences.
#' 
#' @details 
#' \itemize{ 
#'   \item person. Character in the play
#'   \item tot. 
#'   \item sex. Gender
#'   \item fam.aff. Family affiliation of character
#'   \item died. Dummy coded death variable (0-no; 1-yes);  if yes the character 
#'   dies in the play
#'   \item dialogue. The spoken dialogue 
#'   \item stem.text. 
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name mraja1spl 
#' @usage data(mraja1spl) 
#' @format A data frame with 508 rows and 7 variables 
#' @references 
#' \url{http://shakespeare.mit.edu/romeo_juliet/full.html}
NULL
 
#' Negating Words
#' 
#' A dataset containing a vector of words that negate word meaning.
#' 
#' @details 
#' Valence shifters are words that alter or intensify the meaning of the polarized
#' words and include negators and amplifiers. Negators are, generally, adverbs
#' that negate sentence meaning; for example the word like in the sentence, "I do
#' like pie.", is given the opposite meaning in the sentence, "I do not like
#' pie.", now containing the negator not. Amplifiers are, generally, adverbs or
#' adjectives that intensify sentence meaning. Using our previous example, the
#' sentiment of the negator altered sentence, "I seriously do not like pie.", is
#' heightened with addition of the amplifier seriously.
#' 
#' @docType data 
#' @keywords datasets 
#' @name negation.words 
#' @usage data(negation.words) 
#' @format A vector with 16 elements 
NULL
 
#' Negative Words
#' 
#' A dataset containing a vector of negative words.
#' 
#' @details 
#' A sentence containing more negative words would be deemed a negative sentence,
#' whereas a sentence containing more positive words would be considered positive.
#' 
#' @docType data 
#' @keywords datasets 
#' @name negative.words 
#' @usage data(negative.words) 
#' @format A vector with 4783 elements 
#' @references Hu, M., & Liu, B. (2004). Mining opinion features in customer 
#' reviews. National Conference on Artificial Intellgience. 
#' 
#' \url{http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html}
NULL
 
#' Positive Words
#' 
#' A dataset containing a vector of positive words.
#' 
#' @details 
#' A sentence containing more negative words would be deemed a negative sentence,
#' whereas a sentence containing more positive words would be considered positive.
#' 
#' @docType data 
#' @keywords datasets 
#' @name positive.words 
#' @usage data(positive.words) 
#' @format A vector with 2006 elements 
#' @references Hu, M., & Liu, B. (2004). Mining opinion features in customer 
#' reviews. National Conference on Artificial Intellgience. 
#' 
#' \url{http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html}
NULL
 
#' Preposition Words
#' 
#' A dataset containing a vector of common prepositions.
#' 
#' 
#' @docType data 
#' @keywords datasets 
#' @name preposition 
#' @usage data(preposition) 
#' @format A vector with 162 elements 
NULL
 
#' Romeo and Juliet (Unchanged & Complete)
#' 
#' A dataset containing the original transcript from Romeo and Juliet as it was 
#' scraped from: \url{http://shakespeare.mit.edu/romeo_juliet/full.html}.
#' 
#' @details 
#' \itemize{ 
#'   \item person. Character in the play
#'   \item dialogue. The spoken dialogue 
#'   \item act. The act (akin to repeated measures)
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name raj 
#' @usage data(raj) 
#' @format A data frame with 840 rows and 3 variables 
#' @references 
#' \url{http://shakespeare.mit.edu/romeo_juliet/full.html}
NULL
 
#' Romeo and Juliet: Act 1
#' 
#' A dataset containing Romeo and Juliet: Act 1.
#' 
#' @details 
#' \itemize{ 
#'   \item person. Character in the play
#'   \item dialogue. The spoken dialogue
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name raj.act.1 
#' @usage data(raj.act.1) 
#' @format A data frame with 235 rows and 2 variables 
#' @references 
#' \url{http://shakespeare.mit.edu/romeo_juliet/full.html}
NULL
 
#' Romeo and Juliet: Act 2
#' 
#' A dataset containing Romeo and Juliet: Act 2.
#' 
#' @details 
#' \itemize{ 
#'   \item person. Character in the play
#'   \item dialogue. The spoken dialogue
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name raj.act.2 
#' @usage data(raj.act.2) 
#' @format A data frame with 205 rows and 2 variables 
#' @references 
#' \url{http://shakespeare.mit.edu/romeo_juliet/full.html}
NULL
 
#' Romeo and Juliet: Act 3
#' 
#' A dataset containing Romeo and Juliet: Act 3.
#' 
#' @details 
#' \itemize{ 
#'   \item person. Character in the play
#'   \item dialogue. The spoken dialogue
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name raj.act.3 
#' @usage data(raj.act.3) 
#' @format A data frame with 197 rows and 2 variables 
#' @references 
#' \url{http://shakespeare.mit.edu/romeo_juliet/full.html}
NULL
 
#' Romeo and Juliet: Act 4
#' 
#' A dataset containing Romeo and Juliet: Act 4.
#' 
#' @details 
#' \itemize{ 
#'   \item person. Character in the play
#'   \item dialogue. The spoken dialogue
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name raj.act.4 
#' @usage data(raj.act.4) 
#' @format A data frame with 115 rows and 2 variables 
#' @references 
#' \url{http://shakespeare.mit.edu/romeo_juliet/full.html}
NULL
 
#' Romeo and Juliet: Act 5
#' 
#' A dataset containing Romeo and Juliet: Act 5.
#' 
#' @details 
#' \itemize{ 
#'   \item person. Character in the play
#'   \item dialogue. The spoken dialogue
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name raj.act.5 
#' @usage data(raj.act.5) 
#' @format A data frame with 88 rows and 2 variables 
#' @references 
#' \url{http://shakespeare.mit.edu/romeo_juliet/full.html}
NULL
 
#' Romeo and Juliet Demographics
#' 
#' A dataset containing Romeo and Juliet demographic information for the 
#' characters.
#' 
#' @details 
#' \itemize{ 
#'   \item person. Character in the play
#'   \item sex. Gender
#'   \item fam.aff. Family affiliation of character
#'   \item died. Dummy coded death variable (0-no; 1-yes);  if yes the character 
#'   dies in the play
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name raj.demographics 
#' @usage data(raj.demographics) 
#' @format A data frame with 34 rows and 4 variables 
#' @references 
#' \url{http://shakespeare.mit.edu/romeo_juliet/full.html}
NULL
 
#' Romeo and Juliet Split in Parts of Speech
#' 
#' A dataset containing a list from \code{\link[qdap]{pos}} using the 
#' \code{\link[qdap]{raj}} data set (see \code{\link[qdap]{pos}} for more 
#' information).
#' 
#' @details 
#' \describe{ 
#' \item{text}{The original text} 
#' \item{POStagged}{The original words replaced with parts of speech in context.} 
#' \item{POSprop}{Dataframe of the proportion of parts of speech by row.} 
#' \item{POSfreq}{Dataframe of the frequency of parts of speech by row.} 
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name rajPOS 
#' @usage data(rajPOS) 
#' @format A list with 4 elements 
#' @references 
#' \url{http://shakespeare.mit.edu/romeo_juliet/full.html}
NULL
 
#' Romeo and Juliet (Complete & Split)
#' 
#' A dataset containing the complete dialogue of Romeo and Juliet with turns of 
#' talk split into sentences.
#' 
#' @details 
#' \itemize{ 
#'   \item person. Character in the play
#'   \item sex. Gender
#'   \item fam.aff. Family affiliation of character
#'   \item died. Dummy coded death variable (0-no; 1-yes);  if yes the character 
#'   dies in the play
#'   \item dialogue. The spoken dialogue
#'   \item act. The act (akin to repeated measures) 
#'   \item stem.text. Text that has been stemmed
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name rajSPLIT 
#' @usage data(rajSPLIT) 
#' @format A data frame with 2151 rows and 8 variables 
#' @references 
#' \url{http://shakespeare.mit.edu/romeo_juliet/full.html}
NULL
 
