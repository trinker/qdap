#' qdap: Quantitative Discourse Analysis Package
#'
#' This package automates many of the tasks associated with quantitative 
#' discourse analysis of transcripts containing discourse.  The package 
#' provides parsing tools for preparing transcript data, coding tools and 
#' analysis tools for richer understanding of the data.  Many functions 
#' allow the user to aggregate data by any number of grouping variables, 
#' providing analysis and seamless integration with other R packages which 
#' enable higher level analysis and visualization of text.  This empowers 
#' the researcher with more flexible, efficient and targeted methods and tools.
#'
#' @docType package
#' @name qdap
#' @aliases qdap package-qdap
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
 
#' Fictitious Split Sentence Classroom Dialogue
#' 
#' A \code{\link[qdap]{sentSplit}} version of the \code{\link[qdap]{DATA}} 
#' dataset.
#' 
#' @details 
#' \itemize{ 
#'   \item person. Speaker
#'   \item tot. Turn of talk with sub sentences
#'   \item TOT. Turn of talk
#'   \item sex. Gender
#'   \item adult. Dummy coded adult (0-no; 1-yes)
#'   \item code. Dialogue coding scheme 
#'   \item state. Statement (dialogue)
#'   \item stem.text. A stemmed version of the text.var
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name DATA.SPLIT 
#' @usage data(DATA.SPLIT) 
#' @format A data frame with 15 rows and 8 variables 
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

#' 2012 U.S. Presidential Debates
#' 
#' A dataset containing a clenaed version of all three presidential debates for 
#' the 2012 election. 
#' 
#' @details 
#' \itemize{ 
#'   \item person. The speaker
#'   \item tot. Turn of talk
#'   \item dialogue. The words spoken
#'   \item time. Variable indicating which of the three debates the dialogue is from
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name pres_debates2012 
#' @usage data(pres_debates2012) 
#' @format A data frame with 2912 rows and 4 variables 
NULL
 
#' First 2012 U.S. Presidential Debate
#' 
#' A dataset containing the raw version of the first presidential debate.
#' 
#' @details 
#' \itemize{ 
#'   \item person. The speaker
#'   \item dialogue. The words spoken 
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name pres_debate_raw2012 
#' @usage data(pres_debate_raw2012) 
#' @format A data frame with 94 rows and 2 variables 
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
 
#' Minimal Time Span Data Set
#' 
#' A ficticious dataset containing time spans for codes A and B.
#' 
#' @details 
#' \itemize{ 
#'   \item code. The qualitative code.
#'   \item start. The integer start time.
#'   \item end. The integer end time. 
#'   \item Start. The chron start time.
#'   \item End. The chron end time.
#'   \item variable. An arbitrary single time repeated measures variable (ignore).
#' } 
#' 
#' @docType data 
#' @keywords datasets 
#' @name sample.time.span 
#' @usage data(sample.time.span) 
#' @format A data frame with 9 rows and 6 variables 
NULL
