#' Generate Random Dialogue Data
#' 
#' \code{random_sent} - Generates a random sample of sentences (sentences are 
#' sampled at the word level and there for are likely nonsensical).
#' 
#' @param n Number of sentences to create.
#' @param len Average length of sentences (in words).
#' @param range Range around \code{len} that number of words may vary.  This may 
#' be a recycled single integer vector or an integer vector of length 2.
#' @param dictionary A dictionary of words to sample from.
#' @param endmark.fun A function to create random end marks. 
#' @return \code{random_sent} - Returns a random vector of sentence strings.
#' @keywords sample random sentence
#' @export
#' @rdname random_data
#' @examples
#' \dontrun{
#' random_sent()
#' random_sent(200, 10)
#' 
#' dict <- sort(unique(bag_o_words(pres_debates2012[["dialogue"]])))
#' random_sent(dictionary=dict)
#' 
#' random_data()
#' random_data(ages = seq(10, 20, by = .5))
#' random_data(50) %&% word_stats(person)
#' random_data(100) %&% word_stats(list(race, sex))
#' random_data(dictionary = dict)
#' }
random_sent <- function(n =10, len = 14, range = len - 1, 
    dictionary = qdapDictionaries::Top200Words, 
    endmark.fun = function() sample(c(".", "!", "|", "?"), 1, 
        prob=c(.85, .05, .05,  .05))){

    x <- seq(len - utils::tail(range, 1), len + utils::head(range, 1), by = 1)
    lens <- replicate(n, max(1, sample(x, 1)))

    sapply(lens, function(x) {
        Caps(paste0(
            unbag(sample(dictionary, x, replace=TRUE)), 
            endmark.fun()
        ))
    })
}

#' Generate Random Dialogue Data
#' 
#' \code{random_data} - Generate random dialogue, people, and demographic 
#' variables
#' 
#' @param \ldots Other arguments passed to \code{random_sent}
#' @param n.people An integer of the number of people to include in the sample 
#' (number of people is sampled from; if \code{n} is smaller not all people may 
#' be included).
#' @param ages The possible ages to choose from (numeric).
#' @param people.names A vector of names to choose from at least as large as 
#' \code{n.people}.
#' @return \code{random_data} - Returns a \code{\link[base]{data.frame}} of
#' people, dialogue, and demographic variables of the class \code{sent_split}.
#' @export
#' @rdname random_data
random_data <- function(n = 10, ..., n.people = 10, ages = 7:10, 
    people.names = unique(tolower(qdapDictionaries::NAMES[[1]]))) {

    m <- random_sent(n = n, ...)
    len <- length(m)

    races <- structure(list(race = c("White", "Black", "Native", "Asian", 
        "Hawaiian", "Bi-Racial", "Other", "Hispanic"), percent = c(0.637, 
        0.122, 0.007, 0.047, 0.0015, 0.019, 0.002, 0.163)), .Names = c("race", 
        "percent"), row.names = c(NA, -8L), class = "data.frame")  

    nms <- sample(people.names, n.people)
    
    output <- data.frame(
        person = nms,
        sex = name2sex(nms),
        age = sample(ages, n.people, replace=TRUE),
        race = sample(races[[1]], n.people, replace=TRUE, prob = races[[2]]),  
        stringsAsFactors = FALSE 
    )

    output <- output[sample(seq_len(n.people), len, TRUE), ]
    output[["dialogue"]] <- m
    rownames(output) <- NULL

    text.var <- "dialogue"
    # removed on 10/1 per tbl_df no longer returning vector for df[, 1]
    # output <- dplyr::tbl_df(output)
    class(output) <- unique(c("sent_split", "qdap_df", paste0("sent_split_text_var:", 
        text.var), class(output)))
    attributes(output)[["text.var"]] <- text.var
    attributes(output)[["qdap_df_text.var"]] <- substitute(text.var)
    output
}


