# get the email adresses of project members
#' @param x Path to CONTACT_INFO.
#' @param all logical. If TRUE emails the lead and members
#' @param cc logical. If TRUE carbon copy to the analyst/repo owner.
#' @param copy2clip logical. If TRUE attempts to copy the output to the clipboard.
.loc1 <- paste0(getwd(),"/CORRESPONDENCE/CONTACT_INFO")

email <- function(x = .loc1, all = TRUE, cc = TRUE, copy2clip = TRUE) {
    info <- suppressWarnings(readLines(x))
    analyst <- which(grepl("ANALYST:", info))
    sel <- which(grepl("CLIENT/LEAD RESEARCHER:", info))
    other <- 1:length(info)
    other <- other[!other %in% c(analyst, sel)]
    if (cc) {
        sel <- c(sel, analyst)
    }
    if (all) {
        sel<- c(sel, other)
    }
    info <- info[sel]
    emails <- unique(unlist(bracketXtract(info, bracket = "angle")))
    emails <- paste(emails[grepl("@", emails)], collapse = "; ")
    if(copy2clip){
        if (Sys.info()["sysname"] == "Windows") {
            writeClipboard(emails, format = 1)
        }
        if (Sys.info()["sysname"] == "Darwin") {
            j <- pipe("pbcopy", "w")
            writeLines(emails, con = j)
            close(j)
        }
    }
    cat(c(emails, "\n"))
}

# Report items from the to do list
#' @param x Path to CONTACT_INFO.
#' @param report.completed logical. If FALSE completed tasks are not completed.
#' @param copy2clip logical. If TRUE attempts to copy the to do tasks to the clipboard.
.loc2 <- paste0(getwd(),"/TO_DO")

todo <- function(x = .loc2, report.completed = FALSE, copy2clip = TRUE) {
    info <- suppressWarnings(readLines(x))
    info <- info[!grepl("#", info)]
    info <- info[info != ""]
    starts <- which(substring(info, 1, 1) != " ")
    ends <- c(starts[-1]-1, length(info))
    spt <- split(info, rep(seq_along(ends), (ends-starts) + 1))
    comp <- which(substring(info[starts], 1, 1) == "-")
    to.do <- seq_along(spt)[!seq_along(spt) %in% comp]
    cat("TASKS TO COMPLETE:\n\n")
    invisible(lapply(spt[to.do], function(x) { 
        cat(paste(x, collapse = "\n"))
        cat("\n\n")
    }))
    if (report.completed && !identical(comp, integer(0))) {
        cat("COMPLETED:\n\n")
        spt2 <- lapply(spt[comp], function(x) {
            x[1] <- substring(x[1], 2)
            x
        })
        invisible(lapply(spt2, function(x) { 
            cat(paste(x, collapse = "\n"))
            cat("\n\n")
        }))
    }
    z <- spt[to.do]
    for (i in head(seq_along(z), -1)) {
        z[[i]] <- c(z[[i]], "")
    }
    z <- matrix(unlist(z), ncol = 1)
    if(copy2clip){
        if (Sys.info()["sysname"] == "Windows") {
            writeClipboard(z, format = 1)
        }
        if (Sys.info()["sysname"] == "Darwin") {
            j <- pipe("pbcopy", "w")
            writeLines(z, con = j)
            close(j)
        }
    }
}