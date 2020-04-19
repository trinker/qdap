#' Time Span Code Sheet
#'
#' Generates a time span coding sheet and coding format sheet.
#' 
#' @param codes List of codes.
#' @param grouping.var The grouping variables.  Also takes a single grouping 
#' variable or a list of 1 or more grouping variables. 
#' @param start A character string in the form of "00:00" indicating start time 
#' (default is ":00").
#' @param end A character string in the form of "00:00" indicating end time.
#' @param file A connection, or a character string naming the file to print to 
#' (.txt or .doc is recommended).
#' @param coding logical.  If \code{TRUE} a coding list is provided with the 
#' time span coding sheet.  \code{coding} is ignored if \code{end = NULL}.
#' @param print logical.  If \code{TRUE} the time spans are printed to the 
#' console.
#' @references Miles, M. B. & Huberman, A. M. (1994). An expanded sourcebook: 
#' Qualitative   data analysis. 2nd ed. Thousand Oaks, CA: SAGE Publications.
#' @keywords coding
#' @seealso 
#' \code{\link[qdap]{cm_range.temp}},
#' @export
#' @importFrom qdapTools pad
#' @examples
#' \dontrun{
#' ## cm_time.temp(qcv(AA, BB, CC), ":30", "7:40", file = "foo.txt")
#' ## delete("foo.txt")
#' cm_time.temp(qcv(AA, BB, CC), ":30", "7:40")
#' 
#' x <- list(
#'     transcript_time_span = qcv(terms="00:00 - 1:12:00"),
#'     A = qcv(terms="2.40:3.00, 5.01, 6.52:7.00, 9.00"),
#'     B = qcv(terms="2.40, 3.01:3.02, 5.01, 6.52:7.00, 9.00, 1.12.00:1.19.01"),
#'     C = qcv(terms="2.40:3.00, 5.01, 6.52:7.00, 9.00, 17.01")
#' )
#' cm_time2long(x)
#' cm_time.temp(qcv(AA, BB, CC))
#' }
cm_time.temp <-
function(codes, grouping.var = NULL, start = ":00", end = NULL, file=NULL, 
    coding = FALSE, print = TRUE) {
    if (Sys.info()["sysname"] != "Windows") {
        writeClipboard <- NULL
    }  

    if (!is.null(end)) {
        start <- hms2ms(start)
        end <- hms2ms(end)
    }

    if (!is.null(grouping.var)) {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            G <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
        } else {
            G <- as.character(substitute(grouping.var))
            G <- G[length(G)]
        }

        lvs <- lapply(grouping.var, unique)
        if (missing(codes)) {
            codes <- NULL
        }
        codes <- c(unlist(lapply(seq_along(G), function(i) {
            paste(G[i], lvs[[i]], sep="_")
        })), codes)
    }

    if (!is.null(end)) {
        wid <- options()$width
        wdth <- options()[["width"]]
        on.exit(options(width = wdth))
        options(width=1000)

        if (!missing(codes)) {

            x1 <- matrix(c("list(", 
                "    transcript_time_span = qcv(terms=\"00:00 - 00:00\"),", 
                paste0("    ", codes[1:(length(codes)-1)], " = qcv(terms=\"\"),"),
                paste0("    ", codes[length(codes)], " = qcv(terms=\"\")"),
                ")"), ncol = 1)

        } else {
            x1 <- NULL
        }

        st <- unlist(strsplit(start, ":"))
        en <- as.numeric(unlist(strsplit(end, ":")))
        st[1] <- ifelse(st[1]=="", "0", st[1])
        st <- as.numeric(st)
        x <- (en[1] - st[1]) + 1
        z <- matrix(rep(0:59, x), nrow = x, byrow = TRUE)
        rownames(z) <- c(paste0("[", st[1]:en[1], "]"))
        colnames(z) <- rep("", ncol(z))
        if (st[2] > 0) {
            z[1, 0:(st[2])] <- NA
        }
        if (en[2] < 59) {
            z[x, (en[2] + 2):60] <- NA
        }
        zz <- matrix(utils::capture.output(print(z, na.print=""))[-1], ncol =1)
        if (print){
            print(z, na.print=""); message("\n")
        }
        if (coding) {
            message(paste0("list(\n",
                "    transcript_time_span = qcv(terms=\"00:00 - 00:00\"),\n",
                paste0("    ", paste0(paste(codes, 
                collapse = " = qcv(terms=\"\"),\n    "), " = qcv(terms=\"\")")), "\n)"))
        }
        dimnames(zz) <- list(c(rep("", x)), c(""))
        if (Sys.info()["sysname"] == "Windows") {
            writeClipboard(noquote(rbind(zz, "", "", x1)), format = 1)                        
        }                                                        
        if (Sys.info()["sysname"] == "Darwin") {                 
            j <- pipe("pbcopy", "w")                             
            writeLines(noquote(rbind(zz, "", "", x1)), con = j)                               
            close(j)  
        }
        if (!is.null(file)) { 
            v <- paste0(zz, "\n")
            cat(v[1], file=file)   
            lapply(2:x, function(i) cat(v[i], file=file, append = TRUE)) 
            if (coding) {
                cat(paste0("\nlist(\n",
                    "    transcript_time_span = qcv(terms=\"00:00 - 00:00\"),\n",
                    paste0("    ", paste0(paste(codes, 
                        collapse = " = qcv(terms=\"\"),\n    "), " = qcv(terms=\"\")")), 
                        "\n)\n"), file = file, append = TRUE) 
            }
        }   
        options(width=wid)
    } else {
        zz <- x1 <- matrix(c("list(", 
            "    transcript_time_span = qcv(terms=\"00:00 - 00:00\"),", 
            paste0("    ", codes[1:(length(codes)-1)], " = qcv(terms=\"\"),"),
            paste0("    ", codes[length(codes)], " = qcv(terms=\"\")"),
            ")"), ncol = 1)
        dimnames(zz) <- list(c(rep("", nrow(zz))), c(""))
        print(noquote(zz))
        if (Sys.info()["sysname"] == "Windows") {
            writeClipboard(noquote(x1), format = 1)                        
        }                                                        
        if (Sys.info()["sysname"] == "Darwin") {                 
            j <- pipe("pbcopy", "w")                             
            writeLines(noquote(x1), con = j)                               
            close(j)  
        }
        if (!is.null(file)) {                                    
            cat(paste0("list(\n",
                "    transcript_time_span = qcv(terms=\"00:00 - 00:00\"),\n",
                paste0("    ", paste0(paste(codes, 
                collapse = " = qcv(terms=\"\"),\n    "), " = qcv(terms=\"\")")), 
                "\n)\n"), file = file, append = TRUE) 
        } 
    }
}

hms2ms <- 
function(x) {
    hms <- as.character(x)
    op <- FALSE
    if (length(hms) == 1) {
        hms <- c(hms, "00:00:00")
        op <- TRUE  
    }
    spl <- strsplit(hms, ":")
    spl2 <- lapply(spl, function(x) {
       if (length(x) == 1) {
           if (x[1] == "") {
               stop("An element is blank")
           } 
           x <- c(rep("00", 2), x[1])
       }        
       if (length(x) == 2) {
           if (x[1] == "") {
               x <- c(rep("00", 2), x[2])
           } else {
               x <- c(rep("00", 1), x[1:2])
           }
       }
       if (x[1] == "") {
           x <- c(rep("00", 1), x[1:2])
       } 
       x
    })

    DF <- sapply(data.frame(do.call(rbind, spl2)), function(x){
        as.numeric(as.character(x))
    })

    cmb <- apply(cbind(DF[, 1]*60 + DF[, 2], DF[, 3]), 2, pad, 2, sort=FALSE)
    
    out <- paste2(cmb, sep=":")
    if (op) {
        out <- out[1]
    }
    out
}
