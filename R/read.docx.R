#helper function for read.transcript (not exported)
read.docx <-
function(file, skip = 0, sep = ":") {
    tmp <- tempfile()
    if (!dir.create(tmp))
      stop("Temporary directory could not be established.")
    utils::unzip(file, exdir = tmp)  # Unzip to temporary directory
    xmlfile <- file.path(tmp, "word", "document.xml")  # Path to xml document
    doc     <- xmlTreeParse(xmlfile, useInternalNodes=TRUE)  # Import XML
    unlink(tmp, recursive = TRUE)  # Delete unzipped files; no longer needed
    nodeSet <- getNodeSet(doc, "//w:p")  # Access all p-nodes in document
    pvalues <- sapply(nodeSet, xmlValue)  # Return their (textual) values
    pvalues <- pvalues[pvalues != ""]  # Remove empty lines
    if (skip > 0) pvalues <- pvalues[-seq(skip)]  # Ignore these many lines
    keys    <- sapply(gregexpr(paste0("^.*?", sep), pvalues), function(x) x > 0)
    speaker <- regmatches(pvalues, gregexpr(paste0("^.*?", sep), pvalues))
    pvalues <- gsub(paste0("^.*?", sep), "", pvalues)  # Remove speaker from lines
    speaker <- rep(speaker[which(keys)], diff(c(which(keys), length(speaker)+1)))
    speaker <- unlist(speaker)  # Make sure it's a vector
    speaker <- substr(speaker, 1, nchar(speaker)-nchar(sep)) # Remove ending colon
    transcript <- data.frame(X1 = speaker, 
        X2 = pvalues, stringsAsFactors = FALSE)
    return(transcript)
}
