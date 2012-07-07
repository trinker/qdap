#' Create a directory and place multiple csv files there
#' 
#' Create a directory and place multiple csv files there
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param \dots %% ~~Describe \code{\dots} here~~
#' @param dir %% ~~Describe \code{dir} here~~
#' @param open %% ~~Describe \code{open} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (..., dir = NULL, open = FALSE) 
#' {
#'     x <- match.call(expand.dots = FALSE)
#'     z <- as.character(x[[2]])
#'     x2 <- list(...)
#'     names(x2) <- z
#'     y <- qdap::folder(folder.name = dir)
#'     files <- paste0(y, "/", z, ".csv")
#'     lapply(seq_along(x2), function(i) {
#'         write.table(x2[i], file = files[i], sep = ",", col.names = T, 
#'             row.names = F, qmethod = "double")
#'     })
#'     if (open) {
#'         if (.Platform["OS.type"] == "windows") {
#'             shell.exec(y)
#'         }
#'         else {
#'             system(paste(Sys.getenv("R_BROWSER"), y))
#'         }
#'     }
#'   }
#' 
csv.dump <-
function(..., dir = NULL, open = FALSE){
  x <- match.call(expand.dots = FALSE)
  z <- as.character(x[[2]])
  x2 <- list(...)
  names(x2) <- z
  y <- qdap::folder(folder.name = dir)
  files <- paste0(y, "/", z, ".csv")
  lapply(seq_along(x2), function(i){
    x3 <-x2[i]
    names(x3) <- gsub(names(x2)[i], "", names(x3))
    write.table(x3, file = files[i],  sep = ",", col.names = TRUE, 
                row.names=F, qmethod = "double")
  }
  )
  if(open){
    if (.Platform['OS.type'] == "windows"){
      shell.exec(y)
    } else {
      system(paste(Sys.getenv("R_BROWSER"), y))
    }
  }
}
