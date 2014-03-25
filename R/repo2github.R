#' Upload a Local Repo to GitHub 
#' 
#' Allows uploading a local repository to GitHub without first creating the 
#' repository in the clouds. \code{\link[qdap]{repo2github}} is designed for 
#' the initial push to github.  Future pushes can be handled via RStudio or 
#' other git interface.
#' 
#' @param password GitHub user password (character string).  If this is not 
#' supplied the user will be prompted to enter a password.
#' @param project.dir The path to the root directory of the report/presentation.
#' @param repo A character string naming the repo; default attempts to use the 
#' report project directory name.
#' @param github.user GitHub user name (character string).
#' @param gitpath Path to the location of git.  If \code{NULL} 
#' \code{repo2github} will attempt to locate the path if necessary.
#' @param readme logical.  If \code{TRUE} repo initializes with a README.md file.
#' @return Creates GitHub repository.
#' @author Simon O'Hanlon, Daniel Chaffiol, and Tyler Rinker <tyler.rinker@@gmail.com>
#' @references \url{http://stackoverflow.com/a/15047013/1000343} \cr
#' \url{http://stackoverflow.com/a/18692400/1000343}
#' @section Warning: For Windows users this function creates a temporary _netrc 
#' file in the home directory and attempts to delete this file.  The _netrc 
#' contains username and password information for github. 
#' \code{\link[qdap]{repo2github}} attempts to delete this file but care 
#' should be taken.  The file is created in: \cr
#' \code{file.path(Sys.getenv()["HOME"], "DELETE_ME_REPORTS_PACKAGE/_nectrc")}.
#' @details The arguments \code{project.dir} and \code{repo} use 
#' \code{\link[base]{getwd}}.  This assumes is the current working directoy is 
#' the root directory and is done for convenience.  The user should ensure that 
#' either their working directory is the root directory or supply the correct 
#' root directory/name to these arguments.
#' @note The user will need to have a \href{https://github.com/}{GitHub} account 
#' established.
#' @section Suggestion: The user may want to set \code{\link[base]{options}} for 
#' \code{github.user} in the user's primary \code{.Rprofile}.
#' @export
#' @examples
#' \dontrun{
#' repo2github()
#' }
repo2github <- function(password, project.dir = getwd(), 
    repo = basename(getwd()), github.user = getOption("github.user"), 
	gitpath = NULL, readme = TRUE) {

	#check for github user name
    if (is.null(github.user)) {	
        message("Enter [GitHub username] and press [Enter] to continue")
        github.user <- readLines(n=1)
    }	
	
    #check for password
    if (missing(password)) {	
        message("Enter [GitHub password] and press [Enter] to continue")
        password <- readLines(n=1)
    }
    
    OSiswin <- Sys.info()["sysname"] != "Windows"
    
    ## .gitignore content
    GI <- paste("# History files\n.Rhistory\n\n# Example code in package build",
        sprintf("process\n*-Ex.R\n\n.Rprofile\n.Rproj.user\n%s.Rproj\n", repo))
    
    #Create the repo
    if (OSiswin) {
        gitpath <- "git"
        cmd1 <- paste0("curl -u '", github.user, ":", password, 
            "' https://api.github.com/user/repos -d '{\"name\":\"", repo, "\"}'")
        
        ## Make .gitignore
        cat(GI, file=file.path(project.dir, ".gitignore"))
        cat(sprintf("%s\n===", repo), file=file.path(project.dir, "README.md"))    
        
    } else {
        if (is.null(gitpath)){  
            test <- c(file.exists("C:/Program Files (x86)/Git/bin/git.exe"),
                file.exists("C:/Program Files/Git/bin/git.exe"))
            if (sum(test) == 0) {
                stop("Git not found.  Supply path to 'gitpath'")    
            }
            gitpath <- c("C:/Program Files (x86)/Git/bin/git.exe",
                "C:/Program Files/Git/bin/git.exe")[test][1]
        }
        url <- "http://curl.askapache.com/download/curl-7.23.1-win64-ssl-sspi.zip"
        tmp <- tempfile( fileext = ".zip" )
        download.file(url,tmp)
        unzip(tmp, exdir = tempdir())       
        system(paste0(tempdir(), "/curl http://curl.haxx.se/ca/cacert.pem -o " , 
            tempdir() , "/curl-ca-bundle.crt"))
     	json <- paste0(" { \"name\":\"" , repo , "\" } ") #string we desire formatting
	    json <- shQuote(json , type = "cmd" )
        cmd1 <- paste0( tempdir() ,"/curl -i -u \"" , github.user , ":" , password , 
            "\" https://api.github.com/user/repos -d " , json )
        
        ## Make .gitignore and README
        cat(GI, file=file.path(project.dir, ".gitignore"))     
    	if (!file.exists(file.path(project.dir, "README.md"))) {
            cat(sprintf("%s\n===", repo), file=file.path(project.dir, "README.md"))     
    	}
	
    }
    system(cmd1)  
    
    ## push the directory to github
    if (is.null(project.dir)) stop("\"project.dir\" must be supplied")
    
    if (!OSiswin) {    

        wd <- getwd()
        setwd(project.dir)
        cmd2 <- paste0(shQuote(gitpath), " init")
        system(cmd2, intern = TRUE)
        cmd3 <- paste0(shQuote(gitpath), " add .")  
        system(cmd3, intern = TRUE)       
        
        ## Set email
        x <- file.path(path.expand("~"), ".gitconfig")
        if (file.exists(x)) {
            y <- readLines(x)
            email <- Trim(unlist(strsplit(y[grepl("email = ", y)], "email ="))[2])
        } else {
            z <- file.path(Sys.getenv("HOME"), ".gitconfig")
            if (file.exists(z)) {
                email <- Trim(unlist(strsplit(y[grepl("email = ", y)], "email ="))[2])
            } else {
                warning(paste("Set `email` in", x))
            }
        }
        cmdEM <- paste0(shQuote(gitpath), sprintf(" config --global user.email %s", email))        
        system(cmdEM)
        
        ## Initial commit
        cmd4 <- paste0(shQuote(gitpath), ' commit -m "Initial commit"')  
        system(cmd4, intern = TRUE) 

        ## add a new remote
        cmd5 <- paste0(shQuote(gitpath), " remote add origin https://github.com/",
            github.user, "/", repo, ".git")  
        system(cmd5, intern = TRUE) 
        
        #Make a temp _netrc file
        home <- Sys.getenv()["HOME"]
        newhome <- file.path(home, paste0("DELETE_ME_", gsub(":", "\\.", 
            gsub("'", "", gsub("\\s+", "_", Sys.time())))))
        dir.create(newhome)
        loc <- file.path(newhome, "_netrc")
        on.exit(Sys.setenv(HOME = home))
        Sys.setenv(HOME = newhome)
        netrc <- sprintf("machine github.com\nlogin %s\npassword %s\nprotocol https", 
            github.user, password)
        cat(netrc, file=loc)

        ## Push the repo to github
        cmd6 <- paste0(shQuote(gitpath), " push -u origin master")  
        system(cmd6, intern = TRUE) 
        
        setwd(wd)
        
        ## Delete the _netrc file
        unlink(loc, recursive = TRUE, force = FALSE)
        unlink(newhome, recursive = TRUE, force = FALSE)

        if (file.exists(loc)) {
            warn <- paste("For Windows users this function creates a temporary", 
                "_netrc\nfile in the temp directory and attempts to delete this", 
            	"file.\nThe _netrc contains username and password information for", 
                "github.\n\nThis file was created:\n", loc, "\n\nThe results of",
            	"file.exists(loc) is:", file.exists(loc), "\n\nIf TRUE delete file",
                "manually.\n\nThe file can be found via:\n", loc)
            warning(warn)
        }
    } else {
        system( paste0( "cd ", project.dir , " && " , gitpath , " init" ) )
        system( paste0( "cd ", project.dir , " && " , gitpath , " add \\." ) )
        system( paste0( "cd ", project.dir , " && " , gitpath , 
            " commit -m \"Initial commit\"" ) )
        system( paste0( "cd ", project.dir , " && " , gitpath, 
            " remote add origin https://github.com/", github.user, "/", repo, ".git") ) 
    }
    message(sprintf("%s pushed to github", repo))
} 
