.onLoad <- function(libname = find.package("qdap"),        
    pkgname = "qdap") {                                    
#     if (!"pacman" %in% .packages(all.available = TRUE)) {  
#         install.packages("devtools")                       
#         library(devtools)                                  
#         install_github("pacman", "trinker")                
#     }   
browser()                                                   
    if (!"openNLP" %in% .packages(all.available = TRUE)) { 
        install.packages("openNLP", type = "source")       
    }                                                      
    require(pacman)                                        
    if (!"RCurl" %in% .packages(all.available = TRUE)) {     
        URL <- "http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/2.15/"
        install.packages("RCurl", contriburl = URL)                                
    }                                                      
    if (!"XML" %in% .packages(all.available = TRUE)) {     
        URL <- "http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/2.15/"
        install.packages("XML", contriburl = URL)                       
    }                                                      
    require(openNLP)                                       
    require(XML)                                           
    require(RCurl)                                         
} 
#, openNLP, XML, RCurl