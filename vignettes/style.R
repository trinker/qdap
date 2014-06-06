options(rstudio.markdownToHTML = 
  function(inputFile, outputFile) {      
    require(markdown)
    markdownToHTML(inputFile, outputFile, stylesheet='C:/Users/trinker/GitHub/qdap/vignettes/css/style.css')   
  }
)