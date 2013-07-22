<!--
%\VignetteEngine{knitr}
%\VignetteIndexEntry{A Markdown Vignette with knitr}
-->



# qdap Package Vignette
# Tyler Rinker

qdap (<a href="http://github.com/trinker/qdap">Rinker, 2013</a>) is an R package designed to assist in quantitative discourse analysis. The package stands as a bridge between qualitative transcripts of dialogue and statistical analysis and visualization.  qdap was born out of a frustration with current discourse analysis programs. Packaged programs are a closed system, meaning the researcher using the method has little, if any, influence on the program applied to her data.

R already has thousands of excellent packages for statistics and visualization. qdap is designed to stand as a bridge between the qualitative discourse of a transcript and the computational power and freedom that R offers. As qdap returns the power to the researcher it will also allow the researcher to be more efficient and thus effective and productive in data analysis.  The qdap package provides researchers with the tools to analyze data and more importantly is a dynamic system governed by the data, shaped by theory, and continuously refined by the field.

...if you can dream up an analysis then qdap and R can help get you there.

<div style="width:367.5px;margin:auto;">
    <p><img src="https://dl.dropbox.com/u/61803503/qdap_logo.png" width="350" height="250"></p>
</div>


The following vignette is a loose road map for utilizing the tools provided by qdap.

<hr>
<h3 id="toc">Select from sections below:</h3>

<div style="float: left; width: 50%;">
<ul>
<div>1.  <a href="#project">Starting a New Project</a>
    </div> 
<div>2.  <a href="#import_export">Import/Export Discourse Data</a>
  </div>  
<div>3.  <a href="#tools">Generic qdap Tools</a>
    </div> 
<div>4.  <a href="#cleaning">Cleaning/Preparing the Data</a>
    </div> 
<div>5.  <a href="#viewing">View the Data</a>
    </div> 
<div>6.  <a href="#reshaping">Reshaping the Data</a>
    </div> 
<div>7.  <a href="#word">Extract/Analyze Words</a>
    </div> 
<div>8.  <a href="#coding">Qualitative Coding System</a>
    </div> 
<div>9.  <a href="#counts">Word Counts and Descriptive Statistics</a>
    </div> 
<div>10.  <a href="#measures">Word Measures and Scoring</a>
    </div> 
<div>11.  <a href="#visualization">Visualizing Discourse Data</a>
    </div> 
<div>12.  <a href="#id">ID Sentences</a>
    </div> 


</ul>
</div>
<div style="float: right; width: 50%;">
<ul>
<div><b>Symbol Conventions:</b></div>  
<div><font size="5" color="gold">&diams;</font> = Example (R code)    </div> 
<div><b><font size="5" color="firebrick">[YT]</font></b> = Video Demo (click to watch)    </div> 
</ul>
</div>
<br style="clear:both;"/>




<hr>
<h3 id="project">Starting a New Project</h3>
<h3 id="import_export">Import/Export Discourse Data</h3>
<h3 id="tools">Generic qdap Tools</h3>
<h3 id="cleaning">Cleaning/Preparing the Data</h3>
<h3 id="viewing">View the Data</h3>
<h3 id="reshaping">Reshaping the Data</h3>
<h3 id="word">Extract/Analyze Words</h3>
<h3 id="coding">Qualitative Coding System</h3>
<h3 id="counts">Word Counts and Descriptive Statistics</h3>
<h3 id="measures">Word Measures and Scoring</h3>
<h3 id="visualization">Visualizing Discourse Data</h3>
<h3 id="id">ID Sentences</h3>


<hr>
## Acknowledgements

The qdap package was my first R package and a learning process.  Several people contributed imensely to my learning.  I’d like to particularly thank <a href="https://github.com/Dasonk/" target="_blank">Dason Kurkiewicz</a>
 for his constant mentoring/assistance in learning the R language, GitHub and package developmentas well as collaboration on numerous qdap functions.  Thank you to <a href="https://twitter.com/bryangoodrich" target="_blank">Bryan Goodrich</a>
 for his teaching, feedback and collaboration on serveral qdap functions.  Thank you to <a href="https://github.com/hadley" target="_blank">Dr. Hadley Wickham</a>
 for roxygen2, ggplot2, devtools and GitHub repos which I referenced often.  I'd also like to thank the many folks at <a href="http://www.talkstats.com/" target="_blank">talkstats.com</a>
 and <a href="http://stackoverflow.com/questions/tagged/r" target="_blank">stackover<U+FB02>ow.com</a>
 for their help in answering many R questions related to qdap.

## Improvements

If the reader spots an error in this Vignette or would like to suggest an improvement please contact me Tyler Rinker&lt;<a href="mailto:tyler.rinker@gmail.com" target="_blank">tyler.rinker@gmail.com</a>
&gt;.  To submit bug reports and feature requests related to the qdap package please visit <a href="https://github.com/trinker/qdap/issues?state=open" target="_blank">qdap's GitHub issues page</a>
.

<hr> 

*<em><font size="3">Vignette created with the reports package (<a href="http://github.com/trinker/reports">Rinker, 2013b</a>)</font><em>





## References


