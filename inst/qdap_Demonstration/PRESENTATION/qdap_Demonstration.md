---
title      : qdap Demonstration
subtitle   : ...if you can dream up an analysis then qdap and R can help get you there
author     : Tyler Rinker
job        : University at Buffalo
logo       : qdap_logo_clipped_rev_2.png
biglogo    : 
license    : 
framework  : io2012
highlighter: highlight.js
hitheme    : tomorrow
mode       : standalone
widgets    : [mathjax, quiz, bootstrap]
--- &twocol


<style>
body {
  background-color: #000;
}
.quiz-option label{
  display: inline;
  font-size: 1em;
}
.refs {
  padding-left: 80px;
  text-indent: -35px;
}
ul.nav li::before { content: ""; } 
ul.nav li{ font-size: 18px; line-height: 24px;}
</style>



## About Me ##


*** =right

</br></br>
- Former First Grade Teacher
- Literacy PhD Student 
- Quantitatively Bent
- Accidental Programmer


*** =left

<div style="width:367.5px;margin:auto;">
    <p><img src="assets/img/norah.png" width="350" height="233"></p>
</div>


*** =pnotes

## Research Interests

- Engagement, Motivation and Feedback
- Multimodal Analysis
- Discourse Analysis
- Data Visualization
- Improving Research Methods

--- &twocol

## Why R?


*** =left

</br></br>
- Cutting Edge    
- Powerful
- Visualization  

</br></br></br></br> &nbsp;&nbsp;&nbsp;&nbsp; *Everything can be quantified...*


*** =right

</br></br>
<div style="width:367.5px;margin:auto;">
    <p><img src="assets/img/r.jpg" width="350" height="233"></p>
</div>




--- {class: class, tpl: tabs}

## Why qdap?

*** {class: active, id: qdap}

<div style="width:567px;margin:auto;">
    <p><img src="assets/img/whyqdap.png" width="540" height="360"></p>
</div>


*** {id: Birth}

## Frustration
<div style="width:420px;margin:auto;">
    <p><img src="assets/img/fustration.jpg" width="400" height="267"></p>
</div>


*** {id: Affordances}

<h2> Affordances <img src="assets/img/dollar.jpg" width="100"></h2>

- Fexible    
- Bridge to <a href="http://cran.r-project.org/">5087</a> + <a href="http://www.bioconductor.org/">749</a> + <a href="https://github.com">?</a> <font color="#DBDBDB">= <sup>+</sup>5836</font>

- Dynamic <font color="#DBDBDB">(Field Driven)</font>      
- Data Focused      



---


## Recomendations
<br>
>- <a href="http://www.rstudio.com/" target="_blank"><img src="assets/img/logo_rstudio.jpg" width="200" height="133"></a>
         
>- <a href="https://github.com/" target="_blank"><img src="assets/img/github-social.png" width="200" height="133"></a>
          
>- <a href="http://yihui.name/knitr/" target="_blank"><img src="assets/img/knitr.png" width="200" height="133"></a>
 &#43; <a href="http://en.wikipedia.org/wiki/LaTeX" target="_blank"><img src="assets/img/latex.png" width="160" height="107"></a>
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <font size="2.5"><em><a href="C:/Users/trinker/Desktop/proximity/REPORT/proximity_measure.Rnw">.Rmd</a> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="C:/Users/trinker/Desktop/proximity/REPORT/proximity_measure.pdf">.pdf</a>
</em></font>
>- <a href="http://docs.ggplot2.org/current/" target="_blank"><img src="assets/img/ggplot.png" width="200" height="133"></a>
 <br><br><br><em><font size="2.5">press p</font></em>


*** =pnotes

<div style="width:567px;margin:auto;">
    <p><img src="assets/img/ggplot2demo.png" width="540" height="360"></p>
</div>


--- 

# <center>Let's Dig In </center>
<br>
<div style="width:567px;margin:auto;">
    <p><img src="assets/img/dig.jpg" width="540" height="360"></p>
</div>


*** =pnotes

## Agenda

1. Installing qdap
2. Projects
3. Read In Data
4. Word Counts and Descriptive Statistics
6. Word Measures and Scoring
7. Qualitative Coding System
8. Visualizing Discourse Data
9. Discussion

---

## Installing qdap

<pre><code class="r"># install.packages("devtools", eval = FALSE)
library(devtools)
install_github(c("slidify", "slidifyLibraries"), "ramnathv", ref = "dev")
install_github("knitcitations", "cboettig")
install_github(c("reports", "qdapDictionaries", "qdap"), "trinker")
install_github("ggthemes", "jrnold")
install.packages("scales")
 
invisible(lapply(c("qdap", "ggplot2", "ggthemes", "scales", "grid"), 
    require, character.only = TRUE))
</code></pre>

</br></br>

<a href="https://github.com/trinker/qdap"><font size="3" color="#DBDBDB"><a href="https://github.com/trinker/qdap">https://github.com/trinker/qdap</a></font></a>


--- .YT yt:chQlpEj8g2Q &youtube 

## Projects

*** =pnotes

<div style="text-align:center;">
     <iframe src="vid1.html" width="640" height="360">Your browser does not support iframes.</iframe>
</div>


--- 

## General qdap Function Format
<br>
<pre><code>Function(Text_Variable, list(Grouping_Variables))
</code></pre>

</br></br>

<pre><code>with(Data_Set, Function(Text_Variable, list(Grouping_Variables)))
</code></pre>

--- 

## Read In Data

- Word (docx)
- Text (txt)
- Excel (csv/xlsx)

--- 

## Read In Data

<div style="width:892.5px;margin:auto;">
    <p><img src="assets/img/transcript.png" width="850" height="567"></p>
</div>


--- 

## Read In Data


```r
dat1 <- read.transcript(doc1)
truncdf(dat1, 50)
```

```
                 X1                                                 X2
1      Researcher 2                                   October 7, 1892.
2         Teacher 4 Students it's time to learn. [Student discussion; 
3 Multiple Students                  Yes teacher we're ready to learn.
4     [Cross Talk 3                                                00]
5         Teacher 4 Let's read this terrific book together. It's calle
```


</br></br>
Plenty of parsing tools to clean up!!!

---

## Our Data Set


```r
DATA
```


<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Wed Dec 18 00:43:29 2013 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> person </TH> <TH> sex </TH> <TH> adult </TH> <TH> state </TH> <TH> code </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> sam </TD> <TD> m </TD> <TD align="right">   0 </TD> <TD> Computer is fun. Not too fun. </TD> <TD> K1 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD> greg </TD> <TD> m </TD> <TD align="right">   0 </TD> <TD> No it's not, it's dumb. </TD> <TD> K2 </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> teacher </TD> <TD> m </TD> <TD align="right">   1 </TD> <TD> What should we do? </TD> <TD> K3 </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD> sam </TD> <TD> m </TD> <TD align="right">   0 </TD> <TD> You liar, it stinks! </TD> <TD> K4 </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD> greg </TD> <TD> m </TD> <TD align="right">   0 </TD> <TD> I am telling the truth! </TD> <TD> K5 </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD> sally </TD> <TD> f </TD> <TD align="right">   0 </TD> <TD> How can we be certain? </TD> <TD> K6 </TD> </TR>
  <TR> <TD align="right"> 7 </TD> <TD> greg </TD> <TD> m </TD> <TD align="right">   0 </TD> <TD> There is no way. </TD> <TD> K7 </TD> </TR>
  <TR> <TD align="right"> 8 </TD> <TD> sam </TD> <TD> m </TD> <TD align="right">   0 </TD> <TD> I distrust you. </TD> <TD> K8 </TD> </TR>
  <TR> <TD align="right"> 9 </TD> <TD> sally </TD> <TD> f </TD> <TD align="right">   0 </TD> <TD> What are you talking about? </TD> <TD> K9 </TD> </TR>
  <TR> <TD align="right"> 10 </TD> <TD> researcher </TD> <TD> f </TD> <TD align="right">   1 </TD> <TD> Shall we move on?  Good then. </TD> <TD> K10 </TD> </TR>
  <TR> <TD align="right"> 11 </TD> <TD> greg </TD> <TD> m </TD> <TD align="right">   0 </TD> <TD> I'm hungry.  Let's eat.  You already? </TD> <TD> K11 </TD> </TR>
   </TABLE>


---

## Word Counts and Descriptive Statistics

1.  Word Frequency Matrix
2.  Word Stats
3.  Term Counts
4.  Question Types
5.  Parts of Speech     
6.  Syllablication


---

### Word Frequency Matrix


```r
with(DATA, wfm(state, person))[1:14, ]
```

```
##          greg researcher sally sam teacher
## about       0          0     1   0       0
## already     1          0     0   0       0
## am          1          0     0   0       0
## are         0          0     1   0       0
## be          0          0     1   0       0
## can         0          0     1   0       0
## certain     0          0     1   0       0
## computer    0          0     0   1       0
## distrust    0          0     0   1       0
## do          0          0     0   0       1
## dumb        1          0     0   0       0
## eat         1          0     0   0       0
## fun         0          0     0   2       0
## good        0          1     0   0       0
```


---

### Word Frequency Matrix


```r
plot(with(DATA, wfm(state, person)), values = TRUE, plot = FALSE) +
   coord_flip()
```

<img src="figure/unnamed-chunk-5.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" style="display: block; margin: auto;" />

    

---

### Word Frequency Matrix (Correlations)


```r
dat2 <- wfm(DATA$state, seq_len(nrow(DATA)))
qheat(cor(t(dat2)), low = "yellow", high = "red",
    grid = "grey90", diag.na = TRUE, by.column = NULL)
```

<img src="figure/unnamed-chunk-6.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" style="display: block; margin: auto;" />

    

---

### Word Stats (1 of 3)


```r
(desc_wrds <- with(mraja1spl, word_stats(dialogue, person, tot = tot)))
```






```
           person n.tot n.sent n.words n.char n.syl n.poly sptot wptot
1           Romeo    49    113    1163   4757  1441     48   2.3  23.7
2        Benvolio    34     51     621   2563   780     25   1.5  18.3
3           Nurse    20     59     599   2274   724     20   3.0  29.9
4         Sampson    20     28     259    912   294      7   1.4  12.9
5          Juliet    16     24     206    789   238      5   1.5  12.9
6         Gregory    15     20     149    553   166      1   1.3   9.9
7         Capulet    14     72     736   2900   902     35   5.1  52.6
8    Lady Capulet    12     27     288   1205   370     10   2.2  24.0
9        Mercutio    11     24     549   2355   704     29   2.2  49.9
10        Servant    10     19     184    725   226      5   1.9  18.4
11         Tybalt     8     17     160    660   207      9   2.1  20.0
12       Montague     6     13     217    919   284     13   2.2  36.2
13        Abraham     5      6      24     79    26      0   1.2   4.8
14  First Servant     3      7      69    294    87      2   2.3  23.0
15 Second Servant     3      4      41    160    49      0   1.3  13.7
16  Lady Montague     2      4      28     88    30      0   2.0  14.0
17          Paris     2      3      32    124    41      2   1.5  16.0
18 Second Capulet     2      2      17     64    21      0   1.0   8.5
19         Prince     1      9     167    780   228     17   9.0 167.0
20  First Citizen     1      5      16     79    22      3   5.0  16.0
```



---


### Word Stats (2 of 3)


```
           person  wps  cps  sps psps cpw spw pspw n.state n.quest n.exclm
1           Romeo 10.3 42.1 12.8  0.4 4.1 1.2  0.0      69      22      22
2        Benvolio 12.2 50.3 15.3  0.5 4.1 1.3  0.0      39       8       4
3           Nurse 10.2 38.5 12.3  0.3 3.8 1.2  0.0      37       9      13
4         Sampson  9.2 32.6 10.5  0.2 3.5 1.1  0.0      27       1       0
5          Juliet  8.6 32.9  9.9  0.2 3.8 1.2  0.0      16       5       3
6         Gregory  7.5 27.6  8.3  0.0 3.7 1.1  0.0      14       3       3
7         Capulet 10.2 40.3 12.5  0.5 3.9 1.2  0.0      40      10      22
8    Lady Capulet 10.7 44.6 13.7  0.4 4.2 1.3  0.0      20       6       1
9        Mercutio 22.9 98.1 29.3  1.2 4.3 1.3  0.1      20       2       2
10        Servant  9.7 38.2 11.9  0.3 3.9 1.2  0.0      14       2       3
11         Tybalt  9.4 38.8 12.2  0.5 4.1 1.3  0.1      13       2       2
12       Montague 16.7 70.7 21.8  1.0 4.2 1.3  0.1      11       2       0
13        Abraham  4.0 13.2  4.3  0.0 3.3 1.1  0.0       3       2       1
14  First Servant  9.9 42.0 12.4  0.3 4.3 1.3  0.0       3       2       2
15 Second Servant 10.2 40.0 12.2  0.0 3.9 1.2  0.0       4       0       0
16  Lady Montague  7.0 22.0  7.5  0.0 3.1 1.1  0.0       2       2       0
17          Paris 10.7 41.3 13.7  0.7 3.9 1.3  0.1       2       1       0
18 Second Capulet  8.5 32.0 10.5  0.0 3.8 1.2  0.0       2       0       0
19         Prince 18.6 86.7 25.3  1.9 4.7 1.4  0.1       7       1       1
20  First Citizen  3.2 15.8  4.4  0.6 4.9 1.4  0.2       0       0       5
```



---


### Word Stats  (3 of 3)


```
           person p.state p.quest p.exclm n.hapax n.dis grow.rate prop.dis
1           Romeo     0.6     0.2     0.2     365    84       0.3      0.1
2        Benvolio     0.8     0.2     0.1     252    43       0.4      0.1
3           Nurse     0.6     0.2     0.2     147    48       0.2      0.1
4         Sampson     1.0     0.0     0.0      81    22       0.3      0.1
5          Juliet     0.7     0.2     0.1      94    22       0.5      0.1
6         Gregory     0.7     0.2     0.2      72    17       0.5      0.1
7         Capulet     0.6     0.1     0.3     232    46       0.3      0.1
8    Lady Capulet     0.7     0.2     0.0     135    28       0.5      0.1
9        Mercutio     0.8     0.1     0.1     253    28       0.5      0.1
10        Servant     0.7     0.1     0.2      71    19       0.4      0.1
11         Tybalt     0.8     0.1     0.1      79    17       0.5      0.1
12       Montague     0.8     0.2     0.0     117    21       0.5      0.1
13        Abraham     0.5     0.3     0.2       3     7       0.1      0.3
14  First Servant     0.4     0.3     0.3      33     8       0.5      0.1
15 Second Servant     1.0     0.0     0.0      32     3       0.8      0.1
16  Lady Montague     0.5     0.5     0.0      24     2       0.9      0.1
17          Paris     0.7     0.3     0.0      25     2       0.8      0.1
18 Second Capulet     1.0     0.0     0.0       7     5       0.4      0.3
19         Prince     0.8     0.1     0.1      83    15       0.5      0.1
20  First Citizen     0.0     0.0     1.0       9     2       0.6      0.1
```


---

### Word Stats Plot


```r
plot(desc_wrds, label = TRUE, high="red")
```

<img src="figure/unnamed-chunk-12.png" title="plot of chunk unnamed-chunk-12" alt="plot of chunk unnamed-chunk-12" style="display: block; margin: auto;" />



---

### Term Counts


```r
ml2 <- list(
    theme_1 = c(" the ", " a ", " an "),
    theme_2 = c(" I'" ),
    "good",
    the_words = c("the", " the ", " the", "the ")
)
```



```r
out <- with(raj.act.1,  termco(dialogue, person, ml2))
```


*** =pnotes

</br></br> <font size="2.5">\*<em>Press <b>p</b></em></font>

```r
ml2 <- list(
    theme_1 = c(" the ", " a ", " an "),
    theme_2 = c(" I'" ),
    "good",
    the_words = c("the", " the ", " the", "the ")
)
```



---


### Term Counts


```
           person word.count   theme_1 theme_2     good   the_words
1         Abraham         24         0       0        0           0
2        Benvolio        621 32(5.15%) 2(.32%)  2(.32%) 123(19.81%)
3         Capulet        736 39(5.30%) 3(.41%)  3(.41%)  93(12.64%)
4   First Citizen         16 2(12.50%)       0        0  10(62.50%)
5   First Servant         69 8(11.59%)       0 1(1.45%)  20(28.99%)
6         Gregory        149  9(6.04%)       0        0  48(32.21%)
7          Juliet        206  5(2.43%) 1(.49%)  1(.49%)   20(9.71%)
8    Lady Capulet        286 20(6.99%)       0        0  63(22.03%)
9   Lady Montague         28  2(7.14%)       0        0           0
10       Mercutio        552 49(8.88%)       0  2(.36%) 146(26.45%)
11       Montague        217 12(5.53%)       0  1(.46%)  41(18.89%)
12          Nurse        598 44(7.36%) 1(.17%)  2(.33%) 103(17.22%)
13          Paris         32         0       0        0    1(3.12%)
14         Prince        167  8(4.79%)       0        0  35(20.96%)
15          Romeo       1164 56(4.81%) 3(.26%)  3(.26%) 142(12.20%)
16        Sampson        259 19(7.34%)       0  1(.39%)  70(27.03%)
17 Second Capulet         17         0       0        0           0
18 Second Servant         41  2(4.88%)       0 1(2.44%)   8(19.51%)
19        Servant        183 12(6.56%) 1(.55%)  1(.55%)  46(25.14%)
20         Tybalt        160 11(6.88%) 1(.62%)        0  24(15.00%)
```


*** =pnotes


```r
ml2 <- list(
    theme_1 = c(" the ", " a ", " an "),
    theme_2 = c(" I'" ),
    "good",
    the_words = c("the", " the ", " the", "the ")
)
```


---


### Term Counts Plot


```r
plot(out, high = "red", low = "yellow", label = TRUE)
```

<img src="figure/unnamed-chunk-18.png" title="plot of chunk unnamed-chunk-18" alt="plot of chunk unnamed-chunk-18" style="display: block; margin: auto;" />


---

### Question Types


```r
(x <- with(mraja1spl, question_type(dialogue, person)))
```

```
           person tot.quest    whose    who     where       what  which       why      when   were      was      did         do         is       will       how   should    shall        can      have implied_do/does/did   unknown
1         Abraham         2        0      0         0          0      0         0         0      0        0        0 2(100.00%)          0          0         0        0        0          0         0                   0         0
2        Benvolio         8        0      0         0  2(25.00%)      0 1(12.50%)         0      0        0        0  1(12.50%)          0  1(12.50%)         0        0        0          0 1(12.50%)                   0 2(25.00%)
3         Capulet        10        0      0         0  1(10.00%) 1(10%)         0         0      0        0        0          0  1(10.00%)  1(10.00%) 1(10.00%)        0        0          0         0              1(10%) 4(40.00%)
4   First Servant         2        0      0 1(50.00%)          0      0         0         0      0        0        0          0          0          0         0        0        0          0         0                   0 1(50.00%)
5         Gregory         3        0      0         0          0      0         0         0      0        0        0  1(33.33%)          0          0         0        0        0          0         0                   0 2(66.67%)
6          Juliet         5        0 1(20%)         0  4(80.00%)      0         0         0      0        0        0          0          0          0         0        0        0          0         0                   0         0
7    Lady Capulet         6        0      0 1(16.67%)  1(16.67%)      0 1(16.67%)         0      0        0        0          0          0          0 1(16.67%)        0        0  2(33.33%)         0                   0         0
8   Lady Montague         2        0      0 1(50.00%)          0      0         0         0      0        0        0          0          0          0         0        0        0          0         0                   0 1(50.00%)
9        Mercutio         2        0      0         0  1(50.00%)      0 1(50.00%)         0      0        0        0          0          0          0         0        0        0          0         0                   0         0
10       Montague         2        0 1(50%)         0          0      0         0         0 1(50%)        0        0          0          0          0         0        0        0          0         0                   0         0
11          Nurse         9        0      0 1(11.11%)  2(22.22%)      0         0 2(22.22%)      0        0        0          0          0          0 1(11.11%)        0        0          0         0                   0 3(33.33%)
12          Paris         1        0      0         0 1(100.00%)      0         0         0      0        0        0          0          0          0         0        0        0          0         0                   0         0
13         Prince         1        0      0         0          0      0         0         0      0        0        0          0          0 1(100.00%)         0        0        0          0         0                   0         0
14          Romeo        22 1(4.55%)      0  1(4.55%)  8(36.36%)      0         0         0      0 1(4.55%) 1(4.55%)          0  4(18.18%)          0         0 1(4.55%) 1(4.55%)          0  1(4.55%)                   0 3(13.64%)
15        Sampson         1        0      0         0          0      0         0         0      0        0        0          0 1(100.00%)          0         0        0        0          0         0                   0         0
16        Servant         2        0      0         0          0      0         0         0      0        0        0          0          0          0         0        0        0 2(100.00%)         0                   0         0
17         Tybalt         2        0      0         0 2(100.00%)      0         0         0      0        0        0          0          0          0         0        0        0          0         0                   0         0
18  First Citizen         0        0      0         0          0      0         0         0      0        0        0          0          0          0         0        0        0          0         0                   0         0
19 Second Capulet         0        0      0         0          0      0         0         0      0        0        0          0          0          0         0        0        0          0         0                   0         0
20 Second Servant         0        0      0         0          0      0         0         0      0        0        0          0          0          0         0        0        0          0         0                   0         0
```


---

### Question Types Plot


```r
plot(x)
```

<img src="figure/unnamed-chunk-20.png" title="plot of chunk unnamed-chunk-20" alt="plot of chunk unnamed-chunk-20" style="display: block; margin: auto;" />


---

### Parts of Speech   


<pre><code class="r">(posbydat <- with(DATA, pos_by(state, list(adult, sex))))
</code></pre>


```
##   adult&sex wrd.cnt      DT      EX      FW       IN       JJ       MD       NN      PRP    PRP$       RB       VB      VBG      VBP     VBZ     WP    WRB
## 1       0.f      10       0       0       0 1(10.0%) 1(10.0%) 1(10.0%)        0 2(20.0%)       0        0 1(10.0%) 1(10.0%) 1(10.0%)       0 1(10%) 1(10%)
## 2       0.m      33 3(9.1%) 1(3.0%) 1(3.0%)        0  2(6.1%)        0 6(18.2%) 6(18.2%) 2(6.1%) 4(12.1%)  2(6.1%)  1(3.0%)  2(6.1%) 3(9.1%)      0      0
## 3       1.f       6       0       0       0 1(16.7%) 1(16.7%) 1(16.7%)        0 1(16.7%)       0 1(16.7%) 1(16.7%)        0        0       0      0      0
## 4       1.m       4       0       0       0        0        0 1(25.0%)        0 1(25.0%)       0        0 1(25.0%)        0        0       0 1(25%)      0
```


</br></br></br> <a href="http://faculty.washington.edu/dillon/GramResources/penntable.html">Penn Treebank Project (1991)</a>

*** =pnotes


```r
posbydat[["POStagged"]]
```

```
##                                              POStagged
## 1       computer/NN is/VBZ fun/NN not/RB too/RB fun/NN
## 2               no/DT its/PRP$ not/RB its/PRP$ dumb/JJ
## 3                       what/WP should/MD we/PRP do/VB
## 4                    you/PRP liar/VBP it/PRP stinks/VB
## 5             i/PRP am/VBP telling/VBG the/DT truth/NN
## 6               how/WRB can/MD we/PRP be/VB certain/JJ
## 7                         there/EX is/VBZ no/DT way/NN
## 8                             i/FW distrust/NN you/PRP
## 9         what/WP are/VBP you/PRP talking/VBG about/IN
## 10       shall/MD we/PRP move/VB on/IN good/JJ then/RB
## 11 im/PRP hungry/JJ lets/VBZ eat/VB you/PRP already/RB
##                      POStags word.count
## 1    NN, VBZ, NN, RB, RB, NN          6
## 2     DT, PRP$, RB, PRP$, JJ          5
## 3            WP, MD, PRP, VB          4
## 4          PRP, VBP, PRP, VB          4
## 5      PRP, VBP, VBG, DT, NN          5
## 6       WRB, MD, PRP, VB, JJ          5
## 7            EX, VBZ, DT, NN          4
## 8                FW, NN, PRP          3
## 9      WP, VBP, PRP, VBG, IN          5
## 10   MD, PRP, VB, IN, JJ, RB          6
## 11 PRP, JJ, VBZ, VB, PRP, RB          6
```


---

### Parts of Speech Plot


```r
plot(posbydat, label = TRUE)
```

<img src="figure/unnamed-chunk-23.png" title="plot of chunk unnamed-chunk-23" alt="plot of chunk unnamed-chunk-23" style="display: block; margin: auto;" />


---

## Word Measures and Scoring

1. Readability   
2. Formality    
3. Polarity

---

### Readability   

1. Automated Readability Index    
2. Coleman Liau    
3. SMOG    
4. Flesch Kincaid     
5. Fry  
6. Linsear Write

---

### Readability


```r
with(rajSPLIT, coleman_liau(dialogue, list(fam.aff)))
```

```
  fam.aff word.count sentence.count character.count Coleman_Liau
1     cap      10662           1062           42089          4.5
2   escal       3292            262           13406          5.8
3    mont       6384            559           26113          5.7
4    none       3389            262           14192          6.5
```


---

### Formality

Heylighen & Dewaele(1999a, 1999b, 2002)

<br>

$$ F = 50(\frac{n_{f}-n_{c}}{N} + 1) $$

Where:
<br>

$$ f = \left \{noun, \;adjective, \;preposition, \;article\right \} $$
$$ c = \left \{pronoun, \;verb, \;adverb, \;interjection\right \} $$
$$ N = \sum{(f \;+ \;c \;+ \;conjunctions)} $$
<br>

---

### Formality


```r
(form <- with(raj, formality(dialogue, act)))
```



```
  act word.count formality
1   5       3379     58.38
2   2       5358     58.10
3   1       5525     57.59
4   3       6354     57.22
5   4       3167     55.89
```



---

### Formality Plot


```r
plot(form, bar.colors=c("Set2", "RdBu"))
```

<img src="figure/unnamed-chunk-27.png" title="plot of chunk unnamed-chunk-27" alt="plot of chunk unnamed-chunk-27" style="display: block; margin: auto;" />


---

### Polarity


```r
(poldat <- with(mraja1spl, polarity(dialogue, list(sex, fam.aff, died))))
```

```
   sex&fam.aff&died total.sentences total.words ave.polarity sd.polarity stan.mean.polarity
1       f.cap.FALSE             158        1810        0.076       0.262              0.292
2        f.cap.TRUE              24         221        0.042       0.209              0.204
3       f.mont.TRUE               4          29        0.079       0.398              0.199
4       m.cap.FALSE              73         717        0.026       0.256              0.104
5        m.cap.TRUE              17         185       -0.160       0.313             -0.510
6     m.escal.FALSE               9         195       -0.153       0.313             -0.488
7      m.escal.TRUE              27         646       -0.069       0.256             -0.272
8      m.mont.FALSE              70         952       -0.044       0.384             -0.114
9       m.mont.TRUE             114        1273       -0.004       0.409             -0.009
10     m.none.FALSE               7          78        0.062       0.107              0.583
11  none.none.FALSE               5          18       -0.282       0.439             -0.642
```


---

<img src="figure/unnamed-chunk-29.png" title="plot of chunk unnamed-chunk-29" alt="plot of chunk unnamed-chunk-29" style="display: block; margin: auto;" />



---

## Qualitative Coding System

Apply codes to:

- The dialogue    
- The time spans     

---

<pre><code class="r">codes <- qcv(AA, BB, CC)
X <- cm_df.transcript(DATA$state, DATA$person, file="DATA.txt")
</code></pre>

<pre><code>sam:
                                  
     1        2  3    4   5   6   
     Computer is fun. Not too fun.
greg:
                           
     7  8    9    10   11   
     No it's not, it's dumb.
teacher:
                       
     12   13     14 15 
     What should we do?
sam:
                         
     16  17    18 19     
     You liar, it stinks!
</code></pre>

---

<div style="width:840px;margin:auto;">
    <p><img src="assets/img/transcript2.png" width="800" height="533"></p>
</div>

</br>
Coding time spans works similarly...
<br><br><em><font size="2.5">press p</font></em>

*** =pnotes


```r
x <- list(
    transcript_time_span = qcv(00:00 - 1:12:00),
    A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 
        12.00:14.00, 00.51.00:00.59.00"),
    B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00,
        9.00, 30.00:39.00, 1.12.00:1.19.01"),
    C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
)
```


---

### After Reading Qualitative Codes...


```r
(y <- cm_2long(x))
```

```
   code start  end    Start      End variable
1     A   159  180 00:02:39 00:03:00        x
2     A   300  301 00:05:00 00:05:01        x
3     A   361  420 00:06:01 00:07:00        x
4     A   539  540 00:08:59 00:09:00        x
5     A   719  840 00:11:59 00:14:00        x
6     A  3059 3540 00:50:59 00:59:00        x
7     B   159  160 00:02:39 00:02:40        x
8     B   180  182 00:03:00 00:03:02        x
9     B   300  301 00:05:00 00:05:01        x
10    B   361  420 00:06:01 00:07:00        x
11    B   539  540 00:08:59 00:09:00        x
12    B  1799 2340 00:29:59 00:39:00        x
13    B  4319 4741 01:11:59 01:19:01        x
14    C   159  180 00:02:39 00:03:00        x
15    C   300  301 00:05:00 00:05:01        x
16    C   361  420 00:06:01 00:07:00        x
17    C   539  540 00:08:59 00:09:00        x
18    C  1020 1021 00:17:00 00:17:01        x
```


---

### Gantt Plot of Codes


```r
plot(y)
```

<img src="figure/unnamed-chunk-32.png" title="plot of chunk unnamed-chunk-32" alt="plot of chunk unnamed-chunk-32" style="display: block; margin: auto;" />



---

### Summary of Codes


```r
summary(y)
```

```
  code total percent_total n percent_n       mean(sd) min max
1    A 11:24        15.83% 6    33.33% 114.00(185.45)   1 481
2    B 17:07        23.77% 7    38.89% 146.71(232.23)   1 541
3    C 01:23         1.92% 5    27.78%   16.60(25.23)   1  59
```

```r
plot(summary(y))
```

<img src="figure/unnamed-chunk-33.png" title="plot of chunk unnamed-chunk-33" alt="plot of chunk unnamed-chunk-33" style="display: block; margin: auto;" />


---


## Visualizing Discourse Data

1. Lexical Dispersion Plot     
2. Word Cloud    
3. Turn of Talk Plot    
4. Venn Diagram    
5. Word Network Plot    

---

### Lexical Dispersion Plot


```r
with(rajSPLIT , dispersion_plot(dialogue, c("love", "night"),
    grouping.var = list(fam.aff, sex), rm.vars = act))
```

![plot of chunk unnamed-chunk-34](figure/unnamed-chunk-34.png) 


---

### Lexical Dispersion Plot

<img src="figure/unnamed-chunk-35.png" title="plot of chunk unnamed-chunk-35" alt="plot of chunk unnamed-chunk-35" style="display: block; margin: auto;" />


---

## Word Cloud (Colored Terms) 

<pre><code>terms <- list(
    I = c("i", "i'm"),
    mal = qcv(stinks, dumb, distrust),
    articles = qcv(the, a, an),
    pronoun = qcv(we, you)
)

with(DATA, trans_cloud(state, target.words=terms,
    cloud.colors=qcv(red, green, blue, black, gray65),
    expand.target=FALSE, proportional=TRUE, legend=c(names(terms),
    "other")))
 </code></pre>   
    



---

## Word Cloud Plot (Colored Terms) 

<img src="figure/unnamed-chunk-37.png" title="plot of chunk unnamed-chunk-37" alt="plot of chunk unnamed-chunk-37" style="display: block; margin: auto;" />


---

## Gradient Cloud




<pre><code>gradient_cloud(DATA$state, DATA$sex, title="Houghton Colors", 
    max.word.size = 8, min.word.size = .01, X ="purple" , Y = "yellow")
</code></pre>

---

## Gradient Cloud 

<img src="figure/unnamed-chunk-39.png" title="plot of chunk unnamed-chunk-39" alt="plot of chunk unnamed-chunk-39" style="display: block; margin: auto;" />


---

## Turn of Talk Plot    
</br>
<img src="figure/unnamed-chunk-40.png" title="plot of chunk unnamed-chunk-40" alt="plot of chunk unnamed-chunk-40" style="display: block; margin: auto;" />


---


## Venn Diagram    


```r
with(DATA , trans_venn(state, person, legend.location = "topright"))
```

<img src="figure/unnamed-chunk-41.png" title="plot of chunk unnamed-chunk-41" alt="plot of chunk unnamed-chunk-41" style="display: block; margin: auto;" />


---


### Word Network Plot


```r
word_network_plot(text.var=DATA$state, DATA$person, stopwords=NULL)
```

<img src="figure/unnamed-chunk-42.png" title="plot of chunk unnamed-chunk-42" alt="plot of chunk unnamed-chunk-42" style="display: block; margin: auto;" />


---

## Discussion

1. How might qdap + R fit into your workflow?
2. What do you want to know more about?
3. Are there any points that need to be clarified?
4. &hellip;


