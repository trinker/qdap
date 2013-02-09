<p><img src="https://dl.dropbox.com/u/61803503/qdapicon.png" alt="qdapicon"/><br/>
qdap (Quantitative Discourse Analysis Package) is an R package designed to assist in quantitative discourse analysis.  The package stands as a bridge between qualitative transcripts of dialogue and statistical analysis and visualization.</p>

<h2>Installation</h2>

<p><strong>Note</strong>: Windows users may need to install <code><a href="http://cran.r-project.org/web/packages/RCurl/index.html">RCurl</a></code> before installing qdap.  Use the following short script:</p>

<pre><code class="r">URL &lt;- &quot;http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/3.0/&quot;
install.packages(&quot;RCurl&quot;, contriburl = URL)
install.packages(&quot;qdap&quot;)
</code></pre>

<p><strong>Note</strong>: Mac users must install <code><a href="http://cran.r-project.org/web/packages/openNLP/index.html">openNLP</a></code> from source before attempting to install <code>qdap</code>.  </p>

<p>This may require installing the appropriate version <a href="https://developer.apple.com/xcode/">XTools</a> from the <a href="https://developer.apple.com/">Apple Developer site</a>.  You may need to <a href="https://developer.apple.com/programs/register/">register as an Apple developer</a>.  An older version of XTools may also be required.</p>

<pre><code class="r">install.packages(&quot;openNLP&quot;, type = &quot;source&quot;)
install.packages(&quot;qdap&quot;, type = &quot;source&quot;)
</code></pre>


<p>Download the development version <a href="https://github.com/trinker/qdap/">here</a> 