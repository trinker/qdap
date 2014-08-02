library(staticdocs)

sd_section("Project Template",
  "A function to generate a project template of folders, scripts and documents.",  
  c(
    "new_project"
  )
)

sd_section("Import/Export Data",
  "Functions for importing data and exporting output.",  
  c(
    "condense",        
    "dir_map",
    "mcsv_r",
    "read.transcript"      
  )
)

sd_section("Cleaning/Parsing",
  "Function to clean and parse text data.",
  c(
    "bracketX",
    "beg2char",
    "capitalizer",  
    "check_spelling",
    "check_text",
    "clean",
    "comma_spacer",
    "incomplete_replace",
    "multigsub",
    "name2sex",
    "potential_NA",
    "qprep",
    "replace_abbreviation",
    "replace_contraction",
    "replace_number",
    "replace_ordinal", 
    "replace_symbol",
    "rm_email",
    "rm_hash",
    "rm_row",
    "rm_stopwords", 
    "rm_tag",
    "rm_url",          
    "scrubber",
    "space_fill",
    "spaste",
    "stemmer",
    "Trim"
  )
)

sd_section("Viewing", 
  "Functions to aid data viewing.", 
  c(
    "htruncdf",
    "left_just",
    "Search",        
    "strWrap"
  )
)

sd_section("Chaining",
  "Functions chain together qdap data and functions.",  
  c(
    "qdap_df",
    "%&%"
  )
)


sd_section("Generic qdap Methods", 
  "Functions to aid in selection of data elements.", 
  c(
    "counts",
    "Filter",
    "preprocessed",
    "proportions",        
    "scores",
    "visual"
  )
)

sd_section("Reshaping",
  "Functions to reshape data.",
  c(
    "adjacency_matrix",
    "colSplit",
    "colsplit2df",
    "colcomb2class",
    "gantt",
    "gantt_rep",
    "key_merge", 
    "paste2",
    "prop",
    "qcombine",
    "sentSplit",
    "speakerSplit",
    "trans_context"
  )
)

sd_section("Word Extraction/Comparison", 
  "Functions for working with dialogue at the word level.",
  c(
    "all_words",
    "bag_o_words",
    "common",
    "exclude",
    "freq_terms",
    "ngrams",
    "strip",
    "synonyms",
    "word_associate",
    "word_diff_list",
    "word_list"
  )
)

sd_section("Coding Tools",
  "cm functions are code matrix functions.  These functions are used for coding and reshaping transcripts, dataframes, and time spans for further use in analysis and visualization.",
  c(
    "summary.cmspans",
    "cm_range.temp",
    "cm_df.transcript",
    "cm_time.temp",
    "cm_df.temp", 
    "cm_2long",
    "cm_df2long",
    "cm_range2long",
    "cm_time2long",        
    "cm_code.blank",
    "cm_code.combine",
    "cm_code.exclude",
    "cm_code.overlap",
    "cm_code.transform",
    "cm_distance",
    "cm_dummy2long",
    "cm_long2dummy"
  )
)

sd_section("tm Package Integration",
  "Functions for working between the tm and qdap packages.",
  c(
    "as.tdm"        
  )
)

sd_section("Counts/Descriptives",
  "Functions for word counts and descriptive statistics.",
  c(
    "dist_tab",
    "multiscale",
    "object_pronoun_type",
    "outlier_detect",
    "outlier_labeler",
    "pos",
    "pronoun_type",
    "question_type",
    "subject_pronoun_type", 
    "syllable_sum",     
    "termco",
    "termco_c",
    "wfm",
    "word_count",
    "word_stats"
  )
)

sd_section("Measures",
  "Word measures and scoring.",
  c(
    "automated_readability_index",
    "Dissimilarity",
    "diversity",
    "formality",
    "kullback_leibler",
    "polarity",
    "word_cor",
    "word_proximity"
  )
)

sd_section("qdap Tools", 
  "Tools to assist in transcript/discourse analysis.",
  c(
    "blank2NA",
    "build_qdap_vignette",
    "duplicates",
    "qcv",
    "replacer"
  ) 
)

sd_section("Identification", 
  "Identify sentence elements/types.",
  c(
    "end_inc",
    "end_mark",
    "imperative",
    "NAer"
  )
)

sd_section("Visualization", 
  "Plotting functions.",
  c(
    "dispersion_plot",
    "gradient_cloud",
    "gantt_plot",
    "gantt_wrap",
    "phrase_net", 
    "plot.character_table",
    "plot.cmspans",
    "plot.diversity",
    "plot.formality",
    "plot.gantt", 
    "plot.kullback_leibler", 
    "plot.polarity",
    "plot.pos_by",
    "plot.question_type",
    "plot.rmgantt",
    "plot.sent_split", 
    "plot.sum_cmspans",
    "plot.sums_gantt",
    "plot.termco",
    "plot.wfdf",
    "plot.wfm", 
    "plot.word_stats",
    "qheat",
    "rank_freq_mplot",
    "tot_plot",
    "trans_cloud",
    "trans_venn",
    "word_network_plot"
  )
)

sd_section("Network Plots", 
  "Network Plots for qdap Objects.",
  c(
    "discourse_map", 
    "Network",
    "Network.formality",      
    "Network.polarity",
    "qtheme",
    "theme_badkitchen",           
    "theme_cafe",                  
    "theme_duskheat",             
    "theme_grayscale",             
    "theme_greyscale",            
    "theme_hipster",               
    "theme_nightheat",            
    "theme_norah"  
  )
)

sd_section("Cumulative Plots", 
  "Network Plots for qdap Objects.",
  c(
    "cumulative", 
    "cumulative.end_mark",       
    "cumulative.formality",      
    "cumulative.polarity"
  )
)

sd_section("Animation", 
  "Animate qdap Objects.",
  c(
    "Animate",
    "Animate.discourse_map",
    "Animate.formality", 
    "Animate.gantt",
    "Animate.gantt_plot", 
    "Animate.polarity",
    "vertex_apply"
  )
)

sd_section("Print Functions", "",
  c(
    "print.all_words",
    "print.adjacency_matrix",
    "print.boolean_qdap",
    "print.character_table",
    "print.cm_distance",
    "print.colsplit2df",
    "print.Dissimilarity",
    "print.diversity",
    "print.formality",
    "print.kullback_leibler",
    "print.ngrams",
    "print.polarity",
    "print.pos",
    "print.pos_by",
    "print.qdap_context",
    "print.question_type",
    "print.sent_split",
    "print.sum_cmspans",
    "print.sums_gantt",
    "print.termco",
    "print.wfm",
    "print.word_associate",
    "print.word_list",
    "print.word_stats"
  )
)

sd_section("Data", 
  "Data sets included in qdap and used in examples.",
  c( 
    "DATA",
    "DATA2",
    "DATA.SPLIT",
    "pres_debates2012",
    "pres_debate_raw2012",
    "mraja1",
    "mraja1spl",
    "raj.act.1",
    "raj.act.2",
    "raj.act.3",
    "raj.act.4",
    "raj.act.5",
    "raj.demographics",
    "raj",
    "rajPOS",
    "rajSPLIT",
    "raw.time.span",
    "sample.time.span"
  )
)


