---
title: "Team ACDC Case Study 6: Spam Ham, Problem 19"
author: "David Josephs, Andy Heroy, Carson Drake, Che' Cobb"
date: "2020-02-14"
output: 
  html_document:
    toc: true
    number_sections: true
    theme: readable
    highlight: haddock
    code_folding: hide
    df_print: paged
    keep_md: TRUE
    fig_width: 10
    fig_height: 10
    fig_retina: true
    fig_caption: true
---





```r
# we are starting from here, the code used to create the RDA will be shown
# in the appendix, it seems to run fine on windows but crash on linux so
# this was a good compromise without digging too deeply into the web of
# lapplys
load("data.Rda")
emails <- emailDFrp
emails
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
  </script>
</div>