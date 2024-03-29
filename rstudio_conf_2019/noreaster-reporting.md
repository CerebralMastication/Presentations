Empathy in Action
========================================================
author: JD Long
date: 2019-01-18
autosize: true
font-family: 'Helvetica'
css: custom.css
    
Disclaimer
========================================================
class: small-code
```
* I am sharing my personal views not the views of RenaissanceRe Holdings, Ltd. or any of its subsidiary companies.
* Thought provoking only: The materials are provided for information purposes only, are in summary form, and thus do not include all information necessary for analysis. 
* This is not consulting: No representations or warranties are give as to the accuracy or completeness of the materials and no professional advice or guidance is given by the presentation. If you want to use these ideas, professional consultation should be sought. 
* This is not financial advice: No financial advice is being given nor any recommendation to invest in any financial product. Historical data is no assurance as to future results.
```


Source
========================================================
class: small-body

All data, code, and examples: 
[CerebralMastication/Presentations](https://github.com/CerebralMastication/Presentations/)

<div align="center">
<img src="./fig/github.png" width=700 >
</div>
Take Pix, Dance, Ask, Tweet 
**@cmastication**

## What's Happening Here? {.build}

JD's Lema:

To migrate Excel analysts to R, help them be better at R first. Then help them use a bit of R for the painful parts. If you fail to migrate them to R you will at least have better Excel!



JD Who?
========================================================
Stack Overflow...
<div align="center">
<img src="./fig/so.png" width=1200 >
 
</div>

@cmastication
========================================================
<div align="center">
<img src="./fig/twitter_jdlong.png" width=400 >
</div>

All Corporate Risk... All the Time
========================================================
<div align="center">
<img src="./fig/brands.png" width=1400 >
</div>

JD Who?
========================================================
<div align="center">
<img src="./fig/cookbook.png" width=400 >
</div>


Second Edition, Y'all!
========================================================
<div align="center">
<img src="./fig/cookbook2ed.png" width=400 >
</div>

Speed Survey...
========================================================

* Who works at a "for profit" business?


https://xkcd.com/1205/ 
========================================================

<div align="center">
<img src="./fig/is_it_worth_the_time.png" width=700 >
</div>



========================================================
<br>
<div align="center">
<img src="./fig/all-wrong.jpg" width=1300 >
</div>


Assumptions:
========================================================
class: medium-body 

-- All time is of equal value and is interchangeable

Assumptions:
========================================================
class: medium-body 

-- All time is of equal value and is interchangeable

-- Current frequency of reporting = future freq

Assumptions:
========================================================
class: medium-body 

-- All time is of equal value and is interchangeable

-- Current frequency of reporting = future freq

-- All activities are of equal utility (nothing sucks your will to live!)

Assumptions:
========================================================
class: medium-body 

-- All time is of equal value and is interchangeable

-- Current frequency of reporting = future freq

-- All activities are of equal utility (nothing sucks your will to live!)

-- Automated and manual workflows produce the same product (errors?)


A Moment on Empathy...
========================================================

A Moment on Empathy...
========================================================


<div align="center">
<img src="./fig/clippy.jpg" width=300 >
</div>


========================================================
class: medium-body 

It looks like you're doing a data science!
<div align="center">
<img src="./fig/clippy.jpg" width=300 >
</div>
Would you like me to format your integers as dates?!?


What Problem are We Solving?
========================================================
* Reproducibility
* Multiple Users
* Easy to alter
* Looks right to audience
* Useful


Structuring Reporting Workflow
========================================================
class: medium-body 

-- Use R Studio Project structure (or `here` package)

Structuring Reporting Workflow
========================================================
class: medium-body 

-- Use R Studio Project structure (or `here` package)

-- Keep all files in sub directories in ***relative paths***

Structuring Reporting Workflow
========================================================
class: medium-body 

-- Use R Studio Project structure (or `here` package)

-- Keep all files in sub directories in ***relative paths***

-- Naming convention - make it sort right

Structuring Reporting Workflow
========================================================
class: medium-body 

-- Use R Studio Project structure (or `here` package)

-- Keep all files in sub directories in ***relative paths***

-- Naming convention - make it sort right

-- Sub Directories: make it easy to grok

Structuring Reporting Workflow
========================================================
class: medium-body 

-- Use R Studio Projects (or `here` package)

-- Keep all files in ***relative paths***

-- Naming convention: sorting

-- Sub Directories: easy to grok

-- Passwords in environ variables


Three Homes for Code:
========================================================
class: medium-body 

-- Markdown File (*.Rmd) - minimal

-- Sourced R script (*.R) - most here

-- Package: if used more than 2 times


Tips:
========================================================
class: small-body 

-- **`Params`** - Knitr YAML - For easy maintenance

-- **`Worded`** package by David Gohel - Page Breaks and page orientation hacks

-- **`Flextable`** package by David Gohel - Table formatting controls

-- **`Officer`** package by (guess who?) David Gohel! - more table formatting


Whatsit look like?
========================================================

````markdown
---
title: "My Amazing Report"
output:
  worded::rdocx_document: 
     reference_docx: 10_amazing_report_template.docx
     toc: false
     toc_depth: 2
date: '2018-10-26'
params:
  main:    'mycompany 2018-10-01'
  compare: 'mycompany 2018-07-01'
---

Whatsit look like?
========================================================
````
```{r, include=FALSE}
library(flextable)
library(our_kickass_corp_package)

my_corp_db_con <- 
   our_db_connect(Sys.getenv('MYUID'), 
                  Sys.getenv('MYPWD'))
````


Flextable...
========================================================
<div align="center">
<img src="./fig/flextable.png" width=800 >
</div>


Flextable Continued...
========================================================
class: small-code


```r
regulartable(my_cars) %>%
  color(i = 1, color = "#003e51", part = "header") %>%
  autofit %>%
  theme_zebra(odd_header = "transparent", 
              even_header = "transparent") %>%
  fontsize(size = 9, part = 'all') %>%
  border_outer(
    border = officer::fp_border(color = "gray70", 
                                width = 2),
    part = "all") %>%
  align(align = "center", part = 'header')  %>%
  align(j = 1, align = "right", part = 'body')  %>%
  align(j = 2:4, align = "center", part = 'body') %>%
```

Flextable Continued...
========================================================
class: small-code


```r
  set_formatter(
    `mpg` = function(x)
      formatC(x, 
              digits = 1, 
              format = "f", 
              big.mark = ","),
  ) 
```

Figure Tip - Set DPI & sizes:
========================================================
````markdown
```{r, echo=FALSE, dpi=300, fig.width=5, fig.height=2.9}




Loop Output...
========================================================

<div align="center">
<img src="./fig/group_loop.png" width=800 >
</div>



Child Doc Loop:
========================================================
class: small-code
````markdown
```{r, echo=FALSE}
my_groups <- unique(mtcars$cyl)
out <- list()
for (group in my_groups){
    out = c(out, knitr::knit_child('10_sub.Rmd'))
}
```
Children are evaluated in the context (namespace etc.) of the parent


```r
`r paste(out, collapse='\n')`
```

Hack: Excel Output
========================================================


```r
library(openxlsx)

xlfile <- 'my_junk.xlsx'
wb <- loadWorkbook(xlfile)
removeTable(wb=wb, sheet='Sheet1', table="our_table")

writeDataTable(
  wb, sheet='Sheet1', x=my_df,
  tableName="our_table", 
  startCol=3, startRow=10
)

saveWorkbook(wb=wb, file=xlfile, 
             overwrite = TRUE)
```


JD Long
========================================================
class: small-body 

Github: [CerebralMastication/Presentations](https://github.com/CerebralMastication/Presentations/)

Twitter: **@cmastication** Email: <jdlong@gmail.com>
<div align="center">
<img src="./fig/twitter_jdlong.png" width=250 >
</div>



