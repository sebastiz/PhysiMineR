#R code template Sebastian Zeki 2017
#Date code written 

######################################### Data acquisiton######################################### 

######################################### Data merging######################################### 

######################################### Data cleaning######################################### 
#Date cleaning
######################################### Data accordionisation######################################### 

######################################### Data forking (filtering and subsetting)######################################### 

######################################### Data analysis######################################### 

######################################### Code overview (with CodeDepends)######################################### 
#library(codeDepends)
#sc = readScript("S:\\Gastroenterology\\Seb\\R\\Scripts\\Eosinophilics\\Eosinophilix.R")
#g = makeVariableGraph( info =getInputs(sc))


#if(require(Rgraphviz))
#  plot(g)


---
  title: "A good workman blames his tools. Understanding the contribution of endoscope model to the adenoma detection rate for experienced endoscopists"
author: "Sebastian Zeki PhD"
date: "27 June 2017"
output: word_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = F)

```

## Introduction

The adenoma detection rate (ADR) is a crucial component when assessing the skill of an endoscopist. The rate is attributed to an individual endoscopist and reflects a composite of various techniques that the endoscopist employs such as withdrawl time, use of endocuff etc. However the endoscope used has not been assessed.

## Aim
<http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:
  
  ## Method---Can I extract the comments from an r script to 
  #Data acquisition, cleaning and filtration goes here
  
  Data was acquired from retrospectively from hospital records. `r 1+1` (#Filter 1 here) barium swallow reports were analysed. High resolution manometry reports were then acquired and cross referenced with the barium swallow reports. (#Filter 2 here) HRM's with a barium study within 1 year of the barium were retained. Fundoplication and post therapy patients were excluded from the analysis. 
    
    ## Results
    Table 1: Demographics
    
    
    ```{r cars}
    library(xtable)
    knitr::kable(summary(cars))
    ```
    
    ## Including Plots
    
    You can also embed plots, for example:
      
      ```{r pressure, echo=FALSE}
    plot(pressure)
    ```
    
    Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
    