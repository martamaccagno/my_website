---
categories:
- ""
- ""
date: "2020-10-05"
description: "Alcohol consumption in different countries"
draft: false
image: IMG_7498.jpg
keywords: ""
slug: blog1
title: Alcohol Cunsumption
---

```{r, setup, echo=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```


```{r load-libraries, warning=FALSE, message=FALSE, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(fivethirtyeight)
library(here)
library(skimr)
library(janitor)
library(vroom)
library(tidyquant)
library(rvest)    # scrape websites
library(purrr)  
library(lubridate) #to handle dates
library(ggrepel)
```
