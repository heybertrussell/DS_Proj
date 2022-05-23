###The Libraries!

library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

###The Data!

raw_data <- read.csv("https://heybertrussell.github.io/NYT_Data/nytcrosswords.csv", stringsAsFactors = FALSE)

### numbers of cols & rows

ncol(raw_data)
nrow(raw_data)

culled <- raw_data %>% filter(str_detect(Clue, "^(?=.*\\?$)(?:(?!\\_).)*$|perhaps$"))

### I can barely understand any of this, but it gets all that end in either "?" or "perhaps" and omits any with "_"
###


###The Test!
str_detect(culled, "\\_")

###The Math!
x <- 24880
y <- 781573
z <- ((x/y)*100)

### Separating dates into seperate columns

clean <- separate(raw_data, Date, c("Month", "Day", "Year"))

rlang::last_error()
?separate
