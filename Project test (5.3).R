#### The Libraries!

install.packages("tidyverse")
install.packages("plotly")
library(tidyverse) ## loads ggplot2, dplyr, tidyr, & stringr (& readr, purr, tibble, & forcats)
library(lubridate) ##tidyverse non-core
library(plotly)

#### The Data!

raw_data <- read.csv("https://heybertrussell.github.io/DS_Proj/nytcrosswords.csv", stringsAsFactors = TRUE)

#### numbers of cols & rows

ncol(raw_data)
nrow(raw_data)

#### Regex for what we want

culled <- raw_data %>% filter(str_detect(Clue, "^(?=.*\\?$)(?:(?!\\_).)*$|perhaps$"))

#### I can barely understand any of this, but it gets all that end in either "?" or "perhaps" and omits any with "_"

###The Test!
str_detect(culled, "\\_") #### Success![?]

###The Math!
x <- 24880
y <- 781573
z <- ((x/y)*100)

###Adding column identifying each as "direct" or "indirect"

raw_data_dicol <- raw_data %>% transform(direct=ifelse((str_detect(Clue, "^(?=.*\\?$)(?:(?!\\_).)*$|perhaps$")), "indirect", "direct"))

###group by date w/ columns for direct & indirect sums:

### These are seperate dfs

ttl_clues_pd <- raw_data_dicol %>% 
  group_by(Date) %>% summarise(num_day = n())

ttl_ind_pd <- raw_data_dicol %>% 
  group_by(Date) %>% summarise(num_ind = sum(direct == "indirect"))

ttl_dir_pd <- raw_data_dicol %>% 
  group_by(Date) %>% summarise(num_dir = sum(direct == "direct")) 

##### how to put all these into one string!?!?!?!?!?

##### Warmer:

test <- raw_data_dicol %>% 
  group_by(Date, direct) %>% summarise(n())

test2 <- raw_data_dicol %>% 
  group_by(Date, direct)

#### YES:

test3 <- ttl_clues_pd <- raw_data_dicol %>% 
  group_by(Date) %>% summarise(num_day = n(), num_ind = sum(direct == "indirect"), num_dir = sum(direct == "direct"))

#### Now for the percent!!!!

percenttest <- test3 %>% transform(percent_ind = ((num_ind/num_dir) * 100))

#### Export as CSV

write.csv(percenttest, "percenttest.csv")