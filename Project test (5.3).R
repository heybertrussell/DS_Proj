#### The Libraries!

install.packages("tidyverse")
install.packages("plotly")
library(tidyverse) ## loads ggplot2, dplyr, tidyr, & stringr (& readr, purr, tibble, & forcats)
library(lubridate) ##tidyverse non-core
library(plotly)

#### The Data!

raw_data <- read.csv("https://heybertrussell.github.io/DS_Proj/nytcrosswords.csv", stringsAsFactors = FALSE)

#### numbers of cols & rows

ncol(raw_data)
nrow(raw_data)

#### Regex for what we want

examples <- raw_data %>% filter(str_detect(Clue, "^(?=.*\\?$)(?:(?!\\_).)*$|perhaps$"))

#### I can barely understand any of this, but it gets all that end in either "?" or "perhaps" and omits any with "_"

###The Test!
str_detect(culled, "\\_") #### Success![?]

###The Math!
x <- 24880
y <- 781573
z <- ((x/y)*100)

###Adding column identifying each as "direct" or "indirect"

raw_data_dicol <- raw_data %>%
  transform(direct=ifelse((str_detect(Clue, "^(?=.*\\?$)(?:(?!\\_).)*$|perhaps$")), "indirect", "direct"))

###group by date w/ columns for direct & indirect sums:

### These are separate dfs

ttl_clues_pd <- raw_data_dicol %>% 
  group_by(Date) %>%
  summarise(num_day = n())

ttl_ind_pd <- raw_data_dicol %>% 
  group_by(Date) %>%
  summarise(num_ind = sum(direct == "indirect"))

ttl_dir_pd <- raw_data_dicol %>% 
  group_by(Date) %>%
  summarise(num_dir = sum(direct == "direct")) 

##### how to put all these into one string!?!?!?!?!?

##### Warmer:

test <- raw_data_dicol %>% 
  group_by(Date, direct) %>%
  summarise(n())

test2 <- raw_data_dicol %>% 
  group_by(Date, direct)

#### YES:

dates_nums <- raw_data_dicol %>% 
  group_by(Date) %>% summarise(num_day = n(),
                               num_ind = sum(direct == "indirect"),
                               num_dir = sum(direct == "direct"))

###### Maybe check numbers add col3&4 and check if matches col 2?????

#### Now for the percent!!!!

dn_percents <- dates_nums %>%
  transform(percent_ind = ((num_ind/num_dir) * 100))

#### Export as CSV: write.csv(dates_nums, "datesnums.csv")

#### Modifying Dates

dater_nums <- dn_percents %>% transform(Date = mdy(dates_nums$Date))

#### adding month name, day of week, week of year

dater_nums_plus <- dater_nums %>% 
  transform(Month = month(dater_nums$Date, label = TRUE), ### Month Name
            WkDay = wday(dater_nums$Date, label = TRUE), ### Day of Week
            WkYear = week(dater_nums$Date), ### Week of Year
            YDay = yday(dater_nums$Date), ### Day of Year
            Year = year(dater_nums$Date))

my_tst <- dater_nums_plus %>% mutate(Year = year(Date)) %>% group_by(Year, Month) %>% summarize(mean(percent_ind))

### Groupby year

mytst2 <- dater_nums_plus %>% group_by(Year) %>%  summarize(av_per = mean(percent_ind))

### Groupby WkDay

wkday_grp <- dater_nums_plus %>% group_by(WkDay) %>%  summarize(av_per = mean(percent_ind))

#ttl number of each

ttldir <- dater_nums_plus %>% summarise(sum(num_dir))
ttlind <- dater_nums_plus %>% summarise(sum(num_ind))

ggplot()

?count

no_out <- filter(dater_nums_plus$Date, !2014-06-19)

?filter

test2 <- ggplot(dater_nums_plus)+
  geom_point(aes(x=Date,
                 y=percent_ind,
                 text = paste(Date)))
##  geom_smooth(aes(x=original_publication_year,
               ##   y=ratings_count,
 ## ), method = "lm")

ggplotly(test2, tooltip = "text")




### year barplot

linetest <- ggplot(mytst2)+
  geom_col(aes(x=Year,
                y=av_per,
               text=paste(Year, av_per)))
ggplotly(linetest, tooltip = "text")

### WkDay Barplot

wkday_bar <- ggplot(wkday_grp)+
  geom_col(aes(x=WkDay,
               y=av_per,
               text=paste(WkDay, "\n",av_per)))
ggplotly(wkday_bar, tooltip = "text")

####grpby month

month_grp <- dater_nums_plus %>% group_by(Month) %>%  summarize(av_per = mean(percent_ind))

month_bar <- ggplot(month_grp)+
  geom_col(aes(x=Month,
               y=av_per,
               text=paste(Month, "\n",av_per)))
ggplotly(month_bar, tooltip = "text")

month_line <- ggplot(month_grp)+
  geom_line(aes(x=Month,
                y=av_per,
                group=1))
month_line


###grpby wk/yr

wkyr_grp <- dater_nums_plus %>% group_by(WkYear) %>%  summarize(av_per = mean(percent_ind))

wkyr_line <- ggplot(wkyr_grp)+
  geom_line(aes(x=WkYear,
                y=av_per,
                group=1))

wkyr_grp

wkyr_line

#####wkyr scatter

wk_yr_gp <- dater_nums_plus %>% group_by(WkYear,Year) %>% summarise(av_per = mean(percent_ind))

wk_yr_sp <- ggplot(wk_yr_gp)+
  geom_point(aes(x=WkYear,
                 y=av_per))+
  geom_smooth(aes(x=WkYear, 
                  y=av_per))

wk_yr_sp

##### piechart

options(scipen=999)

pie_dat <- raw_data_dicol %>% group_by(direct) %>% summarise(total_num = n())

pie <- ggplot(pie_dat, aes(x="", y=total_num, fill=direct))+
  geom_col()+
  coord_polar(theta="y")

pie


bartestyr <- ggplot(dater_nums_plus)+
  geom_col(aes(x=(group_by(year), summarise(mean(percent_ind)),
                y=percent_ind)))

bartestyr
