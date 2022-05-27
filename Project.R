#### The Libraries! ----

### Installing and loading the packages 
#install.packages("tidyverse")
#install.packages("plotly")
#install.packages("RColorBrewer")
library(tidyverse) ## loads ggplot2, dplyr, tidyr, & stringr (& readr, purr, tibble, & forcats)
library(lubridate) ##tidyverse non-core
library(plotly)
library(RColorBrewer)

### Connecting to Chart Studio
Sys.setenv(plotly_username="robbrownell")
Sys.setenv(plotly_api_key="Y7ytptmQ7sjZpguIAmEb")

### Chart Studio Template
#api_create(your_plot, filename = "your-filename")

api_create(last_plot(), filename = NULL)



?api_create.ggplot

#### The Data! ----
### Loading the New York Times (NYT) Crossword Puzzle data and saving it to a new dataframe called raw_data

raw_data <- read.csv("https://heybertrussell.github.io/DS_Proj/nytcrosswords.csv", stringsAsFactors = FALSE)

### Identifying the numbers of columns and rows in the dataset

ncol(raw_data) # 3 columns - Date, Word, Clue
nrow(raw_data) # 781,573 rows 


### Creating a regex for finding indirect clues
# It detects all strings in the "Clue" column that end in "?" and omits any with "_"

examples <- raw_data %>% 
  filter(!raw_data$Date %in% c("6/19/2014")) %>%
  filter(str_detect(Clue, "^(?=.*\\?$)(?:(?!\\_).)*"))

###Export csv for examples

write.csv(examples, "examples.csv")

### * Modified Data ----
### Creating a new dataframe that includes a new column identifying whether the clue type is "direct" or "indirect"

crossword_cluetype <- raw_data %>%
  transform("ClueType"=ifelse((str_detect(Clue, "^(?=.*\\?$)(?:(?!\\_).)*")), "indirect", "direct"))


### Testing how to group by the column "Date" and add new columns calculating sums of direct and indirect clue types


  ## These are separate dataframes that:
    # Calculate the total number of clues in the crossword puzzle for each date
        #total_clues_perday <- crossword_cluetype %>%
        #group_by(Date) %>%
        #summarise(num_day = n())
  
   # Calculate the total number of indirect clue types in the crossword puzzle for each date
      #total_indirect_perday <- crossword_cluetype %>% 
        #group_by(Date) %>%
        #summarise(num_ind = sum(ClueType == "indirect"))
  
  # Calculate the total number of direct clue types in the crossword puzzle for each date
    #total_direct_perday <- crossword_cluetype %>% 
      #group_by(Date) %>%
      #summarise(num_dir = sum(ClueType == "direct")) 


  ## Testing how to put all those calculations from the separate dataframes into one dataframe
    # First test:
      #test <- crossword_cluetype %>% 
      #group_by(Date, ClueType) %>%
      #summarise(n())

    #Second test:
      #test2 <- crossword_cluetype %>% 
      #group_by(Date, ClueType)


### Creating a new dataframe that takes the existing crossword_cluetype dataframe, groups the rows by the column "Date," and add new columns calculating the sums of all, direct, and indirect clue types per day

nums_by_date <- crossword_cluetype %>% 
  group_by(Date) %>% summarise(num_day = n(),
                               num_ind = sum(ClueType == "indirect"),
                               num_dir = sum(ClueType == "direct"))

### Creating a new dataframe that adds a new column to the existing nums_by_date dataframe that calculates the percent of indirect clue types per day 

percent_ind_by_date <- nums_by_date %>%
  transform(percent_ind = ((num_ind/num_day) * 100))


### Modifying the format of the dates in the existing percent_ind_by_date dataframe to make the dates readable by the lubridate package by changing the data type into a date object

percent_ind_date_mod <- percent_ind_by_date %>% 
  filter(!percent_ind_by_date$Date %in% c("6/19/2014")) %>% #removing an outlier 
  transform(Date = mdy(nums_by_date$Date))

###  Adding 5 columns to the existing percent_ind_date_mod dataframe that identify the month, day of the week, week of the year, day of the year, and year associated with the date of the crossword puzzle

crossword_final <- percent_ind_date_mod %>% 
  transform(Month = month(percent_ind_date_mod$Date, label = TRUE), # Month 
            WkDay = wday(percent_ind_date_mod$Date, label = TRUE), # Day of week
            WkYear = week(percent_ind_date_mod$Date), # Week of the year
            YDay = yday(percent_ind_date_mod$Date), # Day of the year
            Year = year(percent_ind_date_mod$Date)) # Year


###   Calculating the total number of direct and indirect clue types in the existing crossword_final dataframe

total_direct <- crossword_final %>% summarise(sum(num_dir))
total_indirect <- crossword_final %>% summarise(sum(num_ind))

### Removing scientific notations

options(scipen=999)

### * Group and Plot ----

### Plotting the percent of indirect clues for each day in the crossword_final dataframe

      percent_scatter <- ggplot(crossword_final)+ #scatter plot
        geom_point(aes(x=Date,
                       y=percent_ind,
                       text = paste(Date),
                       color = Year)) +
        geom_smooth(aes(x=Date,
                        y=percent_ind),
                        color = "#333333")+
        geom_smooth(aes(x=Date,
                        y=percent_ind),
                    method = "lm",
                    color="black") +
        scale_color_viridis_c(option = 'turbo',direction = -1) +
        labs( title = "Each Day Plotted",
              x = "Month",
              y = "Average Percent Indirect") +
        theme(legend.position="none")
      
      ggplotly(percent_scatter, tooltip = "text")


###   Grouping the existing crossword_final dataframe in different ways, creating new dataframes, and plotting them:

  ## ** By year ----

    # Grouping by year
      year_group <- crossword_final %>% 
        group_by(Year) %>%  
        summarize(Average_Percent_Indirect = round(mean(percent_ind), digits = 2))
      
    # Plotting by year
      year_bar <- ggplot(year_group, aes(x=Year,
                                         y=Average_Percent_Indirect,
                                         fill=Average_Percent_Indirect,
                                         text=paste("<b>Year</b>:",Year,"\n<b>Percent Indirect</b>:",Average_Percent_Indirect)))+ #bar plot
        geom_col() +
        labs( title = "By Year",
              x = "Year",
              y = "Average Percent Indirect") +
        theme(legend.position="none") +
        scale_fill_viridis_c(option = 'turbo',direction = -1)
      
      ggplotly(year_bar, tooltip = "text")


  ## ** By date of the week ----
      
      #Grouping by day of the week
      weekday_group <- crossword_final %>% 
        group_by(WkDay) %>%  
        summarize(Average_Percent_Indirect = round(mean(percent_ind), digits = 2))
      
      #Plotting by day of the week
      weekday_bar <- ggplot(weekday_group)+ #bar plot
        geom_col(aes(x=WkDay,
                     y=Average_Percent_Indirect,
                     fill=Average_Percent_Indirect,
                     text=paste("<b>Weekday</b>:",WkDay, "\n<b>Percent Indirect</b>:",paste0(Average_Percent_Indirect,"%"))))+
        labs( title = "By Day of the Week",
              x = "Day",
              y = "Average Percent Indirect") +
        theme(legend.position="none") +
        scale_fill_viridis_c(option = 'turbo',direction = -1)
      
      ggplotly(weekday_bar, tooltip = "text")
      

  ## ** By month ----
      
      #Grouping by month
      month_group <- crossword_final %>% 
        group_by(Month) %>%  
        summarize(Average_Percent_Indirect = round(mean(percent_ind), digits = 2))

      #Plotting by month
      month_bar <- ggplot(month_group)+ #bar plot
        geom_col(aes(x=Month,
               y=Average_Percent_Indirect,
               fill=Average_Percent_Indirect,
               text=paste("<b>Month</b>:",Month,"\n<b>Average Indirect</b>:",paste0(Average_Percent_Indirect,"%")))) +
        labs( title = "By Month of the Year",
              x = "Month",
              y = "Average Percent Indirect") +
        scale_fill_viridis_c(option = 'turbo',direction = -1) +
        theme(legend.position="none")
      
      ggplotly(month_bar, tooltip = "text")

      month_line <- ggplot(month_group)+ #line plot
        geom_line(aes(x=Month,
                y=Average_Percent_Indirect,
                group=1))+
        labs( title = "By Month of the Year",
              x = "Month",
              y = "Average Percent Indirect") +
        theme(legend.position="none")

      ggplotly(month_line)
      

  ## ** By week of the year ----
      
      #Grouping by week of the year
      weekyear_group <- crossword_final %>% 
        group_by(WkYear) %>%  
        summarize(Average_Percent_Indirect = round(mean(percent_ind), digits = 2))

      #Plotting by week of the year
      weekyear_line <- ggplot(weekyear_group)+ #line plot
        geom_line(aes(x=WkYear,
                y=Average_Percent_Indirect,
                group=1))
      
      ggplotly(weekyear_line)
      

  ## ** By week of the year and year ----
      
      #Grouping by week of the year and year
      week_year_group<- crossword_final %>% 
        group_by(WkYear,Year) %>% 
        summarize(Average_Percent_Indirect = round(mean(percent_ind), digits = 2))

      #Plotting by week of the year and year 
      week_year_scatter <- ggplot(week_year_group)+ #scatter plot
        geom_point(aes(x=WkYear,
                 y=Average_Percent_Indirect))+
        geom_smooth(aes(x=WkYear, 
                  y=Average_Percent_Indirect))
      
      ggplotly(week_year_scatter)

      
  ## ** By clue type and summarized total number of each clue type ----
    
    # Grouping by clue type and total of each
      clue_total <- crossword_cluetype %>% 
        group_by(ClueType) %>% 
        summarize(total_num = n())

    # Plotting by clue type and total of each
      # Plotting by clue type and total of each
      clue_total_pie <- ggplot(clue_total, aes(x="", y=total_num, fill=ClueType)) + #pie chart
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        geom_label(aes(label = total_num),
          position = position_stack(vjust = 0.5),
          show.legend = FALSE) +
        labs(x = NULL,
             y = NULL,
                fill = "Clue Type") +
        theme_void() +
        theme(legend.position = 'left') +
        ggtitle("Total of Number of Clues by Type") +
        scale_fill_discrete(labels = c("Direct", "Indirect"))

      clue_total_pie
