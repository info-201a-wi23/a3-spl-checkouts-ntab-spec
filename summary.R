install.packages("styler")
install.packages("lintr")
library(dplyr)
library(tidyverse)
library(stringr)

library_data_set <- read.csv("~/Documents/info201/a3-spl-checkouts-ntab-spec/2022-2023-All-Checkouts-SPL-Data.csv")


#This code creates a new variable called digital_books by filtering a library dataset based on the MaterialType column containing specific values. 
#It then groups the resulting subset of data by MaterialType and calculates the mean number of checkouts for each group using the mean() function. 
#The resulting output is a summary table with two columns: MaterialType and avg_checkouts.

digital_books <- library_data_set %>% # Creates a new variable called digital_books, which is a subset of the library_data_set dataset.
  filter(MaterialType %in% c("EBOOK", "VISUAL","SOUNDCASS", "VIDEODISC", "BOOK", "SOUNDDISC", "ATLAS","MUSIC", "REGPRINT", "AUDIOBOOK")) %>% # Filters the library_data_set based on MaterialType column and keeps rows with specific values.
  group_by(MaterialType) %>% # Groups the filtered data by MaterialType column.
  summarize(avg_checkouts = mean(Checkouts)) # Calculates the mean of Checkouts column for each group of MaterialType and creates a new column avg_checkouts in the resulting summary table.


#This R code creates a new variable top_ten_most_checked_out_books, which filters and extracts the top 10 most checked out 
#books from the library_data_set dataset based on the Title and Creator columns.
#The resulting table contains the book titles, their creators, and the total number of checkouts for each book.

top_ten_most_checked_out_books <- library_data_set %>% # Create a new variable called 'top_ten_most_checked_out_books' and use the pipe operator to pass 'library_data_set' to the next function
  filter(!grepl("Headphones / Seattle Public Library|SPL HotSpot : connecting Seattle / \\[distributed by T-Mobile\\]", Title)) %>% # Filter out rows that contain specific strings in their Title column
  group_by(Title, Creator) %>% # Group the data by the Title and Creator columns
  summarize(total_checkouts = sum(Checkouts), .groups = "drop") %>% # Sum the checkouts for each group and drop the grouping information from the output
  arrange(desc(total_checkouts)) %>% # Sort the data in descending order based on total_checkouts column
  top_n(n = 10, total_checkouts) # Extract the top 10 rows based on the total_checkouts column



#This R code creates a new variable called most_checkouts_month_physical, which filters the library_data_set dataset to include only physical books, groups the data by checkout month, and calculates the total checkouts for each month. 
#Finally, the code sorts the resulting table in descending order of checkouts.

most_checkouts_month_physical <- library_data_set %>%  # Start with the library data set
  filter(MaterialType == "BOOK") %>%  # Filter only physical books
  group_by(CheckoutMonth) %>%  # Group by checkout month
  summarize(total_checkouts = sum(Checkouts)) %>%  # Sum the total checkouts for each month
  arrange(desc(total_checkouts))  # Sort by descending order of checkouts


#This R code creates a new variable total_checkouts_by_year, which groups the library_data_set dataset by the CheckoutYear column and calculates the total checkouts for each year. 
#The resulting table contains two columns, CheckoutYear and total_checkouts, with the total number of checkouts for each year.

total_checkouts_by_year <- library_data_set %>% # Create a new variable called 'total_checkouts_by_year' and use the pipe operator to pass 'library_data_set' to the next function
  group_by(CheckoutYear) %>% # Group the data by the CheckoutYear column
  summarize(total_checkouts = sum(Checkouts)) # Sum the checkouts for each group and create a new column called 'total_checkouts' that contains the summed values



#This R code creates a new variable top_fiction_books, which filters and extracts the top checked out fiction books from the library_data_set dataset. 
#The filtering is done by searching for the word "Fiction" in the Subjects column. 
# The resulting table contains the MaterialType categories, the total number of checkouts for each MaterialType group, sorted in descending order based on the total number of checkouts.

top_fiction_books <- library_data_set %>%  # filter and extract top checked out fiction books from library_data_set
  filter(str_detect(Subjects, fixed("Fiction"))) %>% # filter rows where the Subjects column contains the word "Fiction"
  group_by(MaterialType) %>%    # group the filtered data by the MaterialType column
  summarize(total_checkouts = sum(Checkouts)) %>%   # calculate the sum of Checkouts for each MaterialType group
  arrange(desc(total_checkouts))    # sort the summarized data in descending order of total_checkouts



