# Quick find summary statistics
kinney_pop_title
kinney_pop_media
kinney_books_checkouts
kinney_highest_checkout_month
kinney_highest_checkout_month_count
kinney_2023_checkouts
kinney_2022_checkouts
kinney_2021_checkouts
kinney_2020_checkouts
kinney_2019_checkouts
kinney_total_checkouts

library(dplyr)

# Create the necessary df's to compute summary statistics
## Create a df of medias that have over 10 checkouts over from 2017 to now
spl_df <- read.csv("~/Downloads/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

## Create a dataframe with just Jeff Kinney  checkouts
kinney_df <- spl_df %>%
  filter(Creator %in% c("Jeff Kinney", "Kinney, Jeff"))

## Creat a column called `FullName` that combines all versions of their names into one recognizable name
kinney_df <- kinney_df %>% 
  mutate(FullName = ifelse(str_detect(Creator, "Kinney"), "Jeff Kinney"))

## Add a column called `date` that combines CheckoutYear and Checkout Month
kinney_df <- kinney_df %>%  
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

## Format this new column `Date` to As.Date
kinney_df$date <- as.Date(kinney_df$date, format = "%Y-%m-%d")

## Create a df that only includes necessary columns `FullName, Title, MaterialType, Checkouts, CheckoutYear, CheckoutMonth, date`
new_kinney_df <- kinney_df %>% 
  select(FullName, Title, MaterialType, Checkouts, CheckoutYear, CheckoutMonth, date)

# Clean up the Title column so it only shows the book title
new_kinney_df$Title <- gsub(" / by Jeff Kinney.", "", new_kinney_df$Title)
new_kinney_df$Title <- gsub("\\(.*", "", new_kinney_df$Title)



# Calculate the necessary summary statistics
### What is Jeff Kinney's most popular checkout?
kinney_pop_title <- new_kinney_df %>% 
  group_by(Title) %>% 
  summarise(title_checkouts = sum(Checkouts)) %>% 
  filter(title_checkouts == max(title_checkouts)) %>% 
  pull(Title)

### What is Jeff Kinney's most popular media type?
kinney_pop_media <- new_kinney_df %>% 
  group_by(MaterialType) %>% 
  summarise(media_checkouts = sum(Checkouts)) %>% 
  filter(media_checkouts == max(media_checkouts)) %>% 
  pull(MaterialType)

### How many checkouts did Jeff Kinney's books have overall
kinney_books_checkouts <- new_kinney_df %>% 
  group_by(MaterialType) %>% 
  summarise(media_checkouts = sum(Checkouts)) %>% 
  filter(media_checkouts == max(media_checkouts)) %>% 
  pull(media_checkouts)

## Create a datafram showing Jeff Kinney's checkouts each year
yearly_kinney_checkouts <- kinney_2023_checkouts <- new_kinney_df %>% 
  group_by(date = as.integer(format(date, '%Y'))) %>% 
  summarise(yearly_checkouts = sum(Checkouts))

### What month had Jeff Kinney highest checkout number
kinney_highest_checkout_month <- new_kinney_df %>% 
  group_by(date) %>% 
  summarise(monthly_checkouts = sum(Checkouts)) %>% 
  filter(monthly_checkouts == max(monthly_checkouts)) %>% 
  pull(date)

### How many checkouts did Jeff Kinney have in the month where he his highest checkouts.
kinney_highest_checkout_month_count <- new_kinney_df %>% 
  group_by(date) %>% 
  summarise(monthly_checkouts = sum(Checkouts)) %>% 
  filter(monthly_checkouts == max(monthly_checkouts)) %>% 
  pull(monthly_checkouts)

### How many checkouts did Jeff Kinney have in 2023?
kinney_2023_checkouts <- yearly_kinney_checkouts %>% 
  filter(date == 2023) %>% 
  pull(yearly_checkouts)

### How many checkouts did Jeff Kinney have in 2022?
kinney_2022_checkouts <- yearly_kinney_checkouts %>% 
  filter(date == 2022) %>% 
  pull(yearly_checkouts)

### How many checkouts did Jeff Kinney have in 2021?
kinney_2021_checkouts <- yearly_kinney_checkouts %>% 
  filter(date == 2021) %>% 
  pull(yearly_checkouts)

### How many checkouts did Jeff Kinney have in 2020?
kinney_2020_checkouts <- yearly_kinney_checkouts %>% 
  filter(date == 2020) %>% 
  pull(yearly_checkouts)

### How many checkouts did Jeff Kinney have in 2019?
kinney_2019_checkouts <- yearly_kinney_checkouts %>% 
  filter(date == 2019) %>% 
  pull(yearly_checkouts)

### How many checkouts did Jeff Kinney have in 2018?
kinney_2018_checkouts <- yearly_kinney_checkouts %>% 
  filter(date == 2018) %>% 
  pull(yearly_checkouts)

### How many checkouts did Jeff Kinney have in 2017?
kinney_2017_checkouts <- yearly_kinney_checkouts %>% 
  filter(date == 2017) %>% 
  pull(yearly_checkouts)

### How many total checkout did Jeff Kineey have from 2017 to now?
kinney_total_checkouts <- sum(new_kinney_df$Checkouts)
