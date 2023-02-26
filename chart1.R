library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)

# Create a df of medias that have over 10 checkouts from 2017 to now
spl_df <- read.csv("~/Downloads/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Create a dataframe with just Jeff Kinney and Colleen Hoover checkouts
hoover_and_kinney_df <- spl_df %>%
  filter(Creator %in% c("Jeff Kinney", "Kinney, Jeff", 
                        "Colleen Hoover", "Hoover, Colleen"))

# Creat a column called `FullName` that combines all versions of their names into one recognizable name
hoover_and_kinney_df <- hoover_and_kinney_df %>% 
  mutate(FullName = ifelse(str_detect(Creator, "Kinney"), 
                           "Jeff Kinney", "Colleen Hoover"))


# Add a column called `date` that combines CheckoutYear and Checkout Month
hoover_and_kinney_df <- hoover_and_kinney_df %>%  
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

# Format this new column `Date` to As.Date
hoover_and_kinney_df$date <- as.Date(hoover_and_kinney_df$date, format = "%Y-%m-%d")


# Create a df that only includes necessary columns `FullName, Title, MaterialType, Checkouts, CheckoutYear, CheckoutMonth, date`
new_df <- hoover_and_kinney_df %>% 
  select(FullName, Title, MaterialType, Checkouts, CheckoutYear, CheckoutMonth, date) 


# Create a df that shows each* authors checkouts for each* month
checkouts_per_month <- new_df %>% 
  group_by(date, FullName) %>% 
  summarise(month_total = sum(Checkouts))

# Create a line plot

chart1_x_values <- checkouts_per_month$date
chart1_y_values <- checkouts_per_month$month_total

chart1.breaks.vec.x <- seq(min(checkouts_per_month$date), 
                           max(checkouts_per_month$date), 
                           by = "3 months")

chart1 <- ggplot(data = checkouts_per_month) +
  geom_point(aes(x = chart1_x_values,
                 y = chart1_y_values,
                 color = FullName,
                 text = paste("Date:", date, "<br>",
                              "Author:", FullName, "<br>", 
                              "Checkouts:", month_total))) +
  geom_line(aes(x = chart1_x_values, 
                y = chart1_y_values, 
                color = FullName)) +
  scale_color_brewer(palette = "Pastel1") +
  scale_x_date(breaks = chart1.breaks.vec.x, 
               date_labels = "%b '%y") +
  scale_y_continuous(breaks = seq(0, 3000, 200)) +
  labs(title = "Colleen Hoover Checkouts Surpasses Jeff Kinney's",
       x = "Date",
       y = "Number of Checkouts",
       color = "Author") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = .5))

ggplotly(chart1, tooltip = "text")
