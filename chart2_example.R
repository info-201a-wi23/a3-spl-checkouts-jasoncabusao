library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)

# Create a df of medias that have over 10 checkouts over from 2017 to now
spl_df <- read.csv("~/Downloads/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)


# Create a dataframe with just Jeff Kinney  checkouts
kinney_df <- spl_df %>%
  filter(Creator %in% c("Jeff Kinney", "Kinney, Jeff"))

# Creat a column called `FullName` that combines all versions of their names into one recognizable name
kinney_df <- kinney_df %>% 
  mutate(FullName = ifelse(str_detect(Creator, "Kinney"), "Jeff Kinney"))


# Add a column called `date` that combines CheckoutYear and Checkout Month
kinney_df <- kinney_df %>%  
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

# Format this new column `Date` to As.Date
kinney_df$date <- as.Date(kinney_df$date, format = "%Y-%m-%d")



# Create a df that only includes necessary columns `FullName, Title, MaterialType, Checkouts, CheckoutYear, CheckoutMonth, date`
new_kinney_df <- kinney_df %>%
  select(FullName, Title, MaterialType, Checkouts, 
         CheckoutYear, CheckoutMonth, date)

# Create a df that shows each* media type checkouts for each* month
kinney_checkouts_per_month <- new_kinney_df %>%
  group_by(date, MaterialType) %>%
  summarise(month_total = sum(Checkouts))


# Create a line plot

chart2_x_values <- kinney_checkouts_per_month$date
chart2_y_values <- kinney_checkouts_per_month$month_total

chart2.breaks.vec.x <- seq(min(kinney_checkouts_per_month$date), 
                           max(kinney_checkouts_per_month$date), 
                           by = "4 months")

chart2 <- ggplot(data = kinney_checkouts_per_month) +
  geom_point(aes(x = chart2_x_values,
                 y = chart2_y_values,
                 color = MaterialType,
                 text = paste("Date:", date, "<br>",
                              "Medium:", MaterialType, "<br>", 
                              "Checkouts:", month_total))) +
  geom_line(aes(x = chart2_x_values, 
                y = chart2_y_values, 
                color = MaterialType)) +
  scale_color_brewer(palette = "Accent") +
  scale_x_date(breaks = chart2.breaks.vec.x, 
               date_labels = "%b '%y") +
  scale_y_continuous(breaks = seq(0, 2000, 100)) +
  labs(title = "Jeff Kinney's Checkouts at the SPL   (2017 - Present)",
       x = "Date",
       y = "Number of Checkouts",
       color = "Type of Media") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = .5))

ggplotly(chart2, tooltip = "text")
