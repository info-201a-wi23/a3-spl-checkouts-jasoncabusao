library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)

# Create a df of medias that have over 10 checkouts from 2017 to now
spl_df <- read.csv("~/Downloads/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Create a dataframe with just Jeff Kinney  checkouts
chart3_kinney_df <- spl_df %>%
  filter(Creator %in% c("Jeff Kinney", "Kinney, Jeff"))

# Creat a column called `FullName` that combines all versions of their names into one recognizable name
chart3_kinney_df <- chart3_kinney_df %>% 
  mutate(FullName = ifelse(str_detect(Creator, "Kinney"), "Jeff Kinney"))

# Add a column called `date` that combines CheckoutYear and Checkout Month
chart3_kinney_df <- chart3_kinney_df %>%  
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

# Format this new column `Date` to As.Date
chart3_kinney_df$date <- as.Date(chart3_kinney_df$date, format = "%Y-%m-%d")

# Create a df with only titles that have over 2220 checkouts
kinney_top_titles <- chart3_kinney_df %>% 
  group_by(Title) %>% 
  summarise(title_total = sum(Checkouts)) %>% 
  filter(title_total > 2220)

# Clean up the Title column so it only shows the book title
kinney_top_titles$Title <- gsub(" / by Jeff Kinney.", "", kinney_top_titles$Title)
kinney_top_titles$Title <- gsub("\\(.*", "", kinney_top_titles$Title)


# Create a bar graph showing these titles
chart3_custom_colors <- c("#8bdafc", "#e8d52c", "#d90902", "#6ee827", "#2344eb", "#19b5f7", "#5359d4", "#056125", "#f06d0a", "#7a0aad")



chart3 <- ggplot(data = kinney_top_titles) +
  geom_col(aes(x = title_total,
               y = reorder(Title, +title_total), 
               fill = Title,
               text = paste("Title:", Title, "<br>",
                            "Checkouts:", title_total))) + 
  labs(title = "Top 10 Jeff Kinney Works at the SPL   (2017 - Present)",
       y = "Title of Checkout",
       x = "Number of Checkouts") + 
  scale_fill_manual(values = chart3_custom_colors) +
  scale_x_continuous(breaks = seq(0, 3000, 200)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 3, size = 7),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = .5))

ggplotly(chart3, tooltip = "text")
