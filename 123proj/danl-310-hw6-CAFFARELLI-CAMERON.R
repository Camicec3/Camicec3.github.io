
packages_needed <- c(
  "tidyverse",
  "plotly",
  "gganimate",
  "gifski",
  "scales"
)

packages_to_install <- packages_needed[!(packages_needed %in% installed.packages()[, "Package"])]
if (length(packages_to_install) > 0) {
  install.packages(packages_to_install)
}


library(tidyverse)
library(plotly)
library(gganimate)
library(gifski)
library(scales)

############################################################
# Read and clean the Airbnb data
############################################################


if (!dir.exists("data")) {
  dir.create("data")
}

# Unzip the archive CSV files 
if (length(list.files("data", pattern = "\\.csv$")) == 0 && file.exists("archive (5).zip")) {
  unzip("archive (5).zip", exdir = "data")
}

csv_files <- list.files("data", pattern = "\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) {
  stop("No CSV files were found. Make sure archive (5).zip is in the same folder as this R script.")
}

airbnb <- csv_files |>
  map_dfr(function(file) {
    file_name <- basename(file)
    city_name <- str_extract(file_name, "^[^_]+") |> str_to_title()
    day_name <- case_when(
      str_detect(file_name, "weekdays") ~ "Weekday",
      str_detect(file_name, "weekends") ~ "Weekend",
      TRUE ~ NA_character_
    )

    read_csv(file, show_col_types = FALSE) |>
      mutate(
        city = city_name,
        day_type = day_name
      )
  }) |>
  select(-starts_with("Unnamed")) |>
  mutate(
    city = factor(city),
    day_type = factor(day_type, levels = c("Weekday", "Weekend")),
    room_type = factor(room_type),
    host_is_superhost = factor(host_is_superhost)
  )

############################################################
# Summary data for plotting
############################################################

city_day_summary <- airbnb |>
  group_by(city, day_type) |>
  summarize(
    listings = n(),
    average_price = mean(realSum, na.rm = TRUE),
    median_price = median(realSum, na.rm = TRUE),
    average_satisfaction = mean(guest_satisfaction_overall, na.rm = TRUE),
    average_cleanliness = mean(cleanliness_rating, na.rm = TRUE),
    .groups = "drop"
  )

weekend_premium <- city_day_summary |>
  select(city, day_type, average_price) |>
  pivot_wider(names_from = day_type, values_from = average_price) |>
  mutate(
    weekend_premium = Weekend - Weekday,
    percent_change = weekend_premium / Weekday
  ) |>
  arrange(desc(weekend_premium))

############################################################
# Part 1: Interactive plot
############################################################

interactive_price_plot <- ggplot(
  city_day_summary,
  aes(
    x = reorder(city, average_price),
    y = average_price,
    fill = day_type,
    text = paste0(
      "City: ", city,
      "<br>Day type: ", day_type,
      "<br>Average price: ", dollar(round(average_price, 2), prefix = "€"),
      "<br>Median price: ", dollar(round(median_price, 2), prefix = "€"),
      "<br>Listings: ", listings,
      "<br>Average satisfaction: ", round(average_satisfaction, 2),
      "<br>Average cleanliness: ", round(average_cleanliness, 2)
    )
  )
) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "€")) +
  labs(
    title = "Average Airbnb Prices by City and Day Type",
    subtitle = "Hover over each bar to compare weekday and weekend listing prices",
    x = NULL,
    y = "Average listing price",
    fill = "Day type"
  ) +
  theme_minimal()

interactive_price_plotly <- ggplotly(interactive_price_plot, tooltip = "text") |>
  layout(legend = list(orientation = "h", x = 0.1, y = -0.15))

interactive_price_plotly

# Save the interactive plot as an HTML file.
htmlwidgets::saveWidget(
  interactive_price_plotly,
  file = "interactive-airbnb-price-plot.html",
  selfcontained = TRUE
)

############################################################
# Part 2: Animated plot
############################################################

animated_price_plot <- ggplot(
  city_day_summary,
  aes(
    x = reorder(city, average_price),
    y = average_price,
    fill = city
  )
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "€")) +
  labs(
    title = "Average Airbnb Prices Across Europe: {closest_state}",
    subtitle = "The animation compares weekday and weekend prices by city",
    x = NULL,
    y = "Average listing price"
  ) +
  theme_minimal() +
  transition_states(
    day_type,
    transition_length = 2,
    state_length = 2
  ) +
  ease_aes("cubic-in-out")


animated_airbnb_gif <- animate(
  animated_price_plot,
  nframes = 80,
  fps = 10,
  width = 900,
  height = 600,
  renderer = gifski_renderer("animated-airbnb-price-plot.gif")
)

animated_airbnb_gif

