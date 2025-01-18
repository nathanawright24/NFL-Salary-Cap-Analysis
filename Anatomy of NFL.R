# Read the CSV file into R
data <- read.csv("C:/Users/acbdm/OneDrive/Desktop/BGA Projects/Anatomy of NFL.csv", stringsAsFactors = FALSE)

# View the first few rows of the data
head(data)


# Load the necessary packages
library(dplyr)
library(scales)

# Format the averaged data to add dollar signs and commas
average_spending_formatted <- average_spending %>%
  mutate(
    avg_QB = dollar(avg_QB),
    avg_RB = dollar(avg_RB),
    avg_WR = dollar(avg_WR),
    avg_TE = dollar(avg_TE),
    avg_OL = dollar(avg_OL),
    avg_Edge = dollar(avg_Edge),
    avg_IDL = dollar(avg_IDL),
    avg_LB = dollar(avg_LB),
    avg_CB = dollar(avg_CB),
    avg_S = dollar(avg_S),
    
  )

# View the resulting formatted dataset
print(average_spending_formatted)

library(ggplot2)
library(dplyr)

# Function to create a pie chart for a given year
create_pie_chart <- function(year_data, year) {
  ggplot(year_data, aes(x = "", y = Spending, fill = Position)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
              position = position_stack(vjust = 0.5), 
              color = "white", size = 3) +
    labs(title = paste("Spending Distribution by Position -", year), fill = "Position") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank())
}

# Loop through each year and plot the pie chart
years <- unique(average_spending_long$Year)

for (year in years) {
  # Filter data for the current year
  year_data <- average_spending_long %>% filter(Year == year)
  
  # Replace any missing values (NA) with zeros in the spending data
  year_data <- year_data %>%
    mutate(Spending = ifelse(is.na(Spending), 0, Spending)) %>%
    filter(!is.na(Position))  # Ensure Position is not NA
  
  # Calculate Percentage
  if (sum(year_data$Spending) > 0) {
    year_data <- year_data %>%
      mutate(Percentage = (Spending / sum(Spending)) * 100)
  } else {
    next  # Skip if there's no spending data
  }
  
  # Plot the pie chart for the current year
  print(create_pie_chart(year_data, year))
}

library(ggplot2)
library(dplyr)
library(scales)

# Box plot for spending by position
create_box_plot <- function(data) {
  ggplot(data, aes(x = Position, y = Spending, fill = Position)) +
    geom_boxplot(outlier.shape = 19, outlier.size = 1) +  # Show outliers with specified shape and size
    labs(title = "Spending Distribution by Position", x = "Position", y = "Spending (in millions)") +
    scale_y_continuous(
      labels = label_number(scale = 1e-6, suffix = "M"),  # Format y-axis labels
      limits = c(10e6, NA),  # Set y-axis limits starting at 10 million
      breaks = seq(10e6, max(data$Spending, na.rm = TRUE), by = 10e6)  # Set breaks at every 10 million
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x labels for better readability
          legend.position = "none")  # Remove legend if not needed
}

# Check if average_spending_long has the necessary columns
if (!all(c("Position", "Spending") %in% colnames(average_spending_long))) {
  stop("The dataset must contain 'Position' and 'Spending' columns.")
}

# Create the box plot
box_plot <- create_box_plot(average_spending_long)
print(box_plot)


# Load necessary packages
library(rvest)
library(dplyr)

# Define a function to scrape and filter the draft data for each year
scrape_draft_data <- function(url, teams) {
  # Read the HTML of the page
  webpage <- read_html(url)
  
  # Extract the draft table (first table on the page)
  draft_data <- webpage %>%
    html_node("table") %>% 
    html_table(fill = TRUE)
  
  # Set first row as column names and remove it from the data
  colnames(draft_data) <- draft_data[1, ]  # Set first row as column names
  draft_data <- draft_data[-1, ]  # Remove the first row from the data
  
  # Fix any duplicate column names
  colnames(draft_data) <- make.unique(colnames(draft_data))
  
  # Check for and fix any NA or empty column names
  colnames(draft_data)[is.na(colnames(draft_data)) | colnames(draft_data) == ""] <- "Unnamed"
  
  # Convert factor columns to character type if needed
  draft_data <- draft_data %>%
    mutate(across(where(is.factor), as.character))
  
  # Optionally, remove rows with NA in the 'Tm' column (team column)
  draft_data <- draft_data %>%
    filter(!is.na(Tm))
  
  # Filter the draft data for the specified teams
  filtered_draft_data <- draft_data %>%
    filter(Tm %in% teams)
  
  return(filtered_draft_data)
}

# URLs for each year
urls <- list(
  "2018" = "https://www.pro-football-reference.com/years/2018/draft.htm#drafts",
  "2019" = "https://www.pro-football-reference.com/years/2019/draft.htm#drafts",
  "2020" = "https://www.pro-football-reference.com/years/2020/draft.htm#drafts",
  "2021" = "https://www.pro-football-reference.com/years/2021/draft.htm#drafts",
  "2022" = "https://www.pro-football-reference.com/years/2022/draft.htm#drafts",
  "2023" = "https://www.pro-football-reference.com/years/2023/draft.htm#drafts"
)

# Teams for each year
teams_list <- list(
  "2018" = c("LAR", "KAN", "NOR", "NWE"),
  "2019" = c("GNB", "TEN", "KAN", "SFO"),
  "2020" = c("GNB", "BUF", "KAN", "TAM"),
  "2021" = c("SFO", "KAN", "LAR", "CIN"),
  "2022" = c("SFO", "KAN", "PHI", "CIN"),
  "2023" = c("KAN", "SFO", "BAL", "DET")
)

# Scrape and filter the draft data for each year
draft_2018 <- scrape_draft_data(urls$`2018`, teams_list$`2018`)
draft_2019 <- scrape_draft_data(urls$`2019`, teams_list$`2019`)
draft_2020 <- scrape_draft_data(urls$`2020`, teams_list$`2020`)
draft_2021 <- scrape_draft_data(urls$`2021`, teams_list$`2021`)
draft_2022 <- scrape_draft_data(urls$`2022`, teams_list$`2022`)
draft_2023 <- scrape_draft_data(urls$`2023`, teams_list$`2023`)


head(draft_2018)
head(draft_2019)
head(draft_2020)
head(draft_2021)
head(draft_2022)
head(draft_2023)

# Combine the draft data for all years
combined_draft_data <- bind_rows(draft_2018, draft_2019, draft_2020, draft_2021, draft_2022, draft_2023)

# Check if combined data is empty or has missing values
if (nrow(combined_draft_data) == 0) {
  stop("Combined draft data is empty. Check your source data for issues.")
}

# Make sure relevant columns exist, such as Rnd (Round), Tm (Team), Pos (Position)
combined_draft_data <- combined_draft_data %>%
  select(Rnd, Tm, Pos) %>%
  filter(!is.na(Rnd), !is.na(Pos))  # Remove rows with missing Rnd or Pos

# Check the data after filtering
if (nrow(combined_draft_data) == 0) {
  stop("No valid data available after filtering for Rnd and Pos.")
}

# Group data by Rnd and Pos to get counts for each round and position
draft_position_counts <- combined_draft_data %>%
  group_by(Rnd, Pos) %>%
  summarise(Count = n(), .groups = "drop")

# Check if draft_position_counts has data
if (nrow(draft_position_counts) == 0) {
  stop("No data available after grouping by Rnd and Pos.")
}

# Add percentages to the data
draft_position_counts <- draft_position_counts %>%
  group_by(Rnd) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Check the structure of draft_position_counts
head(draft_position_counts)

# Create pie charts for each round
library(ggplot2)

# Function to generate pie charts with percentages
generate_piechart <- function(round_data, round_number) {
  ggplot(round_data, aes(x = "", y = Count, fill = Pos)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) + # Add percentage labels
    labs(title = paste("Position Distribution - Round", round_number),
         fill = "Position") +
    theme_void() +
    theme(legend.position = "right")
}

# Generate and plot pie charts for each round
for(round_num in unique(draft_position_counts$Rnd)) {
  round_data <- draft_position_counts %>% filter(Rnd == round_num)
  print(generate_piechart(round_data, round_num))
}
