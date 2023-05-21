#Install all required packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(naniar)
library(mapdata)
library(maps)
library(seqinr)
library(knitr)
library(corrplot)
library(lubridate)
library(readxl)



#A) DATA CLEANING OF KAGGLE DATASET

#load dataset
startups_total <- read_excel("startup dataset.xlsx")


#remove permalink & homepage column (unnecessary for prediction)

startups_total <- startups_total[,-1]
startups_total <- startups_total[,-2]
startups_total <- startups_total[,-6]
startups_total <- startups_total[,-7]

summary(startups_total)

# Convert total funding to numeric
startups_total$funding_total_usd <- as.numeric(startups_total$funding_total_usd)
startups_total$funding_total_usd <- round(startups_total$funding_total_usd)

# Convert founding tiMne to Date format
startups_total$founded_at <- as.Date(startups_total$founded_at, format = "%Y-%m-%d")

# Convert first funding to Date format
startups_total$first_funding_at <- as.Date(startups_total$first_funding_at, format = "%Y-%m-%d")

# Convert last funding to Date format
startups_total$last_funding_at <- as.Date(startups_total$last_funding_at, format = "%Y-%m-%d")

# Clean the category list feature (i.e. industry)
startups_total$category_list <- sapply(strsplit(as.character(startups_total$category_list), split = "\\|"), `[`, 1)



#B) DESCRIPTIVE STATS 

#B.0) BASICS

#checking out the structure of the dataset
str(startups_filtered)

#summarizing the dataset
summary(startups_filtered)

#calculating basic statistics for numeric values
# assuming 'funding_total_usd' and 'funding_rounds' are the only numeric variables in the dataset
mean(startups_filtered$funding_total_usd)
median(startups_filtered$funding_total_usd)
sd(startups_filtered$funding_total_usd)
quantile(startups_filtered$funding_total_usd, c(0.25, 0.75))
range(startups_filtered$funding_rounds)

#creating frequency tables for categorical variables
# assuming 'status' and 'country_code' are categorical variables in the dataset
table(startups_filtered$status)
table(startups_filtered$country_code)

# Count the number of unique countries in the dataset
num_countries <- startups_filtered %>% 
  distinct(country_code) %>% 
  nrow()

# Create a list of all the countries in the dataset
country_list <- startups_filtered %>% 
  distinct(country_code) %>% 
  pull(country_code)

# Print the results
cat("Number of countries represented in the dataset:", num_countries, "\n")
cat("List of countries represented in the dataset:\n")
cat(country_list, sep = "\n")


# Show total number of companies per country
startups_filtered %>%
  group_by(country_code) %>%
  summarise(num_companies = n()) %>%
  arrange(desc(num_companies))

# Create a bar chart to visualize the top 5 countries with the most companies 
startups_filtered %>%
  group_by(country_code) %>%
  summarise(num_companies = n()) %>%
  arrange(desc(num_companies)) %>%
  top_n(5, num_companies) %>%
  ggplot(aes(x = country_code, y = num_companies)) +
  geom_bar(stat = "identity") +
  xlab("Country") +
  ylab("Number of Companies") +
  ggtitle("Number of Companies per Country (Top 5)")


# Select only the numeric columns in the dataset
numeric_cols <- startups_filtered %>% 
  select_if(is.numeric)

# Calculate the correlation matrix
cor_mat <- cor(numeric_cols)

# Print the correlation matrix
print(cor_mat)

# Select only the numeric columns in the dataset
numeric_cols <- startups_filtered %>% 
  select_if(is.numeric)

# Calculate the correlation matrix
cor_mat <- cor(numeric_cols)

# Plot the correlation matrix using corrplot
corrplot(cor_mat, method = "color", type = "upper", tl.col = "black", tl.srt = 45)


# Check for missing values in the numeric columns
numeric_cols <- startups_filtered %>% 
  select_if(is.numeric)
sum(is.na(numeric_cols))

# Check if all columns are numeric
all_cols_numeric <- sapply(startups_filtered, is.numeric)
sum(!all_cols_numeric)


#B.1) ORIGINAL KAGGLE DATASET

#Missing values absolute: show how large the gaps of data missing are in the Kaggle dataset
# Create a missing data matrix plot
gg_miss_var(startups_total) +
  ggtitle("Missing Data Matrix Plot") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

# Missing values percent: Create a bar chart showing percentage of missing data per column
startups_total %>% 
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>% 
  pivot_longer(everything(), names_to = "Column", values_to = "Percent Missing") %>%
  ggplot(aes(x = Column, y = `Percent Missing`)) +
  geom_bar(stat = "identity", fill = "firebrick2") +
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = "Percent missing")


#B.2) ORIGINAL KAGGLE DATASET vs FILTERED

# Count unique values for all columns in startups_total and startups_filtered
total_counts <- startups_total %>% 
  summarise(across(everything(), n_distinct)) %>% 
  mutate(Dataset = "Original Kaggle Dataset")

filtered_counts <- startups_filtered %>% 
  summarise(across(everything(), n_distinct)) %>% 
  mutate(Dataset = "Filtered Dataset")

# Combine the two data frames
combined_counts <- bind_rows(total_counts, filtered_counts)

# Replace missing values with 0
extra_cols <- setdiff(names(startups_filtered), names(startups_total))
if (length(extra_cols) > 0) {
  combined_counts <- combined_counts %>%
    mutate(across(extra_cols, ~ ifelse(is.na(.x), 0, .x)))
}

# Reshape the data and create the table
table_counts <- combined_counts %>% 
  pivot_longer(cols = -Dataset, names_to = "Column", values_to = "Unique_Counts") %>% 
  pivot_wider(names_from = "Dataset", values_from = "Unique_Counts", values_fill = 0) %>% 
  kable()

# Print the table
print(table_counts)


#B.3) GEOGRAPHIC DISTRIBUTION: WHERE DO MOST STARTUPS COME FROM?

#TOP COUNTRIES IN MAP

library(countrycode)
library(ggthemes)

# convert iso-3 country codes to full names
# those 2 outside the countrycode package
startups_filtered$country_code[startups_filtered$country_code == "ROM"] <- "Romania"
startups_filtered$country_code[startups_filtered$country_code == "TAN"] <- "Tanzania"
# rest can be matched
for (i in 1:length(startups_filtered$country_code)) {
  country_name <- countrycode(startups_filtered$country_code[i], "iso3c", "country.name")
  if (!is.na(country_name)) {
    startups_filtered$country_code[i] <- country_name
  }
}

# rename column to "country"
names(startups_filtered)[names(startups_filtered) == 'country_code'] <- 'country'


# Count the frequency of each country name
country_counts <- startups_filtered %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n))


# Correct manually any inconsistency in country naming to world map data
world_map <- map_data("world")
non_matches <- setdiff(unique(country_counts$country), unique(world_map$region))
non_matches

country_counts$country[country_counts$country == "Hong Kong SAR China"] <- "China"
country_counts[country_counts$country == "China", "n"] <- 
  sum(country_counts[country_counts$country == "China", "n"])
country_counts <- unique(country_counts)

country_counts$country[country_counts$country == "Czechia"] <- "Czech Republic"
country_counts$country[country_counts$country == "Saint Martin (French part)"] <- "Saint Martin"
country_counts$country[country_counts$country == "United States"] <- "USA"
country_counts$country[country_counts$country == "United Kingdom"] <- "UK"


# Aggregate country_counts data by country name
country_counts_agg <- country_counts %>%
  group_by(country) %>%
  summarize(n = sum(n))

# Merge country_counts data with world_map data
merged_data <- merge(world_map, country_counts_agg, by.x = "region", by.y = "country", all.x = TRUE)

# Create a ggplot object with the merged data
ggplot(merged_data, aes(x = long, y = lat, group = group, fill = n)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "blue", high = "red") +
  coord_equal() +
  labs(title = "Frequency of Startups by Country")

# Export corresponding table

library("stargazer")
stargazer(country_counts,                 # Export txt
          summary = FALSE,
          type = "text",
          out = "Geographic Distribution.txt")

# TOP 7 COUNTRIES IN PIE CHART

# calculate the relative percentage and add a new column
country_counts_agg$percent <- (country_counts_agg$n / 17089) * 100
country_counts_agg <- country_counts_agg %>% 
  mutate(percent = round(percent, 2))

# sort the data frame by percent in descending order
country_counts_agg <- country_counts_agg[order(-country_counts_agg$percent),]

# combine the top 7 countries and group the rest as "Other"
top_countries <- country_counts_agg$country[1:7]
country_counts_agg$country <- ifelse(country_counts_agg$country %in% top_countries,
                                     country_counts_agg$country,
                                     "Other")

# Assign colors to each country
my_colors <- c(rep("orange", 1), rep("blue", 1), rep("brown", 1), rep("green", 1), rep("red", 1), rep("pink", 1), rep("yellow", 1), rep("grey", 1))


# Set the levels of the country variable in the desired order
country_counts_agg$country <- factor(country_counts_agg$country,
                                     levels = c("USA", "UK", "Canada", "China", "India", "Israel", "France", "Other"))

# create a pie chart of the top 7 countries and "Other"
ggplot(country_counts_agg[1:8,], aes(x="", y=percent, fill=country)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = my_colors) +
  labs(title = "Country Percentages", fill = "Country") +
  theme_void() # make pie chart with labels in excel?


#B.4.) STRICT VS LENIENT DEFINITION OF SUCCESS: HOW MUCH DOES IT CHANGE IN OUR DATASET?

# Create a summary table of counts
startups_filtered %>% dplyr::count(status_strict)
startups_filtered %>% dplyr::count(status_lenient)

combined_table <- bind_cols(
  startups_filtered %>% dplyr::count(status_strict),
  startups_filtered %>% dplyr::count(status_lenient)
)
combined_table <- combined_table %>%
  select(-status_lenient) %>%
  rename("Status" = "status_strict", "Strict" = "n...2" , "Lenient" = "n...4")


# Bar plot of counts by strict status
ggplot(combined_table, aes(x = Status, y = Strict)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(x = "Strict Status", y = "Count", title = "Counts by Strict Status")

# Bar plot of counts by lenient status
ggplot(combined_table, aes(x = Status, y = Lenient)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(x = "Strict Status", y = "Count", title = "Counts by Lenient Status")


#MOST POPULAR COUNTRY
#creation of world map and only highlighting the country with the most counts

# Calculate counts by country code
iso_counts <- startups_filtered %>%
  group_by(country_code) %>%
  summarise(counts = n())

# Merge counts with country boundaries
world_map <- map_data("world")
merged <- merge(world_map, iso_counts, by.x = "region", by.y = "country_code", all.x = TRUE)

# Plot the map
ggplot(merged, aes(x = long, y = lat, group = group, fill = counts)) +
  geom_polygon() +
  scale_fill_gradientn(colours = terrain.colors(10)) +
  labs(title = "Title of the Map") +
  theme_void() +
  
  
#B.4) FUNDING DISTRIBUTION: WHEN/ HOW FREQUENTLY & HOW MUCH DID THE START UPS GET?

# Extract the year from the founded_at column
startups_filtered$year <- as.numeric(format(as.Date(startups_filtered$founded_at), "%Y"))

# Count the number of companies by year
num_companies_by_year <- startups_filtered %>% 
  group_by(year) %>% 
  summarise(num_companies = n())

# Print the results
num_companies_by_year

# Create a bar chart of the number of companies by year
ggplot(num_companies_by_year, aes(x = year, y = num_companies)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of Companies by Year") +
  xlab("Year") +
  ylab("Number of Companies")


# Count the number of companies in each industry
num_companies_by_industry <- startups_filtered %>% 
  group_by(category_list) %>% 
  summarise(num_companies = n()) %>%
  arrange(desc(num_companies))

# Print the results
num_companies_by_industry


# Reorder data frame by number of companies in descending order
num_companies_by_industry <- num_companies_by_industry[order(-num_companies_by_industry$num_companies),]

# Create a bar chart of the number of companies by industry
ggplot(num_companies_by_industry[1:10,], aes(x = reorder(category_list, -num_companies), y = num_companies)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of Companies by Industry") +
  xlab("Industry") +
  ylab("Number of Companies") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Sum the funding amounts by region
total_funding_by_region <- startups_filtered %>%
  group_by(region) %>%
  summarise(total_funding = sum(funding_total_usd, na.rm = TRUE)) %>%
  arrange(desc(total_funding))

# Print the results
total_funding_by_region

# Sum the funding amounts by region (used for the below bar chart)
total_funding_by_region <- startups_filtered %>%
  group_by(region) %>%
  summarise(total_funding = sum(funding_total_usd, na.rm = TRUE)) %>%
  arrange(desc(total_funding)) %>%
  slice(1:10)

total_funding_by_region %>%
  mutate(total_funding_mil = total_funding / 1000000) %>%
  ggplot(aes(x = reorder(region, -total_funding_mil), y = total_funding_mil)) +
  geom_bar(stat = "identity", fill = "grey") +
  theme_minimal() +
  labs(x = "Region", y = "Total Funding (Million USD)") +
  scale_y_continuous(labels = scales::comma_format()) +
  coord_flip()


#visualization of continents by total funding
# create sample data
set.seed(123)
total_funding_by_region <- data.frame(
  region = sample(c("North America", "Europe", "Asia", "Latin America"), 100, replace = TRUE),
  total_funding = runif(100, 1000000, 50000000)
)

# reorder the regions by their total funding in descending order
total_funding_by_region <- total_funding_by_region %>% 
  arrange(desc(total_funding)) %>% 
  mutate(region = factor(region, levels = unique(region)))

# create a ggplot object and add the data
plot <- ggplot(total_funding_by_region, aes(x = total_funding/1000000, y = region))

# add a horizontal bar chart
plot <- plot + geom_bar(stat = "identity", fill = "gray30")

# set the title and axis labels
plot <- plot + ggtitle("Top 10 Regions by Total Funding") +
  xlab("Total Funding (Million USD)") +
  ylab("Region") +
  theme(axis.text.y = element_text(hjust = 0),
        axis.title.y = element_text(hjust = 0),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

# display the plot
plot

# Filter the data for the "SF Bay Area" region
sf_bay_area <- startups_filtered %>%
filter(region == "SF Bay Area")

# Extract the year from the "founded_at" column
sf_bay_area$founded_year <- year(ymd(sf_bay_area$founded_at))

# Sum the funding amounts by year and convert to Million USD
funding_by_year <- sf_bay_area %>%
  group_by(founded_year) %>%
  summarise(total_funding = sum(funding_total_usd, na.rm = TRUE)/1000000) %>%
  filter(!is.na(founded_year))

# Create a line plot of the funding by year
ggplot(funding_by_year, aes(x = founded_year, y = total_funding)) +
  geom_line() +
  ggtitle("Total Funding by Year in SF Bay Area") +
  xlab("Year Founded") +
  ylab("Total Funding (Million USD)")


# Create scatterplot to visualize average amount per round (USD) and average time between rounds (days)
ggplot(startups_filtered, aes(x = average_time_between_rounds, y = average_amount_per_round / 1000000)) +
  geom_point() +
  labs(x = "Average Time Between Rounds (Days)", y = "Average Amount per Round (Million USD)") +
  theme_classic()


# Create scatterplot to show if companies with the most funding_rounds raised the most money
ggplot(startups_filtered, aes(x = funding_rounds, y = funding_total_usd/1000000)) +
  geom_point() +
  labs(x = "Funding Rounds", y = "Total Funding (Million USD)") +
  theme_classic()


# Create a new variable "rounds_range"
startups_filtered <- startups_filtered %>%
  mutate(rounds_range = cut(funding_rounds, breaks = c(0, 1, 3, 5, Inf), 
                            labels = c("1 round", "2-3 rounds", "4-5 rounds", "6+ rounds")))

# Create a bar chart of number of companies by rounds range
ggplot(startups_filtered, aes(x = rounds_range, fill = rounds_range)) +
  geom_bar() +
  labs(x = "Funding Rounds", y = "Number of Companies", 
       title = "Number of Companies by Funding Rounds Range") +
  theme_bw()


# Create a scatter plot of funding amount per round vs. number of funding rounds
ggplot(startups_filtered, aes(x = funding_rounds, y = funding_total_usd/1000000, color = status_strict)) +
  geom_point() +
  labs(x = "Number of Funding Rounds", y = "Funding Amount per Round (Million USD)", 
       title = "Funding Amount per Round vs. Number of Funding Rounds by Company Status") +
  theme_bw()


# Create a bar chart of average time between rounds by funding rounds range
ggplot(startups_filtered, aes(x = rounds_range, y = average_time_between_rounds, fill = rounds_range)) +
  geom_bar(stat = "identity") +
  labs(x = "Funding Rounds", y = "Average Time Between Rounds (Days)", 
       title = "Average Time Between Rounds by Funding Rounds Range") +
  theme_bw()

# Create a bar chart of average amount per round by funding rounds range
ggplot(startups_filtered, aes(x = rounds_range, y = average_amount_per_round/1000000, fill = rounds_range)) +
  geom_bar(stat = "identity") +
  labs(x = "Funding Rounds", y = "Average Amount per Round (Million USD)", 
       title = "Average Amount per Round by Funding Rounds Range") +
  theme_bw()

# Create a scatter plot of funding amount vs. number of funding rounds
ggplot(startups_filtered, aes(x = funding_rounds, y = funding_total_usd/1000000, color = status_strict)) +
  geom_point() +
  labs(x = "Number of Funding Rounds", y = "Funding Amount (Million USD)", 
       title = "Funding Amount vs. Number of Funding Rounds by Company Status") +
  theme_bw()

# Create a scatter plot of average time between rounds vs. number of funding rounds
ggplot(startups_filtered, aes(x = funding_rounds, y = average_time_between_rounds, color = status_strict)) +
  geom_point() +
  labs(x = "Number of Funding Rounds", y = "Average Time Between Rounds (Days)", 
       title = "Average Time Between Rounds vs. Number of Funding Rounds by Company Status") +
  theme_bw()


# Filter data to include only companies with "success" or "failure" status
startups_filtered_sf <- startups_filtered %>%
  filter(status_strict %in% c("success", "failure") & region == "SF Bay Area")

# Create a scatter plot of funding amount vs. average time between rounds
ggplot(startups_filtered_sf, aes(x = average_time_between_rounds, y = funding_total_usd/1000000, color = status_strict)) +
  geom_point() +
  labs(x = "Average Time Between Rounds (Days)", y = "Funding Amount (Million USD)", 
       title = "Funding Amount vs. Average Time Between Rounds by Company Status in SF Bay Area") +
  theme_bw()


# Create a bar chart of number of companies by region with the most funding
top_regions <- startups_filtered %>%
  group_by(region) %>%
  summarise(total_funding = sum(funding_total_usd/1000000, na.rm = TRUE)) %>%
  arrange(desc(total_funding)) %>%
  slice(1:10)

ggplot(top_regions, aes(x = reorder(region, -total_funding), y = total_funding, fill = region)) +
  geom_bar(stat = "identity") +
  labs(x = "Region", y = "Total Funding (Million USD)", 
       title = "Top 10 Regions by Total Funding") +
  theme_bw()


# Show if companies with the most funding_rounds were successful ("status_strict" = success)and therefore, if companies with the lowest funding_rounds failed ("status_strict" = failure)
# Filter data to include only companies with "success" or "failure" status
startups_filtered_sf <- startups_filtered %>%
  filter(status_strict %in% c("success", "failure") & region == "SF Bay Area")

# Create a bar chart of funding_total_usd by number of funding_rounds
ggplot(startups_filtered_sf, aes(x = funding_rounds, y = funding_total_usd/1000000, fill = status_strict)) +
  geom_col(position = "dodge") +
  labs(x = "Funding Rounds", y = "Total Funding (Million USD)", title = "Funding Rounds vs Total Funding by Success/Failure Status for SF Bay Area Companies") +
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +
  theme_minimal()



#C) APPLY INCLUSION CRITERIA (AFTER 2000 AND 2 OR MORE FUNDING ROUNDS) -> FILTERED DF

# Filter out startups founded before 2010 and with funding_rounds less than 2
startups_filtered <- subset(startups_total, as.integer(format(as.Date(founded_at), "%Y")) >= 2000 & funding_rounds > 1)

#Take out all NAs
startups_filtered <- na.omit(startups_filtered)

# Change a faulty date (2105 doesn't exist)
# Find the row index where the name is "Rasyonel R&D"
row_index <- which(startups_filtered$name == "Rasyonel R&D")

# Change the value in the "last_funding_at" column to "2015-05-01"
startups_filtered$last_funding_at[row_index] <- "2015-05-01"



#D) FEATURE CREATION

#Feature 1: Difference between first and last funding in months
startups_filtered$delta_first_last_funding <- round(as.numeric(difftime(startups_filtered$last_funding_at, startups_filtered$first_funding_at, units = "weeks"))/4.345, 0)

#Feature 2: Average Time between Funding Rounds (in months)
startups_filtered$average_time_between_rounds <- round(startups_filtered$delta_first_last_funding / startups_filtered$funding_rounds, 0)

#Feature 3: Average Amount raised per round (in USD)
startups_filtered$average_amount_per_round <- round(startups_filtered$funding_total_usd / startups_filtered$funding_rounds, 0)

#Feature 4: Average monthly expense (in USD)
startups_filtered$average_monthly_expense <- round((startups_filtered$funding_total_usd / startups_filtered$delta_first_last_funding) , 0)

#Feqture 5: Burn rate (estimation) - i.e. rate at which each start-up is spending its funding (which fraction of their total money are they burning per month between the first and last funding round?)
startups_filtered$burn_rate_estimate <- round((startups_filtered$average_monthly_expense / startups_filtered$funding_total_usd) , 2)

#Feature 6: Time between founding and first funding round (i.e. how long did they bootstrap (if positive) or how long did they have money before officially founding (if negative)? -> indicator of risk aversion (negative) or risk openness (positive)

startups_filtered$delta_founded_first_funding <- round(as.numeric(difftime(startups_filtered$first_funding_at, startups_filtered$founded_at, units = "weeks"))/4.345, 0)

#Feature 7: Time between founding and last funding round (some approximation of running time)
startups_filtered$delta_founded_last_funding <- round(as.numeric(difftime(startups_filtered$last_funding_at, startups_filtered$founded_at, units = "weeks"))/4.345, 0)

# Exclude rows with Inf values
startups_filtered <- startups_filtered[is.finite(startups_filtered$average_monthly_expense) & is.finite(startups_filtered$burn_rate_estimate), ]



#E) CREATION OF 2 OUTCOME VARIABLE VERSIONS (SUCCESS/FAILURE STRICT DEFINITION & SUCCESS/FAILURE LENIENT DEFINITION)

# Strict version: as defined in Kaggle dataframe
success_status <- c("acquired", "ipo")
failure_status <- c("closed")

startups_filtered$status <- ifelse(startups_filtered$status %in% success_status, "success",
                                   ifelse(startups_filtered$status %in% failure_status, "failure", "neutral"))
# Rename the "status" column to "status_strict"
names(startups_filtered)[names(startups_filtered) == "status"] <- "status_strict"

# Lenient version: failure if 2.5+ years no funding, success if 4 years and still operating, otherwise neutral

# Add column at position 5 called "status_lenient"
startups_filtered$status_lenient <- ""

# Loop through each row and fill in the "status_lenient" column
# Loop through each row and fill in the "status_lenient" column
for (i in 1:nrow(startups_filtered)) {
  if (startups_filtered$status_strict[i] %in% c("success", "failure")) {
    # If the status is "success" or "failure", copy it over to "status_lenient"
    startups_filtered$status_lenient[i] <- startups_filtered$status_strict[i]
  } else if (startups_filtered$status_strict[i] == "neutral") {
    # If the status is "neutral", check the conditions for "success" or "failure"
    if (!is.na(startups_filtered$founded_at[i])) {
      if (as.numeric(difftime(as.Date("2015-12-01"), startups_filtered$founded_at[i], units = "weeks")) > 48*4) {
        # If the startup was founded at least 4 years before 2015-12-01, classify as "success"
        startups_filtered$status_lenient[i] <- "success"
      } else if (!is.na(startups_filtered$last_funding_at[i])) {
        if (as.numeric(difftime(as.Date("2015-12-01"), startups_filtered$last_funding_at[i], units = "weeks")) > 30*4) {
          # If the last funding round was 2.5 years or longer before 2015-12-01, classify as "failure"
          startups_filtered$status_lenient[i] <- "failure"
        } else {
          # If none of the conditions are met, leave the status as "neutral"
          startups_filtered$status_lenient[i] <- "neutral"
        }
      } 
    }
  }
}


# Move column from position 16 to position 5 and reorder other columns
startups_filtered <- startups_filtered[c(1:4, 18, 5:17)]



#F) FURTHER PREPARATION STEPS -> MAKE DF FIT FOR PREDICTION

#F.1) Change date features to extract only year from date features to make numeric
startups_filtered_prediction <- startups_filtered

#founding year (founded_in)
startups_filtered_prediction$founded_at <- year(as.POSIXlt(startups_filtered_prediction$founded_at, format="%d/%m/%Y"))
names(startups_filtered_prediction)[names(startups_filtered_prediction) == 'founded_at'] <- 'founded_in'

#first funding round year (first_funding_in)
startups_filtered_prediction$first_funding_at <- year(as.POSIXlt(startups_filtered_prediction$first_funding_at, format="%d/%m/%Y"))
names(startups_filtered_prediction)[names(startups_filtered_prediction) == 'first_funding_at'] <- 'first_funding_in'

#last funding round year (last_funding_in)
startups_filtered_prediction$last_funding_at <- year(as.POSIXlt(startups_filtered_prediction$last_funding_at, format="%d/%m/%Y"))
names(startups_filtered_prediction)[names(startups_filtered_prediction) == 'last_funding_at'] <- 'last_funding_in'


#F.2) take out name column & change data classes to only numeric or factor

# remove name column
startups_filtered_prediction <- startups_filtered_prediction[,-1]

# check classes
column_classes <- lapply(startups_filtered_prediction, class)
for (i in 1:length(column_classes)) {
  cat(names(startups_filtered_prediction)[i], ":", column_classes[[i]], "\n")
}


#F.3) Fuzzy matching on the Levenshtein distance algorithm for category_list (attempt 1 to overcome granular categories)
library(fuzzywuzzyR)
# Define a threshold value for approximate string matching
threshold <- 0.05

# Iterate over each unique category in the column
for (cat in unique(startups_filtered_prediction$category_list)) {
  
  # Find similar categories
  similar_cats <- agrep(cat, unique(startups_filtered_prediction$category_list), max.distance = threshold, value = TRUE)
  
  # Replace similar categories with the current category
  if (length(similar_cats) > 1) {
    startups_filtered_prediction$category_list[startups_filtered_prediction$category_list %in% similar_cats] <- cat
  }
}

# Get unique categories
unique_categories <- data.frame(category = unique(startups_filtered_prediction$category_list))

#F.4) Group industries into even larger categories (74) (attempt 2 to overcome granular categories)

# Create a data frame with the matching pattern
matching_pattern <- data.frame(
  old_category_275 = c("3D", "Accounting", "Ad Targeting", "Adaptive Equipment", "Advanced Materials", "Advice", "Aerospace", "Agriculture", "Algorithms", "All Students", "Alumni", "Analytics", "Android", "Angels", "Animal Feed", "Anything Capital Intensive", "Art", "Assisted Living", "Auctions", "Audio", "Augmented Reality", "B2B", "Babies", "Baby Accessories", "Baby Boomers", "Baby Safety", "Banking", "Batteries", "Beauty", "Bicycles", "Billing", "Bio-Pharm", "Bioinformatics", "Biometrics", "Bitcoin", "Blogging Platforms", "Boating Industry", "Brand Marketing", "Brewing", "Bridging Online and Offline", "Broadcasting", "Brokers", "Browser Extensions", "Building Owners", "Building Products", "Business Intelligence", "Business Productivity", "Cable", "Cannabis", "Cars", "Celebrity", "Chat", "Chemicals", "Classifieds", "Clinical Trials", "Cloud Computing", "Cloud Management", "Coffee", "Collaboration", "Collaborative Consumption", "Collectibles", "Colleges", "Comics", "Commodities", "Communities", "Computers", "Construction", "Consulting", "Consumers", "Contact Centers", "Contact Management", "Content", "Cooking", "Corporate Wellness", "Cosmetic Surgery", "Coupons", "Coworking", "Craft Beer", "Creative", "Credit", "CRM", "Crowdfunding", "Crowdsourcing", "Curated Web", "Cyber", "Dental", "E-Books", "E-Commerce", "Ediscovery", "EdTech", "Education", "Edutainment", "Electric Vehicles", "Electronics", "Email", "Employer Benefits Programs", "Employment", "Energy", "Enterprises", "Entrepreneur", "Events", "Exercise", "Face Recognition", "Families", "Farmers Market", "Farming", "Fashion", "Film", "Finance", "FinTech", "Fitness", "Flash Sales", "Fleet Management", "Food Processing", "Freelancers", "Freemium", "Fruit", "Fuels", "Furniture", "Gadget", "Game", "Gay & Lesbian", "General Public Worldwide", "Geospatial", "Gift Exchange", "Governance", "Governments", "Gps", "Graphics", "Green", "Groceries", "Group SMS", "Guides", "Hardware", "Heavy Industry", "High Tech", "Home & Garden", "Home Renovation", "Hospitals", "Hotels", "Human Resources", "Humanitarian", "ICT", "Identity", "Image Recognition", "Incentives", "Incubators", "Indoor Positioning", "Industrial", "Infrastructure", "Innovation Engineering", "Innovation Management", "Insurance", "Intellectual Asset Management", "Interest Graph", "Internet", "Investment Management", "iOS", "iPad", "iPhone", "Journalism", "Kids", "Language Learning", "Lasers", "Law Enforcement", "Lead Generation", "Lead Management", "Legal", "Leisure", "Licensing", "Life Sciences", "Lifestyle", "Linux", "Local", "Logistics", "M2M", "Machine Learning", "Manufacturing", "Marketplaces", "Material Science", "Media", "Mens Specific", "Messaging", "mHealth", "MicroBlogging", "Mobile", "Monetization", "Music", "Natural Language Processing", "Navigation", "Networking", "News", "Nightlife", "Non Profit", "Nonprofits", "Nutrition", "Online Reservations", "Online Scheduling", "Open Source", "Opinions", "Optimization", "Organic Food", "Outdoors", "Outsourcing", "P2P Money Transfer", "Parking", "Payments", "Peer-to-Peer", "Pets", "Pharmaceuticals", "Photo Editing", "Photo Sharing", "Photography", "Politics", "Presentations", "Price Comparison", "Printing", "Privacy", "Project Management", "Promotional", "Public Relations", "Public Safety", "Publishing", "QR Codes", "Real Estate", "Real Time", "Religion", "Restaurants", "Retail", "Retirement", "Ride Sharing", "Risk Management", "Robotics", "SaaS", "Sales and Marketing", "Search", "Security", "Semiconductors", "Senior Citizens", "Sensors", "SEO", "Services", "SexTech", "Shipping", "Simulation", "Skill Assessment", "Soccer", "Social Business", "Social Buying", "Social Commerce", "Social Television", "Software", "Solar", "Stock Exchanges", "Storage", "Synchronization", "Systems", "Technology", "Telecommunications", "Therapeutics", "Ticketing", "Tourism", "Toys", "Tracking", "Trading", "Translation", "Travel", "Video", "VoIP", "Watch", "Web Hosting", "Wine And Spirits", "Wireless"), # Add all the old categories here
  new_category_74 = c("Technology", "Professional Services", "Advertisement", "Gadgets", "Industry", "Professional Services", "Science", "Farming", "Science", "Education", "Education", "Science", "Software", "Funding", "Nutrition", "Financial Services", "Art", "People", "Financial Services", "Audio", "Technology", "Business", "Baby", "Baby", "Generation", "Baby", "Financial Services", "Energy", "Wellbeing", "Sports", "Professional Services", "Life Science", "BioTech", "BioTech", "Payment", "Online", "Industry", "Advertisement", "Beverages", "Logistics", "Services", "Financial Services", "Software", "Real Estate", "Real Estate", "Business", "Business", "Hardware", "Plants", "Mobility", "People", "Messaging", "Life Science", "Life Science", "Life Science", "Technology", "Technology", "Beverages", "Collaboration", "Collaboration", "Gadgets", "Education", "Leisure", "Commodities", "People", "Technology", "Industry", "Professional Services", "People", "People", "People", "Online", "Leisure", "Wellbeing", "Wellbeing", "Pricing", "Collaboration", "Beverages", "Creative", "Credit", "CRM", "Funding", "Funding", "Online", "Online", "Health", "Online", "Online", "Online", "Online", "Education", "Education", "Electronics", "Electronics", "Messaging", "Professional Services", "Employment", "Energy", "Business", "Business", "Events", "Sports", "Technology", "People", "Farming", "Farming", "Fashion", "Film", "Financial Services", "Financial Services", "Sports", "Financial Services", "Logistics", "Industry", "Employment", "Business", "Nutrition", "Energy", "Home", "Gadgets", "Game", "People", "Public", "Geography", "Public", "Politics", "Politics", "Geography", "Creative", "Health", "Health", "Messaging", "Education", "Hardware", "Industry", "Technology", "Home & Garden", "Home & Garden", "Health", "Travel", "Employment", "People", "ICT", "Identity", "Technology", "Employment", "Business", "Other", "Industry", "Infrastructure", "Innovation", "Innovation", "Financial Services", "Financial Services", "Visual", "Software", "Financial Services", "Software", "Hardware", "Hardware", "Journalism", "People", "Education", "Technology", "Legal", "Business", "Business", "Legal", "Leisure", "Business", "Life Science", "Wellbeing", "Linux", "Local", "Logistics", "Business", "Technology", "Industry", "Online", "Science", "Media", "Gender", "Messaging", "Health", "Online", "Mobility", "Business", "Leisure", "Technology", "Travel", "People", "Journalism", "Leisure", "Public", "Public", "Nutrition", "Online", "Online", "Technology", "Public", "Business", "Nutrition", "Sports", "Business", "Financial Services", "Mobility", "Payments", "People", "Leisure", "Life Science", "Photography", "Photography", "Photography", "Politics", "Business", "Pricing", "Journalism", "People", "Business", "Advertisement", "Public", "Public", "Journalism", "Technology", "Real Estate", "Time", "Religion", "Nutrition", "Retail", "People", "Mobility", "Business", "Technology", "Technology", "Business", "Search", "Security", "Hardware", "People", "Hardware", "Advertising", "Services", "Technology", "Logistics", "Business", "Employment", "Sports", "Social", "Social", "Social", "Social", "Software", "Energy", "Financial Services", "Logistics", "Process", "Hardware", "Technology", "Communication", "Life Science", "Services", "Travel", "Gadgets", "Geography", "Financial Services", "Speaking", "Travel", "Visual", "Technology", "Gadgets", "Online", "Beverages", "Technology") # Add all the new categories here
)

# Replace the values in the category_list column according to the matching pattern
startups_filtered_prediction <- startups_filtered_prediction %>%
  left_join(matching_pattern, by = c("category_list" = "old_category_275")) %>%
  mutate(category_list = ifelse(is.na(new_category_74), category_list, new_category_74)) %>%
  select(-new_category_74)

# View the updated unique categories
unique_categories <- data.frame(category = unique(startups_filtered_prediction$country_code))



#G) ML PREDICTION ON STRICT SUCCESS DEFINITION

# G.1) Abstraction level 1: All regions, countries & 74 industries

# Load necessary packages
library(mlr3verse)
library(mlr3pipelines)
library(mlr3viz)
library(mlr3learners)
library(ranger)
library(R6)
library(forcats)
library(mlr3filters)
library(mlr3fselect)
library(FSelectorRcpp)

# Subset the data to include only strict definition of success / failure without the neutral status
startups_filtered_prediction_strict <- startups_filtered_prediction %>%
  select(-status_lenient) %>%
  rename("status" = "status_strict") %>%
  filter(status != "neutral") %>%
  mutate_at(vars(category_list, country_code), as.factor) %>%
  mutate(status = as.numeric(factor(status, levels = c("failure", "success")))-1) %>%
  na.omit()

# Group low-frequency levels as "Other" (final attempt 3 to overcome granular categories)
startups_filtered_prediction_strict <- startups_filtered_prediction_strict %>%
  mutate(category_list = fct_lump_min(category_list, min = 5, other_level = "Other"),
         country_code = fct_lump_min(country_code, min = 5, other_level = "Other"),
         region = fct_lump_min(region, min = 5, other_level = "Other"))


# LOGISTIC REGRESSION & RANDOM FOREST

# Create task using filtered data & stratify
task_status <- as_task_classif(startups_filtered_prediction_strict, target = "status", id = "status_classif", strata = "status",
                               stratify = TRUE)


# Define logistic regression learner & pipeline
imputer = po("imputemedian")
learner_logreg = lrn("classif.log_reg")
learner_logreg_pipe = as_learner(imputer %>>% learner_logreg)

# Define random forest learner & pipeline
learner_rf = lrn("classif.ranger")
learner_rf_pipe = as_learner(imputer %>>% learner_rf)

# Train logistic regression and random forest models on the entire dataset
learner_logreg_pipe$train(task = task_status)
learner_rf_pipe$train(task = task_status)

# Set up cross-validation with stratification
rdesc <- rsmp("cv", folds = 5)
set.seed(1)

# Cross-validation for logistic regression
res_logreg <- resample(learner = learner_logreg_pipe, task = task_status, resampling = rdesc)
print(res_logreg$aggregate(msr("classif.acc")))
autoplot(res_logreg, measure = msr("classif.acc"))

# Cross-validation for random forest
res_rf <- resample(learner = learner_rf_pipe, task = task_status, resampling = rdesc)
print(res_rf$aggregate(msr("classif.acc")))
autoplot(res_rf, measure = msr("classif.acc"))

# BENCHMARKING

# Train featureless learner
learner_fl = lrn("classif.featureless")
learner_fl$train(task = task_status)
set.seed(2)

# Define resampling scheme and benchmarking design
design_classif = benchmark_grid(
  tasks = task_status,
  learners = list(learner_logreg_pipe, learner_rf_pipe, learner_fl), # Add random forest learner to the list
  resamplings = rsmp("cv", folds = 5)
)

# Benchmark
bm_classif = benchmark(design_classif)

# Define performance measures
bmr_classif = bm_classif$aggregate(msr("classif.acc"))
print(bmr_classif)
plot_to_save <- autoplot(bm_classif, measure = msr("classif.acc")) + 
  ggtitle("Benchmarking: Strict")

# Save plot
png("benchmarking_strict.png", width = 1600, height = 1200, res = 300)
print(plot_to_save)
dev.off()

# CONFUSION MATRIX

#featureless
prediction = learner_fl$predict(task = task_status)
prediction
table(prediction$truth, prediction$response)

#logistic regression
prediction = learner_logreg_pipe$predict(task = task_status)
prediction
table(prediction$truth, prediction$response)


#random forest
prediction = learner_rf_pipe$predict(task = task_status)
prediction
table(prediction$truth, prediction$response)




# G.2) Abstraction level 2: region SF Bay v. others, all countries & 74 industries

# Subset the data to include only strict definition of success / failure without the neutral status
startups_filtered_prediction_strict <- startups_filtered_prediction %>%
  select(-status_lenient) %>%
  rename("status" = "status_strict") %>%
  filter(status != "neutral") %>%
  mutate_at(vars(category_list, country_code), as.factor) %>%
  mutate(status = as.numeric(factor(status, levels = c("failure", "success")))-1) %>%
  na.omit()

# Group low-frequency levels as "Other" (final attempt 3 to overcome granular categories)
startups_filtered_prediction_strict <- startups_filtered_prediction_strict %>%
  mutate(category_list = fct_lump_min(category_list, min = 5, other_level = "Other"),
         country_code = fct_lump_min(country_code, min = 5, other_level = "Other"),
         region = fct_lump_min(region, min = 5, other_level = "Other"))

# make SF Bay Area v other out of region column
startups_filtered_prediction_strict <- startups_filtered_prediction_strict %>%
  mutate(region = case_when(
    region == "SF Bay Area" ~ "SF Bay Area",
    region == "Other" ~ "Other",
    TRUE ~ "Other"
  )) %>%
  mutate(region = factor(region, levels = c("SF Bay Area", "Other")))


# LOGISTIC REGRESSION & RANDOM FOREST

# Create task using filtered data & stratify
task_status <- as_task_classif(startups_filtered_prediction_strict, target = "status", id = "status_classif", strata = "status",
                               stratify = TRUE)


# Define logistic regression learner & pipeline
imputer = po("imputemedian")
learner_logreg = lrn("classif.log_reg")
learner_logreg_pipe = as_learner(imputer %>>% learner_logreg)

# Define random forest learner & pipeline
learner_rf = lrn("classif.ranger")
learner_rf_pipe = as_learner(imputer %>>% learner_rf)

# Train logistic regression and random forest models on the entire dataset
learner_logreg_pipe$train(task = task_status)
learner_rf_pipe$train(task = task_status)

# Set up cross-validation with stratification
rdesc <- rsmp("cv", folds = 5)
set.seed(1)

# Cross-validation for logistic regression
res_logreg <- resample(learner = learner_logreg_pipe, task = task_status, resampling = rdesc)
print(res_logreg$aggregate(msr("classif.acc")))
autoplot(res_logreg, measure = msr("classif.acc"))

# Cross-validation for random forest
res_rf <- resample(learner = learner_rf_pipe, task = task_status, resampling = rdesc)
print(res_rf$aggregate(msr("classif.acc")))
autoplot(res_rf, measure = msr("classif.acc"))

# BENCHMARKING

# Train featureless learner
learner_fl = lrn("classif.featureless")
learner_fl$train(task = task_status)
set.seed(2)

# Define resampling scheme and benchmarking design
design_classif = benchmark_grid(
  tasks = task_status,
  learners = list(learner_logreg_pipe, learner_rf_pipe, learner_fl), # Add random forest learner to the list
  resamplings = rsmp("cv", folds = 5)
)

# Benchmark
bm_classif = benchmark(design_classif)

# Define performance measures
bmr_classif = bm_classif$aggregate(msr("classif.acc"))
print(bmr_classif)
plot_to_save <- autoplot(bm_classif, measure = msr("classif.acc")) + 
  ggtitle("Benchmarking: Strict")

# Save plot
png("benchmarking_strict_big_region.png", width = 1600, height = 1200, res = 300)
print(plot_to_save)
dev.off()

# CONFUSION MATRIX

#featureless
prediction = learner_fl$predict(task = task_status)
prediction
table(prediction$truth, prediction$response)

#logistic regression
prediction = learner_logreg_pipe$predict(task = task_status)
prediction
table(prediction$truth, prediction$response)


#random forest
prediction = learner_rf_pipe$predict(task = task_status)
prediction
table(prediction$truth, prediction$response)



# G.3) Abstraction level 3: regions SF Bay v other, no countries & 74 industries

# Subset the data to include only strict definition of success / failure without the neutral status
startups_filtered_prediction_strict <- startups_filtered_prediction %>%
  select(-status_lenient) %>%
  rename("status" = "status_strict") %>%
  filter(status != "neutral") %>%
  mutate_at(vars(category_list, country_code), as.factor) %>%
  mutate(status = as.numeric(factor(status, levels = c("failure", "success")))-1) %>%
  na.omit()

# Group low-frequency levels as "Other" (final attempt 3 to overcome granular categories)
startups_filtered_prediction_strict <- startups_filtered_prediction_strict %>%
  mutate(category_list = fct_lump_min(category_list, min = 5, other_level = "Other"),
         country_code = fct_lump_min(country_code, min = 5, other_level = "Other"),
         region = fct_lump_min(region, min = 5, other_level = "Other"))

# make SF Bay Area v other out of region column
startups_filtered_prediction_strict <- startups_filtered_prediction_strict %>%
  mutate(region = case_when(
    region == "SF Bay Area" ~ "SF Bay Area",
    region == "Other" ~ "Other",
    TRUE ~ "Other"
  )) %>%
  mutate(region = factor(region, levels = c("SF Bay Area", "Other")))

# remove country 
startups_filtered_prediction_strict <- startups_filtered_prediction_strict %>%
  select(-country_code, -average_monthly_expense) %>%
  na.omit()


# LOGISTIC REGRESSION & RANDOM FOREST

# Create task using filtered data & stratify
task_status <- as_task_classif(startups_filtered_prediction_strict, target = "status", id = "status_classif", strata = "status",
                               stratify = TRUE)


# Define logistic regression learner & pipeline
imputer = po("imputemedian")
learner_logreg = lrn("classif.log_reg")
learner_logreg_pipe = as_learner(imputer %>>% learner_logreg)

# Define random forest learner & pipeline
learner_rf = lrn("classif.ranger")
learner_rf_pipe = as_learner(imputer %>>% learner_rf)

# Train logistic regression and random forest models on the entire dataset
learner_logreg_pipe$train(task = task_status)
learner_rf_pipe$train(task = task_status)

# Set up cross-validation with stratification
rdesc <- rsmp("cv", folds = 5)
set.seed(1)

# Cross-validation for logistic regression
res_logreg <- resample(learner = learner_logreg_pipe, task = task_status, resampling = rdesc)
print(res_logreg$aggregate(msr("classif.acc")))
autoplot(res_logreg, measure = msr("classif.acc"))

# Cross-validation for random forest
res_rf <- resample(learner = learner_rf_pipe, task = task_status, resampling = rdesc)
print(res_rf$aggregate(msr("classif.acc")))
autoplot(res_rf, measure = msr("classif.acc"))


# BENCHMARKING

# Train featureless learner
learner_fl = lrn("classif.featureless")
learner_fl$train(task = task_status)
set.seed(2)

# Define resampling scheme and benchmarking design
design_classif = benchmark_grid(
  tasks = task_status,
  learners = list(learner_logreg_pipe, learner_rf_pipe, learner_fl), # Add random forest learner to the list
  resamplings = rsmp("cv", folds = 5)
)

# Benchmark
bm_classif = benchmark(design_classif)

# Define performance measures
bmr_classif = bm_classif$aggregate(msr("classif.acc"))
print(bmr_classif)
plot_to_save <- autoplot(bm_classif, measure = msr("classif.acc")) + 
  ggtitle("Benchmarking: Strict")

# Save plot
png("benchmarking_strict_big_region_no_country.png", width = 1600, height = 1200, res = 300)
print(plot_to_save)
dev.off()

# CONFUSION MATRIX

#featureless
prediction = learner_fl$predict(task = task_status)
prediction
table(prediction$truth, prediction$response)

#logistic regression
prediction = learner_logreg_pipe$predict(task = task_status)
prediction
table(prediction$truth, prediction$response)


#random forest
prediction = learner_rf_pipe$predict(task = task_status)
prediction
table(prediction$truth, prediction$response)



#H) ML PREDICTION ON LENIENT SUCCESS DEFINITION


# H.1) Abstraction level 1: All regions, countries & 74 industries

# Subset the data to include only lenient definition of success / failure without the neutral status
startups_filtered_prediction_lenient <- startups_filtered_prediction %>%
  select(-status_strict) %>%
  rename("status" = "status_lenient") %>%
  filter(status != "neutral") %>%
  mutate_at(vars(category_list, country_code), as.factor) %>%
  mutate(status = as.numeric(factor(status, levels = c("failure", "success")))-1) %>%
  na.omit()

# Group low-frequency levels as "Other" (final attempt 3 to overcome granular categories)
startups_filtered_prediction_lenient <- startups_filtered_prediction_lenient %>%
  mutate(category_list = fct_lump_min(category_list, min = 5, other_level = "Other"),
         country_code = fct_lump_min(country_code, min = 5, other_level = "Other"),
         region = fct_lump_min(region, min = 5, other_level = "Other"))

# LOGISTIC REGRESSION & RANDOM FOREST

# Create task using filtered data & stratify
task_status <- as_task_classif(startups_filtered_prediction_lenient, target = "status", id = "status_classif", strata = "status",
                               stratify = TRUE)


# Define logistic regression learner & pipeline
imputer = po("imputemedian")
learner_logreg = lrn("classif.log_reg")
learner_logreg_pipe = as_learner(imputer %>>% learner_logreg)

# Define random forest learner & pipeline
learner_rf = lrn("classif.ranger")
learner_rf_pipe = as_learner(imputer %>>% learner_rf)

# Train logistic regression and random forest models on the entire dataset
learner_logreg_pipe$train(task = task_status)
learner_rf_pipe$train(task = task_status)

# Set up cross-validation with stratification
rdesc <- rsmp("cv", folds = 5)
set.seed(1)

# Cross-validation for logistic regression
res_logreg <- resample(learner = learner_logreg_pipe, task = task_status, resampling = rdesc)
print(res_logreg$aggregate(msr("classif.acc")))
autoplot(res_logreg, measure = msr("classif.acc"))

# Cross-validation for random forest
res_rf <- resample(learner = learner_rf_pipe, task = task_status, resampling = rdesc)
print(res_rf$aggregate(msr("classif.acc")))
autoplot(res_rf, measure = msr("classif.acc"))


# BENCHMARKING

# Train featureless learner
learner_fl = lrn("classif.featureless")
learner_fl$train(task = task_status)
set.seed(2)

# Define resampling scheme and benchmarking design
design_classif = benchmark_grid(
  tasks = task_status,
  learners = list(learner_logreg_pipe, learner_rf_pipe, learner_fl), # Add random forest learner to the list
  resamplings = rsmp("cv", folds = 5)
)

# Benchmark
bm_classif = benchmark(design_classif)

# Define performance measures
bmr_classif = bm_classif$aggregate(msr("classif.acc"))
print(bmr_classif)
plot_to_save <- autoplot(bm_classif, measure = msr("classif.acc")) + 
  ggtitle("Benchmarking: Lenient")

# Save plot
png("benchmarking_lenient.png", width = 1600, height = 1200, res = 300)
print(plot_to_save)
dev.off()

# CONFUSION MATRIX

#featureless
prediction = learner_fl$predict(task = task_status)
prediction
table(prediction$truth, prediction$response)

#logistic regression
prediction = learner_logreg_pipe$predict(task = task_status)
prediction
table(prediction$truth, prediction$response)


#random forest
prediction = learner_rf_pipe$predict(task = task_status)
prediction
table(prediction$truth, prediction$response)



# H.2) Abstraction level 2: regions SF Bay v other, all countries & 74 industries

# Subset the data to include only lenient definition of success / failure without the neutral status
startups_filtered_prediction_lenient <- startups_filtered_prediction %>%
  select(-status_strict) %>%
  rename("status" = "status_lenient") %>%
  filter(status != "neutral") %>%
  mutate_at(vars(category_list, country_code), as.factor) %>%
  mutate(status = as.numeric(factor(status, levels = c("failure", "success")))-1) %>%
  na.omit()

# Group low-frequency levels as "Other" (final attempt 3 to overcome granular categories)
startups_filtered_prediction_lenient <- startups_filtered_prediction_lenient %>%
  mutate(category_list = fct_lump_min(category_list, min = 5, other_level = "Other"),
         country_code = fct_lump_min(country_code, min = 5, other_level = "Other"),
         region = fct_lump_min(region, min = 5, other_level = "Other"))

# make SF Bay Area v other out of region column
startups_filtered_prediction_lenient <- startups_filtered_prediction_lenient %>%
  mutate(region = case_when(
    region == "SF Bay Area" ~ "SF Bay Area",
    region == "Other" ~ "Other",
    TRUE ~ "Other"
  )) %>%
  mutate(region = factor(region, levels = c("SF Bay Area", "Other")))


# LOGISTIC REGRESSION & RANDOM FOREST

# Create task using filtered data & stratify
task_status <- as_task_classif(startups_filtered_prediction_lenient, target = "status", id = "status_classif", strata = "status",
                               stratify = TRUE)


# Define logistic regression learner & pipeline
imputer = po("imputemedian")
learner_logreg = lrn("classif.log_reg")
learner_logreg_pipe = as_learner(imputer %>>% learner_logreg)

# Define random forest learner & pipeline
learner_rf = lrn("classif.ranger")
learner_rf_pipe = as_learner(imputer %>>% learner_rf)

# Train logistic regression and random forest models on the entire dataset
learner_logreg_pipe$train(task = task_status)
learner_rf_pipe$train(task = task_status)

# Set up cross-validation with stratification
rdesc <- rsmp("cv", folds = 5)
set.seed(1)

# Cross-validation for logistic regression
res_logreg <- resample(learner = learner_logreg_pipe, task = task_status, resampling = rdesc)
print(res_logreg$aggregate(msr("classif.acc")))
autoplot(res_logreg, measure = msr("classif.acc"))

# Cross-validation for random forest
res_rf <- resample(learner = learner_rf_pipe, task = task_status, resampling = rdesc)
print(res_rf$aggregate(msr("classif.acc")))
autoplot(res_rf, measure = msr("classif.acc"))


# BENCHMARKING

# Train featureless learner
learner_fl = lrn("classif.featureless")
learner_fl$train(task = task_status)
set.seed(2)

# Define resampling scheme and benchmarking design
design_classif = benchmark_grid(
  tasks = task_status,
  learners = list(learner_logreg_pipe, learner_rf_pipe, learner_fl), # Add random forest learner to the list
  resamplings = rsmp("cv", folds = 5)
)

# Benchmark
bm_classif = benchmark(design_classif)

# Define performance measures
bmr_classif = bm_classif$aggregate(msr("classif.acc"))
print(bmr_classif)
plot_to_save <- autoplot(bm_classif, measure = msr("classif.acc")) + 
  ggtitle("Benchmarking: Lenient")

# Save plot
png("benchmarking_lenient_big_region.png", width = 1600, height = 1200, res = 300)
print(plot_to_save)
dev.off()

# CONFUSION MATRIX

#featureless
prediction = learner_fl$predict(task = task_status)
prediction
table(prediction$truth, prediction$response)

#logistic regression
prediction = learner_logreg_pipe$predict(task = task_status)
prediction
table(prediction$truth, prediction$response)


#random forest
prediction = learner_rf_pipe$predict(task = task_status)
prediction
table(prediction$truth, prediction$response)


# H.3) Abstraction level 3: regions SF Bay v others, no countries & 74 industries


# Subset the data to include only lenient definition of success / failure without the neutral status
startups_filtered_prediction_lenient <- startups_filtered_prediction %>%
  select(-status_strict) %>%
  rename("status" = "status_lenient") %>%
  filter(status != "neutral") %>%
  mutate_at(vars(category_list, country_code), as.factor) %>%
  mutate(status = as.numeric(factor(status, levels = c("failure", "success")))-1) %>%
  na.omit()

# Group low-frequency levels as "Other" (final attempt 3 to overcome granular categories)
startups_filtered_prediction_lenient <- startups_filtered_prediction_lenient %>%
  mutate(category_list = fct_lump_min(category_list, min = 5, other_level = "Other"),
         country_code = fct_lump_min(country_code, min = 5, other_level = "Other"),
         region = fct_lump_min(region, min = 5, other_level = "Other"))

# make SF Bay Area v other out of region column
startups_filtered_prediction_lenient <- startups_filtered_prediction_lenient %>%
  mutate(region = case_when(
    region == "SF Bay Area" ~ "SF Bay Area",
    region == "Other" ~ "Other",
    TRUE ~ "Other"
  )) %>%
  mutate(region = factor(region, levels = c("SF Bay Area", "Other")))


# remove country 
startups_filtered_prediction_lenient <- startups_filtered_prediction_lenient %>%
  select(-country_code, -average_monthly_expense) %>%
  na.omit()


# LOGISTIC REGRESSION & RANDOM FOREST

# Create task using filtered data & stratify
task_status <- as_task_classif(startups_filtered_prediction_lenient, target = "status", id = "status_classif", strata = "status",
                               stratify = TRUE)


# Define logistic regression learner & pipeline
imputer = po("imputemedian")
learner_logreg = lrn("classif.log_reg")
learner_logreg_pipe = as_learner(imputer %>>% learner_logreg)

# Define random forest learner & pipeline
learner_rf = lrn("classif.ranger")
learner_rf_pipe = as_learner(imputer %>>% learner_rf)

# Train logistic regression and random forest models on the entire dataset
learner_logreg_pipe$train(task = task_status)
learner_rf_pipe$train(task = task_status)

# Set up cross-validation with stratification
rdesc <- rsmp("cv", folds = 5)
set.seed(1)

# Cross-validation for logistic regression
res_logreg <- resample(learner = learner_logreg_pipe, task = task_status, resampling = rdesc)
print(res_logreg$aggregate(msr("classif.acc")))
autoplot(res_logreg, measure = msr("classif.acc"))

# Cross-validation for random forest
res_rf <- resample(learner = learner_rf_pipe, task = task_status, resampling = rdesc)
print(res_rf$aggregate(msr("classif.acc")))
autoplot(res_rf, measure = msr("classif.acc"))


# BENCHMARKING

# Train featureless learner
learner_fl = lrn("classif.featureless")
learner_fl$train(task = task_status)
set.seed(2)

# Define resampling scheme and benchmarking design
design_classif = benchmark_grid(
  tasks = task_status,
  learners = list(learner_logreg_pipe, learner_rf_pipe, learner_fl), # Add random forest learner to the list
  resamplings = rsmp("cv", folds = 5)
)

# Benchmark
bm_classif = benchmark(design_classif)

# Define performance measures
bmr_classif = bm_classif$aggregate(msr("classif.acc"))
print(bmr_classif)
plot_to_save <- autoplot(bm_classif, measure = msr("classif.acc")) + 
  ggtitle("Benchmarking: Lenient")

# Save plot
png("benchmarking_lenient_big_region_no_country.png", width = 1600, height = 1200, res = 300)
print(plot_to_save)
dev.off()

# CONFUSION MATRIX

#featureless
prediction = learner_fl$predict(task = task_status)
prediction
table(prediction$truth, prediction$response)

#logistic regression
prediction = learner_logreg_pipe$predict(task = task_status)
prediction
table(prediction$truth, prediction$response)


#random forest
prediction = learner_rf_pipe$predict(task = task_status)
prediction
table(prediction$truth, prediction$response)



