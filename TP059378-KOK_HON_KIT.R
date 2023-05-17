# TP059378
# KOK HON KIT

install.packages("tidyverse")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("scales")
install.packages("plotly")
install.packages("corrgram")
install.packages("viridis")  # Install
install.packages("corrplot")
install.packages("DescTools")
install.packages("stringr")
install.packages("smooth")
install.packages("ggpubr")
install.packages("treemapify")

# importing required packages
library(tidyverse) # data cleaning
library(dplyr) # data cleaning
library(lubridate) # data transformation (date manipulation and grouping)
library(ggplot2) # data visualization
library(scales) # data formatting in plots
library(RColorBrewer) # for colour palette
library(plotly) # data visualization library
library(corrgram) # for correlation plot
library(viridis) # colour palette for visualization
library(corrplot) # correlation plot
library(DescTools)
library(stringr) # data manipulation
library(smooth)
library(zoo) # moving average
library(ggpubr)
library(treemapify)

# read data into df variable
df <- read.csv(file="C:\\Users\\bpvpe\\OneDrive - Asia Pacific University\\Degree\\Sem 1\\Programming For Data Analysis\\Assignment\\House_Rent_Dataset.csv")

# number of rows for the dataset
nrow(df) # 4767 rows

# number of columns for the dataset
ncol(df) # 12 columns

# dimensions of dataset
dim(df) # 4767, 12 (row, column)

# all columns in the dataset
names(df)

# top 5 rows for the dataset
head(df, 5)

# view dataframe in tabular form
View(df)

# statistics summary of dataframe
summary(df)

# unique categories of categorical variables
unique(df$Floor) # 480 unique categories
unique(df$Area.Type) # 3 unique categories
unique(df$Area.Locality) # 2235 unique categories
unique(df$City) # 6 unique categories
unique(df$Furnishing.Status) # 3 unique categories
unique(df$Tenant.Preferred) # 3 unique categories
unique(df$Point.of.Contact) # 3 unique categories

# distribution of data by city
df %>% 
  count(City) %>%
  ggplot(mapping = aes(x = reorder(City, n), y = n)) +
  labs(title = "Distribution of City", x = "City", y = "Count") +
  geom_bar(mapping = aes(fill = n), stat = "identity", color = "black", width = 0.5) +
  scale_fill_gradient("Count", low = "red", high = "green", space = "Lab") + # gradient for fill
  geom_text(aes(label =  n), hjust = -0.25, size = 5) + # label at bar
  coord_flip() # horizontal bar

# data distribution by Tenant Preferred
# calculate the count frequency by different categorical values
tenant_preferred_count <- 
  df %>% 
  count(Tenant.Preferred) %>% 
  mutate(percent = round(n / nrow(df) * 100, digit = 2))

# plot the pie chart
pie(
  tenant_preferred_count$n, 
  labels = paste(unique(tenant_preferred_count$percent), "%\n -", unique(tenant_preferred_count$n)),
  main = "Tenant Preferred Distribution",
  col = brewer.pal(length(unique(df$Tenant.Preferred)), "Dark2")
)
legend(
  1.5, 1,
  title = "Tenant Preferred",
  legend = unique(tenant_preferred_count$Tenant.Preferred),
  cex = .8,
  fill = brewer.pal(length(unique(tenant_preferred_count$Tenant.Preferred)), "Dark2")
)

# data distribution by Point of Contact
contact_count <- df %>% count(Point.of.Contact) %>% mutate(percent = round(n / nrow(df) * 100, digit = 2))
pie(
  contact_count$n, 
  labels = paste(unique(contact_count$percent), "%\n -", unique(contact_count$n)),
  main = "Point of Contact Distribution",
  col = brewer.pal(length(unique(df$Point.of.Contact)), "Dark2")
)
legend(
  1.5, 1,
  title = "Point of Contact",
  legend = unique(contact_count$Point.of.Contact),
  cex = .8,
  fill = brewer.pal(length(unique(contact_count$Point.of.Contact)), "Dark2")
)

# data distribution by Furnishing Status
df %>% 
  count(Furnishing.Status) %>%
  ggplot(mapping = aes(x = reorder(Furnishing.Status, n), y = n)) +
  labs(title = "Distribution of House Furnishing", x = "Furnishing Status", y = "Count") +
  geom_bar(mapping = aes(fill = n), stat = "identity", color = "black", width = 0.25) +
  scale_fill_gradient("Count", low = "red", high = "green", space = "Lab") + # gradient for fill
  geom_text(aes(label =  n), hjust = -0.25, size = 5) + # label at bar
  coord_flip() # horizontal bar

# distribution of Rent column with histogram
df %>% 
  ggplot(mapping = aes(x = Rent)) +
  labs(title = "Distribution of Rent Variable", x = "Rent", y = "Frequency") +
  scale_x_continuous(labels = comma) +
  geom_histogram()

# for Size
df %>%
  ggplot(mapping = aes(x = " ", y = Size)) +
  labs(title = "Distribution of House Size", y = "Size", x = "") +
  geom_boxplot()

# for Bathroom
df %>%
  ggplot(mapping = aes(x = " ", y = Bathroom)) +
  labs(title = "Distribution of Bathroom Number", x = "", y = "Bathroom Number") +
  geom_boxplot()

# DATA CLEANING
# change column name
colnames(df) <- c(
  "Posted.Date", "Facilities.Number", "Rental.Price", 
  "House.Size", "Floor", "Area.Type", 
  "House.Locality", "House.City", "House.Furnishing", 
  "Tenant.Targeted", "Bathroom.Number", "Contact.Person"
)

# get summary of posted date
summary(df)

# Convert date column from character class to Date class
df$Posted.Date <- as.Date(df$Posted.Date, format="%m/%d/%Y")
class(df$Posted.Date)

# Convert rental price to numeric instead of integer
df$Rental.Price <- as.numeric(df$Rental.Price)
class(df$Rental.Price)

# get summary of data
summary(df)

# sort in ascending order by date
df <- df[order(df$Posted.Date), ]

# check sorted data
head(df, 5)

# check duplicated values
df[duplicated(df), ]

# checking number of missing values in the dataframe
sum(is.na(df))
sum(is.null(df))

# removing outliers
ggplot(data = df, mapping = aes(x = House.Size, y = Rental.Price)) +
  geom_point() + 
  scale_y_continuous("Rental Price", labels = comma) +
  scale_x_continuous("House Size", labels = comma)

ggplot(data = df, mapping = aes(x = Rental.Price)) +
  geom_histogram(bins=20) +
  scale_x_continuous("Rental Price", labels = comma) +
  scale_y_continuous("Frequency", labels = comma)

# outlier's data city is at Bangalore so we use Bangalore's median to replace
df[df$Rental.Price == 3500000, ]$Rental.Price <- median(df[df$House.City == "Bangalore", ]$Rental.Price)

# Data Transformation
# calculate rent / size
df$Rent.Per.Unit <- df %>% with(round(Rental.Price /House.Size, digit = 2))

# parse floor to numbers, then calculate distance
df[c("Current.Floor", "Total.Floor")] <- str_split_fixed(df$Floor, pattern=" out of ", 2)
df$Current.Floor <- str_replace_all(df$Current.Floor, pattern="Ground", replacement="0")
df$Current.Floor <- str_replace_all(df$Current.Floor, pattern="Upper Basement", replacement="-1")
df$Current.Floor <- str_replace_all(df$Current.Floor, pattern="Lower Basement", replacement="-2")
df$Current.Floor <- as.numeric(df$Current.Floor)
df$Total.Floor <- as.numeric(df$Total.Floor)

# having missing values in computed columns
df[is.na(df$Total.Floor), ]

# replace missing values for total floor
df[is.na(df$Total.Floor) & df$House.City == "Delhi", ]$Total.Floor <- median(df[(!is.na(df$Total.Floor)) & (df$House.City == "Delhi"), ]$Total.Floor)
df[is.na(df$Total.Floor) & df$House.City == "Hyderabad", ]$Total.Floor <- median(df[(!is.na(df$Total.Floor)) & (df$House.City == "Hyderabad"), ]$Total.Floor)

# calculate difference in floor (distance between current floor and ground floor)
df$Difference.Floor <- abs(df$Current.Floor - 0)

# remove floor variable
df$Floor <- NULL

# Remove unnecessary values (Remove "Area" and "Contact")
df$Contact.Person <- str_replace_all(df$Contact.Person, pattern="Contact ", replacement="")
df$Area.Type <- str_replace_all(df$Area.Type, pattern=" Area", replacement="")

summary(df)
View(df)

# for calculating percentage of frequency distribution
calculate_percent <- function(frequency) {
  return(round((frequency / sum(frequency) * 100), digit = 2))
}

# QUESTIONS:
# 1. What is the factors that affect price of rental?
# Analysis 1-1: Mean rental price in according to date
df %>%
  group_by(Posted.Date) %>%
  summarize(Rental.Price = mean(Rental.Price)) %>%
  mutate(Smoothed.Rental = rollmean(Rental.Price, k = 10, fill = NA, align = "center")) %>%
  ggplot() +
    labs(title = "Trend of Rental Price", x = "Posted Date", y = "Rental Price", colour = "Lines") +
    geom_line(mapping = aes(x = Posted.Date, y = Rental.Price, colour = "Rental Price"), size = 1.0) +
    geom_line(mapping = aes(x = Posted.Date, y = Smoothed.Rental, colour = "Smoothed Rental Price"), size = 1.0) +
    scale_y_continuous(labels = comma)

# Analysis 1-2: Median rental price in each city by months
# plot in multiple line chart to visualize the changes in Rental Price by Month
# group data by city and month first
grouped_by_city_date <- df[, c("Posted.Date", "Rental.Price", "House.City")]

grouped_by_city_date <- 
grouped_by_city_date %>%
  group_by(Month = lubridate::floor_date(Posted.Date, unit = "month"), House.City) %>%
  summarize(Median.Rental.Price = median(Rental.Price))
  
ggplot(data = grouped_by_city_date, mapping = aes(col = House.City, x = Month, y = Median.Rental.Price)) +
  labs(title = "Rental Price in Each City by Month", x = "Month", y = "Rental Price", colour = "City") +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  facet_wrap(~ House.City)

# Analysis 1-3: Median Rental Price by Facilities Number
rent_by_facilities <- 
df %>%
  group_by(Facilities.Number) %>%
  summarize(Median.Rental.Price = median(Rental.Price))

facilities_number <- unique(df$Facilities.Number)
ggplot(data = rent_by_facilities, mapping = aes(x = Facilities.Number, y = Median.Rental.Price)) +
  labs(
    title = "Median of Rental Price by Facilities Numbers", 
    x = "Median of Rental Price", 
    y = "Facilities Numbers", 
    fill = "Median of Rental Price"
  ) +
  geom_bar(mapping = aes(fill = Median.Rental.Price), stat = "identity", col="#544c4b") +
  scale_fill_gradient(low = "green", high = "red", labels = comma) +
  scale_x_continuous("Facility Number", labels = as.character(facilities_number), breaks = facilities_number) +
  scale_y_continuous("Median Rental Price", labels = comma) +
  coord_flip()

# Analysis 1-4: House Size
# plot scatterplot and line of best fit
ggplot(data = df, mapping = aes(x = House.Size, y = Rental.Price, add = "reg.line")) +
  labs(title = "Relationship between House Size and Rental Price", x = "House Size", y = "Rental Price") +
  geom_point(col = "#9c1a11") +
  geom_smooth(method = lm) +
  scale_y_continuous("Rental Price", labels = comma) + 
  stat_cor() +
  stat_regline_equation(label.y.npc = .9, size = 4, col = "black")

# Analysis 1-5 Area Type
df %>%
  ggplot(mapping = aes(x = Area.Type, y = Rental.Price, colour = Area.Type)) +
    labs(title = "Relationship between Area Type and Rental Price", x = "Area Type", y = "Rental Price") +
    geom_point()

# Analysis 1-6 Median Rental per Unit of each Area Type by City
rent_by_area_city <-
  df %>%
  group_by(Area.Type, House.City) %>%
  summarize(Median.Rental.Per.Unit = median(Rent.Per.Unit))

ggplot(data = rent_by_area_city, mapping = aes(fill = Area.Type, x = House.City, y = Median.Rental.Per.Unit)) +
  ggtitle("Median of Rental per Unit for each Area Type by City") +
  xlab("City") +
  ylab("Median of Rental per Unit") +
  guides(fill = guide_legend(title = "Area Type")) +
  geom_bar(stat = "identity", position = "dodge", col = "#544c4b", width = .75)

# Analysis 1-7: Rental Price by City
df %>%
  ggplot(mapping = aes(x = House.City, y = Rental.Price, fill = House.City)) +
    labs(title = "Rental Price by City", x = "House City", y = "Rental Price") +
    geom_violin()

# Analysis 1-8: House Furnishing
price_by_city_furnishing <- df[, c("House.City", "House.Furnishing", "Rent.Per.Unit")]

# group data by city and furnishing status
grouped_city_furnishing <- 
  df %>%
  group_by(House.City, House.Furnishing) %>%
  summarize(Agg.Rental = median(Rent.Per.Unit))

# plot a stack bar chart
ggplot(data=grouped_city_furnishing, mapping=aes(fill = House.Furnishing, x = House.City, y = Agg.Rental)) + 
  ggtitle("Rental Price in each City by Furnishing Status") +
  xlab("City") +
  ylab("Rental Price per Unit") +
  guides(fill=guide_legend(title="Furnishing Status")) +
  geom_bar(position = "stack", stat = "identity", col="black", width=0.45) +
  geom_text(aes(label = Agg.Rental), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(labels = comma)

# Analysis 1-9: Targeted Tenant
# after log transformation
ggplot(data = df, mapping = aes(x = Tenant.Targeted, y = log(Rental.Price), fill = Tenant.Targeted)) + 
  labs(
    title = "Distribution of Rental Price by Tenant Targeted after Log Transformation", 
    x = "Tenant Targeted", 
    y = "Rental Price after Log Transformation", 
    fill = "Tenant Targeted"
  ) +
  geom_boxplot()

# Analysis 1-10: Bathroom Number
price_by_bathroom <- df[, c("Rental.Price", "Bathroom.Number")]
price_by_bathroom$Bathroom.Number <- as.character(df$Bathroom.Number)
grouped_bathroom <-
  df %>%
  group_by(Bathroom.Number) %>%
  summarize(Median.Rental = median(Rent.Per.Unit))

ggplot(data = grouped_bathroom, mapping = aes(x = Bathroom.Number, y = Median.Rental)) +
  labs(
    title = "Relationship between Bathroom Number and Median of Rental Price per Unit",
    x = "Bathroom Number",
    y = "Median of Rental Price per Unit"
  ) +
  geom_point(col = "#696969") +
  geom_line(col = "#7F7F7F", size = 0.75) +
  scale_x_continuous(labels = unique(df$Bathroom.Number), breaks = unique(df$Bathroom.Number))

# Analysis 1-11: Contact Person
ggplot(data = df, mapping = aes(x = Contact.Person, y = Rental.Price, colour = Contact.Person)) +
  labs(
    title = "Rental Price Distribution by Contact Person", 
    x = "Contact Person", 
    y = "Rental Price"
  ) +
  geom_point()

# Analysis 1-12: Difference In Floor
ggplot(data = df, mapping = aes(x = Difference.Floor, y = Rental.Price)) +
  labs(
    title = "Relationship between Difference in Floor and Rental Price",
    x = "Difference in Floor",
    y = "Rental Price"
  ) +
  geom_point(col = "seagreen")

# Analysis 1-13: Total Floor against Price
# separate the total floor into groups like 0-10, 11-20...
plot_total_floor <- function() {
  final_df <- df
  breaks <- seq(from = -1, to = 90, by = 10)
  labels <- c(
    "0 - 9",
    "10 - 19",
    "20 - 29",
    "30 - 39",
    "40 - 49",
    "50 - 59",
    "60 - 69",
    "70 - 79",
    "80 - 89"
  )
  
  final_df <- 
    df %>%
      mutate(floor_group = cut(Total.Floor, breaks = breaks, labels = labels))
  
  final_df %>%
    ggplot(mapping = aes(x = floor_group, y = Rental.Price, fill = floor_group)) +
      labs(title = "Relationship between Floor Group and Rental Price", x = "Floor Group", y = "Rental Price") +
      geom_boxplot()
  
}
plot_total_floor()

# Analysis 1-14: Correlation between all variables
plot_correlation <- function() {
  final_df <- df
  
  # Integer encoding for categorical variables to conduct correlation analysis
  # Area Type convert to numbers
  replace_number <- 1
  for (area_type in unique(df$Area.Type)) {
    final_df[final_df$Area.Type == area_type, ]$Area.Type <- replace_number
    replace_number <- replace_number + 1
  }
  final_df$Area.Type <- as.numeric(final_df$Area.Type)

  # House City convert to numbers
  replace_number <- 1
  for (city in unique(df$House.City)) {
    final_df[final_df$House.City == city, ]$House.City <- replace_number
    replace_number <- replace_number + 1
  }
  final_df$House.City <- as.numeric(final_df$House.City)
  
  # House Furnishing convert to numbers
  replace_number <- 1
  for (furnishing in unique(df$House.Furnishing)) {
    final_df[final_df$House.Furnishing == furnishing, ]$House.Furnishing <- replace_number
    replace_number <- replace_number + 1
  }
  final_df$House.Furnishing <- as.numeric(final_df$House.Furnishing)
  
  # Tenant Targeted convert to numbers
  replace_number <- 1
  for (tenant in unique(df$Tenant.Targeted)) {
    final_df[final_df$Tenant.Targeted == tenant, ]$Tenant.Targeted <- replace_number
    replace_number <- replace_number + 1
  }
  final_df$Tenant.Targeted <- as.numeric(final_df$Tenant.Targeted)
  
  # Contact Person convert to numbers
  replace_number <- 1
  for (contact_person in unique(df$Contact.Person)) {
    print(contact_person)
    final_df[final_df$Contact.Person == contact_person, ]$Contact.Person <- replace_number
    replace_number <- replace_number + 1
  }
  final_df$Contact.Person <- as.numeric(final_df$Contact.Person)
  
  # House City convert to numbers
  final_df <- final_df[
    c(
      "Facilities.Number", 
      "House.Size", 
      "Area.Type", 
      "Bathroom.Number", 
      "Difference.Floor",
      "House.City",
      "House.Furnishing",
      "Tenant.Targeted",
      "Contact.Person",
      "Rental.Price"
    )
  ]
  correlation_matrix <- cor(final_df, method = "pearson")
  print(correlation_matrix)
  corrplot(correlation_matrix, method = 'color', order = 'alphabet', addCoef.col = 'black', col = COL2('PuOr', 10))
}
plot_correlation()


# 2. What kind of houses targeted Bachelor? (only Bachelors)
# Analysis 2-1: Facilities Number
bachelors_df <- df[df$Tenant.Targeted == "Bachelors", ]

# group data and do aggregation
facility_dist_bachelor <- 
  bachelors_df %>%
    group_by(Facilities.Number) %>%
    summarize(frequency = n()) %>%
    mutate(percent = calculate_percent(frequency))

# tree map
facility_dist_bachelor %>%
  mutate(Facilities.Number = as.character(Facilities.Number)) %>%
    ggplot(
      mapping = aes(
        area = frequency, 
        fill = Facilities.Number, 
        label = paste("Number of Facilities: ", Facilities.Number, "\n", percent, "%", sep = "")
      )
    ) +
    labs(title = "Distribution of Number of Facilities") +
    geom_treemap() +
    geom_treemap_text(size = 10, colour = "black", place = "centre") +
    scale_fill_brewer(palette = "Spectral")

# Analysis 2-2: House Size Distribution
bachelors_df %>%
  ggplot(mapping = aes(x = House.Size, fill = after_stat(count))) +
  labs(title = "Distribution of House Size targeting Bachelors", x = "House Size", y = "Frequency", fill = "Frequency") +
  scale_fill_gradient(low = "lightblue", high = "royalblue1") +
  geom_histogram()

# Analysis 2-3: City
city_dist_bachelor <- 
  bachelors_df %>%
  group_by(House.City) %>%
  summarize(frequency = n()) %>%
  mutate(percent = calculate_percent(frequency))

pie(
  city_dist_bachelor$frequency,
  paste(unique(city_dist_bachelor$percent), "% - ", unique(city_dist_bachelor$frequency), sep = ""),
  main = "Distribution of Rental Houses in each City Targeting Bachelor",
  col = brewer.pal(length(unique(city_dist_bachelor$percent)), "Set3"),
  border = NA
)
legend(
  1.25, 1,
  title="City",
  legend = unique(city_dist_bachelor$House.City), 
  cex = .8, 
  fill = brewer.pal(length(unique(city_dist_bachelor$percent)), "Set3")
)

# Analysis 2-4: Bathroom Number
grouped_bachelor_bathroom <- 
  bachelors_df %>%
  group_by(Bathroom.Number) %>%
  summarize(frequency = n())

ggplot(data = grouped_bachelor_bathroom, mapping = aes(x = Bathroom.Number, y = frequency)) +
  labs(title = "Bathroom Distribution of Houses Targeting Bachelors", x = "Bathroom Number", y = "Frequency") +
  geom_bar(stat = "identity", col = "#27408B", fill = "#4169E1") +
  geom_text(aes(label = frequency), hjust = -0.2) +
  scale_x_continuous(labels = unique(bachelors_df$Bathroom.Number), breaks = unique(bachelors_df$Bathroom.Number)) +
  coord_flip()

# Analysis 2-5: Rent per Unit Distribution for Bachelors
bachelors_df %>%
  ggplot(mapping = aes(x = Rent.Per.Unit, fill = after_stat(count))) +
  labs(title = "Distribution of Rental Price per Unit Targeting Bachelors", x = "Rent per Unit", y = "Frequency") +
  scale_fill_gradient(low = "lightgreen", high = "springgreen3") +
  geom_histogram(bins = 20)

# Analysis 2-6: Contact Person
contact_dist_bachelor <- 
  bachelors_df %>%
  group_by(Contact.Person) %>%
  summarize(frequency = n()) %>%
  mutate(percent = calculate_percent(frequency))

pie(
  contact_dist_bachelor$frequency,
  paste(unique(contact_dist_bachelor$percent), "% - ", unique(contact_dist_bachelor$frequency), sep = ""),
  main = "Distribution of Contact Person Targeting Bachelor",
  col = brewer.pal(length(unique(contact_dist_bachelor$Contact.Person)), "Set1"),
  border = NA
)
legend(
  1.25, 1,
  title="Contact Person",
  legend = unique(contact_dist_bachelor$Contact.Person), 
  cex = .8, 
  fill = brewer.pal(length(unique(contact_dist_bachelor$Contact.Person)), "Set1")
)

# Analysis 2-7: Difference in Floor (distribution among houses that targets Bachelors)
bachelors_df %>%
  ggplot(mapping = aes(x = Difference.Floor)) +
  labs(
    title = "Distribution Difference in Floor among Houses Targeting Bachelors", 
    x = "Difference in Floor",
    y = "Density"
  ) +
  geom_density(fill = "royalblue2", col = "royalblue1", alpha = 0.5)

# Analysis 2-8: Total Floor
bachelors_df %>%
  ggplot(mapping = aes(x = Total.Floor, fill = after_stat(count))) +
  labs(title = "Distribution of Total Floor among Houses Targetting Bachelors", x = "Total Floor", y = "Frequency") +
  scale_fill_gradient(low = "lightsalmon", high = "lightsalmon4") +
  geom_histogram(bins = 20)

# Analysis 2-9: Furnishing Status
grouped_furnishing_bachelor <- 
  bachelors_df %>%
    group_by(House.Furnishing) %>%
    summarize(frequency = n()) %>%
    mutate(percent = calculate_percent(frequency))

pie(
  grouped_furnishing_bachelor$frequency,
  paste(unique(grouped_furnishing_bachelor$percent), "% - ", grouped_furnishing_bachelor$frequency, sep = ""),
  main = "Distribution of House Furnishing Targeting Bachelor",
  col = brewer.pal(length(unique(grouped_furnishing_bachelor$House.Furnishing)), "Set3"),
  border = NA
)
legend(
  1.3, 1,
  title="House Furnishing",
  legend = unique(grouped_furnishing_bachelor$House.Furnishing),
  cex = .8, 
  fill = brewer.pal(length(unique(grouped_furnishing_bachelor$House.Furnishing)), "Set3")
)

# Analysis 2-10: Area Type (0 built area type housing for Bachelors)
grouped_area_bachelor <- 
  bachelors_df %>%
  group_by(Area.Type) %>%
  summarize(frequency = n()) %>%
  mutate(percent = calculate_percent(frequency)) %>%
  mutate(
    ymax = cumsum(percent), 
    ymin = c(0, head(ymax, n = -1)), 
    label = paste0(Area.Type, "\nValue: ", frequency, "\nPercentage: ", percent, "%"), 
    label_position = (ymax + ymin) / 2
  )

ggplot(data = grouped_area_bachelor, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Area.Type)) +
  ggtitle("Distribution of Area Type Percentage Targeting Bachelor") +
  geom_rect() +
  geom_label(x = 3.5, aes(y = label_position, label = label), size = 3, col = "#363636") +
  scale_fill_brewer(palette = "Accent") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")

# 3. Compare Houses targeting Family and Bachelors.
# Analysis 3-1: Frequency of Houses by Furnishing Status for Each type of Tenant
grouped_tenant_furnishing <-
  df %>%
  group_by(Tenant.Targeted, House.Furnishing) %>%
  summarize(frequency = n()) %>%
  mutate(percentage = calculate_percent(frequency))

ggplot(data = grouped_tenant_furnishing, mapping = aes(fill = House.Furnishing, x = Tenant.Targeted, y = frequency)) +
  ggtitle("Frequency of Houses by Furnishing Status for Each type of Tenant") +
  xlab("Tenant Targetted") +
  ylab("Percentage") +
  guides(fill = guide_legend(title = "House Furnishing")) +
  geom_bar(stat = "identity", position = "fill", width = 0.4) +
  geom_text(aes(label = paste0(percentage, "%")), size = 3, position = position_fill(vjust = 0.5)) +
  scale_y_continuous(labels = percent)

# Analysis 3-2: Median of Rental Price for each type of Tenant in each city
grouped_city_tenant <-
  df %>%
  group_by(House.City, Tenant.Targeted) %>%
  summarize(Median.Rental.Price = median(Rental.Price))

ggplot(data = grouped_city_tenant, mapping = aes(fill = Tenant.Targeted, x = House.City, y = Median.Rental.Price)) +
  ggtitle("Median of Rental Price for Each Type of Tenant in each City") +
  xlab("City") +
  ylab("Median of Rental Price") + 
  geom_bar(stat = "identity", position = "dodge", width = 0.5, col = "#424242")

# Analysis 3-3: Different Area Type of Houses Targeting Each Type of Tenant in Each City
grouped_area_city <-
  df %>%
  group_by(House.City, Tenant.Targeted, Area.Type) %>%
  summarize(frequency = n()) %>%
  mutate(percentage = calculate_percent(frequency))

ggplot(data = grouped_area_city, mapping = aes(fill = Area.Type, x = Tenant.Targeted, y = frequency)) +
  ggtitle("Different Area Type of Houses Targeting Each Type of Tenant in Each City") +
  xlab("Area Type") +
  ylab("Frequency") +
  geom_bar(stat = "identity", position = "fill", width = 0.5, col = "#404040") +
  geom_text(aes(label = paste0(percentage, "%")), size = 3, position = position_fill(vjust = 0.5)) +
  scale_y_continuous(labels = percent) +
  facet_wrap(~House.City)

# Analysis 3-4: The distribution of rental price by facility number for each type of tenant
df %>%
  ggplot(mapping = aes(x = Facilities.Number, y = Rental.Price, fill = Facilities.Number, group = Facilities.Number)) +
    geom_boxplot() +
    labs(
      title = "Rental Price Distribution by Facility Number According To Tenant Type",
      x = "Facility Number",
      y = "Rental Price",
      fill = "Facility Number"
    ) +
    scale_x_continuous(labels = 1:6, breaks = 1:6) +
    scale_fill_viridis_c(option = "magma") +
    facet_wrap(~Tenant.Targeted)

# Analysis 3-5: the common difference in floor for each type of tenants
ggplot(data = df, mapping = aes(x = Difference.Floor, fill = after_stat(count))) +
  labs(
    title = "Distribution of difference in floor for each type of tenants",
    x = "Difference in Floor",
    y = "Frequency"
  ) +
  geom_histogram(bins = 30) +
  scale_fill_distiller(palette = "PuOr") +
  facet_wrap(~Tenant.Targeted)

# Analysis 3-6: the common total floor for each type of tenants
common_total_floor <- function() {
  final_df <- df
  breaks <- seq(from = -1, to = 90, by = 5)
  labels <- c(
    "0 - 4", "5 - 9",
    "10 - 14", "15 - 19",
    "20 - 24", "25 - 29",
    "30 - 34", "35 - 39",
    "40 - 44", "45 - 49",
    "50 - 54", "55 - 59",
    "60 - 64", "65 - 69",
    "70 - 74", "75 - 79",
    "80 - 84", "85 - 89"
  )
  
  final_df <- 
    df %>%
      mutate(floor_group = cut(x = Total.Floor, labels = labels, breaks = breaks))
  
  final_df %>%
    group_by(Tenant.Targeted, floor_group) %>%
    summarize(frequency = n()) %>%
    ggplot(mapping = aes(x = floor_group, y = frequency, fill = Tenant.Targeted)) + 
      labs(title = "Most Common Floor Group By Tenants", x = "Floor Group", y = "Frequency", fill = "Type of Tenants") +
      geom_bar(stat = "identity") +
      coord_flip() +
      facet_wrap(~Tenant.Targeted)
}
common_total_floor()


# Analysis 3-7: For each city, the most common house size for each type of tenants
ggplot(data = df, mapping = aes(x = Tenant.Targeted, y = House.Size, fill = Tenant.Targeted)) +
  labs(title = "House Size Distribution for Each type of Tenants", x = "Tenant Targeted", y = "House Size") +
  geom_violin() +
  stat_summary(fun.y = median, geom = "point", color = "black")

# Analysis 3-8: The most common contact person for each type of tenants
plot_dist_tenant <- function() {
  grouped_city_contact_tenant <- 
    df %>%
    group_by(Contact.Person, Tenant.Targeted) %>%
    summarize(frequency = n())
  
  fig_contact_tenant <- plot_ly()
  fig_contact_tenant <- fig_contact_tenant %>% 
    add_pie(
      data = grouped_city_contact_tenant[grouped_city_contact_tenant$Tenant.Targeted == "Bachelors", ], 
      name = "Bachelors", 
      value = ~frequency, 
      labels = ~Contact.Person, 
      title = "Distribution of Contact Person for Bachelors", 
      domain = list(row = 0, column = 0)
    )
  
  fig_contact_tenant <- fig_contact_tenant %>% 
    add_pie(
      data = grouped_city_contact_tenant[grouped_city_contact_tenant$Tenant.Targeted == "Family", ], 
      name = "Family", 
      value = ~frequency,
      labels = ~Contact.Person, 
      title = "Distribution of Contact Person for Family", 
      domain = list(row = 0, column = 1)
    )
  
  fig_contact_tenant <- fig_contact_tenant %>% 
    add_pie(
      data = grouped_city_contact_tenant[grouped_city_contact_tenant$Tenant.Targeted == "Bachelors/Family", ], 
      name = "Bachelors/Family", 
      value = ~frequency, ===
      labels = ~Contact.Person, 
      title = "Distribution of Contact Person for Bachelors/Family", 
      domain = list(row = 0, column = 2)
    )
  
  fig_contact_tenant <- fig_contact_tenant %>% layout(
    title = "Distribution of Contact Person by Tenant Targeted",
    showlegend = TRUE,
    grid = list(rows = 1, columns = 3),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    margin = 1
  )
  fig_contact_tenant
}

plot_dist_tenant()

# 4. What is the trend of rental houses?
# Analysis 4-1: Facility Numbers against Date
df %>%
  group_by(Posted.Date) %>%
  summarize(agg = mean(Facilities.Number)) %>%
  mutate(Smoothed.Agg = rollmean(agg, k = 10, fill = NA, align = "center")) %>%
    ggplot() +
    labs(title = "Trend of the Number of Facilities", x = "Posted Date", y = "Number of Facility", colour = "Lines") +
    geom_line(mapping = aes(x = Posted.Date, y = agg, colour = "Facility Numbers")) +
    geom_line(mapping = aes(x = Posted.Date, y = Smoothed.Agg, colour = "Smoothed Facility Number"))

# Analysis 4-2: House Size against Date
df %>%
  group_by(Posted.Date) %>% 
  summarize(agg = mean(House.Size)) %>%
  ggplot(mapping = aes(x = Posted.Date, y = agg)) +
    labs(title = "House Size against Date", x = "Posted Date", y = "House Size") +
    geom_point(col = "#424242")

# Analysis 4-3: Count of Area Type against Date
df %>%
  group_by(Area.Type, Posted.Date) %>%
  summarize(frequency = n()) %>%
  ggplot(mapping = aes(x = Posted.Date, y = frequency, col = Area.Type)) +
    labs(title = "Trend of Area Type", x = "Posted Date", y = "Frequency") +
    geom_line() +
    facet_wrap(~Area.Type)

# Analysis 4-4: House City against Date
grouped_city_date <- 
  df %>%
  group_by(month = lubridate::floor_date(Posted.Date, unit = "month"), House.City) %>%
  summarize(frequency = n())

ggplot(data = grouped_city_date, mapping = aes(x = month, y = frequency, fill = House.City)) +
  labs(title = "Trend of Rental House Posted in City against Date", x = "Month", y = "Frequency of Rental House Posted") +
  guides(fill=guide_legend(title="House City")) +
  geom_bar(stat = "identity", position = "dodge", width = 20, col = "#424242") +
  scale_fill_brewer(palette = "Set2")

# Analysis 4-5: House Furnishing against Date (Bar chart group by Furnishing)
df %>% 
  group_by(House.Furnishing, Posted.Date) %>% 
  summarize(agg = n()) %>%
  ggplot(mapping = aes(x = Posted.Date, y = agg, col = House.Furnishing)) +
    labs(title = "Trend of Rental Posting by House Furnishing", x = "Posted Date", y = "Frequency") +
    geom_point() +
    geom_segment(mapping = aes(x = Posted.Date, xend = Posted.Date, y = 0, yend = agg)) +
    facet_wrap(~House.Furnishing)

# Analysis 4-6: Tenant Targeted against Date
df %>%
  group_by(month = lubridate::floor_date(Posted.Date, unit = "month"), Tenant.Targeted) %>%
  summarize(frequency = n()) %>%
  ggplot(mapping = aes(x = month, y = frequency, fill = Tenant.Targeted)) +
    labs(title = "Trend of Tenant Targeted against Date", x = "Month", y = "Frequency of Tenant Targeted") +
    geom_bar(stat = "identity", position = "dodge", width = 10, col = "#424242") +
    scale_fill_brewer(palette = "Accent")

# Analysis 4-7: Bathroom Number against Date (line)
df %>% 
  group_by(Posted.Date) %>% 
  summarize(agg = mean(Bathroom.Number)) %>%
  ggplot(mapping = aes(x = Posted.Date, y = agg)) +
    labs(title = "Trend of Bathroom Number of Rental House against Date", x = "Posted Date", y = "Bathroom Number") +
    geom_smooth(method = lm) + 
    geom_point(col = "orange") +
    stat_regline_equation(label.y.npc = 0.9)

# Analysis 4-8: Trend of Contact Person against Date
df %>%
  group_by(month = lubridate::floor_date(Posted.Date, unit = "month"), Contact.Person) %>%
  summarize(frequency = n()) %>%
  ggplot(mapping = aes(x = month, y = frequency, fill = Contact.Person)) +
    labs(title = "Trend of Contact Person over Date", x = "Month", y = "Frequency of Contact Person") +
    geom_bar(stat = "identity", width = 10, col = "#424242") +
    facet_wrap(~Contact.Person)

# Analysis 4-9: Trend of Current Floor against Date
df %>% 
  group_by(Posted.Date) %>% 
  summarize(agg = mean(Current.Floor)) %>%
  ggplot(mapping = aes(x = Posted.Date, y = agg)) +
    labs(title = "Trend of Current Floor against Date", x = "Posted Date", y = "Current Floor") +
    geom_smooth(method = lm) +
    geom_point(col = "#68228B") +
    stat_regline_equation(label.y.npc = 0.9)

# Analysis 4-10: Trend of Total Floor against Date
df %>%
  group_by(Posted.Date) %>%
  summarize(Mean.Total.Floor = mean(Total.Floor)) %>%
  ggplot(mapping = aes(x = Posted.Date, y = Mean.Total.Floor)) +
    geom_line(col = "#5D478B") +
    labs(title = "Trend of Total Floor over Date", x = "Posted Date", y = "Total Floor")

# 5. Characteristics of House posted by Owner and Agent comaparison
# Analysis 5-1: House Size comparison between House posted by Owner and Agent
contact_df <- df %>% filter(Contact.Person == "Agent" | Contact.Person == "Owner")

contact_df %>%
  ggplot(mapping = aes(x = House.Size)) +
    labs(title = "House Size Distribution of Owner and Agent", x = "House Size", y = "Frequency") +
    geom_histogram(colour = "orange", aes(fill = ..count..)) +
    scale_fill_gradient("Count", low = "green", high = "red") +
    facet_wrap(~Contact.Person)

# Analysis 5-2: On each city, the preference of owner or agent posting rental house
contact_df %>%
  group_by(House.City, Contact.Person) %>%
  summarize(total_count = n()) %>%
  ggplot(mapping = aes(x = House.City, y = total_count)) +
    labs(title = "Count of Rental House that has Agent or Owner as Contact Person in Each City", x = "House City", y = "Total Count") +
    geom_bar(stat = "identity", position = "dodge", width = 0.5, mapping = aes(fill = total_count)) +
    scale_fill_gradientn(colors = c("#00BFFF", "#1E90FF", "#104E8B")) +
    facet_wrap(~Contact.Person)

# Analysis 5-3: House Furnishing of Rental House posted by Agent and Owner.
plot_furnishing_contact <- function() {
  data <- contact_df %>% group_by(House.Furnishing, Contact.Person) %>% summarize(Count = n())
  fig <- plot_ly()
  fig <- fig %>% 
    add_pie(
      data = data %>% filter(Contact.Person == "Agent"), 
      name = "Agent", 
      value = ~Count, 
      labels = ~House.Furnishing, 
      title = "Agent Distribution", 
      domain = list(row = 0, column = 0)
    )
  
  fig <- fig %>% 
    add_pie(
      data = data %>% filter(Contact.Person == "Owner"), 
      name = "Owner", 
      value = ~Count, 
      labels = ~House.Furnishing, 
      title = "Owner Distribution", 
      domain = list(row = 0, column = 1)
    )
  
  fig <- fig %>% layout(
    title = "Distribution of Furnishing Status of Rental House by Agent and Owner",
    showlegend = TRUE,
    grid = list(rows = 1, columns = 2),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
  )
  fig
}
plot_furnishing_contact()

# Analysis 5-4: Tenant Targeted by Agent or Owner
contact_df %>%
  ggplot(mapping = aes(x = Tenant.Targeted)) + 
    labs(
      title = "Tenant Targeted by the Rental Posting that has Agent or Owner as Contact Person",
      x = "Tenant Targeted",
      y = "Frequency"
    ) +
    geom_bar(col = "#2F4F4F", width = 0.5, alpha = 0.7, mapping = aes(fill = after_stat(count))) +
    scale_fill_gradient("Count", low = "#00FFFF", high = "#008B8B") +
    facet_wrap(~Contact.Person)

# Analysis 5-5: Total Floor of the Rental House posted by Agent and Owner
contact_df %>%
  ggplot(mapping = aes(x = Total.Floor)) +
    labs(
      title = "Total Floor Distribution by Type of Contact Person", 
      x = "Total Floor", 
      y = "Frequency",
      fill = "Frequency"
    ) +
    geom_histogram(col = "#FF4500", aes(fill = after_stat(count))) +
    scale_fill_gradient(low = "#FFA07A", high = "#8B5742") +
    facet_wrap(~Contact.Person)

# Analysis 5-6: In each city, the Facilities Number of Rental House posted by Agent and Owner
contact_df %>%
  ggplot(mapping = aes(x = Contact.Person, y = Facilities.Number)) +
    labs(
      title = "Facilities Number of Rental House by Agent and Owner in Each City", 
      x = "Contact Person", 
      y = "Facilities Number",
      fill = "Contact Person"
    ) +
    geom_boxplot(aes(fill = Contact.Person)) +
    facet_wrap(~House.City)

# Analysis 5-7: In each city, Area Type of Rental House posted by Agent and Owner
contact_df %>%
  group_by(House.City, Area.Type, Contact.Person) %>%
  summarize(Count = n()) %>%
    ggplot(mapping = aes(x = House.City, y = Count, fill = Area.Type)) +
      labs(
        title = "Area Type of Rental House by Agent and Owner in Each City", 
        x = "City", 
        y = "Count",
        fill = "Frequency"
      ) +
      geom_bar(stat="identity", position = "dodge", width = 0.5, col = "#4D4D4D") +
      facet_wrap(~Contact.Person)

# Analysis 5-8: Bathroom Number of Rental Houses Posted by Agent and Owner
plot_percent_stack <- function() {
  final_df <- contact_df
  final_df$Bathroom.Number <- as.character(final_df$Bathroom.Number)
  
  final_df %>%
    group_by(Contact.Person, Bathroom.Number) %>%
    summarize(frequency = n()) %>%
      ggplot(mapping = aes(x = Bathroom.Number, y = frequency, fill = Contact.Person)) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = "Bathroom Number Count by Owner and Agent", x = "Bathroom Number", y = "Frequency") +
        scale_y_continuous(labels = percent)
  
}
plot_percent_stack()

# 6. Areas / Localities with highest potential
# Analysis 6-1 Tenant Distribution in each City
df %>%
  group_by(House.City, Tenant.Targeted) %>%
  summarize(Total.Tenant = n()) %>%
  mutate(percentage = calculate_percent(Total.Tenant)) %>%
  ggplot(mapping=aes(fill = Tenant.Targeted, x = House.City, y = Total.Tenant)) + 
    labs(title = "Distribution of Tenant Preferred in Each City", x = "City", y = "Frequency") +
    geom_bar(position = "fill", stat = "identity", col="black", width=0.45) +
    scale_y_continuous(labels = scales::percent) +
    geom_text(aes(label = paste0(percentage, "%")), size = 3, position = position_fill(vjust = 0.5)) +
    scale_fill_brewer(palette = "Spectral") +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1), 
      axis.title.x = element_text(margin = margin(t=30))
    )

# Analysis 6-2: Rental per Unit based on Locality (Top 20 Expensive)
plot_top20_expensive <- function() {
  # group data
  grouped_data <- 
    df %>%
      mutate(House.Locality = paste(House.Locality, ", ", House.City, sep = "")) %>%
      group_by(House.Locality) %>%
      summarize(agg = max(Rent.Per.Unit))
  
  # sort in descending order and get the top 20 (which is the most expensive 20)
  top20_expensive <- head(grouped_data[order(-grouped_data$agg), ], 20)
  ggplot(data = top20_expensive, mapping = aes(x = reorder(House.Locality, +agg), y = agg)) +
    labs(
      title = "Top 20 Expensive Rental Price per Unit by Locality", 
      x = "Localities",
      y = "Rental Price per Unit"
    ) +
    geom_bar(stat = "identity", mapping = aes(fill = agg), width = 0.5) +
    scale_fill_viridis("Median of Rent Per Unit Scale", option = "D") +
    geom_text(aes(label = agg), hjust = -0.2) +
    coord_flip()
  
}
plot_top20_expensive()

# Analysis 6-3: Rental per Unit based on Locality (Top 20 Cheapest)
plot_top20_cheapest <- function() {
  grouped_data <- 
    df %>%
      mutate(House.Locality = paste(House.Locality, ", ", House.City, sep = "")) %>%
      group_by(House.Locality) %>%
      summarize(agg = min(Rent.Per.Unit))
  
  top20_cheapest <- head(grouped_data[order(grouped_data$agg), ], 20)
  ggplot(data = top20_cheapest, mapping = aes(x = reorder(House.Locality, -agg), y = agg)) +
    labs(
      title = "Top 20 Cheapest Rental Price per Unit by Locality",
      x = "Localities",
      y = "Rental Price per Unit"
    ) +
    geom_bar(stat = "identity", mapping = aes(fill = agg), width = 0.5) +
    scale_fill_viridis("Median of Rent Per Unit Scale", option = "D") +
    geom_text(aes(label = agg), hjust = -0.2) +
    coord_flip()
  
}
plot_top20_cheapest()

# Analysis 6-4 Total Floor in each city
ggplot(data = df, mapping = aes(x = House.City, y = Total.Floor, col = House.City)) +
  labs(
    title = "Total Floor Distribution in Each City",
    x = "City",
    y = "Total Floor",
    col = "City"
  ) +
  geom_point()

# Analysis 6-5: Difference in Floor for City (highest and lowest)
df %>%
  ggplot(mapping = aes(x = House.City, y = Difference.Floor, fill = House.City)) +
    scale_fill_brewer(palette = "Accent") +
    geom_violin() +
    labs(
      title = "Difference in Floor Distribution in Each City",
      x = "City",
      y = "Total Floor",
      fill = "City"
    ) +
    stat_summary(fun.y = median, geom = "point", color = "black")

# Analysis 6-6: Common house furnishing each city
df %>%
  group_by(House.City, House.Furnishing) %>%
  summarize(frequency = n()) %>%
  mutate(percentage = calculate_percent(frequency)) %>%
  ggplot(mapping = aes(x = House.City, y = frequency, fill = House.Furnishing)) +
    geom_bar(stat = "identity", position = "fill", width = 0.5) +
    geom_text(aes(label = paste0(percentage, "%")), size = 3, position = position_fill(vjust = 0.5)) +
    labs(
      title = "Common House Furnishing in Each City",
      x = "City",
      y = "Percentage of Frequency",
      fill = "House Furnishing"
    ) +
    scale_y_continuous(labels = percent) +
    scale_fill_brewer(palette = "Pastel2")


