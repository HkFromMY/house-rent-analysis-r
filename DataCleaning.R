# libraries required
library(tidyverse) # data cleaning
library(dplyr) # data cleaning
library(lubridate) # data transformation (date manipulation and grouping)
library(ggplot2) # data visualization
library(scales) # data formatting in plots
library(stringr) # data manipulation
library(RColorBrewer) # for colour palette

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

# export data in .csv format
write.csv(df, "C:\\Users\\bpvpe\\OneDrive - Asia Pacific University\\Degree\\Sem 1\\Programming For Data Analysis\\Assignment\\CleanedHouseData.csv", row.names=FALSE)

# might have used for comparing relationship
df %>%
  group_by(House.City) %>%
  summarize(Mean.Rental = mean(Rental.Price), Mean.House.Size = mean(House.Size)) %>%
  ggplot() +
  geom_line(mapping = aes(x = House.City, y = Mean.Rental, group = 1)) +
  geom_point(mapping = aes(x = House.City, y = Mean.Rental)) +
  geom_bar(mapping = aes(x = House.City, y = Mean.House.Size * 10), stat = "identity") +
  scale_y_continuous(sec.axis = sec_axis(~. / 10, name = "House Size"))
