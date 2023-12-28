install.packages(c("dplyr", "ggplot2"))
library(dplyr)
library(ggplot2)

movies_data = read.csv("/Users/moizshaikh/Documents/McGill Fall 2023/MGSC 661/Project/IMDB_data_Fall_2023.csv")
attach(movies_data)

cat("Basic Summary of Dataset:\n")
str(movies_data)
cat("\nFirst few rows of the dataset:\n")
head(movies_data)

# Drop the 'labels and identifiers' columns
movies_data_cleaned <- movies_data %>%
  select(-c(movie_title, movie_id, imdb_link))


# Summary statistics for numerical variables
numerical_summary <- summary(movies_data_cleaned %>% select_if(is.numeric))

# Summary statistics for categorical variables
categorical_summary <- summary(movies_data_cleaned %>% select_if(is.factor))

# Display the summaries
cat("\nSummary Statistics for Numerical Variables:\n")
print(numerical_summary)

cat("\nSummary Statistics for Categorical Variables:\n")
print(categorical_summary)

str(movies_data)

# Identify character columns
character_columns <- names(movies_data_cleaned)[sapply(movies_data_cleaned, is.character)]

# Convert character columns to factors
movies_data_cleaned[character_columns] <- lapply(movies_data_cleaned[character_columns], as.factor)

# Confirm the changes
str(movies_data_cleaned)

numerical_summary <- summary(movies_data_cleaned %>% select_if(is.numeric))

# Summary statistics for categorical variables
categorical_summary <- summary(movies_data_cleaned %>% select_if(is.factor))

# Display the summaries
cat("\nSummary Statistics for Numerical Variables:\n")
print(numerical_summary)

cat("\nSummary Statistics for Categorical Variables:\n")
print(categorical_summary)



# Filter only numeric columns
numeric_data <- movies_data_cleaned %>% select_if(is.numeric)



library(ggplot2)

# Create a bar plot for the language column
p <- ggplot(movies_data_cleaned, aes(x = language)) + 
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Languages", x = "Language", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotating x-axis labels for better readability

print(p)



# Count the number of unique directors
num_unique_directors <- length(unique(movies_data$director))

# Print the result
print(num_unique_directors)


# Using ggplot2 to create a bar plot for the colour_film column
ggplot(movies_data, aes(x = colour_film)) +
  geom_bar(aes(fill = colour_film), position = "dodge") +
  labs(title = "Distribution of Colour Film", 
       x = "Colour Film Type", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1", name = "Colour Film Type")



# Split the plot_keywords by delimiter and unlist to get all keywords
all_keywords <- unlist(strsplit(as.character(movies_data_cleaned$plot_keywords), split = "\\|"))

# Get unique keywords
unique_keywords <- unique(all_keywords)

# Count the number of unique keywords
num_unique_keywords <- length(unique_keywords)

num_unique_keywords



movies_data_cleaned <- movies_data_cleaned %>% select(-plot_keywords)



num_unique_cinematographers <- length(unique(movies_data$cinematographer))
num_unique_cinematographers



# Drop specified variables
movies_data_cleaned <- movies_data_cleaned %>%
  select(-c(language, release_day, release_year, director, actor1, actor2, actor3, colour_film, cinematographer))

# Confirm the changes
str(movies_data_cleaned)



# Group by distributor and calculate count of movies
distributor_counts <- movies_data_cleaned %>%
  group_by(distributor) %>%
  summarize(movie_count = n())

# Join the count back to the original dataframe
movies_data_cleaned <- movies_data_cleaned %>%
  left_join(distributor_counts, by = "distributor")

# Create the dummy variable
movies_data_cleaned <- movies_data_cleaned %>%
  mutate(distributor_dummy = if_else(movie_count > 20, 1, 0))

# Drop the movie_count column as it's no longer needed
movies_data_cleaned <- movies_data_cleaned %>%
  select(-movie_count)

# Check the new dummy variable
head(movies_data_cleaned[c("distributor", "distributor_dummy")])


# Drop the distributor column
movies_data_cleaned <- movies_data_cleaned %>% select(-distributor)

# Check the structure to confirm
str(movies_data_cleaned)



# Group by production_company and calculate count of movies
production_company_counts <- movies_data_cleaned %>%
  group_by(production_company) %>%
  summarize(movie_count = n())

# Join the count back to the original dataframe
movies_data_cleaned <- movies_data_cleaned %>%
  left_join(production_company_counts, by = "production_company")

# Create the dummy variable and drop the original production_company column
movies_data_cleaned <- movies_data_cleaned %>%
  mutate(production_company_dummy = if_else(movie_count > 20, 1, 0)) %>%
  select(-c(production_company, movie_count))

# Check the new dummy variable
head(movies_data_cleaned[c("production_company_dummy")])



# Create dummy variables based on maturity_rating
movies_data_cleaned <- movies_data_cleaned %>%
  mutate(
    maturity_R = if_else(maturity_rating == "R", 1, 0),
    maturity_PG13 = if_else(maturity_rating == "PG-13", 1, 0),
    maturity_PG = if_else(maturity_rating == "PG", 1, 0),
    maturity_Others = if_else(!(maturity_rating %in% c("R", "PG-13", "PG")), 1, 0)
  )

# Check the changes
head(movies_data_cleaned[c("maturity_rating", "maturity_R", "maturity_PG13", "maturity_PG", "maturity_Others")])

# Drop the maturity_rating column
movies_data_cleaned <- movies_data_cleaned %>% select(-maturity_rating)

# Check the structure to confirm
str(movies_data_cleaned)



# Create dummy variable based on country
movies_data_cleaned <- movies_data_cleaned %>%
  mutate(country_USA = if_else(country == "USA", 1, 0))

movies_data_cleaned <- movies_data_cleaned %>% select(-country)



# Create dummy variables for each month
movies_data_cleaned <- movies_data_cleaned %>%
  mutate(
    month_Jan = if_else(release_month == 1, 1, 0),
    month_Feb = if_else(release_month == 2, 1, 0),
    month_Mar = if_else(release_month == 3, 1, 0),
    month_Apr = if_else(release_month == 4, 1, 0),
    month_May = if_else(release_month == 5, 1, 0),
    month_Jun = if_else(release_month == 6, 1, 0),
    month_Jul = if_else(release_month == 7, 1, 0),
    month_Aug = if_else(release_month == 8, 1, 0),
    month_Sep = if_else(release_month == 9, 1, 0),
    month_Oct = if_else(release_month == 10, 1, 0),
    month_Nov = if_else(release_month == 11, 1, 0),
    month_Dec = if_else(release_month == 12, 1, 0)
  )

# Check the new dummy variables
head(movies_data_cleaned[grepl("month_", names(movies_data_cleaned))])



# Drop the dummy columns created for release_month
movies_data_cleaned <- movies_data_cleaned %>% select(-starts_with("month_"))

# Check the structure to confirm
str(movies_data_cleaned)



# Check unique values of the release_month column
unique_values <- unique(movies_data_cleaned$release_month)

# Check the data type of the release_month column
data_type <- class(movies_data_cleaned$release_month)

unique_values
data_type



# Create dummy variables based on release_month with month names
movies_data_cleaned <- movies_data_cleaned %>%
  mutate(
    month_Jan = if_else(release_month == "Jan", 1, 0),
    month_Feb = if_else(release_month == "Feb", 1, 0),
    month_Mar = if_else(release_month == "Mar", 1, 0),
    month_Apr = if_else(release_month == "Apr", 1, 0),
    month_May = if_else(release_month == "May", 1, 0),
    month_Jun = if_else(release_month == "Jun", 1, 0),
    month_Jul = if_else(release_month == "Jul", 1, 0),
    month_Aug = if_else(release_month == "Aug", 1, 0),
    month_Sep = if_else(release_month == "Sep", 1, 0),
    month_Oct = if_else(release_month == "Oct", 1, 0),
    month_Nov = if_else(release_month == "Nov", 1, 0),
    month_Dec = if_else(release_month == "Dec", 1, 0)
  )

# Check the new dummy variables
head(movies_data_cleaned[grepl("month_", names(movies_data_cleaned))])



# Drop the release_month column
movies_data_cleaned <- movies_data_cleaned %>% select(-release_month)


# Drop existing genre dummy columns
existing_genre_cols <- c("action", "adventure", "animation", "crime", "drama", "horror", "musical", "romance", "scifi", "sport", "thriller", "war", "western")
movies_data_cleaned <- movies_data_cleaned %>% select(-all_of(existing_genre_cols))

# Extract unique genres from the 'genres' column
all_genres <- unique(unlist(strsplit(as.character(movies_data_cleaned$genres), split = "\\|")))

# Create new dummy columns for each unique genre
for (genre in all_genres) {
  movies_data_cleaned <- movies_data_cleaned %>%
    mutate(!!paste0("genre_", genre) := if_else(grepl(paste0("\\b", genre, "\\b"), genres), 1, 0))
}

# Check the new dummy variables
head(movies_data_cleaned[grepl("genre_", names(movies_data_cleaned))])



# Split the plot_keywords by delimiter and unlist to get all keywords
all_keywords <- unlist(strsplit(as.character(movies_data_cleaned$plot_keywords), split = "\\|"))

# Get unique keywords
unique_keywords <- unique(all_keywords)

# Count the number of unique keywords
num_unique_keywords <- length(unique_keywords)

num_unique_keywords



# Split the plot_keywords by delimiter and unlist to get all keywords
all_keywords <- unlist(strsplit(as.character(movies_data_cleaned$plot_keywords), split = "\\|"))

# Count the frequency of each keyword
keyword_frequency <- table(all_keywords)

# Sort the keywords by frequency in descending order
sorted_keywords <- sort(keyword_frequency, decreasing = TRUE)

# Select the top 20 keywords
top_20_keywords <- head(sorted_keywords, 20)

top_20_keywords


# Drop the genres column
movies_data_cleaned <- movies_data_cleaned %>% select(-genres)



# Install the openxlsx package (only need to run once)
install.packages("openxlsx")

# Load the openxlsx package
library(openxlsx)

# Write the dataframe to an Excel file
write.xlsx(movies_data_cleaned, "/Users/moizshaikh/Documents/McGill Fall 2023/MGSC 661/Project/PreprocessedDataset.xlsx", overwrite = TRUE)



# Install the ggcorrplot package (only need to run once)
install.packages("ggcorrplot")

# Load the ggcorrplot package
library(ggcorrplot)

# Compute the correlation matrix for numeric variables
cor_matrix <- cor(movies_data_cleaned %>% select_if(is.numeric), use = "complete.obs")

# Visualize the correlation matrix using a heatmap
ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE)



# Install and load necessary libraries
install.packages("igraph")
library(igraph)

# Define a threshold for strong correlations
threshold <- 0.5

# Extract correlations that exceed the threshold
correlations <- cor(movies_data_cleaned %>% select_if(is.numeric), use = "complete.obs")
strong_correlations <- correlations[abs(correlations) > threshold & correlations != 1]

# Convert the correlation matrix to long format
df_correlations <- as.data.frame(as.table(correlations))

# Filter correlations above the threshold and remove self-correlations
df_strong_correlations <- df_correlations %>% 
  filter(abs(Freq) > threshold, Var1 != Var2)

# Create a graph from the correlation data frame
graph <- graph_from_data_frame(df_strong_correlations, directed = FALSE)

# Plot the graph
plot(graph, vertex.label.cex=0.7, edge.width=abs(E(graph)$Freq)*2, edge.color=ifelse(E(graph)$Freq > 0, "green", "red"))

# Convert the correlation matrix to long format
df_correlations <- as.data.frame(as.table(correlations))



# Convert the correlation matrix to long format
df_correlations <- as.data.frame(as.table(correlations))

# Filter correlations above the threshold and remove self-correlations
df_strong_correlations <- df_correlations %>%
  filter(abs(Freq) > 0.8, Var1 != Var2) %>%
  arrange(desc(abs(Freq)))

# Display the top correlations
head(df_strong_correlations)



library(ggplot2)

# Determine bin width based on range of data
bin_width = (max(movies_data_cleaned$imdb_score, na.rm = TRUE) - min(movies_data_cleaned$imdb_score, na.rm = TRUE)) / 30

# Create a histogram for imdb_score
p <- ggplot(movies_data_cleaned, aes(x = imdb_score)) + 
  geom_histogram(binwidth = bin_width, fill = "skyblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of IMDb Score", x = "IMDb Score", y = "Frequency")

print(p)



# Install and load the e1071 package for skewness function
install.packages("e1071")
library(e1071)

# Compute skewness for each numeric variable
skewness_values <- sapply(movies_data_cleaned %>% select_if(is.numeric), skewness)

# Determine the skewness categories for each variable
highly_skewed_vars <- names(skewness_values[abs(skewness_values) > 1])
moderately_skewed_vars <- names(skewness_values[skewness_values > 0.5 & skewness_values <= 1 | skewness_values <= -0.5 & skewness_values >= -1])
slightly_skewed_vars <- names(skewness_values[skewness_values > -0.5 & skewness_values < 0.5])

list(
  Highly_Skewed = highly_skewed_vars,
  Moderately_Skewed = moderately_skewed_vars,
  Slightly_Skewed = slightly_skewed_vars
)



# Compute skewness for each numeric variable
skewness_values <- sapply(movies_data_cleaned[, sapply(movies_data_cleaned, is.numeric)], skewness)

# Print skewness values
print(skewness_values)



# List of variables to check for outliers
check_vars <- c("movie_budget", "duration", "nb_news_articles", "actor1_star_meter", 
                "actor2_star_meter", "actor3_star_meter", "nb_faces", "movie_meter_IMDBpro")

# Function to count outliers
count_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  sum(column < lower_bound | column > upper_bound, na.rm = TRUE)
}

# Apply the function to each variable in movies_data_cleaned and get the count of outliers
outliers_count <- sapply(movies_data_cleaned[check_vars], count_outliers)

# Print the count of outliers for each variable
print(outliers_count)



# Function to count outliers based on 3 standard deviations from mean or median
count_outliers_sd <- function(column) {
  # Compute skewness
  skew_val <- skewness(column, na.rm = TRUE)
  
  # Decide between mean and median based on skewness
  if (abs(skew_val) < 0.5) {
    center_val <- mean(column, na.rm = TRUE)
  } else {
    center_val <- median(column, na.rm = TRUE)
  }
  
  # Compute standard deviation
  sd_val <- sd(column, na.rm = TRUE)
  
  # Define bounds based on 3 standard deviations from the center value
  lower_bound <- center_val - 3 * sd_val
  upper_bound <- center_val + 3 * sd_val
  
  # Count the outliers
  sum(column < lower_bound | column > upper_bound, na.rm = TRUE)
}

# Apply the function to each variable in movies_data_cleaned and get the count of outliers
outliers_count_sd <- sapply(movies_data_cleaned[check_vars], count_outliers_sd)

# Print the count of outliers for each variable
print(outliers_count_sd)



# Install and load the reshape2 package
install.packages("reshape2")
library(reshape2)

# Melt the data to long format for easy plotting with ggplot2
actor_star_data <- melt(movies_data_cleaned[, c("actor1_star_meter", "actor2_star_meter", "actor3_star_meter")], 
                        variable.name = "Actor", 
                        value.name = "Star_Meter")

# Create the boxplot using ggplot2
ggplot(actor_star_data, aes(x = Actor, y = Star_Meter)) +
  geom_boxplot(fill = "skyblue", color = "black", outlier.color = "red", outlier.shape = 1) +
  labs(title = "Boxplots of Actor Star Meters", x = "Actor", y = "Star Meter Value") +
  theme_minimal()



# Calculate the correlation between Y and each predictor
correlations <- cor(movies_data_cleaned[, sapply(movies_data_cleaned, is.numeric)], use = "complete.obs")

# Extract the correlations with the response variable (assuming it's 'imdb_score')
Y_correlations <- correlations["imdb_score", ]

# Remove the correlation of imdb_score with itself
Y_correlations <- Y_correlations[-which(names(Y_correlations) == "imdb_score")]

# Print the correlations
print(Y_correlations)

# Interpret the strength and direction of each correlation
interpretation <- ifelse(abs(Y_correlations) < 0.3, "Weak", ifelse(abs(Y_correlations) < 0.7, "Moderate", "Strong"))
direction <- ifelse(Y_correlations > 0, "Positive", "Negative")

data.frame(Correlation = Y_correlations, Strength = interpretation, Direction = direction)



# Install and load necessary packages
install.packages(c("lmtest", "car"))
library(lmtest)
library(car)

# Assuming Y is 'imdb_score'
Y <- movies_data_cleaned$imdb_score

# Loop through each numeric predictor and plot scatter plots
numeric_predictors <- names(movies_data_cleaned)[sapply(movies_data_cleaned, is.numeric) & names(movies_data_cleaned) != "imdb_score"]

for(predictor in numeric_predictors) {
  # Scatter plot
  plot(movies_data_cleaned[[predictor]], Y, main=predictor, xlab=predictor, ylab="imdb_score")
}

# Run regressions and test for heteroskedasticity using Breusch-Pagan test
for(predictor in numeric_predictors) {
  model <- lm(Y ~ movies_data_cleaned[[predictor]])
  test <- bptest(model)
  cat("Breusch-Pagan test for predictor", predictor, ":", test$p.value, "\n")
  if(test$p.value < 0.05) {
    cat("Heteroskedasticity likely present for predictor", predictor, "\n\n")
  } else {
    cat("Heteroskedasticity not likely present for predictor", predictor, "\n\n")
  }
}


# List of numeric variables to plot
variables_to_plot <- c("imdb_score", "movie_budget", "duration", "nb_news_articles", "nb_faces", "movie_meter_IMDBpro")

# Loop through each variable and create individual boxplots
for (var in variables_to_plot) {
  
  # Generate the boxplot
  boxplot_graph <- ggplot(movies_data_cleaned, aes_string(y = var)) + 
    geom_boxplot(outlier.color = "red", fill = "skyblue") +
    theme_minimal() +
    labs(title = paste("Boxplot of", var), y = "Value")
  
  # Display the graph
  print(boxplot_graph)
}






# List of variables to check for outliers
variables_to_check <- c("duration", "movie_budget", "nb_news_articles", "actor1_star_meter", 
                        "actor2_star_meter", "actor3_star_meter", "nb_faces", "movie_meter_IMDBpro")

# Function to determine outliers using the IQR method
find_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(column < lower_bound | column > upper_bound)
}

# Apply the function to each variable and create a data frame with outlier flags
outlier_flags <- as.data.frame(lapply(movies_data_cleaned[variables_to_check], find_outliers))

# Sum the outlier flags for each row to determine the number of outliers per row
outlier_counts <- rowSums(outlier_flags)

# Filter rows with more than two outliers
rows_with_multiple_outliers <- movies_data_cleaned[outlier_counts > 2, ]

# Check if there are rows with more than two outliers and display them or print a message
if (nrow(rows_with_multiple_outliers) > 0) {
  head(rows_with_multiple_outliers)
} else {
  cat("There are no rows with more than two outliers based on the specified columns.")
}

# Calculate the count of rows with more than two outliers
count_rows_with_multiple_outliers <- sum(outlier_counts > 2)

# Display the count
cat("Number of rows with more than two outliers:", count_rows_with_multiple_outliers)



# Drop the rows with more than two outliers
movies_data_cleaned <- movies_data_cleaned[outlier_counts <= 2, ]

# Check the dimensions of the updated dataset
cat("Dimensions of the updated dataset:", dim(movies_data_cleaned))



# Install and load necessary libraries
install.packages("car")
library(car)

# Assuming Y is 'imdb_score'
Y <- movies_data_cleaned$imdb_score

# Choose one predictor for the demonstration
predictor <- "duration"

# Create a scatter plot between Y and the predictor
plot(movies_data_cleaned[[predictor]], Y, main=paste("Scatter Plot of", predictor, "vs. IMDb Score"),
     xlab=predictor, ylab="IMDb Score")

# Run a linear regression
model <- lm(Y ~ movies_data_cleaned[[predictor]])

# Test for non-constant variance using ncvTest from the car package
library(car)
test <- ncvTest(model)

# Display the test result
print(test)



# Scatter plot
plot(movies_data_cleaned$movie_budget, Y, main="Scatter Plot of movie_budget vs. IMDb Score",
     xlab="movie_budget", ylab="IMDb Score")

# Linear regression and non-constant variance test
model_budget <- lm(Y ~ movies_data_cleaned$movie_budget)
test_budget <- ncvTest(model_budget)
print(test_budget)



# Scatter plot
plot(movies_data_cleaned$nb_news_articles, Y, main="Scatter Plot of nb_news_articles vs. IMDb Score",
     xlab="nb_news_articles", ylab="IMDb Score")

# Linear regression and non-constant variance test
model_news_articles <- lm(Y ~ movies_data_cleaned$nb_news_articles)
test_news_articles <- ncvTest(model_news_articles)
print(test_news_articles)



# Scatter plot
plot(movies_data_cleaned$actor1_star_meter, Y, main="Scatter Plot of actor1_star_meter vs. IMDb Score",
     xlab="actor1_star_meter", ylab="IMDb Score")

# Linear regression and non-constant variance test
model_actor1 <- lm(Y ~ movies_data_cleaned$actor1_star_meter)
test_actor1 <- ncvTest(model_actor1)
print(test_actor1)



# Scatter plot
plot(movies_data_cleaned$actor2_star_meter, Y, main="Scatter Plot of actor2_star_meter vs. IMDb Score",
     xlab="actor2_star_meter", ylab="IMDb Score")

# Linear regression and non-constant variance test
model_actor2 <- lm(Y ~ movies_data_cleaned$actor2_star_meter)
test_actor2 <- ncvTest(model_actor2)
print(test_actor2)



# Scatter plot
plot(movies_data_cleaned$actor3_star_meter, Y, main="Scatter Plot of actor3_star_meter vs. IMDb Score",
     xlab="actor3_star_meter", ylab="IMDb Score")

# Linear regression and non-constant variance test
model_actor3 <- lm(Y ~ movies_data_cleaned$actor3_star_meter)
test_actor3 <- ncvTest(model_actor3)
print(test_actor3)



# Scatter plot
plot(movies_data_cleaned$nb_faces, Y, main="Scatter Plot of nb_faces vs. IMDb Score",
     xlab="nb_faces", ylab="IMDb Score")

# Linear regression and non-constant variance test
model_faces <- lm(Y ~ movies_data_cleaned$nb_faces)
test_faces <- ncvTest(model_faces)
print(test_faces)



# Scatter plot
plot(movies_data_cleaned$movie_meter_IMDBpro, Y, main="Scatter Plot of movie_meter_IMDBpro vs. IMDb Score",
     xlab="movie_meter_IMDBpro", ylab="IMDb Score")

# Linear regression and non-constant variance test
model_meter <- lm(Y ~ movies_data_cleaned$movie_meter_IMDBpro)
test_meter <- ncvTest(model_meter)
print(test_meter)



residualPlots(model)



# Create blockbuster_month dummy variable
movies_data_cleaned$blockbuster_month <- ifelse(movies_data_cleaned$month_May == 1 | 
                                                  movies_data_cleaned$month_Jun == 1 | 
                                                  movies_data_cleaned$month_Jul == 1 | 
                                                  movies_data_cleaned$month_Nov == 1 | 
                                                  movies_data_cleaned$month_Dec == 1, 1, 0)

# Preview the new column
head(movies_data_cleaned[c("month_May", "month_Jun", "month_Jul", "month_Nov", "month_Dec", "blockbuster_month")])



# Count values of blockbuster_month
blockbuster_counts <- table(movies_data_cleaned$blockbuster_month)
print(blockbuster_counts)



# Check correlation of blockbuster_month with imdb_score
correlation_value <- cor(movies_data_cleaned$blockbuster_month, movies_data_cleaned$imdb_score, use="complete.obs")
print(correlation_value)


# Create dummy variables based on aspect_ratio
movies_data_cleaned$aspect_ratio_2_35 <- ifelse(movies_data_cleaned$aspect_ratio == 2.35, 1, 0)
movies_data_cleaned$aspect_ratio_1_85 <- ifelse(movies_data_cleaned$aspect_ratio == 1.85, 1, 0)
movies_data_cleaned$aspect_ratio_others <- ifelse(movies_data_cleaned$aspect_ratio != 2.35 & 
                                                    movies_data_cleaned$aspect_ratio != 1.85, 1, 0)

# Preview the new columns
head(movies_data_cleaned[c("aspect_ratio", "aspect_ratio_2_35", "aspect_ratio_1_85", "aspect_ratio_others")])



# Drop the original aspect_ratio column
movies_data_cleaned <- movies_data_cleaned %>% select(-aspect_ratio)

# Check the structure to confirm
str(movies_data_cleaned)



# List of numeric predictors (you can modify this list as needed)
numeric_predictors <- c("movie_budget", "duration", "nb_news_articles", "actor1_star_meter", 
                        "actor2_star_meter", "actor3_star_meter", "nb_faces", "movie_meter_IMDBpro")
# Add other numeric predictors here


# Assuming Y is 'imdb_score'
Y <- movies_data_cleaned$imdb_score

# Initialize vectors to store results
rsquared_values <- numeric(0)
pvalues <- numeric(0)
predictor_names <- character(0)

# Loop through each predictor in the list and run a simple linear regression
for(predictor in numeric_predictors) {
  model <- lm(Y ~ movies_data_cleaned[[predictor]])
  rsquared_values <- c(rsquared_values, summary(model)$r.squared)
  pvalues <- c(pvalues, summary(model)$coefficients[2,4])  # Extracting the p-value for the predictor
  predictor_names <- c(predictor_names, predictor)
}

# Combine results into a data frame
results_df <- data.frame(Predictor = predictor_names, R_Squared = rsquared_values, P_Value = pvalues)

# Order by R_Squared values in descending order to see which variables have the most linear predictive power
results_df <- results_df[order(-results_df$R_Squared), ]

# Print the results
print(results_df)





# Load necessary libraries
library(ggplot2)
library(e1071)

# List of numeric predictors
numeric_predictors <- c("duration", "movie_budget", "nb_news_articles", "actor1_star_meter", 
                        "actor2_star_meter", "actor3_star_meter", "nb_faces", "movie_meter_IMDBpro")

# Loop through each numeric predictor to create histograms and compute skewness
for(predictor in numeric_predictors) {
  # Calculate the bin width using Freedman-Diaconis rule
  IQR_val <- IQR(movies_data_cleaned[[predictor]], na.rm = TRUE)
  n_val <- length(na.omit(movies_data_cleaned[[predictor]]))
  bin_width <- 2 * IQR_val / (n_val^(1/3))
  
  # Generate the histogram
  p <- ggplot(movies_data_cleaned, aes_string(x = predictor)) + 
    geom_histogram(fill = "skyblue", color = "black", alpha = 0.7, binwidth = bin_width) +
    theme_minimal() +
    labs(title = paste("Histogram of", predictor), x = predictor, y = "Frequency")
  
  # Display the graph
  print(p)
  
  # Compute and print skewness
  skew_val <- skewness(movies_data_cleaned[[predictor]], na.rm = TRUE)
  cat("Skewness of", predictor, ":", skew_val, "\n\n")
}



# Install and load the openxlsx package
install.packages("openxlsx")
library(openxlsx)

# Define the file path
output_file_path <- "/Users/moizshaikh/Documents/McGill Fall 2023/MGSC 661/Project/PreprocessedDataset.xlsx"

# Write the dataframe to an Excel file
write.xlsx(movies_data_cleaned, output_file_path, overwrite = TRUE)

# Print a confirmation message
cat("Dataframe has been written to", output_file_path)


# Model building code -----------------------------------------------------


#load the preprocessed dataset
install.packages("readxl")
library(readxl)

pre_processed_dataset <- read_excel("D:/Google Drive/McGill/Fall Semester/MGSC 661/Midterm Project/PreprocessedDataset.xlsx")
attach(pre_processed_dataset)

#remove outlier

plot(pre_processed_dataset$nb_news_articles, pre_processed_dataset$imdb_score, 
     xlab="Number of News Articles", ylab="IMDB Score",
     main="Scatter Plot of nb_news_articles vs IMDB Score")
pre_processed_dataset <- pre_processed_dataset[pre_processed_dataset$nb_news_articles <= 40000, ]

plot(pre_processed_dataset$movie_budget, pre_processed_dataset$imdb_score, 
     xlab="Number of movie_budget ", ylab="IMDB Score",
     main="Scatter Plot of movie_budget vs IMDB Score")


plot(pre_processed_dataset$duration, pre_processed_dataset$imdb_score, 
     xlab="Number of duration ", ylab="IMDB Score",
     main="Scatter Plot of duration vs IMDB Score")

plot(pre_processed_dataset$movie_meter_IMDBpro, pre_processed_dataset$imdb_score, 
     xlab="Number of movie_meter_IMDBpro ", ylab="IMDB Score",
     main="Scatter Plot of movie_meter_IMDBpro vs IMDB Score")

#check for linearity assumption of the data and see if the relationships should be modeled non linearly.
library('car')
reg1=lm(imdb_score~movie_budget+duration+nb_news_articles+actor1_star_meter+actor2_star_meter+
          actor3_star_meter+nb_faces+movie_meter_IMDBpro)
residualPlots(reg1)


#p-value larger than 0.1 is linear 
#so actor1_star_meter,actor2_star_meter,actor3_star_meter,nb_faces are linear 


# test different polynomial degrees for each variable and picks the one that minimizes the RMSE
install.packages("caret")
library(caret)

control <- trainControl(method="cv", number=5)

non_linear_vars <- c("movie_budget", "duration", "nb_news_articles", "movie_meter_IMDBpro")
best_degrees <- list()

for (var in non_linear_vars) {
  best_mse <- Inf
  best_degree <- 0
  
  for (degree in 1:4) {
    formula <- as.formula(paste("imdb_score ~ poly(", var, ", ", degree, ")", sep=""))
    set.seed(123)
    fit <- train(formula, data=pre_processed_dataset, method="lm", 
                 trControl=control, metric="RMSE")
    
    mse <- fit$results$RMSE[1]
    
    if (mse < best_mse) {
      best_mse <- mse
      best_degree <- degree
    }
  }
  
  best_degrees[[var]] <- best_degree
}

print(best_degrees)
print(best_mse)

#movie_budget: Degree 2, duration: Degree 2, nb_news_articles: Degree 4, movie_meter_IMDBpro: Degree 3
# Building the final linear model
install.packages("boot")
# Load the package
library(boot)

min_max_scale <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Apply the scaling function to all columns except imdb_score
scaled_columns <- lapply(names(pre_processed_dataset), function(col) {
  if (col != "imdb_score") {
    return(min_max_scale(pre_processed_dataset[[col]]))
  } else {
    return(pre_processed_dataset[[col]])
  }
})

# Convert the scaled columns to a data frame
scaled_dataset <- as.data.frame(do.call(cbind, scaled_columns))

# Name the columns
names(scaled_dataset) <- names(pre_processed_dataset)

final_model <- glm(imdb_score ~ poly(movie_budget,2) + poly(duration, 2) + poly(nb_news_articles,4) + 
                     actor1_star_meter + actor2_star_meter + actor3_star_meter + nb_faces + 
                     poly(movie_meter_IMDBpro, 3) + distributor_dummy + production_company_dummy +
                     maturity_R + maturity_PG13 + maturity_PG + maturity_Others + country_USA + 
                     month_Jan + month_Feb + month_Mar + month_Apr + month_May + month_Jun + 
                     month_Jul + month_Aug + month_Sep + month_Oct + month_Nov + month_Dec + 
                     genre_Drama + genre_Biography + genre_Sport + genre_Horror + genre_Thriller + 
                     genre_Crime + genre_Comedy + genre_Adventure  + 
                     genre_Action  + genre_Fantasy  + genre_Mystery + 
                     genre_Family   + genre_Animation + 
                     genre_Documentary + blockbuster_month + aspect_ratio_2_35 + aspect_ratio_1_85 + 
                     aspect_ratio_others, 
                   data=scaled_dataset)

mse=cv.glm(scaled_dataset,final_model,K=10)$delta[1]
mse
summary(final_model)
#remove most variables with p values greater than 0.05 which means they are insignificant

#actor1_star_meter+actor2_star_meter+actor3_star_meterdistributor_dummy + maturity_R+ maturity_Others+ maturity_PG+maturity_Others+month_Jan + month_Feb + month_Mar + month_Apr + month_May + month_Jun + month_Jul + month_Aug + month_Sep + month_Oct + month_Nov + month_Dec+genre_Adventure+blockbuster_month+aspect_ratio_others +genre_Fantasy, aspect ratio dummys, distributor_dummy

final_model1 <- glm(imdb_score ~ poly(movie_budget,2) + poly(duration, 2) + poly(nb_news_articles,4) + 
                      nb_faces + 
                      poly(movie_meter_IMDBpro, 3)   +
                      maturity_PG13  + country_USA + 
                      
                      genre_Drama  + genre_Sport + genre_Horror + genre_Thriller + 
                      genre_Crime + genre_Comedy   + 
                      genre_Action   + genre_Mystery + 
                      genre_Family   + genre_Animation + 
                      genre_Documentary , 
                    data=scaled_dataset)

mse=cv.glm(scaled_dataset,final_model1,K=10)$delta[1]
mse
summary(final_model1)

final_model1_r2 <- lm(imdb_score ~ poly(movie_budget,2) + poly(duration, 2) + poly(nb_news_articles,4) + 
                      nb_faces + 
                      poly(movie_meter_IMDBpro, 3)   +
                      maturity_PG13  + country_USA + 
                      
                      genre_Drama  + genre_Sport + genre_Horror + genre_Thriller + 
                      genre_Crime + genre_Comedy   + 
                      genre_Action   + genre_Mystery + 
                      genre_Family   + genre_Animation + 
                      genre_Documentary , 
                    data=scaled_dataset)
summary(final_model1_r2)
#testing models with different variables to find what varaibles can give us significant  values and lowest mse

final_model2 <- glm(imdb_score ~ poly(movie_budget,2) + poly(duration, 2) + poly(nb_news_articles,4) + 
                      nb_faces + 
                      poly(movie_meter_IMDBpro, 3)  + production_company_dummy +
                      maturity_PG13  + country_USA + 
                      
                      genre_Drama  + genre_Sport + genre_Horror + genre_Thriller + 
                      genre_Crime + genre_Comedy   + maturity_Others+month_Dec+blockbuster_month+aspect_ratio_others+
                      genre_Action   + genre_Mystery + 
                      genre_Family   + genre_Animation + 
                      genre_Documentary  + aspect_ratio_2_35 + aspect_ratio_1_85, 
                    data=scaled_dataset)
mse=cv.glm(scaled_dataset,final_model2,K=10)$delta[1]
mse
summary(final_model2)

final_model3 <- glm(imdb_score ~ poly(movie_budget,2) + poly(duration, 2) + poly(nb_news_articles,2) + 
                       nb_faces + 
                     poly(movie_meter_IMDBpro, 3)   
                      + maturity_PG13  + country_USA + blockbuster_month+
                       
                      
                     genre_Drama  + genre_Sport + genre_Horror +
                     genre_Crime  + 
                     genre_Action   
                      
                         
                     , 
                   data=scaled_dataset)

mse=cv.glm(scaled_dataset,final_model3,K=10)$delta[1]
mse
summary(final_model3)

#final_model1 is th best model

# Transform the target variable using natural logarithm to fix skewness
# Apply log transformation to the target variable
pre_processed_dataset$log_imdb_score <- log(pre_processed_dataset$imdb_score)

# Define non-linear variables and empty list to store results
non_linear_vars <- c("movie_budget", "duration", "nb_news_articles", "movie_meter_IMDBpro")
best_degrees <- list()

# Loop through each non-linear variable
for (var in non_linear_vars) {
  best_mse <- Inf
  best_degree <- 0
  
  # Loop through different degrees
  for (degree in 1:4) {
    formula <- as.formula(paste("log_imdb_score ~ poly(", var, ", ", degree, ")", sep=""))
    model <- glm(formula, data=pre_processed_dataset)
    cv_results <- cv.glm(pre_processed_dataset, model, K=5)
    
    # Update best degree and MSE if current MSE is better
    if (cv_results$delta[1] < best_mse) {
      best_mse <- cv_results$delta[1]
      best_degree <- degree
    }
  }
  
  # Store the best degree for each variable
  best_degrees[[var]] <- best_degree
}

# Print the best degrees for each non-linear variable
print(best_degrees)
print(best_mse)

#best degrees movie_budget = 2, duration = 2, nb_news_articles = 4, movie_meter_IMDBpro = 1

#train model

scaled_dataset$log_imdb_score <- log(scaled_dataset$imdb_score)

# Train the final model with the best degree polynomials and dummy variables
final_model_log <- glm(log_imdb_score ~ poly(movie_budget, 2) + poly(duration, 2) + poly(nb_news_articles,4) + 
                        actor1_star_meter + actor2_star_meter + actor3_star_meter + nb_faces + 
                        poly(movie_meter_IMDBpro, 1) + distributor_dummy + production_company_dummy +
                        maturity_R + maturity_PG13 + maturity_PG + maturity_Others + country_USA + 
                        month_Jan + month_Feb + month_Mar + month_Apr + month_May + month_Jun + 
                        month_Jul + month_Aug + month_Sep + month_Oct + month_Nov + month_Dec + 
                         genre_Drama + genre_Biography + genre_Sport + genre_Horror + genre_Thriller + 
                         genre_Crime + genre_Comedy + genre_Adventure  + 
                         genre_Action  + genre_Fantasy  + genre_Mystery + 
                         genre_Family   + genre_Animation + 
                         genre_Documentary + blockbuster_month + aspect_ratio_2_35 + aspect_ratio_1_85 + 
                        aspect_ratio_others, 
                      data=scaled_dataset)

# Summarize the final model
mse=cv.glm(scaled_dataset,final_model_log,K=10)$delta[1]
mse
summary(final_model_log)

#run model without insignificant predictors

final_model_log1 <- glm(log_imdb_score ~ poly(movie_budget,2) + poly(duration, 2) + poly(nb_news_articles,4) + 
                      nb_faces + 
                      poly(movie_meter_IMDBpro, 1)  + production_company_dummy +
                      maturity_PG13  + country_USA + 
                      
                      genre_Drama  + genre_Sport + genre_Horror + genre_Thriller + 
                      genre_Crime + genre_Comedy   + 
                      genre_Action   + genre_Mystery + 
                      genre_Family   + genre_Animation + 
                      genre_Documentary,
                    data=scaled_dataset)

mse=cv.glm(scaled_dataset,final_model_log1,K=10)$delta[1]
mse
final_model_log2 <- glm(log_imdb_score ~ poly(movie_budget,2) + poly(duration, 2) + poly(nb_news_articles,2) + 
                      nb_faces + 
                      poly(movie_meter_IMDBpro, 3)   
                    + maturity_PG13  + country_USA + blockbuster_month+
                      
                      
                      genre_Drama  + genre_Sport + genre_Horror +
                      genre_Crime  + 
                      genre_Action   
                    
                    
                    , 
                    data=scaled_dataset)

mse=cv.glm(scaled_dataset,final_model_log2,K=10)$delta[1]
mse
summary(final_model_log2)
# Load the necessary libraries
library(dplyr)
library(tidyr)
install.packages("caret")
library(caret)



#use spline regression 
install.packages("cvms")
library(cvms)


# Load required libraries
library(splines)
install.packages("DAAG")
library(DAAG)

# find n degree spline for non linear variables
non_linear_vars <- c("movie_budget", "duration", "nb_news_articles", "movie_meter_IMDBpro")

# Initialize array to store R-squared values
# Initialize array to store R-squared values
n_degree = 3  
someData <- rep(NA, 2 * n_degree)
ar <- array(someData, c(n_degree, 2))
colnames(ar) = c('degree', 'r2_spline')

# Loop through each non-linear predictor variable
for (var in non_linear_vars) {
  
  print(paste("Results for variable:", var))
  
  # Determine the quantiles for knots
  k1 <- quantile(pre_processed_dataset[[var]], 1/4)
  k2 <- quantile(pre_processed_dataset[[var]], 2/4)
  k3 <- quantile(pre_processed_dataset[[var]], 3/4)
  k4 <- quantile(pre_processed_dataset[[var]], 4/4)
  
  # Loop through different degrees for the spline, now up to 6
  for (degree in 1:n_degree) {
    formula <- as.formula(paste("imdb_score ~ bs(", var, ", knots=c(", k1, ",", k2, ",", k3, ",", k4, "), degree=", degree, ")", sep=""))
    model_spline <- lm(formula, data=pre_processed_dataset)
    
    # Store the R-squared value
    ar[degree, 1] = degree
    ar[degree, 2] = summary(model_spline)$r.squared
  }
  
  print(ar)
}



#For movie_budget, the third-degree spline has the highest R-squared (0.011213855).
#For duration, the second-degree spline has the highest R-squared (0.2014346).
#For nb_news_articles, the second-degree spline has the highest R-squared (0.1321571).
#For movie_meter_IMDBpro, the first-degree spline has the highest R-squared (0.1869006)


#Visualize splines
best_degrees <- list(
  "movie_budget" = 3,
  "duration" = 2,
  "nb_news_articles" = 2,
  "movie_meter_IMDBpro" = 1
)

for (var in names(best_degrees)) {
  
  print(paste("Plot for variable:", var))
  
  # Determine the quantiles for knots
  k1 <- quantile(pre_processed_dataset[[var]], 1/4)
  k2 <- quantile(pre_processed_dataset[[var]], 2/4)
  k3 <- quantile(pre_processed_dataset[[var]], 3/4)
  k4 <- quantile(pre_processed_dataset[[var]], 4/4)
  
  # Use the best degree for the spline
  best_degree <- best_degrees[[var]]
  formula <- as.formula(paste("imdb_score ~ bs(", var, ", knots=c(", k1, ",", k2, ",", k3, ",", k4, "), degree=", best_degree, ")", sep=""))
  model_spline <- lm(formula, data=pre_processed_dataset)
  
  
  new_data <- data.frame(var = seq(min(pre_processed_dataset[[var]]), max(pre_processed_dataset[[var]]), length.out=100))
  colnames(new_data) <- var
  
  
  new_data$imdb_score_pred <- predict(model_spline, newdata=new_data)
  
  
  p <- ggplot(pre_processed_dataset, aes_string(x=var, y="imdb_score")) +
    geom_point(aes(color="Actual"), alpha=0.5) +
    geom_line(data=new_data, aes_string(x=var, y="imdb_score_pred"), color="red", size=1) +
    ggtitle(paste("Best Degree:", best_degree)) +
    labs(color='Data Type')
  
  print(p)
}


#build spline models

# Construct the linear model

final_spline_model <- glm(
  imdb_score ~ 
    bs(movie_budget, degree=3) +
    bs(duration, degree=2) +
    bs(nb_news_articles, degree=2) +
    bs(movie_meter_IMDBpro, degree=1) +
    actor1_star_meter +
    actor2_star_meter +
    actor3_star_meter +
    nb_faces +
    distributor_dummy +
    production_company_dummy +
    maturity_R +
    maturity_PG13 +
    maturity_PG +
    maturity_Others +
    country_USA +
    month_Jan + month_Feb + month_Mar + month_Apr + month_May + month_Jun + month_Jul +
    month_Aug + month_Sep + month_Oct + month_Nov + month_Dec +
    genre_Drama + genre_Biography + genre_Sport + genre_Horror + genre_Thriller + 
    genre_Crime + genre_Comedy + genre_Adventure  + 
    genre_Action  + genre_Fantasy  + genre_Mystery + 
    genre_Family   + genre_Animation + 
    genre_Documentary +
    blockbuster_month +
    aspect_ratio_2_35 +
    aspect_ratio_1_85 +
    aspect_ratio_others,
  data=scaled_dataset
)

mse=cv.glm(scaled_dataset,final_spline_model,K=5)$delta[1]
mse

summary(final_spline_model)
#run model without insignificant predictors


final_spline_model1 <- glm(imdb_score ~ bs(movie_budget, degree=3) + bs(duration, degree=2) + bs(nb_news_articles, degree=2) + 
                      nb_faces + 
                        bs(movie_meter_IMDBpro, degree=1)  + production_company_dummy +
                      maturity_PG13  + country_USA + 
                      
                      genre_Drama  + genre_Sport + genre_Horror + genre_Thriller + 
                      genre_Crime + genre_Comedy   + 
                      genre_Action   + genre_Mystery + 
                      genre_Family   + genre_Animation + 
                      genre_Documentary  + aspect_ratio_2_35 + aspect_ratio_1_85, 
                      data=scaled_dataset)

mse=cv.glm(scaled_dataset,final_spline_model1,K=10)$delta[1]
mse

final_spline_model2 <- glm(imdb_score ~ bs(movie_budget, degree=3) + bs(duration, degree=2) + bs(nb_news_articles, degree=2) + 
                      nb_faces + production_company_dummy+
                        bs(movie_meter_IMDBpro, degree=1)   
                    + maturity_PG13  + country_USA + blockbuster_month+
                      
                      
                      genre_Drama  + genre_Sport + genre_Horror +genre_Family+                        
                      genre_Crime  + 
                      genre_Action   
                    
                    
                    , 
                    data=scaled_dataset)

mse=cv.glm(scaled_dataset,final_spline_model2,K=10)$delta[1]
mse
summary(final_spline_model2)

#check if local regression can fit data better

#movie_budget
reg1=loess(imdb_score~movie_budget,span=0.5)
plot <- ggplot(pre_processed_dataset, aes(x=movie_budget, y=imdb_score)) +
  geom_point(aes(color="Actual"), alpha=0.5)  # Scatter plot
localplot1 <- geom_smooth(method = "loess", aes(color="Fitted"), span = 0.5, se = FALSE)

final_plot <- plot + localplot1

print(final_plot)

#duration
reg2=loess(imdb_score~duration,span=1)

plot <- ggplot(pre_processed_dataset, aes(x=duration, y=imdb_score)) +
  geom_point(aes(color="Actual"), alpha=0.5)  # Scatter plot

localplot1 <- geom_smooth(method = "loess", aes(color="Fitted"), span = 1, se = FALSE)

final_plot <- plot + localplot1

print(final_plot)

#nb_news_articles

reg3=loess(imdb_score~nb_news_articles,span=0.1)

plot <- ggplot(pre_processed_dataset, aes(x=nb_news_articles, y=imdb_score)) +
  geom_point(aes(color="Actual"), alpha=0.5)  # Scatter plot

localplot1 <- geom_smooth(method = "loess", aes(color="Fitted"), span = 0.1, se = FALSE)

final_plot <- plot + localplot1

print(final_plot)

#movie_meter_IMDBpro

reg4=loess(imdb_score~movie_meter_IMDBpro,span=0.1)

plot <- ggplot(pre_processed_dataset, aes(x=movie_meter_IMDBpro, y=imdb_score)) +
  geom_point(aes(color="Actual"), alpha=0.5)  # Scatter plot

localplot1 <- geom_smooth(method = "loess", aes(color="Fitted"), span = 0.1, se = FALSE)

final_plot <- plot + localplot1

print(final_plot)

#Local regressions are not giving fits better than spline and polynomial so model is not trained

# Load the preprocessed training data and test data
#Model to test
#1) final_model (polynomial)
#2) final_spline_model(spline)
#3)final_model_log(polynomial log))

#training dataset was also preprocessed to have same columns it is called PreprocessedTestDataset.csv
test_data = read.csv("D:\\Google Drive\\McGill\\Fall Semester\\MGSC 661\\Midterm Project\\PreprocessedTestDataset.csv")
test_data$movie_budget <- as.integer(gsub(",", "", test_data$movie_budget))

min_max_scale <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

scaled_test_dataset <- as.data.frame(lapply(test_data, min_max_scale))
scaled_test_dataset[is.na(scaled_test_dataset)] <- 0


movie_names=c('Pencils vs Pixels',
              'The Dirty South',
              'The Marvels',
              'The Holdovers',
              'Next Goal Wins',
              'Thanksgiving',
              'The Hunger Games: The Ballad of Songbirds and Snakes',
              'Trolls Band Together',
              'Leo',
              'Dream Scenario',
              'Wish',
              'Napoleon')

####
predictions <- predict(final_model, newdata=scaled_test_dataset)
df=data.frame(movie_names,predictions)
df

scaled_test_dataset1 <- scaled_test_dataset %>%
  select(-c("actor1_star_meter", "actor2_star_meter", "actor3_star_meter", "distributor_dummy", 
            "maturity_R", "maturity_Others", "maturity_PG", "maturity_Others",
            "month_Jan", "month_Feb", "month_Mar", "month_Apr", "month_May", "month_Jun",
            "month_Jul", "month_Aug", "month_Sep", "month_Oct", "month_Nov", "month_Dec",
            "genre_Adventure", "blockbuster_month", "aspect_ratio_others", "genre_Fantasy","genre_Biography"))


scaled_test_dataset11 <- dplyr::select(scaled_test_dataset, -c("production_company_dummy","actor1_star_meter", "actor2_star_meter", "actor3_star_meter", "distributor_dummy", "maturity_R", "maturity_Others", "maturity_PG", "maturity_Others", "month_Jan", "month_Feb", "month_Mar", "month_Apr", "month_May", "month_Jun", "month_Jul", "month_Aug", "month_Sep", "month_Oct", "month_Nov", "month_Dec", "genre_Adventure", "blockbuster_month", "aspect_ratio_others", "genre_Fantasy", "genre_Biography","aspect_ratio_2_35","aspect_ratio_1_85"))

#best model to be used for predictions is final_model1
predictions1 <- predict(final_model1, newdata=scaled_test_dataset11)
df1=data.frame(movie_names,predictions1)
df1


#model trained with imdb score log transformed
predictions_log <- predict(final_model_log, newdata=scaled_test_dataset)
original_scale_predictions = exp(predictions_log)
df_log=data.frame(movie_names,original_scale_predictions)
df_log

scaled_test_dataset2 <- scaled_test_dataset1 %>%
  select(-c("aspect_ratio_2_35","aspect_ratio_1_85"))

predictions_log1 <- predict(final_model_log1, newdata=scaled_test_dataset2)
original_scale_predictions1 = exp(predictions_log1)
df_log1=data.frame(movie_names,original_scale_predictions1)
df_log1

scaled_test_dataset3 <- scaled_test_dataset %>%
  select(c("movie_budget","duration","nb_news_articles","nb_faces","movie_meter_IMDBpro",
          "maturity_PG13","country_USA","blockbuster_month","genre_Drama","genre_Sport"
          ,"genre_Horror","genre_Crime","genre_Action"))

predictions_log2 <- predict(final_model_log2, newdata=scaled_test_dataset3)
original_scale_predictions1 = exp(predictions_log2)
df_log2=data.frame(movie_names,original_scale_predictions1)
df_log2

#spline model
predictions_spline <- predict(final_spline_model, newdata=scaled_test_dataset)
df_spline = data.frame(movie_names, predictions_spline)
df_spline

predictions_spline1 <- predict(final_spline_model1, newdata=scaled_test_dataset1)
df_spline11 = data.frame(movie_names, predictions_spline1)
df_spline11


scaled_test_dataset3 <- dplyr::select(scaled_test_dataset, "country_USA", "maturity_PG13", "blockbuster_month", "genre_Drama", "genre_Sport", "genre_Horror", "genre_Crime", "genre_Action", "movie_budget", "duration", "nb_news_articles", "nb_faces", "movie_meter_IMDBpro","production_company_dummy","genre_Family")

predictions_spline2 <- predict(final_spline_model2, newdata=scaled_test_dataset3)
df_spline1 = data.frame(movie_names, predictions_spline2)
df_spline1


# Creating a consolidated data frame containing movie names and predictions from all models
final_predictions_df <- Reduce(function(x, y) merge(x, y, by="movie_names", all=TRUE),
                               list(df, df_log, df_spline,df1,df_log2,df_spline1))

colnames(final_predictions_df) <- c("Movie Names", "Model Predictions", "Log Model Predictions", "Spline Model Predictions","Model insignificant variables removed","Log Model Predictions insignificant variables removed","Spline Model Predictions insignificant variables removed")
final_predictions_df

write.csv(final_predictions_df, "final_predictions_table.csv", row.names = FALSE)

# Write to HTML table
library(xtable)
print(xtable(final_predictions_df), type = "html", file = "final_predictions_table.html")

colnames(df1) <- c("Movie Names","Model Predictions insignificant variables removed")

write.csv(df1, "final_predictions_model.csv", row.names = FALSE)
print(xtable(df1), type = "html", file = "final_predictions_model.html")

####

install.packages("stargazer")
library(stargazer)

stargazer(final_model1, type = "html")

