# Load required libraries 
library(readr) 
library(stats) 
library(ggplot2) 
library(dplyr) 
library(lmtest) 
 
# Load your dataset 
BTC_USD <- read_csv("C:/Users/umair/Downloads/BTC-USD.csv") 
 
# Function to determine if a variable is quantitative or qualitative 
variable_type <- function(data, variable_name) { 
  variable <- data[[variable_name]] 
   
  # Check if the variable is numeric 
  if (is.numeric(variable)) { 
    return("Quantitative") 
  } else { 
    return("Qualitative") 
  } 
} 
 
# Check the data type of different columns 
columns_to_check <- c("Open", "High", "Low", "Close", "Adj Close", "Volume") 
for (col in columns_to_check) { 
  result <- variable_type(BTC_USD, col) 
  cat("The '", col, "' variable is", result, "\n") 
} 
 
# Calculate descriptive statistics 
descriptive_stats <- function(data, variable_name) { 
  variable <- data[[variable_name]] 
   
  mean_val <- mean(variable, na.rm = TRUE) 
  median_val <- median(variable, na.rm = TRUE) 
  sd_val <- sd(variable, na.rm = TRUE) 
  min_val <- min(variable, na.rm = TRUE) 
  max_val <- max(variable, na.rm = TRUE) 
   
  stats <- data.frame( 
    Variable = variable_name, 
    Mean = mean_val, 
    Median = median_val, 
    StandardDeviation = sd_val, 
    Minimum = min_val, 
    Maximum = max_val 
  ) 
   
  return(stats) 
} 
 
# Calculate and display descriptive statistics for each numeric column 
numeric_columns <- c("Open", "High", "Low", "Close", "Adj Close", "Volume") 
for (col in numeric_columns) { 
  stats <- descriptive_stats(BTC_USD, col) 
  print(stats) 
} 
 
# Create a bar chart for the 'Volume' column 
ggplot(data = BTC_USD, aes(x = Date, y = Volume)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  labs(title = "Bar Chart of Volume", x = "Date", y = "Volume") 
 
# Create a scatter plot for 'High' vs. 'Low' 
ggplot(data = BTC_USD, aes(x = High, y = Low)) + 
  geom_point(color = "green") + 
  labs(title = "Scatter Plot of High vs. Low", x = "High", y = "Low") 
 
# Frequency distribution for the 'Close' column 
num_bins <- 10  # Change this to the desired number of bins 
class_freq_table <- table(cut(BTC_USD$Close, num_bins)) 
rel_freq_table <- prop.table(class_freq_table) 
cum_freq <- cumsum(class_freq_table) 
 
# Display frequency distribution 
print("Frequency Distribution for Close Column:") 
freq_dist <- data.frame( 
  Class = names(class_freq_table), 
  Frequency = class_freq_table, 
  CumulativeFrequency = cum_freq, 
  RelativeFrequency = rel_freq_table 
) 
print(freq_dist) 
 
# Calculate mean and standard deviation of 'Close' prices 
mean_close <- mean(BTC_USD$Close) 
sd_close <- sd(BTC_USD$Close) 
 
print(paste("Mean Close Price:", mean_close)) 
print(paste("Standard Deviation of Close Price:", sd_close)) 
 
# Create a histogram of 'Close' prices 
ggplot(data = BTC_USD, aes(x = Close)) + 
  geom_histogram(binwidth = 5000, fill = "orange", color = "black") + 
  labs(title = "Histogram of Close Prices", x = "Close Price", y = "Frequency") 
 
# Perform linear regression 
model <- lm(Close ~ Open, data = BTC_USD) 
 
# Summary of the regression model 
summary(model) 
 
# Plot the regression line 
ggplot(data = BTC_USD, aes(x = Open, y = Close)) + 
  geom_point(color = "blue") + 
  geom_smooth(method = "lm", color = "red") + 
  labs(title = "Linear Regression: Close vs Open", x = "Open Price", y = "Close Price") 
 
# Make predictions using the model by using covariance OR correlation 
new_data <- data.frame(Open = c(55000, 56000, 57000)) 
predictions <- predict(model, newdata = new_data) 
 
print("Predicted Close Prices:") 
print(predictions) 
 
# Confidence Interval for the Mean of 'Open' prices 
confidence_level <- 0.95 
mean_open <- mean(BTC_USD$Open) 
std_dev_open <- sd(BTC_USD$Open) 
sample_size <- length(BTC_USD$Open) 
margin_of_error <- qt((1 + confidence_level) / 2, df = sample_size - 1) * std_dev_open / sqrt(sample_size) 
confidence_interval <- c(mean_open - margin_of_error, mean_open + margin_of_error) 
cat("Confidence Interval for the Mean of 'Open' prices (95%):", confidence_interval, "\n") 
 
# Confidence Interval for Regression Coefficients 
conf_intervals <- confint(model, level = confidence_level) 
cat("Confidence Intervals for Regression Coefficients (95%):\n") 
print(conf_intervals) 
 
# Additional Distributions 
# Normal Distribution 
ggplot(data = data.frame(x = rnorm(1000)), aes(x = x)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Normal Distribution") 
 
# Chi-square Distribution 
ggplot(data = data.frame(x = rchisq(1000, df = 5)), aes(x = x)) + 
  geom_density(fill = "green", alpha = 0.5) + 
  labs(title = "Chi-square Distribution (df = 5)") 
 
# Uniform Distribution 
ggplot(data = data.frame(x = runif(1000)), aes(x = x)) + 
  geom_density(fill = "orange", alpha = 0.5) + 
  labs(title = "Uniform Distribution") 
 
# Binomial Distribution 
n_trials <- 100 
prob_success <- 0.5 
binomial_data <- rbinom(n_trials, size = 1, prob = prob_success) 
barplot(table(binomial_data), main = "Binomial Distribution", xlab = "Values", ylab = "Frequency") 
 
# Poisson Distribution 
lambda <- 2 
poisson_data <- rpois(100, lambda = lambda) 
barplot(table(poisson_data), main = "Poisson Distribution", xlab = "Values", ylab = "Frequency") 
 
# Pie Chart 
pie(table(factor(c("A", "B", "A", "C", "B"))), main = "Pie Chart")