library(shiny) 
library(readr) 
library(ggplot2) 
library(dplyr) 
library(lmtest) 
 
# Load your dataset 
BTC_USD <- read_csv("D:\PROB PROJECT\BTC-USD.xlsx") 
 
ui <- fluidPage( 
  titlePanel("Bitcoin Analysis"), 
   
  sidebarLayout( 
    sidebarPanel( 
      sliderInput("num_bins", "Number of Bins:", min = 5, max = 30, value = 10), 
      selectInput("visualization", "Choose Visualization:", 
                  choices = c("Bar Chart of Volume", "Scatter Plot", "Histogram", "Box Plot",  
                              "Linear Regression", "Binomial Distribution",  
                              "Poisson Distribution", "Pie Chart", "Normal Distribution",  
                              "Chi-Square Distribution", "Uniform Distribution"), 
                  selected = "Bar Chart of Volume") 
    ), 
     
    mainPanel( 
      verbatimTextOutput("output_text"), 
      plotOutput("selected_plot"), 
      verbatimTextOutput("regression_summary"), 
      verbatimTextOutput("predicted_prices"), 
      verbatimTextOutput("confidence_intervals"), 
      verbatimTextOutput("freq_dist_table") 
    ) 
  ) 
) 
 
server <- function(input, output) { 
   
  variable_type <- function(variable) { 
    if (is.numeric(variable)) { 
      return("Quantitative") 
    } else { 
      return("Qualitative") 
    } 
  } 
   
  output$output_text <- renderPrint({ 
    columns_to_check <- c("Open", "High", "Low", "Close", "Adj Close", "Volume") 
    cat("Variable Types:\n") 
    sapply(BTC_USD[, columns_to_check], variable_type) 
  }) 
   
  output$selected_plot <- renderPlot({ 
    if (input$visualization == "Bar Chart of Volume") { 
      ggplot(data = BTC_USD, aes(x = Date, y = Volume)) + 
        geom_bar(stat = "identity", fill = "blue") + 
        labs(title = "Bar Chart of Volume", x = "Date", y = "Volume") 
    } else if (input$visualization == "Scatter Plot") { 
      ggplot(data = BTC_USD, aes(x = High, y = Low)) + 
        geom_point(color = "green") + 
        labs(title = "Scatter Plot of High vs. Low", x = "High", y = "Low") 
    } else if (input$visualization == "Histogram") { 
      num_bins <- input$num_bins 
      ggplot(data = BTC_USD, aes(x = Close)) + 
        geom_histogram(binwidth = (max(BTC_USD$Close) - min(BTC_USD$Close)) / num_bins, 
                       fill = "orange", color = "black") + 
        labs(title = "Histogram of Close Prices", x = "Close Price", y = "Frequency") 
    } else if (input$visualization == "Box Plot") { 
      ggplot(data = BTC_USD, aes(x = 1, y = Close)) + 
        geom_boxplot(fill = "lightblue") + 
        labs(title = "Box Plot of Close Prices", x = "", y = "Close Price") 
    } else if (input$visualization == "Linear Regression") { 
      model <- lm(Close ~ Open, data = BTC_USD) 
      ggplot(data = BTC_USD, aes(x = Open, y = Close)) + 
        geom_point(color = "blue") + 
        geom_smooth(method = "lm", color = "red") + 
        labs(title = "Linear Regression: Close vs Open", x = "Open Price", y = "Close Price") 
    } else if (input$visualization == "Binomial Distribution") { 
      # Binomial Distribution 
      n_trials <- 100 
      prob_success <- 0.5 
      binomial_data <- rbinom(n_trials, size = 1, prob = prob_success) 
      barplot(table(binomial_data), main = "Binomial Distribution", xlab = "Values", ylab = "Frequency") 
    } else if (input$visualization == "Poisson Distribution") { 
      # Poisson Distribution 
      lambda <- 2 
      poisson_data <- rpois(100, lambda = lambda) 
      barplot(table(poisson_data), main = "Poisson Distribution", xlab = "Values", ylab = "Frequency") 
    } else if (input$visualization == "Pie Chart") { 
      # Pie Chart 
      pie(table(factor(c("A", "B", "A", "C", "B"))), main = "Pie Chart") 
    } else if (input$visualization == "Normal Distribution") { 
      # Normal Distribution 
      normal_data <- rnorm(100) 
      hist(normal_data, main = "Normal Distribution", xlab = "Values", ylab = "Frequency", col = "skyblue") 
    } else if (input$visualization == "Chi-Square Distribution") { 
      # Chi-Square Distribution 
      chi_square_data <- rchisq(100, df = 2) 
      hist(chi_square_data, main = "Chi-Square Distribution", xlab = "Values", ylab = "Frequency", col = "pink") 
    } else if (input$visualization == "Uniform Distribution") { 
      # Uniform Distribution 
      uniform_data <- runif(100) 
      hist(uniform_data, main = "Uniform Distribution", xlab = "Values", ylab = "Frequency", col = "lightgreen") 
    } 
  }) 
   
  output$regression_summary <- renderPrint({ 
    if (input$visualization == "Linear Regression") { 
      model <- lm(Close ~ Open, data = BTC_USD) 
      summary(model) 
    } else { 
      NULL 
    } 
  }) 
   
  output$predicted_prices <- renderPrint({ 
    if (input$visualization == "Linear Regression") { 
      model <- lm(Close ~ Open, data = BTC_USD) 
      new_data <- data.frame(Open = c(55000, 56000, 57000)) 
      predictions <- predict(model, newdata = new_data) 
      paste("Predicted Close Prices:\n", predictions) 
    } else { 
      NULL 
    } 
  }) 
   
  output$confidence_intervals <- renderPrint({ 
    if (input$visualization == "Linear Regression") { 
      confidence_level <- 0.95 
      mean_open <- mean(BTC_USD$Open) 
      std_dev_open <- sd(BTC_USD$Open) 
      sample_size <- length(BTC_USD$Open) 
      margin_of_error <- qt((1 + confidence_level) / 2, df = sample_size - 1) * std_dev_open / sqrt(sample_size) 
      confidence_interval <- c(mean_open - margin_of_error, mean_open + margin_of_error) 
      paste("Confidence Interval for the Mean of 'Open' prices (95%):\n", confidence_interval) 
    } else { 
      NULL 
    } 
  }) 
   
  output$freq_dist_table <- renderPrint({ 
    if (input$visualization %in% c("Histogram", "Binomial Distribution", "Poisson Distribution",  
                                   "Normal Distribution", "Chi-Square Distribution", "Uniform Distribution")) { 
      num_bins <- input$num_bins 
      class_freq_table <- table(cut(BTC_USD$Close, num_bins)) 
      rel_freq_table <- prop.table(class_freq_table) 
      cum_freq <- cumsum(class_freq_table) 
       
      freq_dist <- data.frame( 
        Class = names(class_freq_table), 
        Frequency = class_freq_table, 
        CumulativeFrequency = cum_freq, 
        RelativeFrequency = rel_freq_table 
      ) 
      freq_dist 
    } else { 
      NULL 
    } 
  }) 
} 
 
shinyApp(ui, server)