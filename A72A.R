colnames(Case)

# Subset data for Seoul and Gyeonggi-do
filtered_data <- Case[Case$province %in% c("Seoul", "Gyeonggi-do"), ]

# Box plot for confirmed cases by province
ggplot(filtered_data, aes(x = province, y = confirmed, fill = province)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = 0.7) +
  labs(title = "Box Plot of Confirmed Cases: Seoul vs. Gyeonggi-do",
       x = "Province",
       y = "Number of Confirmed Cases") +
  theme_minimal() +
  scale_fill_manual(values = c("Seoul" = "blue", "Gyeonggi-do" = "orange"))

hist(filtered_data$confirmed,
     main = "Confirmed Cases",
     col = "azure")

# Define the x-axis values for the curve
x <- seq(min(filtered_data$confirmed),
         max(filtered_data$confirmed),
         length.out = 1000)

# Calculate the mean and standard deviation of the Worldwide Box Office
mn <- mean(filtered_data$confirmed)
stdDev <- sd(filtered_data$confirmed)

# Generate the normal distribution curve
yn <- dnorm(x, mean = mn, sd = stdDev)

# Scale the curve to match the histogram's frequency
bin.size <- diff(hist(filtered_data$confirmed, plot = FALSE)$breaks[1:2])
yn <- yn * bin.size * length(filtered_data$confirmed)
    

# Overlay the normal curve on the histogram
lines(x, yn, col = "blue", lwd = 2)