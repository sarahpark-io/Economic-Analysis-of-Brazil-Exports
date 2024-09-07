---
title: "Analyzing Brazil's Exports"
output: html_notebook
---

# Import Data and Libraries
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

data <- read.csv("/Users/sarahpark/Downloads/Brazil Exports/exportacao_full.csv")
print(head(data))

sapply(data, function(x) length(unique(x)))

# Feature Engineering
data <- data %>% select(-Month, -City, -`SH4 Code`, -`SH4 Description`)

unique_years <- unique(data$Year)
print(unique_years)

cpi_data <- c(
  `2010` = 218.056,
  `2011` = 224.939,
  `2012` = 229.594,
  `2013` = 233.504,
  `2014` = 236.736,
  `2015` = 237.017,
  `2016` = 240.007,
  `2017` = 245.120,
  `2018` = 251.107,
  `2019` = 255.657,
  `2020` = 258.811,
  `2021` = 271.696,
  `2022` = 292.655
)

base_cpi <- 292.655

print(base_cpi)

data$Adjusted_USD_FOB <- data$`US..FOB` * (base_cpi / sapply(data$Year, function(y) cpi_data[as.character(y)]))

print(head(data))

# Top 10 Exports
aggregated_data <- data %>%
  group_by(`SH2.Code`) %>%
  summarise(Total_Adjusted_USD_FOB = sum(`Adjusted_USD_FOB`))

aggregated_data$SH2.Code <- factor(aggregated_data$SH2.Code, levels = aggregated_data$SH2.Code[order(aggregated_data$Total_Adjusted_USD_FOB, decreasing = TRUE)])

top_5_data <- aggregated_data[order(aggregated_data$Total_Adjusted_USD_FOB, decreasing = TRUE), ]
top_5_data <- top_10_data[1:5, ]

top_5_data$SH2.Code <- factor(top_5_data$SH2.Code, levels = top_5_data$SH2.Code[order(top_5_data$Total_Adjusted_USD_FOB, decreasing = TRUE)])
top_5_data$Total_Adjusted_USD_FOB_Per_Year <- top_5_data$Total_Adjusted_USD_FOB / 10

ggplot(top_5_data, aes(x = SH2.Code, y = Total_Adjusted_USD_FOB_Per_Year)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = "B")) +
  labs(x = 'SH2 Code', 
       y = 'Total Adjusted US$ FOB (in Billion per year)', 
       title = 'Top 10 SH2 Codes by Total Adjusted US$ FOB') +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 0, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16, face = "bold"))

desired_codes <- c(26, 27, 12, 87, 84)

filtered_data <- data %>%
  filter(`SH2.Code` %in% desired_codes)

aggregated_data <- filtered_data %>%
  group_by(`SH2.Code`, `SH2.Description`) %>%
  summarise(Total_Adjusted_USD_FOB_Per_Year = sum(`Adjusted_USD_FOB`) / 10) %>%
  ungroup()

print(aggregated_data)

# Comparison of Exported Goods: 2010-2020 vs. 2022

data_for_plot <- data.frame(
  Category = rep(c("Iron Ore", "Crude Petroleum", "Soybean"), each = 2),
  Year = rep(c("2010-2020 Average", "2022"), 3),
  Total_Adjusted_USD_FOB_Per_Year = c(46.9, 30.1, 36.7, 43.1, 34.3, 47.2)
)

data_for_plot$Category <- factor(data_for_plot$Category, 
                                 levels = c("Iron Ore", "Crude Petroleum", "Soybean"))

ggplot(data_for_plot, aes(x = Category, y = Total_Adjusted_USD_FOB_Per_Year, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::label_number(scale = 1, suffix = "B")) +
  labs(x = 'Category', 
       y = 'Total Adjusted US$ FOB Per Year', 
       title = 'Total Adjusted US$ FOB Per Year for Different Categories') +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 0, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16, face = "bold"))

# U.S. / China Tariffs

unique_economic_blocks <- unique(data$Economic.Block)
print(unique_economic_blocks)

filtered_data <- data %>%
  filter(Economic.Block %in% c(
    'Asia (minus MIDDLE EAST)', 
    'Europe', 
    'European Union (EU)', 
    'South America', 
    'Southern Common Market (MERCOSUL)'
  ))

aggregated_data <- filtered_data %>%
  group_by(Year, Economic.Block) %>%
  summarise(Total_Adjusted_USD_FOB = sum(Adjusted_USD_FOB, na.rm = TRUE))

ggplot(aggregated_data, aes(x = Year, y = Total_Adjusted_USD_FOB, color = Economic.Block, group = Economic.Block)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = 'Year', 
       y = 'Total Adjusted US$ FOB', 
       title = 'Total Adjusted US$ FOB per Year by Economic Block') +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16, face = "bold"))


# Exports to China: Soybean

filtered_data <- data %>%
  filter(SH2.Code == 23, Economic.Block %in% c(
    'Asia (minus MIDDLE EAST)', 
    'Europe', 
    'European Union (EU)', 
    'South America', 
    'Southern Common Market (MERCOSUL)'
  ))

aggregated_data <- filtered_data %>%
  group_by(Year, Economic.Block) %>%
  summarise(Total_Adjusted_USD_FOB = sum(Adjusted_USD_FOB, na.rm = TRUE))

ggplot(aggregated_data, aes(x = Year, y = Total_Adjusted_USD_FOB, color = Economic.Block, group = Economic.Block)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = 'Year', 
       y = 'Total Adjusted US$ FOB', 
       title = 'Total Adjusted US$ FOB per Year by Economic Block for SH2.Code 23') +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16, face = "bold"))


# U.S. / Brazil Cotton Subsidies

filtered_data_sh2_52 <- data %>%
  filter(SH2.Code == 52)

aggregated_data_sh2_52 <- filtered_data_sh2_52 %>%
  group_by(Year) %>%
  summarise(Total_Adjusted_USD_FOB = sum(Adjusted_USD_FOB, na.rm = TRUE))

print(aggregated_data_sh2_52)

filtered_data_sh2_52 <- data %>%
  filter(SH2.Code == 52)

aggregated_data_sh2_52 <- filtered_data_sh2_52 %>%
  group_by(Year) %>%
  summarise(Total_Adjusted_USD_FOB = sum(Adjusted_USD_FOB, na.rm = TRUE))

ggplot(aggregated_data_sh2_52, aes(x = Year, y = Total_Adjusted_USD_FOB)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = "B")) +
  labs(x = 'Year', 
       y = 'Total Adjusted US$ FOB', 
       title = 'Yearly Totals of Adjusted US$ FOB for SH2 Code 52 with Linear Regression') +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16, face = "bold"))

# Brazil Crude Petroleum Subsidies

filtered_data_sh2_27 <- data %>%
  filter(SH2.Code == 27)

aggregated_data_sh2_27 <- filtered_data_sh2_27 %>%
  group_by(Year) %>%
  summarise(Total_Adjusted_USD_FOB = sum(Adjusted_USD_FOB, na.rm = TRUE))

print(aggregated_data_sh2_27)

filtered_data_sh2_27 <- data %>%
  filter(SH2.Code == 27)

aggregated_data_sh2_27 <- filtered_data_sh2_27 %>%
  group_by(Year) %>%
  summarise(Total_Adjusted_USD_FOB = sum(Adjusted_USD_FOB, na.rm = TRUE))

ggplot(aggregated_data_sh2_27, aes(x = Year, y = Total_Adjusted_USD_FOB)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = "B")) +
  labs(x = 'Year', 
       y = 'Total Adjusted US$ FOB (in Billions)', 
       title = 'Total Adjusted US$ FOB by Year for SH2 Code 27') +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16, face = "bold"))
