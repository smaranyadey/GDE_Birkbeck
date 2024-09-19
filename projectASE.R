setwd("Documents/Birkbeck GDE/project ASE/")

############ install packages ############
install.packages("tidyverse")
install.packages("corrplot")

####
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(corrplot)
library(broom)

############# Correlation #############
View(co2_india_t)
View(forest_area_india_t)
View(electricity_india_t)
View(freshwater_india_t)
View(renew_elec_india_t)
View(safe_sanitation_india_t)

merged_df <- inner_join(co2_india_t, forest_area_india_t, by = "Year")
merged_df <- inner_join(merged_df, electricity_india_t, by = "Year")
merged_df <- inner_join(merged_df, freshwater_india_t, by = "Year")
merged_df <- inner_join(merged_df, renew_elec_india_t, by = "Year")
merged_df <- inner_join(merged_df, safe_sanitation_india_t, by = "Year")

View(merged_df)

# Calculate correlation matrix
correlation_matrix <- cor(merged_df %>% select(-Year))

# Convert correlation matrix to tidy format
correlation_df <- as.data.frame(as.table(correlation_matrix))
colnames(correlation_df) <- c("Var1", "Var2", "Correlation")

# Plot
ggplot(correlation_df, aes(Var1, Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Correlation)), color = "black", size = 3) +  # Add correlation values
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 0, limits = c(-1, 1), 
                       breaks = seq(-1, 1, by = 0.2), 
                       labels = scales::number_format(accuracy = 0.1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Plot", x = NULL, y = NULL)


############# Regression #############
reg_df <- inner_join(co2_india_t, forest_area_india_t, by = "Year")
reg_df <- inner_join(reg_df, electricity_india_t, by = "Year")
# merged_df <- inner_join(merged_df, freshwater_india_t, by = "Year")
reg_df <- inner_join(reg_df, renew_elec_india_t, by = "Year")
# merged_df <- inner_join(merged_df, safe_sanitation_india_t, by = "Year")

View(reg_df)

# taking log
reg_df <- reg_df %>% 
  mutate(log_co2_emissions = log(co2_emissions_india),
         log_forest_areas = log(forest_area_india),
         log_electricity_access = log(electricity_access_india))
         #log_renew_elec = log(renewable_elec_india))

View(reg_df)

model <- lm(log_co2_emissions ~ log_forest_areas + log_electricity_access , 
            data = reg_df)
summary(model)

tidy_results <- tidy(model)

tidy_results %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Coefficients from Linear Regression", x = "Variable", y = "Coefficient")


############ reading data ############
#### CO2 emissions ####
co2_data <- read_csv(file = "data/co2 emission/API_EN.ATM.CO2E.PC_DS2_en_csv_v2_47017.csv", skip = 4)
View(co2_data)

#### Select India from Country
co2_india <- co2_data %>%
  filter(`Country Name` == "India")
View(co2_india)

co2_india_t <- co2_india %>%
  select(`1990`:`2020`) %>%
  t() %>%
  as_tibble()


colnames(co2_india_t) <- "co2_emissions_india"

co2_india_t$co2_emissions_india <- as.numeric(co2_india_t$co2_emissions_india)
co2_india_t <- co2_india_t %>%
  mutate(Year = c(1990:2020))
# 
# summary_stats_co2 <- co2_india_t %>%
#   summarise(
#     Mean = mean(co2_emissions_india),
#     Median = median(co2_emissions_india),
#     Min = min(co2_emissions_india),
#     Max = max(co2_emissions_india),
#     Q1 = quantile(co2_emissions_india, 0.25),
#     Q3 = quantile(co2_emissions_india, 0.75)
#   )
# View(summary_stats_co2)

#### Compare the summary statistics of India with that of South Asia's
co2_sa <- co2_data %>% 
  filter(`Country Name` %in% c("Afghanistan", "Bangladesh", "Bhutan", "India", "Sri Lanka", "Maldives", "Nepal", "Pakistan"))
View(co2_sa)

co2_sa_t <- co2_sa %>%
  select(`1990`:`2020`) %>%
  t() %>%
  as_tibble()

# Add a column for column names
co2_sa_t <- co2_sa_t %>%
  mutate(Year = c(1990:2020))


View(co2_sa_t)

colnames(co2_sa_t) <- c("AFG", "BGD", "BTN", "IND", "LKA", "MDV", "NPL", "PAK", "Year")

#### world average
co2_world <- co2_data %>%
  summarise(
    across(`1961`:(ncol(co2_data) - 1), ~ mean(., na.rm=TRUE))
  )
View(co2_world)

# Add a column for column names
co2_world_t <- co2_world %>%
  select(`1990`:`2020`) %>%
  t() %>%
  as_tibble()

co2_world_t <- co2_world_t %>%
  mutate(Year = c(1990:2020))

colnames(co2_world_t) <- c("Global_Avg", "Year")

co2_compare <- inner_join(co2_sa_t, co2_world_t, by = "Year")
View(co2_compare)

#### Line chart for IND co2 emissions
df <- tibble(
  Year = co2_compare$Year,
  Emissions_IND = co2_compare$IND
)
# Create line chart
ggplot(df, aes(x = Year, y = Emissions_IND)) +
  geom_line() +
  geom_point() +
  labs(title = "CO2 Emissions of India Over Time",
       x = "Year",
       y = "CO2 Emissions (in metric tons per capita)") +
  theme_minimal() +
  ylim(0, max(df$Emissions_IND) + 0.5)  # Adjust the maximum value slightly for better visualization


# Extract column names
columns <- colnames(co2_compare)[-which(colnames(co2_compare) == "Year")]

# Calculate summary statistics for each column
summary_stats <- lapply(columns, function(col) {
  summarise(co2_compare, 
            Mean = mean(.data[[col]]),
            Median = median(.data[[col]]),
            Min = min(.data[[col]]),
            Max = max(.data[[col]]),
            Q1 = quantile(.data[[col]], 0.25),
            Q3 = quantile(.data[[col]], 0.75))
})

# Combine the list of summary statistics into a data frame
summary_co2_df <- do.call(rbind, summary_stats)

# Add column names as row names
rownames(summary_co2_df) <- columns

# Convert the resulting data frame to a tibble
summary_co2_df <- as_tibble(summary_co2_df, rownames = "Country")

View(summary_co2_df)

write.csv(summary_co2_df, "Summary_CO2_Emissions_SA_Global.csv", row.names = FALSE)


#### multiple line chart
# Convert data to long format
data_long <- gather(co2_compare, key = "Country", value = "CO2_Emissions", -Year)

# Create line chart
ggplot(data_long, aes(x = Year, y = CO2_Emissions, color = Country)) +
  geom_line() +
  labs(title = "CO2 Emissions Over Time",
       x = "Year",
       y = "CO2 Emissions (in metric tons per capita)",
       color = "Country") +
  scale_color_brewer(palette = "Set1") + # Use Dark2 color palette
  theme_minimal() +
  theme(legend.position = "right") # Move legend to the right side




#### forest area ####
forest_area <- read_csv(
  file = "data/forest area/API_AG.LND.FRST.ZS_DS2_en_csv_v2_190.csv", 
  skip = 4)
View(forest_area)

#### Select India from Country
forest_area_india <- forest_area %>%
  filter(`Country Name` == "India")
View(forest_area_india)

forest_area_india_t <- forest_area_india %>%
  select(`1990`:`2021`) %>%
  t() %>%
  as_tibble()


colnames(forest_area_india_t) <- "forest_area_india"

forest_area_india_t$forest_area_india <- as.numeric(forest_area_india_t$forest_area_india)
forest_area_india_t <- forest_area_india_t %>%
  mutate(Year = c(1990:2021))


#### Compare the summary statistics of India with that of South Asia's
forest_area_sa <- forest_area %>% 
  filter(`Country Name` %in% c("Afghanistan", "Bangladesh", "Bhutan", "India", "Sri Lanka", "Maldives", "Nepal", "Pakistan"))
View(forest_area_sa)

forest_area_sa_t <- forest_area_sa %>%
  select(`1990`:`2021`) %>%
  t() %>%
  as_tibble()

# Add a column for column names
forest_area_sa_t <- forest_area_sa_t %>%
  mutate(Year = c(1990:2021))


View(forest_area_sa_t)

colnames(forest_area_sa_t) <- c("AFG", "BGD", "BTN", "IND", "LKA", "MDV", "NPL", "PAK", "Year")

#### world average
forest_area_world <- forest_area %>%
  summarise(
    across(`1961`:(ncol(forest_area) - 1), ~ mean(., na.rm=TRUE))
  )
View(forest_area_world)

# Add a column for column names
forest_area_world_t <- forest_area_world %>%
  select(`1990`:`2021`) %>%
  t() %>%
  as_tibble()

forest_area_world_t <- forest_area_world_t %>%
  mutate(Year = c(1990:2021))

colnames(forest_area_world_t) <- c("Global_Avg", "Year")

forest_area_compare <- inner_join(forest_area_sa_t, forest_area_world_t, by = "Year")
View(forest_area_compare)

#### Line chart for IND forest areas
df <- tibble(
  Year = forest_area_compare$Year,
  forest_area_IND = forest_area_compare$IND
)
# Create line chart
ggplot(df, aes(x = Year, y = forest_area_IND)) +
  geom_line() +
  geom_point() +
  labs(title = "Forest Areas in India Over Time",
       x = "Year",
       y = "Forest Areas (% of land area)") +
  theme_minimal() +
  ylim(0, max(df$forest_area_IND) + 0.5)  # Adjust the maximum value slightly for better visualization


# Extract column names
columns <- colnames(forest_area_compare)[-which(colnames(forest_area_compare) == "Year")]

# Calculate summary statistics for each column
summary_stats <- lapply(columns, function(col) {
  summarise(forest_area_compare, 
            Mean = mean(.data[[col]]),
            Median = median(.data[[col]]),
            Min = min(.data[[col]]),
            Max = max(.data[[col]]),
            Q1 = quantile(.data[[col]], 0.25),
            Q3 = quantile(.data[[col]], 0.75))
})

# Combine the list of summary statistics into a data frame
summary_forest_area_df <- do.call(rbind, summary_stats)

# Add column names as row names
rownames(summary_forest_area_df) <- columns

# Convert the resulting data frame to a tibble
summary_forest_area_df <- as_tibble(summary_forest_area_df, rownames = "Country")

View(summary_forest_area_df)

write.csv(summary_forest_area_df, "Summary_Forest_Areas_SA_Global.csv", row.names = FALSE)


#### multiple line chart
# Convert data to long format
data_long <- gather(forest_area_compare, key = "Country", value = "Forest_Areas", -Year)

# Create line chart
ggplot(data_long, aes(x = Year, y = Forest_Areas, color = Country)) +
  geom_line() +
  labs(title = "Forest Areas Over Time",
       x = "Year",
       y = "Forest Area (% of land)",
       color = "Country") +
  scale_color_brewer(palette = "Set1") + # Use Dark2 color palette
  theme_minimal() +
  theme(legend.position = "right") # Move legend to the right side



#### access to electricity ####
electricity <- read_csv(
  file = "data/access_to_electricity/API_EG.ELC.ACCS.ZS_DS2_en_csv_v2_42596.csv", 
  skip = 4)
View(electricity)

#### Select India from Country 
electricity_india <- electricity %>% 
  filter(`Country Name` == "India")
View(electricity_india)

electricity_india_t <- electricity_india %>% 
  select(`1993`:`2021`) %>% 
  t() %>% 
  as_tibble()

colnames(electricity_india_t) <- "electricity_access_india"

electricity_india_t$electricity_access_india <- as.numeric(electricity_india_t$electricity_access_india)
electricity_india_t <- electricity_india_t %>%
  mutate(Year = c(1993:2021))


summary_stats_elec <- electricity_india_t %>%
  summarise(
    Mean = mean(electricity_access_india),
    Median = median(electricity_access_india),
    Min = min(electricity_access_india),
    Max = max(electricity_access_india),
    Q1 = quantile(electricity_access_india, 0.25, na.rm = TRUE),
    Q3 = quantile(electricity_access_india, 0.75, na.rm = TRUE)
  )
View(summary_stats_elec)

#### Compare the summary statistics of India with that of South Asia's
electricity_sa <- electricity %>% 
  filter(`Country Name` %in% c("Afghanistan", "Bangladesh", "Bhutan", "India", "Sri Lanka", "Maldives", "Nepal", "Pakistan"))
View(electricity_sa)

electricity_sa_t <- electricity_sa %>%
  select(`2000`:`2021`) %>%
  t() %>%
  as_tibble()

# Add a column for column names
electricity_sa_t <- electricity_sa_t %>%
  mutate(Year = c(2000:2021))


View(electricity_sa_t)

colnames(electricity_sa_t) <- c("AFG", "BGD", "BTN", "IND", "LKA", "MDV", "NPL", "PAK", "Year")

#### world average
electricity_world <- electricity %>%
  summarise(
    across(`1960`:(ncol(electricity) - 1), ~ mean(., na.rm=TRUE))
  )
View(electricity_world)

# Add a column for column names
electricity_world_t <- electricity_world %>%
  select(`1990`:`2021`) %>%
  t() %>%
  as_tibble()

electricity_world_t <- electricity_world_t %>%
  mutate(Year = c(1990:2021))

colnames(electricity_world_t) <- c("Global_Avg", "Year")

# world average: summary statistics
summary_stats_elec_world <- electricity_world_t %>%
  summarise(
    Mean = mean(Global_Avg),
    Median = median(Global_Avg),
    Min = min(Global_Avg),
    Max = max(Global_Avg),
    Q1 = quantile(Global_Avg, 0.25, na.rm = TRUE),
    Q3 = quantile(Global_Avg, 0.75, na.rm = TRUE)
  )

View(summary_stats_elec_world)

elec_compare <- inner_join(electricity_sa_t, electricity_world_t, by = "Year")
View(elec_compare)

#### Line chart for IND forest areas
df <- tibble(
  Year = elec_compare$Year,
  elec_IND = elec_compare$IND
)
# Create line chart
ggplot(electricity_india_t, aes(x = Year, y = electricity_access_india)) +
  geom_line() +
  geom_point() +
  labs(title = "Access to Electricity in India Over Time",
       x = "Year",
       y = "Access to Electricity (% of population)") +
  theme_minimal() +
  ylim(0, max(df$elec_IND) + 0.5) + # Adjust the maximum value slightly for better visualization
  scale_x_continuous(limits = c(1993, 2021)) 



# Extract column names
columns <- colnames(elec_compare)[-which(colnames(elec_compare) == "Year")]

# Calculate summary statistics for each column
summary_stats <- lapply(columns, function(col) {
  summarise(elec_compare, 
            Mean = mean(.data[[col]]),
            Median = median(.data[[col]]),
            Min = min(.data[[col]]),
            Max = max(.data[[col]]),
            Q1 = quantile(.data[[col]], 0.25),
            Q3 = quantile(.data[[col]], 0.75))
})

# Combine the list of summary statistics into a data frame
summary_elec_df <- do.call(rbind, summary_stats)

# Add column names as row names
rownames(summary_elec_df) <- columns

# Convert the resulting data frame to a tibble
summary_elec_df <- as_tibble(summary_elec_df, rownames = "Country")

View(summary_elec_df)

write.csv(summary_elec_df, "Summary_Access_to_electricity_SA_Global.csv", row.names = FALSE)


#### multiple line chart
# Convert data to long format
data_long <- gather(elec_compare, key = "Country", value = "Access_to_Electricity", -Year)

# Create line chart
ggplot(data_long, aes(x = Year, y = Access_to_Electricity, color = Country)) +
  geom_line() +
  labs(title = "Access to Electricity Over Time",
       x = "Year",
       y = "Access to Electricity (% of population)",
       color = "Country") +
  scale_color_brewer(palette = "Set1") + # Use Dark2 color palette
  theme_minimal() +
  theme(legend.position = "right") # Move legend to the right side



#### freshwater withdrawal ####
freshwater <- read_csv(
  file = "data/freshwater_withdrawal/API_ER.H2O.FWTL.ZS_DS2_en_csv_v2_59459.csv", 
  skip = 4)
View(freshwater)

#### Select India from Country 
freshwater_india <- freshwater %>% 
  filter(`Country Name` == "India")
View(freshwater_india)

freshwater_india_t <- freshwater_india %>% 
  select(`1975`:`2020`) %>% 
  t() %>% 
  as_tibble()

colnames(freshwater_india_t) <- "freshwater_india"

freshwater_india_t$freshwater_india <- as.numeric(freshwater_india_t$freshwater_india)
freshwater_india_t <- freshwater_india_t %>%
  mutate(Year = c(1975:2020))


summary_stats_freshwater <- freshwater_india_t %>%
  summarise(
    Mean = mean(freshwater_india),
    Median = median(freshwater_india),
    Min = min(freshwater_india),
    Max = max(freshwater_india),
    Q1 = quantile(freshwater_india, 0.25, na.rm = TRUE),
    Q3 = quantile(freshwater_india, 0.75, na.rm = TRUE)
  )
View(summary_stats_freshwater)

#### Compare the summary statistics of India with that of South Asia's
freshwater_sa <- freshwater %>% 
  filter(`Country Name` %in% c("Afghanistan", "Bangladesh", "Bhutan", "India", "Sri Lanka", "Maldives", "Nepal", "Pakistan"))
View(freshwater_sa)

freshwater_sa_t <- freshwater_sa %>%
  select(`2008`:`2020`) %>%
  t() %>%
  as_tibble()

# Add a column for column names
freshwater_sa_t <- freshwater_sa_t %>%
  mutate(Year = c(2008:2020))


View(freshwater_sa_t)

colnames(freshwater_sa_t) <- c("AFG", "BGD", "BTN", "IND", "LKA", "MDV", "NPL", "PAK", "Year")

#### world average
freshwater_world <- freshwater %>%
  summarise(
    across(`1960`:(ncol(freshwater) - 1), ~ mean(., na.rm=TRUE))
  )
View(freshwater_world)

# Add a column for column names
freshwater_world_t <- freshwater_world %>%
  select(`1962`:`2020`) %>%
  t() %>%
  as_tibble()

freshwater_world_t <- freshwater_world_t %>%
  mutate(Year = c(1962:2020))

colnames(freshwater_world_t) <- c("Global_Avg", "Year")

# world average: summary statistics
summary_stats_freshwater_world <- freshwater_world_t %>%
  summarise(
    Mean = mean(Global_Avg),
    Median = median(Global_Avg),
    Min = min(Global_Avg),
    Max = max(Global_Avg),
    Q1 = quantile(Global_Avg, 0.25, na.rm = TRUE),
    Q3 = quantile(Global_Avg, 0.75, na.rm = TRUE)
  )

View(summary_stats_freshwater_world)

freshwater_compare <- inner_join(freshwater_sa_t, freshwater_world_t, by = "Year")
View(freshwater_compare)

#### Line chart for IND forest areas
df <- tibble(
  Year = freshwater_compare$Year,
  freshwater_IND = freshwater_compare$IND
)
# Create line chart
ggplot(freshwater_india_t, aes(x = Year, y = freshwater_india)) +
  geom_line() +
  geom_point() +
  labs(title = "Annual Freshwater Withdrawals in India Over Time",
       x = "Year",
       y = "Annual Freshwater Withdrawals (% of internal resources)") +
  theme_minimal() +
  ylim(0, max(freshwater_india_t$freshwater_india) + 0.5) # Adjust the maximum value slightly for better visualization




# Extract column names
columns <- colnames(freshwater_compare)[-which(colnames(freshwater_compare) == "Year")]

# Calculate summary statistics for each column
summary_stats <- lapply(columns, function(col) {
  summarise(freshwater_compare, 
            Mean = mean(.data[[col]]),
            Median = median(.data[[col]]),
            Min = min(.data[[col]]),
            Max = max(.data[[col]]),
            Q1 = quantile(.data[[col]], 0.25),
            Q3 = quantile(.data[[col]], 0.75))
})

# Combine the list of summary statistics into a data frame
summary_freshwater_df <- do.call(rbind, summary_stats)

# Add column names as row names
rownames(summary_freshwater_df) <- columns

# Convert the resulting data frame to a tibble
summary_freshwater_df <- as_tibble(summary_freshwater_df, rownames = "Country")

View(summary_freshwater_df)

write.csv(summary_freshwater_df, "Summary_Freshwater_Withdrawal_SA_Global.csv", row.names = FALSE)


#### multiple line chart
# Convert data to long format
data_long <- gather(freshwater_compare, key = "Country", value = "Freshwater_Withdraw", -Year)

# Create line chart
ggplot(data_long, aes(x = as.integer(Year), y = Freshwater_Withdraw, color = Country)) +
  geom_line() +
  labs(title = "Annual Freshwater Withdrawals Over Time",
       x = "Year",
       y = "Annual Freshwater Withdrawals (% of internal resources)",
       color = "Country") +
  scale_color_brewer(palette = "Set1") + # Use Dark2 color palette
  scale_x_continuous(breaks = seq(min(data_long$Year), max(data_long$Year), by = 5)) + # Specify x-axis ticks every 5 years
  theme_minimal() +
  theme(legend.position = "right") # Move legend to the right side




#### renewable electricity ####
renew_elec <- read_csv(
  file = "data/electricity_from_renewable/API_EG.ELC.RNWX.ZS_DS2_en_csv_v2_57721.csv", 
  skip = 4)
View(renew_elec)

#### Select India from Country 
renew_elec_india <- renew_elec %>% 
  filter(`Country Name` == "India")
View(renew_elec_india)

renew_elec_india_t <- renew_elec_india %>% 
  select(`1986`:`2015`) %>% 
  t() %>% 
  as_tibble()

colnames(renew_elec_india_t) <- "renewable_elec_india"

renew_elec_india_t$renewable_elec_india <- as.numeric(renew_elec_india_t$renewable_elec_india)
renew_elec_india_t <- renew_elec_india_t %>%
  mutate(Year = c(1986:2015))

summary_stats_renew_elec <- renew_elec_india_t %>%
  summarise(
    Mean = mean(renewable_elec_india),
    Median = median(renewable_elec_india),
    Min = min(renewable_elec_india),
    Max = max(renewable_elec_india),
    Q1 = quantile(renewable_elec_india, 0.25, na.rm = TRUE),
    Q3 = quantile(renewable_elec_india, 0.75, na.rm = TRUE)
  )
View(summary_stats_renew_elec)

#### Compare the summary statistics of India with that of South Asia's
renew_elec_sa <- renew_elec %>% 
  filter(`Country Name` %in% c("Afghanistan", "Bangladesh", "Bhutan", "India", "Sri Lanka", "Maldives", "Nepal", "Pakistan"))
View(renew_elec_sa)

renew_elec_sa_t <- renew_elec_sa %>%
  select(`2008`:`2020`) %>%
  t() %>%
  as_tibble()

# Add a column for column names
freshwater_sa_t <- freshwater_sa_t %>%
  mutate(Year = c(2008:2020))


View(freshwater_sa_t)

colnames(freshwater_sa_t) <- c("AFG", "BGD", "BTN", "IND", "LKA", "MDV", "NPL", "PAK", "Year")

#### world average
freshwater_world <- freshwater %>%
  summarise(
    across(`1960`:(ncol(freshwater) - 1), ~ mean(., na.rm=TRUE))
  )
View(freshwater_world)

# Add a column for column names
freshwater_world_t <- freshwater_world %>%
  select(`1962`:`2020`) %>%
  t() %>%
  as_tibble()

freshwater_world_t <- freshwater_world_t %>%
  mutate(Year = c(1962:2020))

colnames(freshwater_world_t) <- c("Global_Avg", "Year")

# world average: summary statistics
summary_stats_freshwater_world <- freshwater_world_t %>%
  summarise(
    Mean = mean(Global_Avg),
    Median = median(Global_Avg),
    Min = min(Global_Avg),
    Max = max(Global_Avg),
    Q1 = quantile(Global_Avg, 0.25, na.rm = TRUE),
    Q3 = quantile(Global_Avg, 0.75, na.rm = TRUE)
  )

View(summary_stats_freshwater_world)

freshwater_compare <- inner_join(freshwater_sa_t, freshwater_world_t, by = "Year")
View(freshwater_compare)

#### Line chart for IND forest areas
df <- tibble(
  Year = freshwater_compare$Year,
  freshwater_IND = freshwater_compare$IND
)
# Create line chart
ggplot(renew_elec_india_t, aes(x = Year, y = renewable_elec_india)) +
  geom_line() +
  geom_point() +
  labs(title = "Renewable Electricity Production in India Over Time",
       x = "Year",
       y = "Renewable Electricity Production (% of total)") +
  theme_minimal() +
  ylim(0, max(renew_elec_india_t$renewable_elec_india) + 0.5) # Adjust the maximum value slightly for better visualization




# Extract column names
columns <- colnames(freshwater_compare)[-which(colnames(freshwater_compare) == "Year")]

# Calculate summary statistics for each column
summary_stats <- lapply(columns, function(col) {
  summarise(freshwater_compare, 
            Mean = mean(.data[[col]]),
            Median = median(.data[[col]]),
            Min = min(.data[[col]]),
            Max = max(.data[[col]]),
            Q1 = quantile(.data[[col]], 0.25),
            Q3 = quantile(.data[[col]], 0.75))
})

# Combine the list of summary statistics into a data frame
summary_freshwater_df <- do.call(rbind, summary_stats)

# Add column names as row names
rownames(summary_freshwater_df) <- columns

# Convert the resulting data frame to a tibble
summary_freshwater_df <- as_tibble(summary_freshwater_df, rownames = "Country")

View(summary_freshwater_df)

write.csv(summary_freshwater_df, "Summary_Freshwater_Withdrawal_SA_Global.csv", row.names = FALSE)


#### multiple line chart
# Convert data to long format
data_long <- gather(freshwater_compare, key = "Country", value = "Freshwater_Withdraw", -Year)

# Create line chart
ggplot(data_long, aes(x = as.integer(Year), y = Freshwater_Withdraw, color = Country)) +
  geom_line() +
  labs(title = "Annual Freshwater Withdrawals Over Time",
       x = "Year",
       y = "Annual Freshwater Withdrawals (% of internal resources)",
       color = "Country") +
  scale_color_brewer(palette = "Set1") + # Use Dark2 color palette
  scale_x_continuous(breaks = seq(min(data_long$Year), max(data_long$Year), by = 5)) + # Specify x-axis ticks every 5 years
  theme_minimal() +
  theme(legend.position = "right") # Move legend to the right side





#### safe sanitation ####
safe_sanitation <- read_csv(
  file = "data/safe_sanitation/API_SH.STA.SMSS.ZS_DS2_en_csv_v2_61.csv", 
  skip = 4)
View(safe_sanitation)

#### Select India from Country 
safe_sanitation_india <- safe_sanitation %>% 
  filter(`Country Name` == "India")
View(safe_sanitation_india)

safe_sanitation_india_t <- safe_sanitation_india %>% 
  select(`2000`:`2022`) %>% 
  t() %>% 
  as_tibble()

colnames(safe_sanitation_india_t) <- "safe_sanitation_india"

safe_sanitation_india_t$safe_sanitation_india <- as.numeric(safe_sanitation_india_t$safe_sanitation_india)
safe_sanitation_india_t <- safe_sanitation_india_t %>%
  mutate(Year = c(2000:2022))


summary_stats_sanitation <- safe_sanitation_india_t %>%
  summarise(
    Mean = mean(safe_sanitation_india),
    Median = median(safe_sanitation_india),
    Min = min(safe_sanitation_india),
    Max = max(safe_sanitation_india),
    Q1 = quantile(safe_sanitation_india, 0.25, na.rm = TRUE),
    Q3 = quantile(safe_sanitation_india, 0.75, na.rm = TRUE)
  )
View(summary_stats_sanitation)


#### Compare the summary statistics of India with that of South Asia's
safe_sanitation_sa <- safe_sanitation %>% 
  filter(`Country Name` %in% c("Bangladesh", "Bhutan", "India", "Nepal"))
View(safe_sanitation_sa)

safe_sanitation_sa_t <- safe_sanitation_sa %>%
  select(`2000`:`2022`) %>%
  t() %>%
  as_tibble()

# Add a column for column names
safe_sanitation_sa_t <- safe_sanitation_sa_t %>%
  mutate(Year = c(2000:2022))


View(safe_sanitation_sa_t)

colnames(safe_sanitation_sa_t) <- c("BGD", "BTN", "IND", "NPL", "Year")

#### world average
safe_sanitation_world <- safe_sanitation %>%
  summarise(
    across(`1960`:(ncol(safe_sanitation) - 1), ~ mean(., na.rm=TRUE))
  )
View(safe_sanitation_world)

# Add a column for column names
safe_sanitation_world_t <- safe_sanitation_world %>%
  select(`2000`:`2022`) %>%
  t() %>%
  as_tibble()

safe_sanitation_world_t <- safe_sanitation_world_t %>%
  mutate(Year = c(2000:2022))

colnames(safe_sanitation_world_t) <- c("Global_Avg", "Year")

# world average: summary statistics
summary_stats_sanitation_world <- safe_sanitation_world_t %>%
  summarise(
    Mean = mean(Global_Avg),
    Median = median(Global_Avg),
    Min = min(Global_Avg),
    Max = max(Global_Avg),
    Q1 = quantile(Global_Avg, 0.25, na.rm = TRUE),
    Q3 = quantile(Global_Avg, 0.75, na.rm = TRUE)
  )

View(summary_stats_sanitation_world)

sanitation_compare <- inner_join(safe_sanitation_sa_t, safe_sanitation_world_t, 
                                 by = "Year")
View(sanitation_compare)

#### Line chart for IND forest areas
# df <- tibble(
#   Year = freshwater_compare$Year,
#   freshwater_IND = freshwater_compare$IND
# )
# Create line chart
ggplot(safe_sanitation_india_t, aes(x = Year, y = safe_sanitation_india)) +
  geom_line() +
  geom_point() +
  labs(title = "Safe Sanitation in India Over Time",
       x = "Year",
       y = "Safe Sanitation (% of population)") +
  theme_minimal() 
  #ylim(0, max(safe_sanitation_india_t$safe_sanitation_india) + 0.5) # Adjust the maximum value slightly for better visualization




# Extract column names
columns <- colnames(sanitation_compare)[-which(colnames(sanitation_compare) == "Year")]

# Calculate summary statistics for each column
summary_stats <- lapply(columns, function(col) {
  summarise(sanitation_compare, 
            Mean = mean(.data[[col]]),
            Median = median(.data[[col]]),
            Min = min(.data[[col]]),
            Max = max(.data[[col]]),
            Q1 = quantile(.data[[col]], 0.25),
            Q3 = quantile(.data[[col]], 0.75))
})

# Combine the list of summary statistics into a data frame
summary_sanitation_df <- do.call(rbind, summary_stats)

# Add column names as row names
rownames(summary_sanitation_df) <- columns

# Convert the resulting data frame to a tibble
summary_sanitation_df <- as_tibble(summary_sanitation_df, rownames = "Country")

View(summary_sanitation_df)

write.csv(summary_sanitation_df, "Summary_Sanitation_SA_Global.csv", row.names = FALSE)


#### multiple line chart
# Convert data to long format
data_long <- gather(sanitation_compare, key = "Country", value = "Sanitation", -Year)

# Create line chart
ggplot(data_long, aes(x = as.integer(Year), y = Sanitation, color = Country)) +
  geom_line() +
  labs(title = "Safe Sanitation in India Over Time",
       x = "Year",
       y = "Safe Sanitation (% of population)",
       color = "Country") +
  scale_color_brewer(palette = "Set1") + # Use Dark2 color palette
  scale_x_continuous(breaks = seq(min(data_long$Year), max(data_long$Year), by = 5)) + # Specify x-axis ticks every 5 years
  theme_minimal() +
  theme(legend.position = "right") # Move legend to the right side







