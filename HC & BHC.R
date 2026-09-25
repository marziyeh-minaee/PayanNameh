############### Libraries ###############

# Data manipulation
library(tidyverse)
library(data.table)

# Visualization
library(ggplot2)
library(scales)

# Missing values
library(naniar)

# Correlation
library(corrplot)

# Outliers
library(outliers)

# Date features
library(lubridate)

# Skewness
library(moments)

############### Preprocessing ###############

#Loading data
train <- fread("C:\\Users\\Mari\\Desktop\\train.csv")
store <- fread("C:\\Users\\Mari\\OneDrive\\store.csv")
test  <- fread("C:\\Users\\Mari\\OneDrive\\test.csv")

# Dimension & Structure of variables
# dim & str
glimpse(train) 
glimpse(store) 
glimpse(test) 

# Statistical summary
summary(train)
summary(store)
summary(test)

# Store information joining
Data <- merge(train, store, by = "Store")

# Check for duplicate data
sum(duplicated(Data))
# Data <- distinct(Data) if there was a duplicate record

#_____ Missing Values _____#

colSums(is.na(Data))
round(colMeans(is.na(Data))*100,2)

# CompetitionDistance
Data %>%
  filter(is.na(CompetitionDistance)) %>%
  select(
    CompetitionOpenSinceMonth,
    CompetitionOpenSinceYear
  ) %>%
  summary() # NA = No Competitor Exists

# Promo variables
table(Data$Promo2)

Data %>%
  filter(Promo2 == 0) %>%
  select(
    Promo2SinceWeek,
    Promo2SinceYear,
    PromoInterval
  ) %>%
  summary() # NA = No Promo2

unique(Data$PromoInterval)
sum(Data$PromoInterval == "") # "" = No Promo2

# NAs => Promo2SinceYear, Promo2SinceWeek, PromoInterval(" ")
# NAs => CompetitionOpenSinceMonth, CompetitionOpenSinceYear
# NAs => CompetitionDistance

#_____ Data conversions & Time feature extraction _____#

# Date
Data$Date <- as.Date(Data$Date)

Data <- Data %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Day = day(Date),
    Week = week(Date),
    Quarter = quarter(Date),
    IsWeekend = ifelse(
      DayOfWeek %in% c(6, 7),
      1,0
    )
  )

# CompetitionDistance
Data <- Data %>%
  mutate(
    # Has or not has any competitor
    HasCompetition = ifelse(
      is.na(CompetitionDistance),
      0,
      1
    ),
    
    # If has, how much far?
    # Median imputation was used because the distribution was highly skewed
    CompetitionDistance = ifelse(
      is.na(CompetitionDistance),
      median(CompetitionDistance, na.rm = TRUE),
      CompetitionDistance
    )
  )

# CompetitionOpenSinceMonth/Year & Promo2SinceWeek/Year
Data <- Data %>%
  mutate(
    # Competition Duration in months
    CompetitionOpen_Months = (Year - CompetitionOpenSinceYear) * 12 + (Month - CompetitionOpenSinceMonth),
    
    # NA & - => 0
    CompetitionOpen_Months = ifelse(is.na(CompetitionOpen_Months), 0, CompetitionOpen_Months),
    CompetitionOpen_Months = ifelse(CompetitionOpen_Months < 0, 0, CompetitionOpen_Months),
    
    # Promo2 Duration in weeks
    Promo2_Weeks = (Year - Promo2SinceYear) * 52 + (Week - Promo2SinceWeek),
    
    # NA & - => 0
    Promo2_Weeks = ifelse(is.na(Promo2_Weeks), 0, Promo2_Weeks),
    Promo2_Weeks = ifelse(Promo2_Weeks < 0, 0, Promo2_Weeks)
  )

# PromoInterval 
library(stringr)
Data <- Data %>%
  mutate(
    # Convert the current month number to its English abbreviation(2 = "Feb")
    # The month.abb function exists in R and it's a list of calendar months
    CurrentMonthName = month.abb[Month],
    
    # Check whether the current month is in the PromoInterval or not?
    IsPromo2ActiveMonth = ifelse(
      # The str_detect function returns a logical vector with TRUE for each element of string that matches pattern and FALSE otherwise.
      !is.na(PromoInterval) & PromoInterval != "" & str_detect(as.character(PromoInterval), CurrentMonthName),
      1, # If it was, it means the promotion is active this month.
      0  # If not (or if the store had no PromoInterval at all) it will be zero
    )
  ) %>% 
  select(
    # Delete primary variables
    #-Date, it's early
    -CompetitionOpenSinceMonth, 
    -CompetitionOpenSinceYear, 
    -Promo2SinceWeek, 
    -Promo2SinceYear,
    -CurrentMonthName,
    -PromoInterval
    )

############### EDA ###############

# Check something
colSums(is.na(Data))
str(Data)
table(Data$StateHoliday)
names(Data)
summary(Data)

Data %>%
  filter(HasCompetition == 0) %>%
  select(
    CompetitionOpen_Months,
    CompetitionDistance
  ) %>%
  summary()


# Demand distribution analysis
sales_skew <- Data %>%
  filter(Sales > 0) %>%
  pull(Sales) %>%
  skewness()

Data %>%
  filter(Sales > 0) %>%
  ggplot(aes(x = Sales)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_density(color = "darkblue", size = 1.2) +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  labs(title = "Distribution of Daily Sales (Demand)", 
       x = "Sales", 
       y = "Density",
       caption = paste("Skewness =", round(sales_skew, 3)))

# Log-transformed Demand distribution analysis
log_sales_skew <- Data %>%
  filter(Sales > 0) %>%
  pull(Sales) %>%
  log() %>%
  skewness()

Data %>%
  filter(Sales > 0) %>%
  ggplot(aes(x = log(Sales))) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "darkorchid1", color = "white", alpha = 0.7) +
  geom_density(color = "darkmagenta", size = 1.2) +
  theme_minimal() +
  labs(title = "Log-Transformed Distribution of Daily Sales", 
       x = "Log(Sales)", 
       y = "Density",
       caption = paste("Skewness =", round(log_sales_skew, 3)))

#qqnorm(log(Data$Sales[Data$Sales > 0]))
#qqline(log(Data$Sales[Data$Sales > 0]))

# Checking sales Volatility on different days of the week
Data %>%
  filter(Sales > 0) %>%
  ggplot(aes(x = as.factor(DayOfWeek), y = Sales, fill = as.factor(DayOfWeek))) +
  geom_boxplot(alpha = 0.8, outlier.color = "khaki3", outlier.alpha = 0.3) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(title = "Sales Volatility by Day of Week", 
       x = "Day of Week", 
       y = "Sales") +
  theme(legend.position = "none")

# Examining the effect of promotions on average sales by store types
Data %>%
  filter(Sales > 0) %>%
  group_by(StoreType, Promo) %>%
  summarise(AvgSales = mean(Sales), .groups = 'drop') %>%
  ggplot(aes(x = StoreType, y = AvgSales, fill = as.factor(Promo))) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("0" = "lightsalmon", "1" = "palegreen")) +
  theme_minimal() +
  labs(title = "Average Demand: Promo vs. No Promo by Store Type", 
       x = "Store Type", 
       y = "Average Sales", 
       fill = "Promo Active")

# Correlation Matrix
Data_numeric <- Data %>%
  filter(Sales > 0) %>%
  select(Sales, Customers, CompetitionDistance, 
         CompetitionOpen_Months, Promo2_Weeks) # Numeric variables

cor_matrix <- cor(Data_numeric, use = "complete.obs")

corrplot(cor_matrix, 
         method = "color",
         type = "upper",         # Only the top half of the matrix
         addCoef.col = "black",  # number's Color
         tl.col = "black",       # Variable's names color
         tl.srt = 45,            # Rotate
         diag = FALSE,           # Delete diag
         number.cex = 0.9)      

# Analyzing Demand Distribution by Store Type and Assortment Level
Data %>%
  filter(Sales > 0) %>%
  ggplot(aes(x = StoreType, y = Sales, fill = Assortment)) +
  geom_boxplot(alpha = 0.8, outlier.color = "khaki", outlier.size = 1, outlier.alpha = 0.4) +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Set2", 
                    labels = c("a = Basic", "b = Extra", "c = Extended")) +
  theme_minimal() +
  labs(title = "Demand Architecture: Store Type vs. Assortment Strategy",
       x = "Store Type",
       y = "Daily Sales",
       fill = "Assortment Level") +
  theme(legend.position = "bottom")


# Examining the effect of promotions on average sales by Day Of Week
Data %>%
  filter(Sales > 0) %>%
  group_by(DayOfWeek, Promo) %>%
  summarise(AvgSales = mean(Sales), .groups = 'drop') %>%
  ggplot(aes(x = as.factor(DayOfWeek), y = AvgSales, fill = as.factor(Promo))) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("0" = "goldenrod1", "1" = "tomato3")) +
  theme_minimal() +
  labs(title = "Average Demand: Promo vs. No Promo by Day Of Week", 
       x = "Day Of Week", 
       y = "Average Sales", 
       fill = "Promo Active")

# Analyzing Seasonality: Demand trends across Months and Years
Data %>%
  filter(Sales > 0) %>%
  group_by(Year, Month) %>%
  summarise(AvgSales = mean(Sales), .groups = 'drop') %>%
  ggplot(aes(x = as.factor(Month), y = AvgSales, fill = as.factor(Year))) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.8) +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  labs(title = "Seasonality of Demand: Average Sales by Month and Year",
       x = "Month",
       y = "Average Daily Sales",
       fill = "Year")

# Analyzing the real impact of Promo2 Activation Status
Data %>%
  filter(Sales > 0) %>%
  mutate(Promo2_Status = case_when(
    Promo2 == 0 ~ "1. No Promo2",
    Promo2 == 1 & IsPromo2ActiveMonth == 0 ~ "2. Enrolled but Inactive",
    Promo2 == 1 & IsPromo2ActiveMonth == 1 ~ "3. Enrolled & Active"
  )) %>%
  ggplot(aes(x = Promo2_Status, y = Sales, fill = Promo2_Status)) +
  geom_boxplot(alpha = 0.8, outlier.color = "khaki2", outlier.alpha = 0.3) +
  scale_fill_manual(values = c("1. No Promo2" = "indianred",
                               "2. Enrolled but Inactive" = "coral1",
                               "3. Enrolled & Active" = "hotpink4")) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(title = "Is the continuous discount working?",
       x = "Promo2 Status",
       y = "Daily Sales") +
  theme(legend.position = "none")

# Sales Time Series for some stores
Data %>%
  filter(Store %in% c(110, 220, 330), Sales > 0) %>%
  ggplot(aes(x = Date, y = Sales, color = as.factor(Store))) +
  geom_line(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Sales Time Series by Store", color = "Store")

############### Stratified Sampling ###############

# Customers => data leakage & Open == 0 => Bias
Data <- Data %>%
  select(-Customers) %>%
  filter(Open == 1)

#_____ Store Stratified Sampling _____#

set.seed(123)
target_n_stores <- 600

# One row per store => for stratified sampling
store_attributes <- Data %>%
  distinct(Store, StoreType, Assortment, HasCompetition, Promo2)

cat("Total number of stores before sampling:", nrow(store_attributes), "\n")

# Stratified Sampling based on the most important qualitative variables
store_attributes_strat <- store_attributes %>%
  mutate(strata = 
           # It creates a factor variable where each level represents a unique combination of these four variables (Ex: "a.c.1.0")
           interaction(StoreType, Assortment, HasCompetition, Promo2,
           # It removes combinations that are not present in the data
           drop = TRUE))

# Total number of unique stores
cat("Number of strata (in stratified sampling):", n_distinct(store_attributes_strat$strata), "\n")

# HasCompetition == 0 => %0.3
# We include all members of this group in the sample
forced_stores <- store_attributes %>%
  filter(HasCompetition == 0) %>%
  pull(Store)

cat("The number of stores facing no competition is forcibly maintained in the sample:",
    length(forced_stores), "\n")

# Removes forcibly stores from the remaining data so they are not sampled again
remaining_pool <- store_attributes_strat %>%
  filter(!(Store %in% forced_stores))

remaining_target <- max(target_n_stores - length(forced_stores), 0)

# Proportionate Stratified Sampling (proportional allocation)
sampled_remaining <- remaining_pool %>%
  # It groups the data based on strata
  group_by(strata) %>%
  
  # It takes samples from each stratum in proportion
  # This causes the distribution of strata in the sample should remain approximately
  # the same as their distribution in the parent population
  slice_sample(prop = remaining_target / nrow(remaining_pool)) %>%
  ungroup()

# Combination of forcibly stores and sampled stores
sampled_stores <- bind_rows(
  store_attributes_strat %>% filter(Store %in% forced_stores),
  sampled_remaining
)

cat("Number of final sampled stores:", nrow(sampled_stores), "\n")

# Analysis of the Sample Compared to the Population
compare_categorical <- function(var_name) {
  pop  <- prop.table(table(store_attributes[[var_name]]))
  samp <- prop.table(table(sampled_stores[[var_name]]))
  data.frame(
    level              = names(pop),
    pop_proportion     = round(as.numeric(pop), 3),
    sample_proportion  = round(as.numeric(samp[names(pop)]), 3)
  )
}

cat("\n=== StoreType: Population vs Sample ===\n"); print(compare_categorical("StoreType"))
cat("\n=== Assortment: Population vs Sample ===\n"); print(compare_categorical("Assortment"))
cat("\n=== HasCompetition: Population vs Sample ===\n"); print(compare_categorical("HasCompetition"))
cat("\n=== Promo2: Population vs Sample ===\n"); print(compare_categorical("Promo2"))

# Continuous Variables? Kolmogorov-Smirnov
compare_continuous <- function(var_name) {
  pop_vals  <- Data %>% distinct(Store, .keep_all = TRUE) %>% pull(!!sym(var_name))
  samp_vals <- Data %>% filter(Store %in% sampled_stores$Store) %>%
    distinct(Store, .keep_all = TRUE) %>% pull(!!sym(var_name))
  ks <- ks.test(pop_vals, samp_vals)
  data.frame(variable = var_name,
             pop_mean = round(mean(pop_vals, na.rm = TRUE), 2),
             sample_mean = round(mean(samp_vals, na.rm = TRUE), 2),
             KS_p_value = round(ks$p.value, 3))
}

cat("\n=== Continuous Variables: Population vs Sample (KS test) ===\n")
print(do.call(rbind, lapply(
  c("CompetitionDistance", "CompetitionOpen_Months", "Promo2_Weeks"),
  compare_continuous
)))
cat("\nKS_p_value > 0.05\n This means there is no significant difference between the sample distribution and the population.\n")

# Applying sampling to the original data
Data <- Data %>%
  filter(Store %in% sampled_stores$Store)

glimpse(Data)

cat("Number of remaining stores in the data after sampling:",
    n_distinct(Data$Store), "\n")

############### Creating dataset for clustering ###############

store_profile <- Data %>%
  group_by(Store) %>%
  summarise(
    
    Promo2 = first(Promo2),
    
    HasCompetition = first(HasCompetition),
    
    CompetitionDistance = first(CompetitionDistance),
    
    CompetitionOpen_Months = max(CompetitionOpen_Months),
    
    Promo2_Weeks = max(Promo2_Weeks),
    
    StoreType = first(StoreType),
    
    Assortment = first(Assortment),
    
    .groups="drop"
  )

glimpse(store_profile)

#_____ Skewness _____#

numeric_cols <- c("CompetitionDistance", "CompetitionOpen_Months", "Promo2_Weeks")

cat("=== Skewness BEFORE transform ===\n")
sapply(
  store_profile[numeric_cols],
  skewness
)

# Log transform
store_profile <- store_profile %>%
  mutate(
    CompetitionDistance = log1p(CompetitionDistance),
    CompetitionOpen_Months = log1p(CompetitionOpen_Months),
  )

cat("=== Skewness AFTER transform ===\n")
sapply(
  store_profile[numeric_cols],
  skewness
)

# Save dataset
#write.csv(store_profile, "store_profile.csv", row.names = FALSE)

############### Hierarchical Clustering (Gower Distance) ###############

# Load necessary libraries
library(cluster)    # For daisy() and silhouette()

#install.packages("factoextra", type = "binary") # R 4.2
library(factoextra) # For elegant academic visualizations

# Converting categorical and binary variables to factor
store_profile_hc <- store_profile %>%
  mutate(
    Promo2 = as.factor(Promo2),
    StoreType = as.factor(StoreType),
    Assortment = as.factor(Assortment),
    HasCompetition = as.factor(HasCompetition)
  )

# Moving 'Store ID' to rownames for dendrogram labels
store_profile_hc <- as.data.frame(store_profile_hc)
rownames(store_profile_hc) <- store_profile_hc$Store
store_profile_hc$Store <- NULL

# Gower Distance Calculation
d_gower <- daisy(store_profile_hc, metric = "gower")

# Average Gower Distance between all stores
total_mean_dist <- mean(d_gower)
cat("Average Gower Distance between all stores:", round(total_mean_dist, 3), "\n")

# Comparison of linkage methods
linkage_methods <- c("complete", "average", "single")
cophenetic_cors <- sapply(linkage_methods, function(m) {
  hc <- hclust(d_gower, method = m)
  cor(d_gower, cophenetic(hc))}
)
print(cophenetic_cors)

# Selecting the best linkage based on cophenetic correlation
best_linkage <- names(which.max(cophenetic_cors))
cat("Best linkage method:", best_linkage, "\n")

# Hierarchical Clustering
hc_model <- hclust(d_gower, method = best_linkage)

# Finding Optimal Number of Clusters (Silhouette Index)
min_nc <- 2
max_nc <- 50
silhouette_scores <- numeric(max_nc)

for (k in min_nc:max_nc) {
  cluster_assignments <- cutree(hc_model, k = k)
  
  # Calculate silhouette index
  sil <- silhouette(cluster_assignments, d_gower)
  
  # Extract the average silhouette width for this k
  silhouette_scores[k] <- mean(sil[, 3]) 
}

# Plotting Silhouette Results
plot_data <- data.frame(k = min_nc:max_nc, S = silhouette_scores[min_nc:max_nc])

ggplot(plot_data, aes(x = k, y = S)) +
  geom_line(color = "lightpink2", size = 1) +
  geom_point(color = "darkred", size = 3, shape = 15) +
  scale_x_continuous(breaks = min_nc:max_nc) +
  theme_minimal() +
  labs(title = "Optimal Number of Clusters (Rousseeuw's Silhouette Index)",
       x = "Number of clusters (k)",
       y = "Average Silhouette Width")

# Finding Optimal Number of Clusters (Dunn Index)
library(clValid)

dunn_scores <- numeric(max_nc)

for(k in min_nc:max_nc){
  
  cluster_assignments <- cutree(hc_model, k = k)
  
  # Calculate Dunn index
  dunn_scores[k] <- dunn(
    dist = as.matrix(d_gower),
    clusters = cluster_assignments)
}

# Plotting Dunn Results
plot_dunn <- data.frame(k = min_nc:max_nc, D = dunn_scores[min_nc:max_nc])

ggplot(plot_dunn, aes(x=k, y=D)) +
  geom_line(color="steelblue", linewidth=1) +
  geom_point(color="navy", size=3, shape=15) +
  scale_x_continuous(breaks=min_nc:max_nc) +
  theme_minimal()+
  labs(
    title="Optimal Number of Clusters (Dunn Index)",
    x="Number of clusters (k)",
    y="Dunn Index")

# Comparison of Clustering Validation Indices
head(cbind(silhouette_scores, dunn_scores), 30)

# Is k=26 the optimal_k ?
cl_16 <- cutree(hc_model, k = 16)
cl_26 <- cutree(hc_model, k = 26)

cat("=== Cluster size distribution at k=16 ===\n")
print(sort(table(cl_16)))

cat("=== Cluster size distribution at k=26 ===\n")
print(sort(table(cl_26)))

cat("\nNumber of very small clusters (<=3 members) at k=16:",
    sum(table(cl_16) <= 3), "\n")
cat("\nNumber of very small clusters (<=3 members) at k=26:",
    sum(table(cl_26) <= 3), "\n")

sil_16 <- silhouette(cl_16, d_gower)
sil_26 <- silhouette(cl_26, d_gower)

cat("\n=== Average silhouette of each cluster at k=16 ===\n")
print(round(tapply(sil_16[, 3], sil_16[, 1], mean), 3))

cat("\n=== Average silhouette of each cluster at k=26 ===\n")
print(round(tapply(sil_26[, 3], sil_26[, 1], mean), 3))

# Optimal Clustering Execution
max_dunn <- max(dunn_scores)
candidates <- which(dunn_scores == max_dunn)
optimal_k <- candidates[which.max(silhouette_scores[candidates])]

cat("The optimal number of clusters is:", optimal_k,
    "\nSilhouette score:", round(silhouette_scores[optimal_k], 4),
    "\nDunn score:", round(dunn_scores[optimal_k], 4))

# Dendrogram Visualization
fviz_dend(hc_model, k = optimal_k,
          cex = 0.4,                 # Label size
          k_colors = "jco",          # Academic color palette
          rect = TRUE,               # Add rectangles around clusters
          rect_border = "jco",
          rect_fill = TRUE,
          main = paste("Dendrogram of Rossmann Stores (Gower +", best_linkage, "Linkage)"),
          show_labels = FALSE)

# Adding the cluster labels back to our original profile dataset
store_profile$HCcluster <- as.factor(cutree(hc_model, k = optimal_k))
head(store_profile)

# Number of members in each cluster
store_profile %>%
  ggplot(aes(x = factor(HCcluster))) +
  geom_bar(fill = "aquamarine4", color = "black", alpha = 0.8) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3.5) +
  theme_minimal() +
  labs(title = "Distribution of Stores",
       x = "Cluster ID",
       y = "Number of Stores") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Visualization of cluster dispersion in a two-dimensional space
# Performing MDS on the Gower distance matrix(7D) to reduce it to 2D
mds_res <- cmdscale(d_gower, k = 2)

# Plotting the scatter plot
mds_df <- data.frame(Dim1 = mds_res[,1], 
                     Dim2 = mds_res[,2], 
                     Cluster = store_profile$HCcluster)

ggplot(mds_df, aes(x = Dim1, y = Dim2, color = Cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  theme_minimal() +
  labs(title = "2D Visualization of Clusters (MDS on Gower Distance)",
       x = "MDS Dimension 1", 
       y = "MDS Dimension 2") +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = 0.5, face = "bold"))

# Understanding the difference between clusters
library(gridExtra)
library(scales)

# First: Examining the role of Promo2
p_promo <- ggplot(store_profile, aes(x = factor(HCcluster), fill = factor(Promo2))) +
  geom_bar(position = "fill", color = "white", size = 0.2) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("0" = "indianred", "1" = "steelblue"), 
                    labels = c("No Promo2", "Has Promo2")) +
  theme_minimal() +
  labs(title = "Impact of Promo2 on Clusters",
       x = "Cluster ID", y = "Proportion", fill = "Promo2 Status") +
  theme(plot.title = element_text(face = "bold", size = 10))

# Second: Examining the role of HasCompetition
p_comp <- ggplot(store_profile, aes(x = factor(HCcluster), fill = factor(HasCompetition))) +
  geom_bar(position = "fill", color = "white", size = 0.2) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("0" = "goldenrod", "1" = "seagreen"), 
                    labels = c("No Competitor", "Has Competitor")) +
  theme_minimal() +
  labs(title = "Impact of Competition on Clusters",
       x = "Cluster ID", y = "Proportion", fill = "Competition") +
  theme(plot.title = element_text(face = "bold", size = 10))

# Third: Examining the role of StoreType
p_store <- ggplot(store_profile, aes(x = factor(HCcluster), fill = StoreType)) +
  geom_bar(position = "fill", color = "white", size = 0.2) +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title = "Store Type Composition",
       x = "Cluster ID", y = "Proportion", fill = "Store Type") +
  theme(plot.title = element_text(face = "bold", size = 10))

# Fourth: Examining the role of Assortment
p_assortment <- ggplot(store_profile, aes(x = factor(HCcluster), fill = Assortment)) +
  geom_bar(position = "fill", color = "white", size = 0.2) +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title = "Assortment Composition",
       x = "Cluster ID", y = "Proportion", fill = "Assortment") +
  theme(plot.title = element_text(face = "bold", size = 10))

# Display plots
grid.arrange(p_promo, p_comp, p_store, p_assortment, ncol = 1)

############### Bayesian Hierarchical Clustering (BHC) ###############

# Load necessary libraries
library(bclust)

#install.packages("clusterSim", type = "binary")
library(RcppArmadillo)
library(ade4)
library(clusterSim)

library(fastDummies)
library(ggplot2)
library(dplyr)

store_profile_bhc <- store_profile %>%
  dplyr::select(-dplyr::any_of("HCcluster"))

# The Gaussian Bayesian model does not accept text variables
# StoreType & Assortment => One-Hot Encoding
store_profile_bhc <- dummy_cols(store_profile_bhc, 
                                select_columns = c("StoreType", "Assortment"),
                                #remove_first_dummy = TRUE, # Collinearity
                                # It's for regression not clustering
                                remove_selected_columns = TRUE)

# Moving 'Store ID' to rownames
store_profile_bhc <- as.data.frame(store_profile_bhc)
rownames(store_profile_bhc) <- store_profile_bhc$Store
store_profile_bhc$Store <- NULL

# Ensuring that the previous binary variables are also entirely numeric
store_profile_bhc$Promo2 <- as.numeric(as.character(store_profile_bhc$Promo2))
store_profile_bhc$HasCompetition <- as.numeric(as.character(store_profile_bhc$HasCompetition))

str(store_profile_bhc)

# Convert to matrix for bclust package
data_matrix <- as.matrix(store_profile_bhc)

# Normalization
data_matrix_scaled <- scale(
  data_matrix,
  center = TRUE,
  scale = TRUE
)

n_obs <- nrow(data_matrix_scaled); n_vars <- ncol(data_matrix_scaled)
cat("Observations :", n_obs, "\n")
cat("Variables :", n_vars, "\n")
cat("NA data : ", sum(is.na(data_matrix_scaled)))
cat("inf data : ", sum(is.infinite(data_matrix_scaled)))
cat("The variance of each variable must be positive:\n")
print(apply(data_matrix_scaled, 2, var))

#_____ Hyper-parameter Estimation _____#

# Having only one observation for each sample
rep.id <- 1:nrow(data_matrix_scaled)

# arguments for MLE
mc_stats <- meancss(data_matrix_scaled, rep.id = rep.id)

# Object function(MLE)
MLE <- function(theta) {
  -loglikelihood(
    x.mean = mc_stats$mean,
    x.css = mc_stats$css,
    repno  = mc_stats$repno,
    transformed.par = theta
  )
}

# hyper-parameters: sigma2_eps, sigma2_eta, sigma2_theta, mu, p, q
set.seed(123)

# Optimization hyper-parameters
opt_fit <- optim(
  # log(s2), log(s2_eta), log(s2_theta), mu, logit(p), logit(q)
  par = c(log(1), log(1), log(1), 0, qlogis(0.5), qlogis(0.5)), 
  fn = MLE,
  method = "L-BFGS-B",
  control = list(maxit = 1000),
  hessian = TRUE
)

if (opt_fit$convergence != 0) {
  warning("Hyperparameter optimization has not converged;
          interpret the output with caution.")
} else {
  cat("convergence =", opt_fit$convergence)
}

# hyper-parameter estimated(transformed)
transpar_hat <- opt_fit$par
cat("hyper-parameter estimated(transformed):\n")
round(transpar_hat, 3)
cat("Optimal value of neg Log-Likelihood = ", opt_fit$value)

# hyper-parameter estimated(original)
sigma2_eps <- exp(transpar_hat[1]); sigma2_eta <- exp(transpar_hat[2])
sigma2_theta <- exp(transpar_hat[3]); mu <- transpar_hat[4]
p <- plogis(transpar_hat[5]); q <- plogis(transpar_hat[6])
cat("hyper-parameter estimated(original):\n",
    "sigma2_eps = ", round(sigma2_eps, 3), "\n", "sigma2_eta = ",
    round(sigma2_eta, 3), "\n", "sigma2_theta = ", round(sigma2_theta, 3),
    "\n", "mu = ", round(mu, 3), "\n", "p = ", round(p, 3), "\n",
    "q = ", round(q, 3))

# Stability test
set.seed(123)
n_runs <- 10
results <- vector("list", n_runs)

for(i in 1:n_runs){
  
  start <- c(rnorm(1,0,1), rnorm(1,0,1), rnorm(1,0,1), rnorm(1,0,1),
             qlogis(runif(1,0.2,0.8)), qlogis(runif(1,0.2,0.8))) # initial values
  
  fit <- optim(par = start, fn = MLE, method = "L-BFGS-B",
               control = list(maxit = 1000))
  
  results[[i]] <- list(
    value = fit$value,
    par = fit$par,
    conv = fit$convergence)
}

stability_df <- data.frame(
  run          = 1:n_runs, # Number of runs
  neg_loglik   = sapply(results, function(x) x$value), # Stability of the objective function
  convergence  = sapply(results, function(x) x$conv)) # Convergence

cat("\n=== Multi-start Optimization ===\n")
print(stability_df)
cat("\nStandard deviation of the optimal value across runs(neg log-lik) = ",
    round(sd(stability_df$neg_loglik), 3), "\n")
cat("Optimal value range = ", range(stability_df$neg_loglik), "\n")

par_matrix <- t(sapply(results, function(x) x$par))
param_names <- c("log_s2_eps", "log_s2_eta", "log_s2_theta",
                 "mu", "logit_p", "logit_q")
colnames(par_matrix) <- param_names
par_matrix
cat("Standard deviation of each parameter across different runs:\n")
# Small SD indicates stability of the optimization solution across starting points
print(round(apply(par_matrix, 2, sd), 3)) # small sd = identifiable

# Checking the eigen values of Hessian in Optimal L (identifiability)
cat("\n=== Hessian Diagnostics ===\n")
eig <- eigen(opt_fit$hessian)$values
round(eig, 3)

if (any(eig <= 1e-6)) {
  warning(paste(
    "At least one Hessian eigenvalue is close to zero or negative.\n",
    "This means the solution lies along a flat direction,
    and certain parameters cannot be clearly identified."
  ))
} else {
  cat("All eigenvalues are positive; the solution is a valid numerical local minimum.")
}

cat("\nCondition Number:", kappa(opt_fit$hessian), "\n")
cat("Very large values indicate ill-conditioning or ridge behavior in the parameter space.\n")

# Correlation between hyper-parameters
vcov_hat <- solve(opt_fit$hessian)
cor_hat <- cov2cor(vcov_hat)
colnames(cor_hat) <- param_names
rownames(cor_hat) <- param_names
round(cor_hat, 3)

#_____ Profile Likelihood for each parameter(identifiability) _____#

# In each plot, only one parameter is held constant, while the other five parameters are re-optimized.
# The result is a one-dimensional curve that represents the "best possible scenario" for each value of that parameter.
profile_likelihood_1d <- function(param_idx, grid, start_full = transpar_hat) {
  vapply(grid, function(fixed_val) {
    
    # New Object Function with 5 parameters (6th is constant)
    obj <- function(theta_rest) {
      theta_full <- numeric(6)
      theta_full[param_idx]  <- fixed_val # Keep this parameter constant
      theta_full[-param_idx] <- theta_rest # Leave the other parameters free
      MLE(theta_full) # Calculate Negative Log-likelihood
    }
    
    # Optimizes 5 other parameters
    fit <- optim(par = start_full[-param_idx], fn = obj,
                 method = "L-BFGS-B", control = list(maxit = 1000))
    
    # It returns only the minimum negative log-likelihood
    fit$value
  }, numeric(1))
}


# Confidence intervals results
ci_results <- data.frame()

# Plots results
profile_plots <- vector("list",6)

# It repeats this process for 6 parameters.
for(j in seq_along(param_names)){
  
  # Grid search = Parameter values
  grid_j <- seq(transpar_hat[j]-3, transpar_hat[j]+3, length.out=100)
  
  # The corresponding negative log-likelihood value(Profile Likelihood)
  prof_vals <- profile_likelihood_1d(j,grid_j)
  
  # It finds the minimum negative log-likelihood(MLE)
  min_val <- min(prof_vals)
  
  # 95% Confidence Interval Threshold => -2log(Likelihood) ~ X2(1) 
  # Any part of the curve that lies below this red line is "statistically acceptable"
  threshold <- min_val + qchisq(0.95,1)/2
  
  # Confidence Intervals for hyper-parameters
  inside <- prof_vals <= threshold
  CI95 <- range(grid_j[inside])
  
  ci_results <- rbind(
    ci_results,
    data.frame(
      Parameter = param_names[j],
      Estimate = transpar_hat[j],
      Lower95 = ifelse(inside[1], -Inf, min(grid_j[inside])),
      Upper95 = ifelse(inside[length(inside)], Inf, max(grid_j[inside]))
    )
  )
  
  # Plots
  df_j <- data.frame(param_value=grid_j, neg_loglik=prof_vals)
  
  profile_plots[[j]] <-
    ggplot(df_j, aes(param_value, neg_loglik))+
    geom_line(colour="steelblue")+
    geom_point(size=1)+
    geom_hline(yintercept=threshold, colour="red", linetype=2)+
    geom_vline(xintercept=transpar_hat[j], linetype=3)+
    geom_vline(xintercept=CI95, colour="darkgreen", linetype=2)+
    labs(title=param_names[j], x=param_names[j], y="Neg LogLik")+
    theme_minimal()
  
}

ci_results

library(patchwork)
# 6 plots together
wrap_plots(profile_plots, ncol = 2) +
  plot_annotation(title = "Profile Likelihood per Hyperparameter",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

saveRDS(list(ci_results = ci_results, profile_plots = profile_plots,
             grid_j = grid_j, prof_vals = prof_vals, min_val = min_val, 
             threshold = threshold, inside = inside, CI95 = CI95),
        "profile_likelihood_results.rds")

#_____ Bayesian Hierarchical Clustering _____#

#t0 = Sys.time()
#store_bclust <- bclust(
#  x = data_matrix_scaled,
#  effect.family  = "gaussian",
#  var.select = TRUE,
#  transformed.par = transpar_hat,
#  labels = rownames(data_matrix_scaled),
#  rep.id = 1:nrow(data_matrix_scaled)
#)
#cat("Execution time:", round(difftime(Sys.time(), t0, units = "mins"), 2), "Minutes\n")

# Save output after successful execution
#saveRDS(store_bclust, "store_bclust_result.rds")

store_bclust = readRDS("C:/Users/Mari/Desktop/payannameh/store_bclust_result.rds")

# Visualizations
# Select the optimal number of clusters based on the maximum log-posterior
par(mfrow = c(2, 1))

plot(as.dendrogram(store_bclust), leaflab = "none",
     main = "Bayesian clustering dendrogram of stores")
abline(h = store_bclust$cut, col = "red", lty = 2)

plot(store_bclust$clust.number, store_bclust$logposterior,
     xlab = "Number of clusters", ylab = "Log Posterior",
     type = "b", main = "Selecting the optimal number of clusters")
abline(h = max(store_bclust$logposterior), col = "red", lty = 2)

optimal_k <- store_bclust$clust.number[which.max(store_bclust$logposterior)]
cat("Optimal number of clusters based on the maximum log-posterior:", optimal_k, "\n")

# Feature Importance
importance <- imp(store_bclust)

importance_df <- data.frame(
  Variable = importance$labels,
  Importance = importance$var)
importance_df

library(paletteer)
ggplot(importance_df,
       aes(reorder(Variable,Importance),
           Importance))+
  geom_col(fill=paletteer_dynamic("cartography::blue.pal", 12))+
  coord_flip()

# It shows the behavior of each variable within each cluster.
ditplot(store_bclust)

# Extracting final cluster labels and adding them to the dataset
hc_obj <- list(merge = store_bclust$merge,
               height = store_bclust$height,
               order = store_bclust$order,
               labels = store_bclust$labels)
class(hc_obj) <- "hclust"
cluster_membership <- cutree(hc_obj, k = optimal_k)

store_profile$BHCcluster <- NA_integer_
match_idx <- match(as.character(store_profile$Store), names(cluster_membership))
store_profile$BHCcluster[!is.na(match_idx)] <- cluster_membership[match_idx[!is.na(match_idx)]]
store_profile$BHCcluster <- as.factor(store_profile$BHCcluster)

# Cluster distributions obtained from the Bayesian approach
cat("\n Distribution of Stores in Bayesian Clusters:\n")
print(table(store_profile$BHCcluster, useNA = "ifany"))

# Number of members in each cluster
store_profile %>%
  ggplot(aes(x = factor(BHCcluster))) +
  geom_bar(fill = "orange2", color = "black", alpha = 0.8) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 3.5) +
  theme_minimal() +
  labs(title = "Distribution of Stores",
       x = "Cluster ID",
       y = "Number of Stores") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#store_profile %>% filter(BHCcluster %in% c(4,5,6)) %>% View()

# 2D Visualization of Clusters (PCA)
library(patchwork)

pca_fit <- prcomp(data_matrix_scaled, center = FALSE, scale. = FALSE)

var_exp <- round(100 * (pca_fit$sdev^2 / sum(pca_fit$sdev^2))[1:2], 1)

pca_df <- data.frame(
  Store = rownames(data_matrix_scaled),
  Dim1  = pca_fit$x[, 1],
  Dim2  = pca_fit$x[, 2]
)
pca_df$Cluster <- factor(cluster_membership[match(pca_df$Store, names(cluster_membership))])

p_pca <- ggplot(pca_df, aes(x = Dim1, y = Dim2, color = Cluster)) +
  geom_point(alpha = 0.75, size = 2) +
  theme_minimal() +
  labs(
    title = "2D Visualization of Bayesian Clusters (PCA)",
    x = paste0("Dimension 1 (", var_exp[1], "%)"),
    y = paste0("Dimension 2 (", var_exp[2], "%)")
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(p_pca)

# Optional alternative: If you want to use MDS instead of PCA
# the result is usually very similar, since Euclidean distance is the basis for both
#mds_fit <- cmdscale(dist(data_matrix_scaled), k = 2)
#pca_df$Dim1 <- mds_fit[,1]; pca_df$Dim2 <- mds_fit[,2]

# Composition charts for each cluster
plot_df <- store_profile %>%
  # Stores assigned to a Bayesian cluster
  filter(!is.na(BHCcluster)) %>% 
  mutate(
    Promo2_lab         = factor(Promo2, levels = c(0, 1),
                                labels = c("No Promo2", "Has Promo2")),
    HasCompetition_lab = factor(HasCompetition, levels = c(0, 1),
                                labels = c("No Competitor", "Has Competitor"))
  )

p1 <- ggplot(plot_df, aes(x = factor(BHCcluster), fill = Promo2_lab)) +
  geom_bar(position = "fill", color = "white") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Impact of Promo2 on Bayesian Clusters",
       x = "Cluster ID", y = "Proportion", fill = "Promo2 Status") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p2 <- ggplot(plot_df, aes(x = factor(BHCcluster), fill = HasCompetition_lab)) +
  geom_bar(position = "fill", color = "white") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Impact of Competition on Bayesian Clusters",
       x = "Cluster ID", y = "Proportion", fill = "Competition") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p3 <- ggplot(plot_df, aes(x = factor(BHCcluster), fill = StoreType)) +
  geom_bar(position = "fill", color = "white") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Store Type Composition (Bayesian Clusters)",
       x = "Cluster ID", y = "Proportion", fill = "Store Type") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p4 <- ggplot(plot_df, aes(x = factor(BHCcluster), fill = Assortment)) +
  geom_bar(position = "fill", color = "white") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Assortment Composition (Bayesian Clusters)",
       x = "Cluster ID", y = "Proportion", fill = "Assortment") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

(p1 / p2 / p3 / p4)

# Save final dataset
write.csv(store_profile, "store_profile_with_clusters.csv", row.names = FALSE)

view(store_profile)

############### compare HC and BHC ###############

library(mclust)     # adjustedRandIndex

comparison_df <- store_profile %>%
  filter(!is.na(HCcluster), !is.na(BHCcluster)) %>%
  mutate(
    HC_Cluster    = as.factor(HCcluster),
    Bayes_Cluster = as.factor(BHCcluster)
  )

cat("Number of common stores for comparison:", nrow(comparison_df), "\n")

# Cross-tabulation
cat("\n=== Crosstabulation of Classical and Bayesian clusters ===\n")
cross_tab <- table(comparison_df$HC_Cluster, comparison_df$Bayes_Cluster,
                   dnn = c("Classical (HC, k=16)", "Bayesian (BHC, k=6)"))
print(cross_tab)

# Adjusted Rand Index
ari_value <- adjustedRandIndex(comparison_df$HC_Cluster, comparison_df$Bayes_Cluster)
cat("\nAdjusted Rand Index between classical and Bayesian clustering:", round(ari_value, 3), "\n")

# Alluvial plot
library(ggalluvial)

alluvial_df <- comparison_df %>%
  dplyr::count(HC_Cluster, Bayes_Cluster, name = "Freq")

ggplot(alluvial_df,
       aes(axis1 = HC_Cluster, axis2 = Bayes_Cluster, y = Freq)) +
  geom_alluvium(aes(fill = Bayes_Cluster), width = 1/4, alpha = 0.8) +
  geom_stratum(width = 1/4, fill = "#FFFACD", color = "#8B7E66") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Classical (k=16)", "Bayesian (k=6)"),
                   expand = c(0.1, 0.1)) +
  theme_minimal() +
  labs(title = "Correspondence Between Classical and Bayesian Clusters",
       y = "Number of Stores", fill = "Bayesian Cluster") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Purity criterion: Is each small classical cluster contained entirely within a larger Bayesian cluster?
purity_per_HC_cluster <- apply(cross_tab, 1, function(row) max(row) / sum(row))

purity_table <- data.frame(
  HC_Cluster    = rownames(cross_tab),
  Size          = rowSums(cross_tab),
  Dominant_Bayes = apply(cross_tab, 1, function(row) colnames(cross_tab)[which.max(row)]),
  Purity        = round(purity_per_HC_cluster, 3)
)

print(purity_table)

# Overall weighted average
# The proportion of total stores within a "classical" cluster 
# that predominantly belong to only one "Bayesian" cluster
weighted_purity <- sum(apply(cross_tab, 1, max)) / sum(cross_tab)
cat("\nOverall Weighted Purity:", round(weighted_purity, 3), "\n")
cat("meaning", round(weighted_purity * 100, 1),
    "% of stores are in classical clusters that fall almost entirely within a single Bayesian cluster\n")

############### Save final dataset(Data+Stores+Clusters) ###############

dim(store_profile)
str(store_profile)
nrow(store_profile)
length(unique(store_profile$Store))
sum(duplicated(store_profile$Store))
sum(is.na(store_profile$HCcluster))
sum(is.na(store_profile$BHCcluster))

dim(Data)
nrow(Data)
length(unique(Data$Store))
sum(is.na(Data$Store))

# Select column from store_profile
store_features <- store_profile %>%
  dplyr::select(Store, HCcluster, BHCcluster)

# Merge
nn_data <- Data %>%
  left_join(
    store_features,
    by = "Store"
  )

dim(nn_data)

sum(duplicated(nn_data))

cat("Original daily rows :", nrow(Data), "\n")
cat("Merged daily rows   :", nrow(nn_data), "\n")

cat("Number of stores    :",
    length(unique(nn_data$Store)), "\n")

cat("Missing HC clusters :",
    sum(is.na(nn_data$HCcluster)), "\n")

cat("Missing BHC clusters:",
    sum(is.na(nn_data$BHCcluster)), "\n")

write.csv(nn_data, "Data_for_NN.csv", row.names = FALSE)
