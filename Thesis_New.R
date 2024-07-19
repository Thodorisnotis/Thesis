library(terra)
install.packages("randomForest") 
library(randomForest)
install.packages("caTools")
install.packages("clhs")
library(caTools)
library(dplyr)
library(ggplot2)
library(sf)
library(leaflet)
library(grid)
library(png)
library(gridExtra)
library(clhs)
library(raster)
library(viridis)
## Set the working directory to your raster_data folder
getwd()
setwd("C:/Users/teosa/Desktop/Thesis/shared/data/raster_data/terrain")

#Load the eberg.csv file

eberg_df<-read.csv("C:/Users/teosa/Desktop/Thesis/shared/data/point_data/eberg.csv")

## Descriptive statistics for dataset : Ebergotzen

# Histogram
hist(eberg_df$clymht_a, main = "Histogram of Clay Content (clymht_a)", xlab = "Clay Content", col = "blue", breaks = 20)

# Columns to check
columns_to_check <- c("clymht_a", "clymht_b", "clymht_c","clymht_d", "clymht_e")

# Create a dataframe to store missing values count
missing_values_df <- data.frame(
  column = columns_to_check,
  missing_count = sapply(eberg_df[columns_to_check], function(x) sum(is.na(x)))
)

# Plot the missing values
ggplot(missing_values_df, aes(x = column, y = missing_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Missing Values , Clay_Content",
       x = "Column",
       y = "Number of Missing Values")


# Convert coordinates to a matrix
coordinates<- as.matrix(eberg_df[, c("x", "y")])

## List all files with the .tif extension in the directory
tif_files <- list.files(pattern = "\\.tif$")
# Print the list of .tif files
print(tif_files)


# Initialize a list to hold the raster information
raster_info <- list()

# Loop through each .tif file
for (file_name in tif_files) {
  # Load the raster file
  raster <- rast(file_name)
  
  # Get the extent and origin of the raster
  extent_info <- ext(raster)  # Extent of the raster
  origin_info <- origin(raster)  # Origin of the raster (x and y)
  
  # Store the information in the list with the file name as the key
  raster_info[[file_name]] <- list(Extent = extent_info, Origin = origin_info)
}
# Print the raster information
print(raster_info)

# Load the extent from the provided GPKG file

####Is there a problem that instead of rast i used vect?
reference_extent_raster <- vect("C:/Users/teosa/Desktop/Thesis/shared/data/raster_data/study_area/extent_mapping.gpkg")

# Extract the extent
ref_extent <- ext(reference_extent_raster)

# Print the extent
print(ref_extent)

# Initialize a data frame to store the results
d <- data.frame(ID = seq_len(nrow(coordinates)))

# Loop through each .tif file
for (file_name in tif_files) {
  # Load the raster file
  r <- rast(file_name)
  
  # Extract values at the defined coordinates
  extracted_values <- extract(r, coordinates) #as.Data.frame instead of extract and no coordinates
  
  # Store the extracted values in the data frame:
  d[[gsub(".tif", "", file_name)]] <- extracted_values[, 1]  # assuming a single band raster
}

# Add X and Y at the beginning
d <- cbind(d, X = eberg_df$x, Y = eberg_df$y)
# Now reorder columns to make X and Y the 2nd and 3rd columns
d <- d[, c(1, ncol(d)-1, ncol(d), 2:(ncol(d)-2))]
head(d)

# Convert degrees to radians
degrees_30 <- 30 * pi / 180
degrees_60 <- 60 * pi / 180

# Mutate to add new columns
d <- d %>%
  mutate(
    x_30 = X * cos(degrees_30) - Y * sin(degrees_30),
    y_30 = X * sin(degrees_30) + Y * cos(degrees_30),
    x_60 = X * cos(degrees_60) - Y * sin(degrees_60),
    y_60 = X * sin(degrees_60) + Y * cos(degrees_60)
  )



# Count missing values for each column
na_counts <- sapply(d, function(x) sum(is.na(x)))

# Print the counts
print(na_counts)



##1st try of RANDOM FOREST

#Load the column "clymht_a" for clay from Eberg data set:
d$clymht_a <- eberg_df$clymht_a

# Check for missing values in d
na_counts <- sapply(d, function(x) sum(is.na(x)))
print(na_counts)


#Remove column lcv_landcover_ocm_2019, ID because of numerous missing values
#d <- d %>%
 # select(-lcv_landcover_0cm_2019)
d <- d %>%
  select(-ID)
# Remove rows with any missing values in the dataframe (5 rows)
d <- na.omit(d)

# Splitting the data: 70% for training, 30% for testing
set.seed(123)  # for reproducibility
split <- sample.split(d$clymht_a, SplitRatio = 0.7)

train_data1 <- subset(d, split == TRUE)
test_data1 <- subset(d, split == FALSE)

# Train the model
model <- randomForest(clymht_a ~ ., data = train_data1, ntree=500, importance=TRUE)

# Print the model summary
print(model)

# Make predictions on the test data
predictions <- predict(model, test_data1)

# Calculate RMSE or any other metric to evaluate the model
test_data1$predicted_clymht_a <- predictions
rmse <- sqrt(mean((test_data1$clymht_a - test_data1$predicted_clymht_a)^2))
print(paste("RMSE:", rmse))

# Get variable importance
importance_scores <- importance(model)

# View importance scores
print(importance_scores)

# Plot variable importance
varImpPlot(model)

# Convert the importance matrix to a data frame for plotting
importance_df <- data.frame(
  Variable = rownames(importance_scores),
  Importance = importance_scores[, "%IncMSE"],
  stringsAsFactors = FALSE
)

# Sort by importance in descending order
importance_df <- importance_df[order(-importance_df$Importance),]

# Create a horizontal bar graph
ggplot(importance_df, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_bar(stat = "identity", fill = "steelblue", orientation = "y") +
  theme_minimal() +
  labs(title = "Variable Importance in Random Forest Model",
       x = "Importance", y = "Variables") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))


# Select the top 25 important features
top_features <- head(importance_df$Variable, 25)

train_data_subset <- train_data1[, c(top_features, "clymht_a")]

# Rerun the model with the subset of top 25 important features
model_subset <- randomForest(clymht_a ~ ., data = train_data_subset, ntree=500, importance=TRUE)
test_data_subset <- test_data1[, c(top_features, "clymht_a")]

# Make predictions
predictions <- predict(model_subset, test_data_subset)
# Mean Squared Error (MSE):
mse <- mean((predictions - test_data_subset$clymht_a)^2)
print(paste("MSE of the model:", mse))

# Get variable importance for new model
importance_scores_subset <- importance(model_subset)

# Convert the importance matrix to a data frame for plotting
importance_df_subset <- data.frame(
  Variable = rownames(importance_scores_subset),
  Importance = importance_scores_subset[, "%IncMSE"],
  stringsAsFactors = FALSE
)

# Create a horizontal bar graph for the new model
ggplot(importance_df_subset, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_bar(stat = "identity", fill = "steelblue", orientation = "y") +
  theme_minimal() +
  labs(title = "Variable Importance in Random Forest Model",
       x = "Importance", y = "Variables") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))



#Create the ground truth map

# List of column names to keep
columns_to_keep <- c(
  "X", "y_60", "x_30", "dtm_elev_0cm_2000", "x_60", "tri100m",
  "y_30", "Y", "TWIsaga100m_s100", "curvup100m", "heigstand30m",
  "curvdown100m", "vrm100m", "heigstand100m", "TWIcatchmod30m",
  "heigval100m", "heigval30m", "water_hdist", "cindex30m", 
  "TWIsaga100m", "heigslope100m", "water_vdist", "mrvbf100m",
  "vrm30m", "asp_nness100m", "clymht_a"
)


#Predict the clay values for every pixel 


# Load the rasters from the tif_files list
rasters <- stack(tif_files)

# Prepare Data for Prediction
raster_df <- as.data.frame(rasters, xy = TRUE)

# Handle NA Values from 159558 values to 153393 values
raster_df <- raster_df[complete.cases(raster_df), ]


# Mutate to add new columns
raster_df <- raster_df %>%
  mutate(
    x_30 = x * cos(degrees_30) - y * sin(degrees_30),
    y_30 = x * sin(degrees_30) + y * cos(degrees_30),
    x_60 = x * cos(degrees_60) - y * sin(degrees_60),
    y_60 = x * sin(degrees_60) + y * cos(degrees_60)
  )

#Rename the x,y values to match the random forest model 
colnames(raster_df)[1:2] <- c("X", "Y")

# Make Predictions
clay_predictions <- predict(model, newdata = raster_df)

# Combine Predictions with Coordinates
raster_df$clay_predictions <- clay_predictions
#Create a Raster of Predictions
prediction_raster <- rasterFromXYZ(raster_df[, c("X", "Y", "clay_predictions")])

# Save the Prediction Raster
#writeRaster(prediction_raster, filename = "prediction_raster.tif", format = "GTiff", overwrite = TRUE)


# Plot the Prediction Raster
plot(prediction_raster, main = "Predicted Values for clymht_a")


# Check for Missing Values   ## 621
na_count <- cellStats(is.na(prediction_raster), sum)
total_cells <- ncell(prediction_raster)

# Print the number of missing values
cat("Number of missing values in the prediction raster:", na_count, "\n")
cat("Total number of cells in the prediction raster:", total_cells, "\n")




#Ground Truth Map 

# Load the raster files
raster1 <- raster("C:/Users/teosa/Desktop/Thesis/shared/data/prediction_raster.tif")
raster2 <- raster("C:/Users/teosa/Desktop/Thesis/shared/data/error_map_1.tif")
extent(raster1)
extent(raster2)
res(raster1)
res(raster2)


# Define the extent and resolution of the output raster
extent_to_match <- extent(raster1)
resolution_to_match <- res(raster1)

# Resample the second raster to match the first raster
raster2_resampled <- resample(raster2, raster1, method='bilinear')

# Check the extents and resolutions
extent(raster1)
extent(raster2_resampled)
res(raster1)
res(raster2_resampled)


#####GROUND_TRUTH_MAP final with error map
ground_truth_map<- raster1+raster2_resampled
plot(raster1)
plot(ground_truth_map)
plot(raster2_resampled)


#Descriptive statistics for GTM 

# Create a histogram for the clay_prediction column
hist(raster_df$clay_prediction,
     main = "Histogram of Clay Prediction",
     xlab = "Clay Prediction",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")
###SAMPLING METHODS

#1.Simple Random Sampling

# Set the number of samples
n_samples <- 100

# Function to perform Simple Random Sampling, train a random forest model, and compute RMSE
train_random_forest_and_get_predictions <- function(data, n_samples) {
  # Simple random sampling
  sample_indices <- sample(1:nrow(data), n_samples)
  sample_data <- data[sample_indices, ]
  
  # Train a new random forest model
  rf_random_sampling <- randomForest(clay_predictions ~ ., data = sample_data)
  
  # Predict on the entire dataset (all pixels)
  predictions_random_sampling <- predict(rf_random_sampling, data)
  
  # Calculate RMSE on the sample data
  rmse <- sqrt(mean((sample_data$clay_predictions - predict(rf_random_sampling, sample_data))^2))
  
  return(list(rmse = rmse, sample_data = sample_data, predictions = predictions_random_sampling, model = rf_random_sampling))
}

# Function to perform the repetitions and plot results
perform_repetitions_and_plot <- function(data, n_samples, n_reps) {
  # Vector to store RMSEs from each repetition
  rmse_values <- numeric(n_reps)
  
  # Perform sampling, training, and RMSE calculation for each repetition
  set.seed(123) # For reproducibility
  for (i in 1:n_reps) {
    result <- train_random_forest_and_get_predictions(data, n_samples)
    rmse_values[i] <- result$rmse
  }
  
  # Convert RMSE values to a data frame for plotting
  rmse_df <- data.frame(RMSE = rmse_values)
  
  # Plot the RMSE values as a box plot
  ggplot(rmse_df, aes(y = RMSE)) +
    geom_boxplot() +
    labs(title = paste("RMSE Box Plot for Random Forest Models (", n_reps, " Repetitions)", sep=""),
         y = "RMSE") +
    theme_minimal()
  
  # Print the RMSE values
  print(rmse_values)
  
  # Get the predictions and sample data for one repetition
  one_rep_result <- train_random_forest_and_get_predictions(data, n_samples)
  sample_data <- one_rep_result$sample_data
  predictions <- one_rep_result$predictions
  
  # Combine sample data and predictions into one data frame
  prediction_df <- sample_data %>%
    mutate(predictions = predictions)
  
  # Plot the true vs predicted clay predictions
  ggplot(prediction_df, aes(x = clay_predictions, y = predictions)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(title = paste("True vs Predicted Clay Predictions (Single Repetition, ", n_reps, " Repetitions)", sep=""),
         x = "True Clay Predictions",
         y = "Predicted Clay Predictions") +
    theme_minimal()
}

# Function to perform the repetitions and return RMSE values
perform_repetitions <- function(data, n_samples, n_reps) {
  # Vector to store RMSEs from each repetition
  rmse_values <- numeric(n_reps)
  
  # Perform sampling, training, and RMSE calculation for each repetition
  set.seed(123) # For reproducibility
  for (i in 1:n_reps) {
    result <- train_random_forest_and_get_predictions(data, n_samples)
    rmse_values[i] <- result$rmse
  }
  
  return(rmse_values)
}

# Perform repetitions for 25, 50, and 100 and combine results
rmse_25 <- perform_repetitions(raster_df, n_samples, 25)
rmse_50 <- perform_repetitions(raster_df, n_samples, 50)
rmse_100 <- perform_repetitions(raster_df, n_samples, 100)

# Combine RMSE values into a single data frame
rmse_df <- data.frame(
  RMSE = c(rmse_25, rmse_50, rmse_100),
  Repetitions = factor(rep(c(25, 50, 100), times = c(length(rmse_25), length(rmse_50), length(rmse_100))))
)

### Plot the RMSE values as a box plot #BOX PLOT FOR RANDOM SAMPLING 
ggplot(rmse_df, aes(x = Repetitions, y = RMSE)) +
  geom_boxplot() +
  labs(title = "RMSE Box Plot for Random Sampling",
       x = "Number of Repetitions",
       y = "RMSE") +
  theme_minimal()

# Print the RMSE values for verification
print(rmse_df)


# Get the predictions and sample data for one repetition with 100 repetitions
one_rep_result <- train_random_forest_and_get_predictions(raster_df, n_samples)
predictions <- one_rep_result$predictions

# Combine the entire raster_df and predictions into one data frame
prediction_df <- raster_df %>%
  mutate(predictions = predictions)

# Create a map plot of the predictions for the entire prediction_df #RANDOM SAMPLING
ggplot(prediction_df, aes(x = X, y = Y, fill = predictions)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Predicted Clay Map (Entire Dataset)",
       x = "X Coordinate",
       y = "Y Coordinate",
       fill = "Predicted Clay") +
  theme_minimal()

# Create a map plot of the predictions for the entire prediction_df #RANDOM SAMPLING
ggplot(prediction_df, aes(x = X, y = Y, fill = predictions)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Predicted Clay Map (Entire Dataset)",
       x = "X Coordinate",
       y = "Y Coordinate",
       fill = "Predicted Clay") +
  theme_minimal()

# Calculate RMSE for the entire dataset
rmse_whole_dataset <- sqrt(mean((raster_df$clay_predictions - prediction_df$predictions)^2))
cat("RMSE for the entire dataset: ", rmse_whole_dataset, "\n")

###***AFTER 100 REPETITIONS***

# Perform one repetition with 100 samples
result <- train_random_forest_and_get_predictions(raster_df, n_samples)

# Combine the ground truth and predictions into one data frame
comparison_df <- raster_df %>%
  mutate(predictions = result$predictions)

# Plot the observed vs predicted clay predictions
ggplot(comparison_df, aes(x = clay_predictions, y = predictions)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Observed vs Predicted Clay Predictions (Entire Dataset)",
       x = "Observed Clay Predictions",
       y = "Predicted Clay Predictions") +
  theme_minimal()

# Create a map plot of the true values
ggplot(comparison_df, aes(x = X, y=Y ,fill = clay_predictions)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Observed Clay Map (Entire Dataset)",
       x = "X Coordinate",
       y = "Y Coordinate",
       fill = "Observed Clay") +
  theme_minimal()


# Create a map plot of the predicted values
ggplot(comparison_df, aes(x = X, y = Y, fill = predictions)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Predicted Clay Map (Entire Dataset)",
       x = "X Coordinate",
       y = "Y Coordinate",
       fill = "Predicted Clay") +
  theme_minimal()



#2.Conditional Latin Hypercube Sampling (cLHS)



# Set the number of samples
n_samples <- 100

# Function to perform Conditional Latin Hypercube Sampling, train a random forest model, and compute RMSE and predictions
train_random_forest_and_get_predictions_clhs <- function(data, n_samples) {
  # Conditional Latin Hypercube Sampling
  sample_indices <- clhs(data, size = n_samples, progress = FALSE)
  sample_data <- data[sample_indices, ]
  
  # Train a new random forest model
  rf_random_sampling <- randomForest(clay_predictions ~ ., data = sample_data)
  
  # Predict on the entire dataset (all pixels)
  predictions_random_sampling <- predict(rf_random_sampling, data)
  
  # Calculate RMSE on the entire dataset
  rmse_whole_dataset <- sqrt(mean((data$clay_predictions - predictions_random_sampling)^2))
  
  return(list(rmse = rmse_whole_dataset, predictions = predictions_random_sampling))
}

# Function to perform the repetitions and return RMSE values
perform_repetitions_clhs <- function(data, n_samples, n_reps) {
  # Vector to store RMSEs from each repetition
  rmse_values <- numeric(n_reps)
  
  # Perform sampling, training, and RMSE calculation for each repetition
  set.seed(123) # For reproducibility
  for (i in 1:n_reps) {
    rmse_values[i] <- train_random_forest_and_get_predictions_clhs(data, n_samples)$rmse
  }
  
  return(rmse_values)
}

# Perform repetitions for 25, 50, and 100 and combine results
rmse_25_clhs <- perform_repetitions_clhs(raster_df, n_samples, 25)
rmse_50_clhs <- perform_repetitions_clhs(raster_df, n_samples, 50)
rmse_100_clhs <- perform_repetitions_clhs(raster_df, n_samples, 100)

  # Combine RMSE values into a single data frame
  rmse_df_clhs <- data.frame(
    RMSE = c(rmse_25_clhs, rmse_50_clhs, rmse_100_clhs),
    Repetitions = factor(rep(c(25, 50, 100), times = c(length(rmse_25_clhs), length(rmse_50_clhs), length(rmse_100_clhs))))
  )
  
  # Plot the RMSE values as a box plot
  ggplot(rmse_df_clhs, aes(x = Repetitions, y = RMSE)) +
    geom_boxplot() +
    labs(title = "RMSE Box Plot for Random Forest Models with cLHS",
         x = "Number of Repetitions",
         y = "RMSE") +
    theme_minimal()
  
  # Print the RMSE values for verification
  print(rmse_df_clhs)
  
  # Perform one repetition with 100 samples using cLHS to get predictions
  result_clhs <- train_random_forest_and_get_predictions_clhs(raster_df, n_samples)
  predictions_clhs <- result_clhs$predictions
  
  # Combine the predictions with the original data
  raster_df <- raster_df %>%
    mutate(predictions_clhs = predictions_clhs)
  
  # Plot the predicted clay values using cLHS
  ggplot(raster_df, aes(x = X, y = Y, fill = predictions_clhs)) +
    geom_tile() +
    scale_fill_viridis_c() +
    labs(title = "Predicted Clay Map using cLHS (Entire Dataset)",
         x = "X Coordinate",
         y = "Y Coordinate",
         fill = "Predicted Clay") +
    theme_minimal()
  
  
  
#Plot maps for clay
  
  # Define the common limits for the clay content
  clay_limits <- c(0, 50)  # Adjust these limits based on your data range
  
  # Create a map plot of the true values
  ggplot(comparison_df, aes(x = X, y=Y ,fill = clay_predictions)) +
    geom_tile() +
    scale_fill_viridis_c(limits = clay_limits) +
    labs(title = "Predicted Clay Map using the dataset (Entire Dataset)",
         x = "X Coordinate",
         y = "Y Coordinate",
         fill = "Observed Clay") +
    theme_minimal()
  
  # Create a map plot of the predictions for the entire prediction_df #RANDOM SAMPLING
  ggplot(prediction_df, aes(x = X, y = Y, fill = predictions)) +
    geom_tile() +
    scale_fill_viridis_c(limits = clay_limits) +
    labs(title = "Predicted Clay Map using random sampling (Entire Dataset)",
         x = "X Coordinate",
         y = "Y Coordinate",
         fill = "Predicted Clay") +
    theme_minimal()
  
  # Plot the predicted clay values using cLHS
  ggplot(raster_df, aes(x = X, y = Y, fill = predictions_clhs)) +
    geom_tile() +
    scale_fill_viridis_c(limits = clay_limits) +
    labs(title = "Predicted Clay Map using cLHS (Entire Dataset)",
         x = "X Coordinate",
         y = "Y Coordinate",
         fill = "Predicted Clay") +
    theme_minimal()


###GIVEN BUDGET E.G 1000
  
  # Define costs and error standard deviations
  cost_A <- 1.0    # 100% relative cost
  error_sd_A <- 1.8 # Error standard deviation for method A
  
  cost_B <- 0.1    # 10% relative cost
  error_sd_B <- 6.0 # Error standard deviation for method B
  
  # Given budget
  budget_X <- 1000

  # Calculate the number of samples that can be taken within the budget
  n_samples_A <- floor(budget_X / cost_A)
  n_samples_B <- floor(budget_X / cost_B)
  
  cat("Number of samples for method A:", n_samples_A, "\n")
  cat("Number of samples for method B:", n_samples_B, "\n")
  
  # Define intermediate scenarios
  n_samples_A_scenario_3 <- floor(budget_X * 0.5 / cost_A)
  n_samples_B_scenario_3 <- floor(budget_X * 0.5 / cost_B)
  
  n_samples_A_scenario_4 <- floor(budget_X * 0.25 / cost_A)
  n_samples_B_scenario_4 <- floor(budget_X * 0.75 / cost_B)
  
  cat("Scenario 3: ", n_samples_A_scenario_3, "samples from A and", n_samples_B_scenario_3, "samples from B\n")
  cat("Scenario 4: ", n_samples_A_scenario_4, "samples from A and", n_samples_B_scenario_4, "samples from B\n")


  # Function to perform sampling, add errors, train a random forest model, and compute RMSE
  train_random_forest_and_get_predictions <- function(data, n_samples_A, n_samples_B, error_sd_A, error_sd_B) {
    set.seed(sample.int(10000, 1)) # Ensure different samples in each iteration
    # Sampling for method A
    if (n_samples_A > 0) {
      sample_indices_A <- sample(1:nrow(data), n_samples_A)
      sample_data_A <- data[sample_indices_A, ]
      sample_data_A$clay_predictions <- sample_data_A$clay_predictions + rnorm(n_samples_A, mean = 0, sd = error_sd_A)
    } else {
      sample_data_A <- data.frame()
    }
    
    # Sampling for method B
    if (n_samples_B > 0) {
      sample_indices_B <- sample(1:nrow(data), n_samples_B)
      sample_data_B <- data[sample_indices_B, ]
      sample_data_B$clay_predictions <- sample_data_B$clay_predictions + rnorm(n_samples_B, mean = 0, sd = error_sd_B)
    } else {
      sample_data_B <- data.frame()
    }
    
    # Combine samples from both methods
    sample_data <- rbind(sample_data_A, sample_data_B)
    
    # Train a new random forest model
    rf_model <- randomForest(clay_predictions ~ ., data = sample_data)
    
    # Predict on the entire dataset (all pixels)
    predictions <- predict(rf_model, data)
    
    # Calculate RMSE on the entire dataset
    rmse <- sqrt(mean((data$clay_predictions - predictions)^2))
    
    return(rmse)
  }
  
  # Function to perform multiple repetitions and return RMSE values
  perform_repetitions <- function(data, n_samples_A, n_samples_B, error_sd_A, error_sd_B, n_reps = 25) {
    rmse_values <- numeric(n_reps)
    for (i in 1:n_reps) {
      rmse_values[i] <- train_random_forest_and_get_predictions(data, n_samples_A, n_samples_B, error_sd_A, error_sd_B)
    }
    return(rmse_values)
  }

# Number of repetitions
n_reps <- 25

# Perform sampling for each scenario
rmse_scenario_1 <- perform_repetitions(raster_df, n_samples_A, 0, error_sd_A, error_sd_B, n_reps)
rmse_scenario_2 <- perform_repetitions(raster_df, 0, n_samples_B, error_sd_A, error_sd_B, n_reps)
rmse_scenario_3 <- perform_repetitions(raster_df, n_samples_A_scenario_3, n_samples_B_scenario_3, error_sd_A, error_sd_B, n_reps)
rmse_scenario_4 <- perform_repetitions(raster_df, n_samples_A_scenario_4, n_samples_B_scenario_4, error_sd_A, error_sd_B, n_reps)

# Combine RMSE values into a single data frame for visualization
errors_df <- data.frame(
  RMSE = c(rmse_scenario_1, rmse_scenario_2, rmse_scenario_3, rmse_scenario_4),
  Scenario = rep(c("All A", "All B", "50% A, 50% B", "25% A, 75% B"), each = 25)
)

## Plot the RMSE values as a box plot
ggplot(errors_df, aes(x = Scenario, y = RMSE, fill = Scenario)) +
  geom_boxplot() +
  labs(title = "RMSE Box Plot for Different Sample Combinations",
       x = "Scenario",
       y = "RMSE") +
  theme_minimal() +
  scale_fill_manual(values = c("All A" = "blue", "All B" = "red", "50% A, 50% B" = "green", "25% A, 75% B" = "purple"))

