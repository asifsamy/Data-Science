
# Classification with knn

install.packages("pacman")

# Install class for knn classification
pacman::p_load("class")

# Load Data ############################
# Read csv
car_data <- read.csv("car_data.csv", header = TRUE)
colnames(car_data)
head(car_data)       # Show the first 6 cases

# Normalization - Important for different ranges ###########
# Define the function for normalization

normal <- function(x) {
  norm <- ((x - min(x))/(max(x)- min(x)))
  return (norm)
}

# Apply function to data frame without ID and default
normal_car_data <- as.data.frame(lapply(car_data[,1:6], normal))
head(normal_car_data)

# Put outcome variable back on and rename
normal_car_data <- cbind(normal_car_data, car_data[,7])
names(normal_car_data)[7] <- "CarClass"

# Check Data
colnames(normal_car_data)
head(normal_car_data)

# Split Data #############################

# Split into training set (80%) and testing set (20%)
set.seed(800) #seed can be any number
normal_car_data.split <- sample(2, nrow(normal_car_data),replace = TRUE, prob = c(0.9, 0.1))

# Create training and testing dataset without outcome labels.
# Use just the first 23 variavles
normal_car_data.train.labels <- normal_car_data[normal_car_data.split == 1, 7]
normal_car_data.test.labels <- normal_car_data[normal_car_data.split == 2, 7]


# Create outcome labels
normal_car_data.train <- normal_car_data[normal_car_data.split == 1, 1:6]
normal_car_data.test <- normal_car_data[normal_car_data.split == 2, 1:6]


# Build and test classifier ##########################

# Build classifier for test data
# k = number of neighbors to compare; odd n avoids ties.
# Try for several k and check accuracy
normal_car_data.pred <- knn(train = normal_car_data.train,
                  test = normal_car_data.test,
                  cl = normal_car_data.train.labels,       # true class
                  k = 5)                         # n neighbors

# Compare predicted outcome to observed outcome
table(normal_car_data.pred, normal_car_data.test.labels)


# Clean up #####################
rm(list = ls())


