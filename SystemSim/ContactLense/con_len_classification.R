
# Classification with knn

install.packages("pacman")

# Install class for knn classification
pacman::p_load("class")

# Load Data ############################
# Read csv
lens_data <- read.csv("ContactLens.csv", header = TRUE)
colnames(lens_data)
head(lens_data)       # Show the first 6 cases

# Normalization - Important for different ranges ###########
# Define the function for normalization

normal <- function(x) {
  norm <- ((x - min(x))/(max(x)- min(x)))
  return (norm)
}

# Apply function to data frame without ID and default
normal_lens_data <- as.data.frame(lapply(lens_data[,1:4], normal))
head(normal_lens_data)

# Put outcome variable back on and rename
normal_lens_data <- cbind(normal_lens_data, lens_data[,5])
names(normal_lens_data)[5] <- "ContactLens"

# Check Data
colnames(normal_lens_data)
head(normal_lens_data)

# Split Data #############################

# Split into training set (80%) and testing set (20%)
set.seed(2) #seed can be any number
normal_lens_data.split <- sample(2, nrow(normal_lens_data),replace = TRUE, prob = c(0.8, 0.2))

# Create training and testing dataset without outcome labels.
# Use just the first 23 variavles
normal_lens_data.train.labels <- normal_lens_data[normal_lens_data.split == 1, 5]
normal_lens_data.test.labels <- normal_lens_data[normal_lens_data.split == 2, 5]


# Create outcome labels
normal_lens_data.train <- normal_lens_data[normal_lens_data.split == 1, 1:4]
normal_lens_data.test <- normal_lens_data[normal_lens_data.split == 2, 1:4]


# Build and test classifier ##########################

# Build classifier for test data
# k = number of neighbors to compare; odd n avoids ties.
# Try for several k and check accuracy
normal_lens_data.pred <- knn(train = normal_lens_data.train,
                  test = normal_lens_data.test,
                  cl = normal_lens_data.train.labels,       # true class
                  k = 3)                         # n neighbors

# Compare predicted outcome to observed outcome
table(normal_lens_data.pred, normal_lens_data.test.labels)


# Clean up #####################
rm(list = ls())


