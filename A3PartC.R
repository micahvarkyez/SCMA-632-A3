# Load required libraries
library(readr)
library(censReg)

# Load your dataset 
data <- read_csv("C:\\Users\\Dell\\Desktop\\MICAH\\NSSO68main.csv")

# Define your dependent variable and independent variables
data$non_vegetarian <- ifelse(rowSums(data[, c('eggsno_q', 'fishprawn_q', 'goatmeat_q', 
                                       'beef_q', 'pork_q', 'chicken_q', 'othrbirds_q')], 
                              na.rm = TRUE) > 0, 1, 0)
y <- data$non_vegetarian
X <- data[, c('Age','MPCE_MRP', 'Sex', 'Sector')]

# Prepare the data for Tobit regression
y_tobit <- pmin(pmax(y, 0), 1)  
X_tobit <- cbind(1, X) 

# Fit the Tobit model
X_tobit_df <- as.data.frame(X_tobit)
model <- censReg(y_tobit ~ ., data = X_tobit_df[, -1])

# Print Tobit model summary
summary(model)

