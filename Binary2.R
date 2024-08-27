library(AutoScore)
library(dplyr)
library(corrplot)
library(openxlsx)
library(caret)
library(pROC)
library(ggplot2)
library(svglite)
library(grDevices)
setwd("C:/Users/11373/Desktop/Auto score")
sample_data <- read.csv("C:/Users/11373/Desktop/Auto score/AIBL_by_id.csv")
names(sample_data)[names(sample_data) == "Neuropsych.Simple.Classification"] <- "label"
sample_data <- subset(sample_data,sample_data$label != "MCI")
sample_data$label[sample_data$label == "HC"] <- 0
sample_data$label[sample_data$label == "MCI"] <- 0
sample_data$label[sample_data$label == "AD"] <- 1
# Grouping and Filtering
sample_data <- sample_data %>%
  group_by(AIBL.Id) %>%
  filter(row_number() == n())

# Function to identify categorical columns
is_categorical <- function(column) {
  is.character(column) || is.factor(column)
}

# Identify categorical columns
categorical_columns <- sapply(sample_data, is_categorical)

# Replace "NI" with NA only in categorical columns
sample_data <- sample_data %>%
  ungroup() %>%  # Remove grouping
  mutate(across(where(is_categorical), ~ifelse(. == "NI", NA, .)))

# Remove rows with NA values
sample_data <- na.omit(sample_data)

# Identify character columns
char_cols <- sapply(sample_data, is.character)
sample_data[char_cols] <- lapply(sample_data[char_cols], as.factor)
dim(sample_data)
check_data(sample_data)

# Step 2: Split the data by participant
participant_ids <- unique(sample_data$AIBL.Id)

# Step 3: Randomly shuffle the participant IDs
set.seed(123) # for reproducibility
shuffled_ids <- sample(participant_ids)

# Step 4: Calculate the number of participants for each set
total_participants <- length(shuffled_ids)
train_size <- round(0.7 * total_participants)
val_size <- round(0 * total_participants)
test_size <- total_participants - train_size - val_size

# Step 5: Assign participants to sets
train_ids <- shuffled_ids[1:train_size]
val_ids <- shuffled_ids[(train_size + 1):(train_size + val_size)]
test_ids <- shuffled_ids[(train_size + val_size + 1):total_participants]

# Step 6: Subset the data based on assigned participants
train_set <- sample_data[sample_data$AIBL.Id %in% train_ids, ]
##validation_set <- sample_data[sample_data$AIBL.Id %in% val_ids, ]
validation_set <- train_set
test_set <- sample_data[sample_data$AIBL.Id %in% test_ids, ]
# Remove the first column from train_data
train_set <- train_set[, -c(1)]
# Remove the first column from val_data
validation_set <- validation_set[, -c(1)]
# Remove the first column from test_data
test_set <- test_set[, -c(1)]
write.csv(test_set, "test_set.csv", row.names = FALSE)
##################################################################################
# Set seed for reproducibility
library(ggplot2)
library(caret)  # For confusionMatrix
library(pROC)   # For AUC and ROC calculations

# Set working directory
setwd("C:/Users/11373/Desktop")

# Load the data
data <- read.csv("C:/Users/11373/Desktop/updated_participants_with_prob.csv")

# Ensure both Estimated.status and label are factors
data$Estimated.status <- factor(data$Estimated.status)
data$Label <- factor(data$Label)

# Ensure the levels of both factors are consistent
levels(data$Estimated.status) <- levels(data$Label)

# Check the levels to ensure they are the same
if (!all(levels(data$Estimated.status) %in% levels(data$Label)) || !all(levels(data$Label) %in% levels(data$Estimated.status))) {
  stop("The levels of Estimated.status and label do not match.")
}

# Calculate the confusion matrix
conf_matrix <- confusionMatrix(data$Estimated.status, data$Label)

# Print the confusion matrix
print(conf_matrix)

# Calculate AUC for the test set
roc_curve <- roc(data$Label, data$prob)
auc_value <- auc(roc_curve)

# Calculate 95% CI for AUC
ci_auc <- ci.auc(roc_curve)

# Print AUC and 95% CI
print(paste("AUC:", auc_value))
print(paste("95% CI for AUC:", ci_auc[1], "-", ci_auc[3]))

# Create a data frame for ggplot
roc_data <- data.frame(
  specificity = 1 - roc_curve$specificities,
  sensitivity = roc_curve$sensitivities
)

# Create the ROC plot
roc_plot <- ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue", size = 1.5) +
  geom_abline(linetype = "dashed", color = "red") +
  labs(
    title = "Receiver Operating Characteristic Curve",
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.title = element_text(size = 12),
    axis.line = element_line(color = "black"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  annotate("text", x = 0.8, y = 0.2, label = sprintf("AUC = %.3f\n95%% CI = %.3f - %.3f", auc_value, ci_auc[1], ci_auc[3]), size = 5, color = "black", hjust = 0)

# Save the plot as a PNG file with 800 dpi
ggsave("roc_curve.jpg", plot = roc_plot, width = 10, height = 6, bg = "white", dpi = 800)
######################################################################################
label_0_set <- test_set[test_set$label == 0, ]
label_1_set <- test_set[test_set$label == 1, ]
# Randomly select 22 participants with label 0 and 8 participants with label 1
set.seed(123) # Setting seed for reproducibility
selected_label_0 <- label_0_set[sample(nrow(label_0_set), 22), ]
selected_label_1 <- label_1_set[sample(nrow(label_1_set), 8), ]

# Combine the selected participants
random_test_set <- rbind(selected_label_0, selected_label_1)

# Save the randomly selected test set to an Excel file
write.csv(sample_data, "long.csv", row.names = FALSE)
###############################################################

jpeg(filename = "AutoScore_rank_plot1.jpg", width = 12, height = 7, units = "in", res = 800)
ranking <- AutoScore_rank(train_set = train_set, method = "rf", ntree = 200)
dev.off()

nam <- names(ranking)
train_set1 <- cbind(train_set[,names(ranking[1:35])])
train_set1$label <- train_set$label
train_set <- train_set1
validation_set1 <- cbind(validation_set[,names(ranking[1:35])])
validation_set1$label <- validation_set$label
validation_set <- validation_set1
test_set1 <- cbind(test_set[,names(ranking[1:35])])
test_set1$label <- test_set$label
test_set <- test_set1

jpeg(filename = "AutoScore_AUC_plot1.jpg", width = 12, height = 7,units = "in", res = 800)
AUC <- AutoScore_parsimony(
  train_set = train_set, validation_set = validation_set, 
  rank = ranking, max_score = 100, n_min = 1, n_max = 35,
  categorize = "quantile", quantiles = c(0, 0.25, 0.5, 0.75, 1), 
  auc_lim_min = 0.5, auc_lim_max = "adaptive",
  cross_validation = TRUE, fold = 10, do_trace = FALSE
)
dev.off()

num_var <-11
final_variables <- names(ranking[c(1:num_var)])
cut_vec <- AutoScore_weighting( 
  train_set = train_set, validation_set = validation_set,
  final_variables = final_variables, max_score = 100,
  categorize = "quantile", quantiles = c(0, 0.25, 0.5, 0.75, 1),metrics_ci=TRUE
)
##Fine-of the selected factors
cut_vec$GDS <- c(5,10)
cut_vec$Blood.pressure.diastolic. <- c(60,80,90)
cut_vec$Blood.pressure.systolic. <- c(120,150)
scoring_table <- AutoScore_fine_tuning(
  train_set = train_set, validation_set = validation_set, 
  final_variables = final_variables, cut_vec = cut_vec, max_score = 100,metrics_ci=TRUE
)

png(filename = "auc3.png", width = 10, height = 10, units = "in", res = 800)
pred_score <- AutoScore_testing(
  test_set = test_set, final_variables = final_variables, cut_vec = cut_vec,
  scoring_table = scoring_table, threshold = "best", with_label = TRUE
)
dev.off()

test_set$pred <- pred_score$pred_score
write.csv(test_set, "test_set.csv", row.names = FALSE)
conversion_table(pred_score = pred_score, 
                 by = "risk", values = c(0.15,0.25,0.5,0.6,0.75))

conversion_table(pred_score = pred_score, 
                 by = "score", values = c(20,40,55,65))


library(ggplot2)
library(plotly)
library(openxlsx)
glmmodel<-glm(Label~pred_score,data = pred_score,family = binomial(link="logit"))
pred_risk<-predict(glmmodel,newdata=pred_score, type = "response")
# Create pred_score ranging from 1 to 100
pred_score <- 0:100

# Create Label column with all values set to 0
Label <- rep(1, 101)

# Combine pred_score and Label into a dataframe
data <- data.frame(pred_score, Label)
pred_risk1 <- predict(glmmodel,newdata=data, type = "response")
data$risk <- pred_risk1
# Create a ggplot object
scatter_plot <- ggplot(data, aes(x = pred_score, y = pred_risk1)) +
  geom_point() + geom_line() + # Add points
  labs(x = "Score", y = "Risk", title = "Risk Plot")  # Add labels and title
png(filename = "scatter_plot.png", width = 15, height = 10, units = "in", res = 800)
# Convert ggplot object to plotly for interactive features
scatter_plot <- ggplotly(scatter_plot, tooltip = c("x", "y"))
# Print the plot
scatter_plot
dev.off()
write.xlsx(data, "Risk_Data.xlsx")

