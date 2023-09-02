#Installing packages
install.packages("haven")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("car")
install.packages("moments")
install.packages("coefplot")
install.packages("corrplot")
install.packages("viridis")
install.packages("gridExtra")
install.packages("purrr")
install.packages("broom")

# Load necessary libraries
library(haven)
library(dplyr)
library(ggplot2)
library(car)
library(moments)
library(coefplot)
library(corrplot)
library(viridis)
library(gridExtra)
library(purrr)
library(broom)
library(grid)


# Setting the working directory to the project folder is achieved automatically by using an .Rproj file saved to the project folder
# Alternatively, the working directory can be set manually using the setwd() function seen below

setwd("/Users/danleeds/Downloads/PRMDS (PSY6009) final project folder") #Dan's laptop


#Load the data file 
data <- read_spss("raw/ATP W42.sav")

#Data cleaning 
#In this data set '99' had been used as a code for "refused to answer"
#The line below replaces all values of '99' with NA in order to make them clear as missing values in the analysis
data[data == 99] <- NA

#Check the number of missing values
summary(is.na(data))

#The data set contains results of 2 surveys, containing different sets of questions, and is indicated within "FORM_W42" variable
#Combining them into one data set has lead to misleading results in the number of missing values.
#Splittng the data set creates two subsets based on the FORM_W42 variable and provides an accurate view of missing values in each subset

# Split data based on FORM_W42
data_form1 <- subset(data, FORM_W42 == 1)
data_form2 <- subset(data, FORM_W42 == 2)

# Data Inspection
# Check the structure of subset data_form1 as this contains all variables of interest
str(data_form1)

# Check the chosen data subset to understand the nature of missing values
summary(data_form1)

# 2. Handle Missing Values
# Creating new subsets for each group of scientist and using listwise deletion to handle missing values; removing rows with any NA values in the columns for variables of interest

# For Medical Research Scientists
data_medical <- data_form1[complete.cases(data_form1[,c("RQ4_F1Aa_W42", "RQ4_F1Ab_W42", "RQ4_F1Ac_W42", "RQ4_F1Ad_W42", "RQ4_F1Ae_W42", "CONFd_F1_W42")]), ]

# For Environmental Research Scientists
data_environmental <- data_form1[complete.cases(data_form1[,c("RQ4_F1Ba_W42", "RQ4_F1Bb_W42", "RQ4_F1Bc_W42", "RQ4_F1Bd_W42", "RQ4_F1Be_W42")]), ]

# For Nutrition Research Scientists
data_nutrition <- data_form1[complete.cases(data_form1[,c("RQ4_F1Ca_W42", "RQ4_F1Cb_W42", "RQ4_F1Cc_W42", "RQ4_F1Cd_W42", "RQ4_F1Ce_W42")]), ]


#Calculate the number of missing values removed by listwise deletion for each scientist group data subset

# Calculate missing values for Medical Research Scientists
missing_values_medical <- nrow(data_form1) - nrow(data_medical)
cat("Missing values for Medical Research Scientists variables:", missing_values_medical, "\n")

# Calculate missing values for Environmental Research Scientists
missing_values_environmental <- nrow(data_form1) - nrow(data_environmental)
cat("Missing values for Environmental Research Scientists variables:", missing_values_environmental, "\n")

# Calculate missing values for Nutrition Research Scientists
missing_values_nutrition <- nrow(data_form1) - nrow(data_nutrition)
cat("Missing values for Nutrition Research Scientists variables:", missing_values_nutrition, "\n")


#calculate percentage of data lost in cleaning

# Calculate percentage of missing values for Medical Research Scientists
percentage_missing_medical <- (missing_values_medical / nrow(data_form1)) * 100
cat("Percentage of missing values for Medical Research Scientists variables:", round(percentage_missing_medical, 2), "%\n")

# Calculate percentage of missing values for Environmental Research Scientists
percentage_missing_environmental <- (missing_values_environmental / nrow(data_form1)) * 100
cat("Percentage of missing values for Environmental Research Scientists variables:", round(percentage_missing_environmental, 2), "%\n")

# Calculate percentage of missing values for Nutrition Research Scientists
percentage_missing_nutrition <- (missing_values_nutrition / nrow(data_form1)) * 100
cat("Percentage of missing values for Nutrition Research Scientists variables:", round(percentage_missing_nutrition, 2), "%\n")



#Further Data Cleaning; Reverse Coding
#For ease of interpretation, Scores on 1 to 4 likert scale are reverse coded creating new variables where higher values correspond to more positive perceptions. 

# For Medical Research Scientists:
data_medical <- data_medical %>%
  mutate(
    med_competence = 5 - RQ4_F1Aa_W42,
    med_comm_bias = 5 - RQ4_F1Ab_W42,
    med_responsibility = 5 - RQ4_F1Ac_W42,
    med_openness = 5 - RQ4_F1Ad_W42,
    med_shared_values = 5 - RQ4_F1Ae_W42,
    med_confidence = 5 - CONFd_F1_W42
  )
# For Environmental Research Scientists:
data_environmental <- data_environmental %>%
  mutate(
    env_competence = 5 - RQ4_F1Ba_W42,
    env_comm_bias = 5 - RQ4_F1Bb_W42,
    env_responsibility = 5 - RQ4_F1Bc_W42,
    env_openness = 5 - RQ4_F1Bd_W42,
    env_shared_values = 5 - RQ4_F1Be_W42
  )

# For Nutrition Research Scientists:
data_nutrition <- data_nutrition %>%
  mutate(
    nut_competence = 5 - RQ4_F1Ca_W42,
    nut_comm_bias = 5 - RQ4_F1Cb_W42,
    nut_responsibility = 5 - RQ4_F1Cc_W42,
    nut_openness = 5 - RQ4_F1Cd_W42,
    nut_shared_values = 5 - RQ4_F1Ce_W42
  )




#Obtain univariate and bivariate statistics for variables of interest

# Defining a function to compute univariate statistics
compute_stats <- function(var_name, data) {
  data %>% 
    summarise(
      Variable = var_name,
      Mean = mean(!!sym(var_name), na.rm = TRUE),
      Median = median(!!sym(var_name), na.rm = TRUE),
      SD = sd(!!sym(var_name), na.rm = TRUE),
      Range = max(!!sym(var_name), na.rm = TRUE) - min(!!sym(var_name), na.rm = TRUE),
      Skewness = skewness(!!sym(var_name), na.rm = TRUE),
      Kurtosis = kurtosis(!!sym(var_name), na.rm = TRUE)
    )
}

#Create lists of variables for each group to use with the compute_stats function

# List of reversed variables for Medical Research Scientists & confidence for medical scientists
variables_medical <- c("med_competence", "med_comm_bias", "med_responsibility", "med_openness", "med_shared_values", "med_confidence")

# Apply the function to each variable in data_medical
ustats_medical <- map_df(variables_medical, ~compute_stats(.x, data_medical))

# List of reversed variables for Environmental Research Scientists
variables_environmental <- c("env_competence", "env_comm_bias", "env_responsibility", "env_openness", "env_shared_values")

# Apply the function to each variable in data_environmental
ustats_environmental <- map_df(variables_environmental, ~compute_stats(.x, data_environmental))

# List of reversed variables for Nutrition Research Scientists
variables_nutrition <- c("nut_competence", "nut_comm_bias", "nut_responsibility", "nut_openness", "nut_shared_values")

# Apply the function to each variable in data_nutrition
ustats_nutrition <- map_df(variables_nutrition, ~compute_stats(.x, data_nutrition))

# View the results
print(ustats_medical)
print(ustats_environmental)
print(ustats_nutrition)

# Sample size for Medical Research Scientists
sample_size_medical <- nrow(data_medical)
cat("Sample size for Medical Research Scientists:", sample_size_medical, "\n")

# Sample size for Environmental Research Scientists
sample_size_environmental <- nrow(data_environmental)
cat("Sample size for Environmental Research Scientists:", sample_size_environmental, "\n")

# Sample size for Nutrition Research Scientists
sample_size_nutrition <- nrow(data_nutrition)
cat("Sample size for Nutrition Research Scientists:", sample_size_nutrition, "\n")

# Create a function to compute "statistical" mode
compute_mode <- function(x) {
  tbl <- table(x)
  mode_values <- as.numeric(names(tbl[tbl == max(tbl)]))
  return(mode_values)
}

# Compute "statistical" mode of variables using function
mode_medical <- sapply(data_medical[variables_medical], compute_mode)
mode_environmental <- sapply(data_environmental[variables_environmental], compute_mode)
mode_nutrition <- sapply(data_nutrition[variables_nutrition], compute_mode)

# Print the "statistical" mode results
print(list(Medical = mode_medical, Environmental = mode_environmental, Nutrition = mode_nutrition))


#Checks before running models
# Linearity
pairs(~med_competence + med_comm_bias + med_responsibility + med_openness + med_shared_values + med_confidence, data = data_medical)
pairs(~med_competence + med_comm_bias + med_responsibility + med_openness + med_shared_values, data = data_medical)
pairs(~env_competence + env_comm_bias + env_responsibility + env_openness + env_shared_values, data = data_environmental)
pairs(~nut_competence + nut_comm_bias + nut_responsibility + nut_openness + nut_shared_values, data = data_nutrition)


#Multicollinearity
cor_matrix_med <- cor(data_medical[,c("med_competence", "med_comm_bias", "med_responsibility", "med_openness", "med_shared_values", "med_confidence")], use = "complete.obs")
print(cor_matrix_med)

cor_matrix_med_2 <- cor(data_medical[,c("med_competence", "med_comm_bias", "med_responsibility", "med_openness", "med_shared_values")], use = "complete.obs")
print(cor_matrix_med_2)

cor_matrix_env <- cor(data_environmental[,c("env_competence", "env_comm_bias", "env_responsibility", "env_openness", "env_shared_values")], use = "complete.obs")
print(cor_matrix_env)

cor_matrix_nut <- cor(data_nutrition[,c("nut_competence", "nut_comm_bias", "nut_responsibility", "nut_openness", "nut_shared_values")], use = "complete.obs")
print(cor_matrix_nut)


vif(lm(med_confidence ~ med_comm_bias + med_responsibility + med_openness + med_shared_values + med_competence, data = data_medical))

vif(lm(med_competence ~ med_comm_bias + med_responsibility + med_openness + med_shared_values, data = data_medical))

vif(lm(env_competence ~ env_comm_bias + env_responsibility + env_openness + env_shared_values, data = data_environmental))

vif(lm(nut_competence ~ nut_comm_bias + nut_responsibility + nut_openness + nut_shared_values, data = data_nutrition))



#visual checks 
corrplot(cor_matrix_med, method = "circle")
corrplot(cor_matrix_med_2, method = "circle")
corrplot(cor_matrix_env, method = "circle")
corrplot(cor_matrix_nut, method = "circle")



#Running models 
#WEIGHT_W42 is the variable containing the weighting for scores - all models here are Weighted multiple linear regression


#Confidence in medical research scientists predicted by competence, openness,Shared Values, Communication Bias, and Responsibility 
lm1 <- lm(med_confidence ~ med_competence + med_comm_bias + med_responsibility + med_openness + med_shared_values, data = data_medical, weights = data_medical$WEIGHT_W42)
summary(lm1)


#Competence predicted by openness,Shared Values, Communication Bias, and Responsibility for each scientist groups 

# For Medical Research Scientists:
lm2 <- lm(med_competence ~ med_comm_bias + med_responsibility + med_openness + med_shared_values, data = data_medical, weights = data_medical$WEIGHT_W42)
summary(lm2)


# For Environmental Research Scientists:
lm3 <- lm(env_competence ~ env_comm_bias + env_responsibility + env_openness + env_shared_values, data = data_environmental, weights = data_environmental$WEIGHT_W42)
summary(lm3)

# For Nutrition Research Scientists:
lm4 <- lm(nut_competence ~ nut_comm_bias + nut_responsibility + nut_openness + nut_shared_values, data = data_nutrition, weights = data_nutrition$WEIGHT_W42)
summary(lm4)



# Model assumption checks
#Homoscedasticity / Independence of Residuals
plot(lm1$fitted.values, lm1$residuals)
abline(h = 0, col = "red")

plot(lm2$fitted.values, lm2$residuals)
abline(h = 0, col = "red")

plot(lm3$fitted.values, lm3$residuals)
abline(h = 0, col = "red")

plot(lm4$fitted.values, lm4$residuals)
abline(h = 0, col = "red")


acf(residuals(lm1))
acf(residuals(lm2))
acf(residuals(lm3))
acf(residuals(lm4))


#Normality
plot(lm1, which = 2) # QQ-plot
plot(lm2, which = 2) # QQ-plot
plot(lm3, which = 2) # QQ-plot
plot(lm4, which = 2) # QQ-plot

shapiro.test(residuals(lm1))
shapiro.test(residuals(lm2))
shapiro.test(residuals(lm3))
shapiro.test(residuals(lm4))


#Outliers -   Cook's distance plot
plot(lm1, which = 5) 
plot(lm2, which = 5)
plot(lm3, which = 5) 
plot(lm4, which = 5) 



#Begin the vizualisation process 

# Remove the intercept from plots for better vizualisation 

# Tidy up the model results so that the intercept doesn't show in plots 
tidy_results_confidence <- tidy(lm1) %>%
  filter(term != "(Intercept)")

tidy_results_lm2 <- tidy(lm2) %>%
  filter(term != "(Intercept)")

tidy_results_lm3 <- tidy(lm3) %>%
  filter(term != "(Intercept)")

tidy_results_lm4 <- tidy(lm4) %>%
  filter(term != "(Intercept)")



#  tidied model - calculating standardised coefficients for viz
tidy_results_confidence$effect_size <- tidy_results_confidence$estimate / sd(data_medical$med_confidence)

#  standardising the coeffiecient for viz
tidy_results_lm2$effect_size <- tidy_results_lm2$estimate / sd(data_medical$med_competence)

#  standardising the coeffiecient for viz
tidy_results_lm3$effect_size <- tidy_results_lm3$estimate / sd(data_environmental$env_competence)

#   standardising the coeffiecient for viz
tidy_results_lm4$effect_size <- tidy_results_lm4$estimate / sd(data_nutrition$nut_competence)



# Plot model of confidence in medical scientists with  standardised coefficients 
plot_lm1xxx <- ggplot(tidy_results_confidence, aes(x = reorder(term, effect_size), y = effect_size)) +
  geom_col(aes(fill = term), show.legend = FALSE) +
  coord_flip() +
  labs(title = "Effect Sizes of Predictors of Confidence in Medical Scientists ", x = "Predictors", y = "Effect Size (Standardized Coefficient)") +
  scale_x_discrete(labels = c("med_competence" = "Medical Competence",
                              "med_comm_bias" = "Communication Bias",
                              "med_responsibility" = "Responsibility",
                              "med_openness" = "Openness",
                              "med_shared_values" = "Shared Values"))+
  theme_minimal()+
  scale_fill_viridis_d(option = "D")   
  
print(plot_lm1xxx)

# Plot for lm2 model using tidied output and with labels for variables and standardised coefficients 
plot_lm2xxx <- ggplot(tidy_results_lm2, aes(x = reorder(term, effect_size), y = effect_size)) +
  geom_col(aes(fill = term), show.legend = FALSE) +
  scale_fill_viridis_d(option = "D") +  
  coord_flip() +
  labs(subtitle = "Medical Research Scientists")+
  scale_x_discrete(labels = c("med_competence" = "Medical Competence",
                              "med_comm_bias" = "Communication Bias",
                              "med_responsibility" = "Responsibility",
                              "med_openness" = "Openness",
                              "med_shared_values" = "Shared Values")) +
  scale_y_continuous(limits = c(-0, 0.7)) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

# Plot for lm3 model using tidied output and with labels for variables and standardised coefficients 
plot_lm3xxx <- ggplot(tidy_results_lm3, aes(x = reorder(term, effect_size), y = effect_size)) +
  geom_col(aes(fill = term), show.legend = FALSE) +
  scale_fill_viridis_d(option = "D") +  
  coord_flip() +
  scale_x_discrete(labels = c("env_competence" = "Environmental Competence",
                              "env_comm_bias" = "Communication Bias",
                              "env_responsibility" = "Responsibility",
                              "env_openness" = "Openness",
                              "env_shared_values" = "Shared Values")) +
  scale_y_continuous(limits = c(-0, 0.7))+
  labs(subtitle = "Environmental Research Scientists")+
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())



# Plot for lm4 model using tidied output and with labels for variables and standardised coefficients 
plot_lm4xxx <- ggplot(tidy_results_lm4, aes(x = reorder(term, effect_size), y = effect_size)) +
  geom_col(aes(fill = term), show.legend = FALSE) +
  scale_fill_viridis_d(option = "D") +  
  coord_flip() +
  scale_x_discrete(labels = c("nut_competence" = "Nutrition Competence",
                              "nut_comm_bias" = "Communication Bias",
                              "nut_responsibility" = "Responsibility",
                              "nut_openness" = "Openness",
                              "nut_shared_values" = "Shared Values")) +
  scale_y_continuous(limits = c(-0, 0.7))+
  labs(subtitle = "Nutrition Research Scientists")+
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())



# Combine the plots for lm2 lm3 lm4 with grid.arrange() and add common x, y labels, and a title
grid.arrange(plot_lm2xxx, plot_lm3xxx, plot_lm4xxx, ncol = 1, 
             top = textGrob("Comparison of Effect Sizes Across Scientist Groups", gp = gpar(fontface = "bold", fontsize = 12)),
             bottom = textGrob("Effect Size (Standardised Coefficient)", gp = gpar(fontface = "bold", fontsize = 10)),
             left = textGrob("Predictors", rot = 90, gp = gpar(fontface = "bold", fontsize = 10)))
