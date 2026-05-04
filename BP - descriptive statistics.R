# ----------------------------- SETUP -----------------------------
install_and_load <- function(package_names) {
    for (package_name in package_names) {
        if (!require(package_name, character.only = TRUE)) {
            install.packages(package_name, dependencies = TRUE)
            library(package_name, character.only = TRUE)
        }
    }
}


required_packages <- c("readxl", "dplyr", "moments", "ggplot2", "corrplot")
install_and_load(required_packages)

# ----------------------------- DATA LOAD -----------------------------
file_path <- "C:/Users/Jindřich/Desktop/BP - data.xlsx"
data <- read_excel(file_path, sheet = "List1")

# Convert to a data frame
data <- as.data.frame(data)

# Exclude 'Year' variable (assuming column name is 'Year')
data <- data %>% select(-Year)

# ----------------------------- DESCRIPTIVE STATISTICS -----------------------------
compute_descriptive_stats <- function(df) {
    numeric_vars <- df[, sapply(df, is.numeric)]  # Select numeric columns (excluding Year)
    stats <- data.frame(
        Variable = colnames(numeric_vars),
        Mean = sapply(numeric_vars, mean, na.rm = TRUE),
        Median = sapply(numeric_vars, median, na.rm = TRUE),
        Min = sapply(numeric_vars, min, na.rm = TRUE),
        Max = sapply(numeric_vars, max, na.rm = TRUE),
        SD = sapply(numeric_vars, sd, na.rm = TRUE),
        Variance = sapply(numeric_vars, var, na.rm = TRUE),
        Skewness = sapply(numeric_vars, skewness, na.rm = TRUE),
        Kurtosis = sapply(numeric_vars, kurtosis, na.rm = TRUE),
        Missing_Values = sapply(numeric_vars, function(x) sum(is.na(x)))
    )
    return(stats)
}

# Compute descriptive statistics
descriptive_stats <- compute_descriptive_stats(data)

# ----------------------------- RESULTS -----------------------------
print(descriptive_stats)

