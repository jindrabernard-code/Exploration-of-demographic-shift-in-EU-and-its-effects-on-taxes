# ----------------------------- SETUP -----------------------------
install_and_load <- function(package_names) {
    for (package_name in package_names) {
        if (!require(package_name, character.only = TRUE)) {
            install.packages(package_name, dependencies = TRUE)
            library(package_name, character.only = TRUE)
        }
    }
}

required_packages <- c("readxl", "plm", "lmtest", "car", "outliers",
                       "dplyr", "sandwich", "glmnet", "pglm", "boot")
install_and_load(required_packages)

# ----------------------------- DATA LOAD -----------------------------
file_path <- "C:/Users/Jindřich/Desktop/finaldata.xlsx"
data <- read_excel(file_path, sheet = "List1")

data <- pdata.frame(data, index = c("Country", "Year"))

# ----------------------------- VIF -----------------------------
independent_vars <- c("GDP", "INF", "UNM", "GE", "GNS", "POP", "DEP_15n", "FER_18", "GDP_h", "GFCF")


independent_vars <- independent_vars[independent_vars %in% colnames(data)]


calculate_avg_vif <- function(data, independent_vars) {
    vif_list <- list()

    for (country in unique(data$Country)) {
        country_data <- data[data$Country == country, ]

        # Check if the subset has enough observations
        if (nrow(country_data) > length(independent_vars) + 1) {
            lm_model <- lm(TT ~ ., data = country_data[, c("TT", independent_vars), drop = FALSE])
            vif_values <- car::vif(lm_model)
            vif_list[[country]] <- vif_values
        }
    }

    vif_df <- do.call(rbind, vif_list)

    avg_vif <- colMeans(vif_df, na.rm = TRUE)

    return(avg_vif)
}

avg_vif_values <- calculate_avg_vif(data, independent_vars)
print("Averaged VIF Values by Country:")
print(avg_vif_values)

# ----------------------------- MODELS -----------------------------
refined_vars1 <- c("GDP", "INF", "UNM", "GE", "GNS", "POP", "DEP_15n", "FER_18", "GDP_h", "GFCF")
refined_vars2 <- c("GDP", "INF", "UNM", "GE", "GNS", "POP", "DEP_15n", "FER_18", "GDP_h", "GFCF")
refined_vars3 <- c("GDP", "INF", "UNM", "GE", "GNS", "POP", "DEP_15n", "FER_18", "GDP_h", "GFCF")

if (all(c("INF", "GDP") %in% colnames(data))) {
    data$INF_GDP <- data$INF * data$GDP
}
if (all(c("DEP_15n", "POP") %in% colnames(data))) {
    data$DEP_POP <- data$DEP_15n * data$POP
}

model_tt <- as.formula(paste("TT ~", paste(refined_vars1, collapse = " + ")))
model_dt <- as.formula(paste("DT ~", paste(refined_vars2, collapse = " + ")))
model_it <- as.formula(paste("IT ~", paste(refined_vars3, collapse = " + ")))

# ----------------------------- GRUBBS TEST -----------------------------
print("Grubbs' test for outliers in TT:")
print(grubbs.test(data$TT))

print("Grubbs' test for outliers in DT:")
print(grubbs.test(data$DT))

print("Grubbs' test for outliers in IT:")
print(grubbs.test(data$IT))

# Recreate pdata.frame to avoid issues with plm functions
data <- pdata.frame(data, index = c("Country", "Year"))

# ----------------------------- PE,FE,RE -----------------------------
pooled_tt <- plm(model_tt, data = data, model = "pooling")
fixed_tt <- plm(model_tt, data = data, model = "within")
random_tt <- plm(model_tt, data = data, model = "random")

pooled_dt <- plm(model_dt, data = data, model = "pooling")
fixed_dt <- plm(model_dt, data = data, model = "within")
random_dt <- plm(model_dt, data = data, model = "random")

pooled_it <- plm(model_it, data = data, model = "pooling")
fixed_it <- plm(model_it, data = data, model = "within")
random_it <- plm(model_it, data = data, model = "random")

# ----------------------------- HAC -----------------------------
robust_se <- function(model) {
    return(coeftest(model, vcov = vcovNW(model, type = "HC1")))
}


cat("\n============================")
cat("\nModel Results for TT\n")
cat("============================\n")
cat("\nPooled Model:\n"); print(robust_se(pooled_tt))
cat("\nFixed Effects Model:\n"); print(robust_se(fixed_tt))
cat("\nRandom Effects Model:\n"); print(robust_se(random_tt))

cat("\n============================")
cat("\nModel Results for DT\n")
cat("============================\n")
cat("\nPooled Model:\n"); print(robust_se(pooled_dt))
cat("\nFixed Effects Model:\n"); print(robust_se(fixed_dt))
cat("\nRandom Effects Model:\n"); print(robust_se(random_dt))

cat("\n============================")
cat("\nModel Results for IT\n")
cat("============================\n")
cat("\nPooled Model:\n"); print(robust_se(pooled_it))
cat("\nFixed Effects Model:\n"); print(robust_se(fixed_it))
cat("\nRandom Effects Model:\n"); print(robust_se(random_it))

# ----------------------------- RESET TEST -----------------------------
lm_tt <- lm(TT ~ ., data = data[, c("TT", independent_vars)])
lm_dt <- lm(DT ~ ., data = data[, c("DT", independent_vars)])
lm_it <- lm(IT ~ ., data = data[, c("IT", independent_vars)])

reset_tt <- resettest(lm_tt, power = 2, type = "fitted")
reset_dt <- resettest(lm_dt, power = 2, type = "fitted")
reset_it <- resettest(lm_it, power = 2, type = "fitted")

cat("\nRESET Test for TT Model:\n")
print(reset_tt)

cat("\nRESET Test for DT Model:\n")
print(reset_dt)

cat("\nRESET Test for IT Model:\n")
print(reset_it)


# ----------------------------- DURBIN-WATSON TEST -----------------------------
print("Durbin-Watson Test for TT:")
print(dwtest(lm_model))

print("Durbin-Watson Test for DT:")
print(dwtest(lm(DT ~ ., data = data[, c("DT", independent_vars)])))

print("Durbin-Watson Test for IT:")
print(dwtest(lm(IT ~ ., data = data[, c("IT", independent_vars)])))

# Breusch-Godfrey Test (Higher-order autocorrelation)
print("Breusch-Godfrey Test for TT:")
print(bgtest(lm_model))

print("Breusch-Godfrey Test for DT:")
print(bgtest(lm(DT ~ ., data = data[, c("DT", independent_vars)])))

print("Breusch-Godfrey Test for IT:")
print(bgtest(lm(IT ~ ., data = data[, c("IT", independent_vars)])))

# ----------------------------- HAUSMAN TEST -----------------------------
hausman_tt <- phtest(fixed_tt, random_tt)
hausman_dt <- phtest(fixed_dt, random_dt)
hausman_it <- phtest(fixed_it, random_it)

print(hausman_tt)
print(hausman_dt)
print(hausman_it)

# ----------------------------- BREUSCH-PAGAN TEST -----------------------------
print("Breusch-Pagan Test for TTit:")
print(bptest(pooled_tt))

print("Breusch-Pagan Test for DTit:")
print(bptest(pooled_dt))

print("Breusch-Pagan Test for ITit:")
print(bptest(pooled_it))

# ----------------------------- WOOLBRIDGE TEST -----------------------------

print("Wooldridge Test for serial correlation in TTit:")
print(pwtest(pooled_tt))

print("Wooldridge Test for serial correlation in DTit:")
print(pwtest(pooled_dt))

print("Wooldridge Test for serial correlation in ITit:")
print(pwtest(pooled_it))

# ----------------------------- R² -----------------------------
summary(fixed_tt)$r.squared
summary(fixed_dt)$r.squared
summary(fixed_it)$r.squared

summary(random_tt)$r.squared
summary(random_dt)$r.squared
summary(random_it)$r.squared

summary(pooled_tt)$r.squared
summary(pooled_dt)$r.squared
summary(pooled_it)$r.squared

data$Year <- as.numeric(as.character(data$Year))

Country <- c("country")

# ----------------------------- ELASTIC NET REGRESSION -----------------------------

independent_vars <- independent_vars[independent_vars %in% colnames(data)]

# Convert to cross-sectional data (latest year per country)
cross_sectional_data <- data %>%
    group_by(Country) %>%
    filter(Year == max(Year, na.rm = TRUE)) %>%
    ungroup()

# Check if cross_sectional_data is empty
if (nrow(cross_sectional_data) == 0) {
    stop("Error: cross_sectional_data is empty. Check Year format or missing values.")
}

# Remove any missing values
cross_sectional_data <- na.omit(cross_sectional_data)

# Convert independent variables to numeric matrix
X <- as.matrix(sapply(cross_sectional_data[, independent_vars, drop = FALSE], as.numeric))

# Ensure X has no missing values
if (sum(is.na(X)) > 0) {
    stop("Error: X contains NA values. Please check your dataset.")
}

# Convert dependent variables
Y_tt <- as.numeric(cross_sectional_data$TT)
Y_dt <- as.numeric(cross_sectional_data$DT)
Y_it <- as.numeric(cross_sectional_data$IT)

# Ensure Y variables are valid
if (sum(is.na(Y_tt)) > 0 | sum(is.na(Y_dt)) > 0 | sum(is.na(Y_it)) > 0) {
    stop("Error: Y variables contain NA values. Please clean your dataset.")
}

# Function for Bootstrapped Elastic Net Regression & R² Calculation
perform_elastic_net_r2 <- function(X, Y, n_bootstrap = 1000) {
    set.seed(123)  # Ensure reproducibility
    coefficients_list <- list()
    predictions_list <- matrix(NA, nrow = length(Y), ncol = n_bootstrap)  # Store predictions

    for (i in 1:n_bootstrap) {
        sample_indices <- sample(1:nrow(X), replace = TRUE)  # Bootstrap sampling
        X_sample <- X[sample_indices, , drop = FALSE]
        Y_sample <- Y[sample_indices]

        # Ensure the sample has variation (avoiding singularity issues)
        if (length(unique(Y_sample)) == 1) {
            next  # Skip iteration if all sampled Y values are identical
        }

        cv_model <- cv.glmnet(X_sample, Y_sample, alpha = 0.5, family = "gaussian")  # Elastic Net
        final_model <- glmnet(X_sample, Y_sample, alpha = 0.5, lambda = cv_model$lambda.min)

        coefficients_list[[i]] <- as.numeric(coef(final_model))
        predictions_list[, i] <- predict(final_model, s = cv_model$lambda.min, newx = X)
    }

    # Ensure predictions_list has valid values
    if (all(is.na(predictions_list))) {
        stop("Error: All predictions are NA. Elastic Net might have failed.")
    }

    # Compute mean coefficients over bootstrap iterations
    mean_coefficients <- Reduce("+", coefficients_list) / length(coefficients_list)
    mean_predictions <- rowMeans(predictions_list, na.rm = TRUE)  # Average predictions

    # Compute R²
    ss_total <- sum((Y - mean(Y))^2)
    ss_residual <- sum((Y - mean_predictions)^2)
    r2 <- 1 - (ss_residual / ss_total)

    return(list(coefficients = mean_coefficients, r2 = r2))
}

# Run Bootstrapped Elastic Net Regression with R² Calculation
elastic_tt_results <- perform_elastic_net_r2(X, Y_tt, n_bootstrap = 1000)
elastic_dt_results <- perform_elastic_net_r2(X, Y_dt, n_bootstrap = 1000)
elastic_it_results <- perform_elastic_net_r2(X, Y_it, n_bootstrap = 1000)

# Print Bootstrapped Elastic Net Coefficients and R²
cat("\nBootstrapped Elastic Net Coefficients for TT:\n")
print(elastic_tt_results$coefficients)
cat("\nR² for Bootstrapped Elastic Net TT Model:", elastic_tt_results$r2, "\n")

cat("\nBootstrapped Elastic Net Coefficients for DT:\n")
print(elastic_dt_results$coefficients)
cat("\nR² for Bootstrapped Elastic Net DT Model:", elastic_dt_results$r2, "\n")

cat("\nBootstrapped Elastic Net Coefficients for IT:\n")
print(elastic_it_results$coefficients)
cat("\nR² for Bootstrapped Elastic Net IT Model:", elastic_it_results$r2, "\n")

