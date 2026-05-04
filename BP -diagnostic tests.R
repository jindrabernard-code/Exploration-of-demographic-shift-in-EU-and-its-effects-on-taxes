# ----------------------------- SETUP -----------------------------
required_packages <- c("plm", "dplyr", "readxl", "tseries", "urca", "pdR",
                       "lmtest", "sandwich", "car", "stargazer", "pcse",
                       "pscl", "corrplot")

for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
lapply(required_packages, library, character.only = TRUE)


# ----------------------------- DATA LOAD -----------------------------
file_path <- "C:/Users/Jindřich/Desktop/finaldata.xlsx"
panel_data <- read_excel(file_path)
colnames(panel_data) <- make.names(colnames(panel_data), unique = TRUE)
panel_data <- na.omit(panel_data)
pdata <- pdata.frame(panel_data, index = c("Country", "Year"))
variables_to_test <- setdiff(colnames(panel_data), c("EMU", "Country", "Year"))


# ------------------------ STATIONARITY TESTS ------------------------
perform_stationarity_tests <- function(pdata, var) {
    ts_data <- na.omit(as.numeric(pdata[[var]]))
    if (length(unique(ts_data)) <= 1) return(NULL)
    test_results <- list()
    try({ test_results$ADF_pval <- adf.test(ts_data, alternative = "stationary", k = 1)$p.value }, silent = TRUE)
    try({ test_results$KPSS_pval <- kpss.test(ts_data, null = "Level")$p.value }, silent = TRUE)
    try({
        llc_test <- purtest(pdata[[var]], test = "levinlin", exo = "intercept", lags = 2)
        test_results$LLC_stat <- summary(llc_test)$statistic
        test_results$LLC_pval <- summary(llc_test)$pvalue
    }, silent = TRUE)
    try({
        ips_test <- purtest(pdata[[var]], test = "ips", exo = "intercept", lags = 2)
        test_results$IPS_stat <- summary(ips_test)$statistic
        test_results$IPS_pval <- summary(ips_test)$pvalue
    }, silent = TRUE)
    return(test_results)
}

perform_hadri_test <- function(pdata, var) {
    panel_var <- as.numeric(na.omit(pdata[[var]]))
    panel_index <- index(pdata)
    country_index <- panel_index[, "Country"]
    time_index <- panel_index[, "Year"]
    if (length(unique(country_index)) > 1 && length(unique(time_index)) > 1) {
        residuals_list <- list()
        for (country in unique(country_index)) {
            country_data <- panel_var[country_index == country]
            if (length(country_data) > 1) {
                time_var <- as.numeric(as.factor(time_index[country_index == country]))
                model <- lm(country_data ~ time_var)
                residuals_list[[country]] <- residuals(model)
            }
        }
        residuals_all <- unlist(residuals_list)
        if (length(residuals_all) == 0) return(list(Hadri_stat = NA, Hadri_pval = NA))
        sigma_squared <- var(residuals_all)
        N <- length(unique(country_index))
        T <- length(unique(time_index))
        hadri_stat <- sum(residuals_all^2) / (N * T * sigma_squared)
        p_value <- 2 * (1 - pnorm(abs(hadri_stat)))
        return(list(Hadri_stat = hadri_stat, Hadri_pval = p_value))
    }
    return(list(Hadri_stat = NA, Hadri_pval = NA))
}

results_summary <- list()
for (var in variables_to_test) {
    stationarity_results <- perform_stationarity_tests(pdata, var)
    if (!is.null(stationarity_results)) {
        hadri_results <- perform_hadri_test(pdata, var)
        stationarity_results$Hadri_stat <- hadri_results$Hadri_stat
        stationarity_results$Hadri_pval <- hadri_results$Hadri_pval
        results_summary[[var]] <- stationarity_results
    }
}

max_col_length <- max(sapply(results_summary, function(x) length(unlist(x))))
results_summary <- lapply(results_summary, function(x) {
    if (length(unlist(x)) < max_col_length) {
        missing_cols <- max_col_length - length(unlist(x))
        return(c(x, rep(NA, missing_cols)))
    } else {
        return(x)
    }
})
results_table <- do.call(rbind, lapply(names(results_summary), function(var) {
    cbind(Variable = var, as.data.frame(t(results_summary[[var]])))
}))
results_table <- as.data.frame(results_table)
rownames(results_table) <- NULL


# -------------------- CORRELATION MATRIX --------------------
cor_data <- panel_data[, variables_to_test]
cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "lower", tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7)


# -------------------- CUMULATIVE (Pooled) VIF --------------------
cat("\n--- Cumulative VIF (All Pooled Data) ---\n")
vif_formula <- as.formula("TT ~ GDP + GE + INF + UNM + GNS + GDP_h + GFCF + DT + IT + DEP_15n + FER_18")
pooled_model <- lm(vif_formula, data = panel_data)
pooled_vif <- vif(pooled_model)
print(pooled_vif)


# -------------------- COUNTRY-SPECIFIC VIF (REPAIRED) --------------------
cat("\n--- Country-Specific VIF Calculation ---\n")

expected_vars <- all.vars(vif_formula)[-1]  # exclude TT
valid_vif_models <- list()
unique_countries <- unique(panel_data$Country)

for (country in unique_countries) {
    data_country <- panel_data[panel_data$Country == country, ]
    model <- tryCatch({ lm(vif_formula, data = data_country) }, error = function(e) return(NULL))
    if (!is.null(model)) {
        vif_vals <- vif(model)
        if (all(expected_vars %in% names(vif_vals))) {
            valid_vif_models[[country]] <- vif_vals[expected_vars]
        }
    }
}

vif_df <- do.call(rbind, lapply(names(valid_vif_models), function(cntry) {
    data.frame(Country = cntry, t(valid_vif_models[[cntry]]))
}))

vif_summary <- data.frame(
    Variable = expected_vars,
    Mean_VIF = sapply(expected_vars, function(var) mean(sapply(valid_vif_models, function(x) x[[var]]), na.rm = TRUE)),
    Max_VIF  = sapply(expected_vars, function(var) max(sapply(valid_vif_models, function(x) x[[var]]), na.rm = TRUE))
)

cat("\n--- Country-Specific VIF Summary (Mean & Max) ---\n")
print(vif_summary, row.names = FALSE)

cat("\n--- Detailed VIFs by Country (First 6 Rows) ---\n")
print(head(vif_df))


# -------------------- PANEL DIAGNOSTIC TESTS --------------------
fe_model <- plm(TT ~ GDP + GE + INF + UNM + GNS + GDP_h + GFCF + DEP_15n + FER_18, data = pdata, model = "within")
wooldridge_test <- pwartest(TT ~ GDP + GE + INF + UNM + GNS + GDP_h + GFCF + DEP_15n + FER_18, data = pdata)
wald_test <- pwaldtest(fe_model)
pesaran_test <- pcdtest(fe_model, test = "cd")


# -------------------- STORE AND PRINT RESULTS --------------------
diagnostic_results <- list(
    Stationarity_Tests = results_table,
    Correlation_Matrix = cor_matrix,
    Pooled_VIF = pooled_vif,
    VIF_Summary = vif_summary,
    VIF_By_Country = vif_df,
    Wooldridge_Test = list(statistic = wooldridge_test$statistic, p_value = wooldridge_test$p.value),
    Wald_Test = list(statistic = wald_test$statistic, p_value = wald_test$p.value),
    Pesaran_CD_Test = list(statistic = pesaran_test$statistic, p_value = pesaran_test$p.value)
)

cat("\n========== ALL DIAGNOSTIC RESULTS ==========\n")
cat("\n--- Wooldridge Test ---\n")
print(diagnostic_results$Wooldridge_Test)

cat("\n--- Wald Test ---\n")
print(diagnostic_results$Wald_Test)

cat("\n--- Pesaran CD Test ---\n")
print(diagnostic_results$Pesaran_CD_Test)

cat("\n--- Cumulative VIF ---\n")
print(diagnostic_results$Pooled_VIF)

cat("\n--- VIF Summary (Mean & Max Across Countries) ---\n")
print(diagnostic_results$VIF_Summary, row.names = FALSE)

cat("\n--- Full Country-Specific VIF Table ---\n")
print(diagnostic_results$VIF_By_Country, row.names = FALSE)

cat("\n--- Full Stationarity Test Results ---\n")
print(diagnostic_results$Stationarity_Tests, row.names = FALSE)



