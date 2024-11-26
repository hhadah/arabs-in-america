###################################
# Counting Script
# First version: Nov 6, 2024
# Edited: Nov 6th. 2024
###################################

# Upload the data
CPS <- fread(file.path(dropbox_dir,"CPS.csv.gz"))

CPS <- CPS[!is.na(bpl) | !is.na(fbpl) | !is.na(mbpl) |
             !is.na(fbpl_pop) | !is.na(fbpl_mom) |
             !is.na(mbpl_pop) | !is.na(mbpl_mom) |
             !is.na(mbpl) | !is.na(mbpl)]

CPS <- CPS[!is.na(bpl) | !is.na(fbpl) | !is.na(mbpl)]
CPS <- CPS[!is.na(hispan)]
CPS <- CPS[citizen<9]

# Generate Education Variables

CPS[,':=' (DadGradCollege = ifelse(educ99_pop >= 15, 1, 0),
           MomGradCollege = ifelse(educ99_mom >= 15, 1, 0),
           GraduateCollege = ifelse(educ99 >= 15, 1, 0),
           DadProf = ifelse(educ99_pop >= 16, 1, 0),
           MomProf = ifelse(educ99_mom >= 16, 1, 0))]

CPS[,':=' (DadYearEduc = case_when(educ99_pop == 2 ~ 2,
                                   educ99_pop == 10 ~ 4,
                                   educ99_pop == 11 ~ 1,
                                   educ99_pop == 12 ~ 2,
                                   educ99_pop == 13 ~ 3,
                                   educ99_pop == 14 ~ 4,
                                   educ99_pop == 20 ~ 6,
                                   educ99_pop == 21 ~ 5,
                                   educ99_pop == 22 ~ 6,
                                   educ99_pop == 30 ~ 8,
                                   educ99_pop == 30 ~ 7,
                                   educ99_pop == 31 ~ 8,
                                   educ99_pop == 40 ~ 9,
                                   educ99_pop == 50 ~ 10,
                                   educ99_pop == 60 ~ 11,
                                   educ99_pop == 70 ~ 12,
                                   educ99_pop == 71 ~ 12,
                                   educ99_pop == 72 ~ 12,
                                   educ99_pop == 73 ~ 12,
                                   educ99_pop == 80 ~ 13,
                                   educ99_pop == 81 ~ 13,
                                   educ99_pop == 90 ~ 14,
                                   educ99_pop == 91 ~ 14,
                                   educ99_pop == 92 ~ 14,
                                   educ99_pop == 100 ~ 15,
                                   educ99_pop == 110 ~ 16,
                                   educ99_pop == 111 ~ 16,
                                   educ99_pop == 120 ~ 16,
                                   educ99_pop == 121 ~ 16,
                                   educ99_pop == 122 ~ 16,
                                   educ99_pop == 123 ~ 18,
                                   educ99_pop == 124 ~ 21,
                                   educ99_pop == 125 ~ 21),
           MomYearEduc = case_when(educ99_mom == 2 ~ 2,
                                   educ99_mom == 10 ~ 4,
                                   educ99_mom == 11 ~ 1,
                                   educ99_mom == 12 ~ 2,
                                   educ99_mom == 13 ~ 3,
                                   educ99_mom == 14 ~ 4,
                                   educ99_mom == 20 ~ 6,
                                   educ99_mom == 21 ~ 5,
                                   educ99_mom == 22 ~ 6,
                                   educ99_mom == 30 ~ 8,
                                   educ99_mom == 30 ~ 7,
                                   educ99_mom == 31 ~ 8,
                                   educ99_mom == 40 ~ 9,
                                   educ99_mom == 50 ~ 10,
                                   educ99_mom == 60 ~ 11,
                                   educ99_mom == 70 ~ 12,
                                   educ99_mom == 71 ~ 12,
                                   educ99_mom == 72 ~ 12,
                                   educ99_mom == 73 ~ 12,
                                   educ99_mom == 80 ~ 13,
                                   educ99_mom == 81 ~ 13,
                                   educ99_mom == 90 ~ 14,
                                   educ99_mom == 91 ~ 14,
                                   educ99_mom == 92 ~ 14,
                                   educ99_mom == 100 ~ 15,
                                   educ99_mom == 110 ~ 16,
                                   educ99_mom == 111 ~ 16,
                                   educ99_mom == 120 ~ 16,
                                   educ99_mom == 121 ~ 16,
                                   educ99_mom == 122 ~ 16,
                                   educ99_mom == 123 ~ 18,
                                   educ99_mom == 124 ~ 21,
                                   educ99_mom == 125 ~ 21))]

CPS[, LogFamilyEarnings:= case_when(ftotval_pop >0 ~ log(ftotval_pop))]
summary(CPS$LogFamilyEarnings)
CPS[, ':=' (
            Loginctot = case_when(inctot > 0 ~ log(inctot)),
            Logincwage = case_when(incwage > 0 ~ log(incwage)),
            Loghourwage = case_when(hourwage > 0 ~ log(hourwage)),
            Logearnweek = case_when(earnweek > 0 ~ log(earnweek))
            )
    ]
CPS[, ':=' (
            Loginctot_mom = case_when(inctot_mom > 0 ~ log(inctot_mom)),
            Loginctot_pop = case_when(inctot_pop > 0 ~ log(inctot_pop)),
            loghourwage_mom = case_when(hourwage_mom > 0 ~ log(hourwage_mom)),
            logearnweek_mom = case_when(earnweek_mom > 0 ~ log(earnweek_mom)),
            loghourwage_pop = case_when(hourwage_pop > 0 ~ log(hourwage_pop)),
            logearnweek_pop = case_when(earnweek_pop > 0 ~ log(earnweek_pop))
            )
    ]

# First standardize each component
CPS[, ':=' (
    z_DadEduc           = scale(DadYearEduc),
    z_MomEduc           = scale(MomYearEduc),
    z_DadInc            = scale(Loginctot_pop),
    z_MomInc            = scale(Loginctot_mom),
    z_mom_hourwage      = scale(loghourwage_mom),
    z_mom_earnweek      = scale(logearnweek_mom),
    z_pop_hourwage      = scale(loghourwage_pop),
    z_pop_earnweek      = scale(logearnweek_pop),
    z_LogFamilyEarnings = scale(LogFamilyEarnings)
)]

# Create SES score (average of standardized components)
CPS[, SES := rowMeans(cbind(z_DadEduc, z_MomEduc, z_mom_earnweek, z_pop_earnweek), 
                      na.rm = TRUE)]
CPS[, SES_faminc := rowMeans(cbind(z_LogFamilyEarnings), 
                      na.rm = TRUE)]

# Create SES quintiles
CPS[, SES_quintile := cut(SES, 
                         breaks = quantile(SES, probs = seq(0, 1, 0.2), na.rm = TRUE),
                         labels = c("Lowest", "Low", "Middle", "High", "Highest"),
                         include.lowest = TRUE)]

# Create binary high/low SES variable (above/below median)
CPS[, HighSES := as.numeric(SES > median(SES, na.rm = TRUE))]

# Quick summary of the SES variables
summary(CPS$SES)
CrossTable(CPS$SES_quintile, CPS$Type, useNA = "ifany")
table(CPS$HighSES, useNA = "ifany")
CrossTable(CPS$HighSES, CPS$Type, useNA = "ifany")
CrossTable(CPS$HighSES, CPS$Type_Arab, useNA = "ifany")
CrossTable(CPS$HighSES, CPS$Type_Asian, useNA = "ifany")
CrossTable(CPS$Type_Arab, useNA = "ifany")

#------------------------------------------------------------
# PCA
#------------------------------------------------------------

# Load necessary library
install.packages("psych")
library(psych)  # For PCA

# Combine the standardized proxies into a matrix
proxy_matrix <- as.matrix(CPS[, .(z_DadEduc, z_MomEduc, z_mom_earnweek, z_pop_earnweek)])

# Perform PCA
pca_result <- principal(proxy_matrix, nfactors = 1, rotate = "none")

# View the PCA results
print(pca_result)

# Add the SES scores to your data
CPS[, SES_PCA := pca_result$scores]

# Quick summary of the SES variable
summary(CPS$SES_PCA)

#------------------------------------------------------------
# Lubotsky-Wittenberg method
#------------------------------------------------------------
# Function to recode detailed occupation codes into broader categories
create_occ_categories_2010 <- function(data) {
  
  # Copy data to avoid modifying the original
  DT <- copy(data)
  
  # Define occupation categories with their ranges
  occ_categories <- list(
    "Management" = list(start = 10, end = 430),
    "Business_Operations" = list(start = 500, end = 730),
    "Financial" = list(start = 800, end = 950),
    "Computer_Mathematical" = list(start = 1000, end = 1240),
    "Architecture_Engineering" = list(start = 1300, end = 1540),
    "Technicians" = list(start = 1550, end = 1560),
    "Life_Physical_Social_Science" = list(start = 1600, end = 1980),
    "Community_Social_Services" = list(start = 2000, end = 2060),
    "Legal" = list(start = 2100, end = 2150),
    "Education" = list(start = 2200, end = 2550),
    "Arts_Entertainment_Media" = list(start = 2600, end = 2920),
    "Healthcare_Practitioners" = list(start = 3000, end = 3540),
    "Healthcare_Support" = list(start = 3600, end = 3650),
    "Protective_Service" = list(start = 3700, end = 3950),
    "Food_Preparation" = list(start = 4000, end = 4150),
    "Building_Maintenance" = list(start = 4200, end = 4250),
    "Personal_Care" = list(start = 4300, end = 4650),
    "Sales" = list(start = 4700, end = 4965),
    "Office_Admin" = list(start = 5000, end = 5940),
    "Farming_Fishing_Forestry" = list(start = 6005, end = 6130),
    "Construction" = list(start = 6200, end = 6765),
    "Extraction" = list(start = 6800, end = 6940),
    "Installation_Repair" = list(start = 7000, end = 7630),
    "Production" = list(start = 7700, end = 8965),
    "Transportation" = list(start = 9000, end = 9750),
    "Military" = list(start = 9800, end = 9830)
  )
  
  # Function to categorize occupation codes
  categorize_occ <- function(code) {
    if (is.na(code) || code == 9920) return("No_Occupation")
    
    for (cat_name in names(occ_categories)) {
      range <- occ_categories[[cat_name]]
      if (code >= range$start && code <= range$end) {
        return(cat_name)
      }
    }
    return("Other")
  }
  
  # Apply categorization to specific 2010 occupation variables
  occ_vars <- c("occ2010_pop", "occ2010_mom")
  for (var in occ_vars) {
    new_var_name <- paste0(var, "_cat")
    DT[, (new_var_name) := sapply(get(var), categorize_occ)]
    
    # Convert to factor with ordered levels
    DT[, (new_var_name) := factor(get(new_var_name), 
                                  levels = c(names(occ_categories), "No_Occupation", "Other"))]
  }
  
  # Create summary of the recoding
  summary_list <- list()
  for (var in occ_vars) {
    new_var_name <- paste0(var, "_cat")
    summary_list[[new_var_name]] <- table(DT[[new_var_name]], useNA = "ifany")
  }
  
  return(list(
    data = DT,
    category_summary = summary_list
  ))
}

# Recode 2010 occupations
recoded_results <- create_occ_categories_2010(CPS)
CPS <- recoded_results$data

# View the distribution of new categories
print(recoded_results$category_summary)

# create logfamearnweek
CPS[, LogFamEarnWeek := logearnweek_mom + logearnweek_pop]
# Function to implement Lubotsky-Wittenberg method with fixed effects
lw_index <- function(data, 
                     outcome = "LogFamilyEarnings",
                     continuous_proxies = c("DadYearEduc", "MomYearEduc"),
                     categorical_proxies = c("occ2010_pop_cat", "occ2010_mom_cat"),
                     fe_vars = NULL,
                     ref_levels = NULL) {
  
  # Clean input variable names
  continuous_proxies <- trimws(continuous_proxies)
  categorical_proxies <- trimws(categorical_proxies)
  if (!is.null(fe_vars)) fe_vars <- trimws(fe_vars)
  outcome <- trimws(outcome)
  
  # Convert to data.table only if necessary
  if (!is.data.table(data)) {
    DT <- as.data.table(copy(data))
  } else {
    DT <- copy(data)
  }
  
  # First identify complete cases across all variables
  vars_to_check <- c(outcome, continuous_proxies, categorical_proxies)
  if (!is.null(fe_vars)) {
    vars_to_check <- c(vars_to_check, fe_vars)
  }
  
  message("Initial dimensions: ", nrow(DT), " rows")
  complete_cases <- complete.cases(DT[, ..vars_to_check])
  message("Complete cases: ", sum(complete_cases), " rows")
  
  # Subset data to complete cases before any processing
  DT <- DT[complete_cases]
  
  # Standardize continuous variables first
  for (var in c(outcome, continuous_proxies)) {
    DT[, paste0("z_", var) := scale(get(var))]
  }
  
  # Process categorical variables
  dummy_vars <- list()
  if (length(categorical_proxies) > 0) {
    for (var in categorical_proxies) {
      # Ensure factor
      if (!is.factor(DT[[var]])) {
        DT[, (var) := as.factor(get(var))]
      }
      
      # Get levels (excluding reference level if specified)
      levels <- levels(DT[[var]])
      ref_level <- if (!is.null(ref_levels)) {
        ref_levels[which(categorical_proxies == var)]
      } else {
        "No_Occupation"  # Use No_Occupation as default reference level
      }
      
      # Remove reference level from levels
      levels <- levels[levels != ref_level]
      
      # Create dummy variables (without scaling)
      for (level in levels) {
        # Sanitize level names to create syntactically valid variable names
        safe_level <- make.names(level)
        col_name <- paste0(var, "_", safe_level)
        DT[, paste0("z_", col_name) := as.numeric(get(var) == level)]
        dummy_vars[[var]] <- c(dummy_vars[[var]], col_name)
      }
    }
  }
  
  # Create list of all variable names
  all_proxies <- c(continuous_proxies, unlist(dummy_vars))
  z_vars <- paste0("z_", all_proxies)
  
  # Handle fixed effects if specified
  if (!is.null(fe_vars)) {
    message("Processing fixed effects...")
    fe_matrix <- model.matrix(~ . - 1, data = DT[, ..fe_vars])
    residualize <- function(y, X) {
      qr.resid(qr(X), y)
    }
    
    # Residualize outcome and proxy variables
    DT[, paste0("z_", outcome) := residualize(get(paste0("z_", outcome)), fe_matrix)]
    for (var in z_vars) {
      DT[, (var) := residualize(get(var), fe_matrix)]
    }
  }
  
  # Create proxy matrix from residualized variables
  message("Creating proxy matrix...")
  proxy_matrix <- as.matrix(DT[, ..z_vars])
  
  # Check for zero variance columns after residualization
  var_sd <- apply(proxy_matrix, 2, sd)
  zero_var <- var_sd == 0
  if (any(zero_var)) {
    message("Removing zero variance columns: ", 
            paste(z_vars[zero_var], collapse = ", "))
    z_vars <- z_vars[!zero_var]
    proxy_matrix <- proxy_matrix[, !zero_var, drop = FALSE]
  }
  
  # Create formula for feols
  formula_str <- if (!is.null(fe_vars)) {
    paste0("z_", outcome, " ~ ", paste(z_vars, collapse = " + "), 
           " | ", paste(fe_vars, collapse = " + "))
  } else {
    paste0("z_", outcome, " ~ ", paste(z_vars, collapse = " + "))
  }
  
  message("Final number of observations: ", nrow(DT))
  message("Number of variables in regression: ", length(z_vars))
  
  # Run regression
  message("Running regression...")
  main_reg <- feols(as.formula(formula_str), data = DT)
  
  # Get variables actually used in regression (excluding collinear ones)
  used_vars <- names(coef(main_reg))
  collinear_vars <- main_reg$collin.var
  
  # Exclude collinear variables from used_vars
  if (!is.null(collinear_vars)) {
    message("Variables removed due to collinearity: ", paste(collinear_vars, collapse = ", "))
    used_vars <- setdiff(used_vars, collinear_vars)
  }
  
  message("Variables used in regression after collinearity check: ", length(used_vars))
  
  # Ensure variable names are syntactically valid in proxy_matrix
  colnames(proxy_matrix) <- make.names(colnames(proxy_matrix))
  used_vars <- make.names(used_vars)
  
  # Adjust used_vars to match proxy_matrix columns
  common_vars <- intersect(used_vars, colnames(proxy_matrix))
  if (length(common_vars) != length(used_vars)) {
    missing_vars <- setdiff(used_vars, colnames(proxy_matrix))
    if (length(missing_vars) > 0) {
      message("Variables in used_vars not found in proxy_matrix: ", paste(missing_vars, collapse = ", "))
    }
  }
  used_vars <- common_vars
  
  # Update proxy_matrix to only include used variables
  proxy_matrix <- proxy_matrix[, used_vars, drop = FALSE]
  
  # Calculate correlations and coefficients
  message("Calculating weights...")
  proxy_cors <- cor(proxy_matrix)
  beta_hat <- coef(main_reg)[used_vars]
  
  # Calculate weights
  w_lw <- solve(proxy_cors, beta_hat)
  w_lw_norm <- w_lw / sum(w_lw)
  
  # Calculate index
  lw_index <- as.vector(proxy_matrix %*% w_lw_norm)
  
  # Return results
  results <- list(
    weights = setNames(as.vector(w_lw_norm), used_vars),
    index = lw_index,
    regression = main_reg,
    proxy_correlations = proxy_cors,
    n_observations = nrow(DT),
    used_variables = used_vars
  )
  
  return(results)
}

# Run the LW method with occupational variables
results <- lw_index(CPS,
                   outcome = "LogFamEarnWeek",
                   continuous_proxies = c("DadYearEduc", "MomYearEduc"), 
                   categorical_proxies = c("occ2010_pop_cat", "occ2010_mom_cat"))

# View results
print(results$weights)
summary(results$regression)

# Step 2: Identify the variables used to determine complete cases
vars_to_check <- c("LogFamEarnWeek", "DadYearEduc", "MomYearEduc", 
                   "occ2010_pop_cat", "occ2010_mom_cat")

# Step 3: Identify the rows with complete cases in the CPS data
complete_cases <- complete.cases(CPS[, ..vars_to_check])

# Step 4: Add the index to the CPS data
# Initialize the ses_lw variable with NA
CPS[, ses_lw := NA_real_]

# Assign the index values to the complete cases
CPS[complete_cases, ses_lw := results$index]

# Now, the CPS dataset has the index added to it
# You can check the summary of the new index variable
summary(CPS$ses_lw)

# Plot the distribution of the index
hist(CPS$ses_lw, main = "Distribution of LW Index", xlab = "LW Index")

# Standardize the ses_lw index
CPS[, ses_lw_std := scale(ses_lw)]

# View the summary statistics of the standardized index
summary(CPS$ses_lw_std)

# Plot the distribution of the index
hist(CPS$ses_lw_std, main = "Distribution of LW Index", xlab = "LW Index")

# Save the data
fwrite(CPS, file.path(dropbox_dir, 'CPS_SES_VAR.csv.gz'))
