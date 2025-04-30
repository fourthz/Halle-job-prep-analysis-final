# -----------------------------------------------
# flag_large_residuals()
# Purpose: function for flagging large residuals
# Author: Halle E. Prine
# Date: April 30th 2025
# -----------------------------------------------

flag_large_residuals <- function(y, x, p) {
  # Fit linear model
  model <- lm(y ~ x)
  
  # Get absolute residuals
  abs_resid <- abs(residuals(model))
  
  # Get indices of the p largest residuals
  cutoff <- sort(abs_resid, decreasing = TRUE)[p]
  
  # Return logical vector
  abs_resid >= cutoff
}

gpa <- read_csv(here("Data", "Job Prep GPA Data.csv"),
                skip = 5,
                col_types = "cnnfnnnn",
                col_names = c("ID", "school_code", "age", "gender",
                              "s1_credits", "s1_gpa", "s2_credits", "s2_gpa"))

gpa$large_resid <- flag_large_residuals(gpa$s2_gpa, gpa$s1_gpa, p = round(0.10 * nrow(gpa)))
