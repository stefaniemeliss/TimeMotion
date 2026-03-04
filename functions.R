#### ALL-PURPOSE HELPER FUNCTIONS ####

# source code
source_code(target_repo = "helper_functions", file_name = "functions.R")

#### PROJECT-SPECIFIC FUNCTIONS ####

# Function to clean and uniform the items
clean_items <- function(x) {
  x <- trimws(x)
  # x <- gsub("([A-Z])(\\d+)", "\\1 \\2", x) # Add space between letter and number
  x <- sub("\\. *$", "", x) # Remove full stop at end
  x <- sub(" e\\.g\\..*$", "", x) # Remove ' e.g.' and everything after
  
  x <- gsub("1-1", "one-to-one", x)
  x <- gsub("Informal conversation with colleague", "Informal conversations with colleague(s)", x)
  x <- gsub("register\\(s\\)", "register", x)
  x <- gsub("Sign", "Signing", x)
  x <- gsub("Attend or deliver", "Attending or delivering", x)
  x <- gsub("conversations", "conversation", x)
  x <- gsub("Parent and carer", "Parent or carer", x)
  x
}


# Create a descriptive statistics table (overall and by group) for inclusion
# in R Markdown outputs. Uses psych::describe and knitr::kable.
table_desc <- function(data = df,
                       group_var = "group",
                       dep_var   = "variable") {
  
  # Combine:
  # 1. Overall descriptives for the dependent variable.
  # 2. Group-specific descriptives, stacked row-wise.
  out <- rbind(
    psych::describe(data[, dep_var]),                                # whole sample
    do.call("rbind",
            psych::describeBy(data[, dep_var],
                              group = data[, group_var]))           # by group
  )
  
  # Remove the 'vars' column (index) from the psych output.
  out$vars <- NULL
  
  # Rename the first row to "all" to indicate whole-sample descriptives.
  rownames(out)[1] <- "all"
  
  # Round statistics to three decimal places for readability.
  out <- round(out, 3)
  
  return(out)
}

