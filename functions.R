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
  x <- gsub(" (please state)", "", x)
  x <- gsub("a colleague", "colleague(s)", x)
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

# function to parse the character time estimates into minutes
parse_duration_to_minutes <- function(x) {
  x_clean <- x %>%
    str_trim() %>%
    str_to_lower() %>%
    str_remove_all("\\(.*?\\)") %>%
    str_replace_all(",", " ") %>%
    str_replace_all("\\s*(plus|\\+).*$", "") %>%
    str_replace_all("^over\\s+", "") %>%
    str_squish() %>%
    str_replace_all("^(\\d+\\.?\\d*)$", "\\1m")
  
  sapply(x_clean, function(val) {
    
    # --- NA and empty ---
    if (is.na(val) || val == "") return(NA_real_)
    
    # --- Hard to say → 0 ---
    if (str_detect(val, "hard to say")) return(0)
    
    # --- Zero ---
    if (val %in% c("0", "0m")) return(0)
    
    # --- Mixed range: "45mins to 2 hours" → midpoint ---
    range_mixed <- str_match(val, "(\\d+\\.?\\d*)\\s*(?:mins?|minutes?)\\s+to\\s+(\\d+\\.?\\d*)\\s*h(?:ours?|r?)")
    if (!is.na(range_mixed[1, 1])) {
      lower <- as.numeric(range_mixed[1, 2])
      upper <- as.numeric(range_mixed[1, 3]) * 60
      return(mean(c(lower, upper)))
    }
    
    # --- Minute-only range: "10 - 20 minutes" → midpoint ---
    range_mins <- str_match(val, "(\\d+)\\s*-\\s*(\\d+)\\s*m(?:inutes?|ins?)?")
    if (!is.na(range_mins[1, 1])) {
      return(mean(as.numeric(range_mins[1, 2:3])))
    }
    
    # --- Hour-only range: "1 - 2 hours" → midpoint ---
    range_hrs <- str_match(val, "(\\d+)\\s*-\\s*(\\d+)\\s*h(?:ours?|r?)")
    if (!is.na(range_hrs[1, 1])) {
      return(mean(as.numeric(range_hrs[1, 2:3])) * 60)
    }
    
    # --- "5h 20" style: bare number after hours = minutes ---
    h_bare_match <- str_match(val, "(\\d+\\.?\\d*)\\s*h(?:ours?|r?)\\s+(\\d+)$")
    if (!is.na(h_bare_match[1, 1])) {
      return(as.numeric(h_bare_match[1, 2]) * 60 + as.numeric(h_bare_match[1, 3]))
    }
    
    # --- Standard parsing ---
    hours   <- 0
    minutes <- 0
    
    h_match <- str_match(val, "(\\d+\\.?\\d*)\\s*h(?:ours?|r?)")
    if (!is.na(h_match[1, 2])) hours <- as.numeric(h_match[1, 2])
    
    m_match <- str_match(val, "(\\d+\\.?\\d*)\\s*m(?:inutes?|ins?)?")
    if (!is.na(m_match[1, 2])) minutes <- as.numeric(m_match[1, 2])
    
    total <- hours * 60 + minutes
    
    if (total == 0 && !str_detect(val, "^0")) return(NA_real_)
    
    return(total)
    
  }, USE.NAMES = FALSE)
}
