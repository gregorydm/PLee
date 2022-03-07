# Load the tidyverse package
require(tidyverse)
  
# Define  function ----
medianPLee <- function(id, start = 0, setPEEP) {
  
  # Check inputs and set error messages ----
    if (is.character(id) == F) {stop("'id' must be a character string")}
    if (is.numeric(start) == F) stop("'start' must be a number in seconds (default: 0 seconds)")
    if (start < 0) stop("'start' must be ≥ 0")
    if (is.numeric(setPEEP) == F) stop("'set PEEP' must be a numeric between 0 and 30")
    if (setPEEP < 0 | setPEEP > 30) stop("'set PEEP' must be a numeric between 0 and 30")
    
  # Import signals ----
    
    # Get the file path
    signalsfile <- list.files(path = paste0("3-DataCollection/3-Data/", as.character(id)),
                              pattern = "*Signals.txt", full.names = T)
    
    # Import
    signals <- read_tsv(signalsfile, 
                        col_types = cols(X10 = col_skip()),
                        col_names = FALSE,
                        locale = locale(decimal_mark = ",", grouping_mark = "."),
                        skip = 7)
    
    # Set column names
    colnames(signals) <- (c("Time", "Flow", "Volume", "Paw", "Pes", "PL", "Pga", "Ptdiaf", "CO2"))
    
  # Filter signals ----
    # Remove all data recorded before "start"
    signals <- signals %>%
      filter(Time > start)
    
  # Median PLee ----
    PLee <- signals %>%
      # Paw around setPEEP & flow around zero (i.e. end-expiratory)
      filter(Paw < setPEEP + 1 & Paw > setPEEP - 1 & Flow < 10 & Flow > -10) %>% 
      # Remove outliers using Tukey's rule
      filter(PL > quantile(PL)[2] - 1.5 * IQR(PL) & PL < quantile(PL)[4] + 1.5 * IQR(PL)) %>%
      # Calculate median
      summarise(medianPLee = median(PL)) %>%
      # Add setPEEP column
      mutate(setPEEP = setPEEP) %>%
      # Add id column
      mutate(id = id) %>%
      # Change order of the columns
      select(id, setPEEP, medianPLee)
    
    return(PLee)
}

# Import CRF data ----
# = input data for the function
ptlist <- read.csv("3-DataCollection/3-Data/CRF.csv")

# Apply function ----

# Initiate empty table
db <- NULL

# Apply function for each id+setPEEP combination (row) in the ptlist table
for (i in (1:nrow(ptlist))) {
  y <- medianPLee(id = ptlist[i,1], start = ptlist[i,2], setPEEP = ptlist[i,3])
  db <- rbind(db, y)
}

# Clean up
rm(i, y)

# Show results ----
print(db)