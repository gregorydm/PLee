# Load the tidyverse package
require(tidyverse)

# Import CRF data----
ptlist <- read.csv("3-DataCollection/3-Data/CRF.csv")
  
# Build  function ----
medianPLeeperPEEPlevel <- function(id, datastart) {
  
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
  # Remove all data recorded before the actual start of the experiment
  signals <- signals %>%
    filter(Time > datastart)
  
# Median PLee @ PEEP 15 ----
  PLee_PEEP15 <- signals %>%
    # PEEP 15 = Paw around 15 & flow around zero (i.e. end-expiratory)
    filter(Paw < 16 & Paw > 14 & Flow < 10 & Flow > -10) %>% 
    # Remove outliers using Tukey's rule
    filter(PL > quantile(PL)[2] - 1.5 * IQR(PL) & PL < quantile(PL)[4] + 1.5 * IQR(PL)) %>%
    # Calculate median
    summarise(medianPLee = median(PL)) %>%
    # Add PEEPlevel column
    mutate(PEEPlevel = factor("15", levels = c("15", "10", "5"), ordered = T))
  
  # Median PLee @ PEEP 10 ----
  PLee_PEEP10 <- signals %>%
    # PEEP 10 = Paw around 10 & flow around zero (i.e. end-expiratory)
    filter(Paw < 11 & Paw > 9 & Flow < 10 & Flow > -10) %>% 
    # Remove outliers using Tukey's rule
    filter(PL > quantile(PL)[2] - 1.5 * IQR(PL) & PL < quantile(PL)[4] + 1.5 * IQR(PL)) %>%
    # Calculate median
    summarise(medianPLee = median(PL)) %>%
    # Add PEEPlevel column
    mutate(PEEPlevel = factor("10", levels = c("15", "10", "5"), ordered = T))
  
  # Median PLee @ PEEP 5 ----
  PLee_PEEP5 <- signals %>%
    # PEEP 5 = Paw around 5 & flow around zero (i.e. end-expiratory)
    filter(Paw < 6 & Paw > 4 & Flow < 10 & Flow > -10) %>% 
    # Remove outliers using Tukey's rule
    filter(PL > quantile(PL)[2] - 1.5 * IQR(PL) & PL < quantile(PL)[4] + 1.5 * IQR(PL)) %>%
    # Calculate median
    summarise(medianPLee = median(PL)) %>%
    # Add PEEPlevel column
    mutate(PEEPlevel = factor("5", levels = c("15", "10", "5"), ordered = T))

  # Build output table
  mediansignals <-
    # Combine the calculations
    rbind(PLee_PEEP15, PLee_PEEP10, PLee_PEEP5) %>%
    # Add a column with the participants id
    mutate(id = as.factor(id)) %>%
    # Change order of the columns
    select(id, PEEPlevel, medianPLee)
  
  return(mediansignals)
}

# Apply function ----

# Initiate empty table
db <- NULL

# Apply function for each id in the ptlist table
for (i in (1:nrow(ptlist))) {
  y <- medianPLeeperPEEPlevel(id = ptlist[i,1], datastart = ptlist[i,2])
  db <- rbind(db, y)
}

# Clean up
rm(i, y)

# Show results ----
print(db)

