
# Meta --------------------------------------------------------------------

## Title:  Dartmouth Atlas Data
## Author: Ian McCarthy
## Date Created: 10/15/2019
## Date Edited:  1/4/2022
## Sources:
## https://data.dartmouthatlas.org/mortality/
## https://data.dartmouthatlas.org/medicare-reimbursements/


# Preliminaries -----------------------------------------------------------

mort.data <- map_dfr(2003:2015, ~ {
  read_xls(
    here("data/input/raw/dartmouth-atlas", paste0(.x, "_mortality_hrr.xls")),
    col_names = c("HRR", "HRR Name", "Enrollees",
                  "Total_Mortality", "NonHMO_Mortality"),
    skip = 1
  ) %>%
    mutate(Year = .x)
})

exp.data <- read_csv(here("data/input/raw/dartmouth-atlas/Atlas_Reimbursements_2003-2015.csv"))

# Tidy --------------------------------------------------------------------

exp.data <- pivot_longer(exp.data, cols=starts_with("Y"),
                         names_to="Year", names_prefix="Y",
                         values_to="Expenditures")
exp.data <- exp.data %>%
  rename(HRR=HRR_ID) %>%
  select(HRR, Year, Expenditures) %>%
  mutate(Year=as.integer(Year))

dartmouth.data <- exp.data %>%
  left_join(mort.data, by=c("HRR","Year")) %>%
  filter(complete.cases(.))
