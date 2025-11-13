
# Meta --------------------------------------------------------------------
# Author:        Ian McCarthy
# Date Created:  11/10/2025
# Date Edited:   11/10/2025
# Notes:         R file to extract snippets of MA data for google colab work



# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

# Load Data ---------------------------------------------------------------

## Enrollment data for January, 2022
enroll202201 <- read_csv("data/input/raw/ma/enrollment/Extracted Data/CPSC_Enrollment_Info_2022_01.csv",
      skip = 1,
      col_names = c("contractid","planid","ssa","fips","state","county","enrollment"),
      col_types = cols(
        contractid = col_character(),
        planid     = col_double(),
        ssa        = col_double(),
        fips       = col_double(),
        state      = col_character(),
        county     = col_character(),
        enrollment = col_double()
      ),
      na = "*")


contract202201 <- read_csv("data/input/raw/ma/enrollment/Extracted Data/CPSC_Contract_Info_2022_01.csv",
      skip = 1,
      col_names = c(
        "contractid","planid","org_type","plan_type","partd","snp","eghp",
        "org_name","org_marketing_name","plan_name","parent_org","contract_date"
      ),
      col_types = cols(
        contractid = col_character(),
        planid     = col_double(),
        org_type   = col_character(),
        plan_type  = col_character(),
        partd      = col_character(),
        snp        = col_character(),
        eghp       = col_character(),
        org_name   = col_character(),
        org_marketing_name = col_character(),
        plan_name  = col_character(),
        parent_org = col_character(),
        contract_date = col_character()
      ))


## Service area file for 2022
sa2022 <- read_csv("data/input/raw/ma/service-area/Extracted Data/MA_Cnty_SA_2022_01.csv",
    skip = 1,
    col_names = c(
        "contractid","org_name","org_type","plan_type","partial","eghp",
        "ssa","fips","county","state","notes"
      ),
      col_types = cols(
        contractid = col_character(),
        org_name   = col_character(),
        org_type   = col_character(),
        plan_type  = col_character(),
        partial    = col_logical(),
        eghp       = col_character(),
        ssa        = col_double(),
        fips       = col_double(),
        county     = col_character(),
        state      = col_character(),
        notes      = col_character()
      ),
      na = "*")



# Limit to Georgia ------------------------------------------------------------

ga.enroll <- enroll202201 %>%
    filter(state == "GA", planid<800 | planid>899, !is.na(enrollment))

ga.sa <- sa2022 %>%
    filter(state == "GA") %>%
    inner_join(ga.enroll %>% select(contractid) %>% distinct(), 
               by = "contractid")

ga.contract <- contract202201 %>%
    inner_join(ga.enroll %>% select(contractid, planid) %>% distinct(), 
               by = c("contractid","planid"))

# Save outputs ------------------------------------------------------------

write_csv(ga.enroll, "data/output/ma-snippets/ga-enrollment-202201.csv")
write_csv(ga.sa, "data/output/ma-snippets/ga-service-area-202201.csv")
write_csv(ga.contract, "data/output/ma-snippets/ga-contract-202201.csv")