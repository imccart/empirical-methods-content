
# Meta --------------------------------------------------------------------
# Author:        Ian McCarthy
# Date Created:  11/10/2025
# Date Edited:   11/10/2025
# Notes:         R file to extract snippets of MA data for google colab work



# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, purrr)


# Simplified data for single year and month --------------------------------------------

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


ga.enroll <- enroll202201 %>%
    filter(state == "GA", planid<800 | planid>899, !is.na(enrollment))

ga.sa <- sa2022 %>%
    filter(state == "GA") %>%
    inner_join(ga.enroll %>% select(contractid) %>% distinct(), 
               by = "contractid")

ga.contract <- contract202201 %>%
    inner_join(ga.enroll %>% select(contractid, planid) %>% distinct(), 
               by = c("contractid","planid"))

write_csv(ga.enroll, "data/output/ma-snippets/ga-enrollment.csv")
write_csv(ga.sa, "data/output/ma-snippets/ga-service-area.csv")
write_csv(ga.contract, "data/output/ma-snippets/ga-contract.csv")



# Expanded data for multiple months --------------------------------------------

## Helper function
build_ga_snippets <- function(year, month) {

  mm     <- sprintf("%02d", month)
  ym_tag <- paste0(year, "-", mm)

  # Paths to raw files
  enroll_path   <- paste0(
    "data/input/raw/ma/enrollment/Extracted Data/CPSC_Enrollment_Info_",
    year, "_", mm, ".csv"
  )
  contract_path <- paste0(
    "data/input/raw/ma/enrollment/Extracted Data/CPSC_Contract_Info_",
    year, "_", mm, ".csv"
  )
  sa_path       <- paste0(
    "data/input/raw/ma/service-area/Extracted Data/MA_Cnty_SA_",
    year, "_", mm, ".csv"
  )

  # Enrollment data -------------------------------------------------------
  enroll <- read_csv(
    enroll_path,
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
    na = "*"
  )

  # Contract data ---------------------------------------------------------
  contract <- read_csv(
    contract_path,
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
    )
  )

  # Service area data -----------------------------------------------------
  sa <- read_csv(
    sa_path,
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
    na = "*"
  )

  # Limit to Georgia ------------------------------------------------------
  ga.enroll <- enroll %>%
    filter(state == "GA", planid < 800 | planid > 899, !is.na(enrollment))

  ga.sa <- sa %>%
    filter(state == "GA") %>%
    inner_join(
      ga.enroll %>% select(contractid) %>% distinct(),
      by = "contractid"
    )

  ga.contract <- contract %>%
    inner_join(
      ga.enroll %>% select(contractid, planid) %>% distinct(),
      by = c("contractid", "planid")
    )

  # Output paths ----------------------------------------------------------
  enroll_out   <- paste0("data/output/ma-snippets/ga-enrollment-", ym_tag, ".csv")
  sa_out       <- paste0("data/output/ma-snippets/ga-service-area-", ym_tag, ".csv")
  contract_out <- paste0("data/output/ma-snippets/ga-contract-", ym_tag, ".csv")

  # Save outputs ----------------------------------------------------------
  write_csv(ga.enroll,   enroll_out)
  write_csv(ga.sa,       sa_out)
  write_csv(ga.contract, contract_out)
}

# Build snippets for 2022-01 to 2022-06 and 2023-01 to 2023-06 ------------
years  <- c(2022, 2023)
months <- 1:3

for (yr in years) {
  for (mo in months) {
    build_ga_snippets(yr, mo)
  }
}
