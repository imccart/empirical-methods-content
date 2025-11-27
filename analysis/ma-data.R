
# Meta --------------------------------------------------------------------
# Author:        Ian McCarthy
# Date Created:  11/10/2025
# Date Edited:   11/22/2025
# Notes:         R file to extract snippets of MA data for in-class examples



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

ga.sa <- sa2022 %>% select(-notes) %>%
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


# Plan characteristics data for 2022 --------------------------------------------

source("analysis/fn_plan_characteristics.R")
y <- 2022

ma.path.a <- paste0("data/input/raw/ma/landscape/Extracted Data/2022LandscapeSource file MA_AtoM 10262021.csv")
ma.data.a <- read_csv(ma.path.a,
                      skip=6,
                      col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                  "drug_type","gap_coverage","drug_type_detail","contractid",
                                  "planid","segmentid","moop","star_rating"),
                      col_types = cols(
                        state = col_character(),
                        county = col_character(),
                        org_name = col_character(),
                        plan_name = col_character(),
                        plan_type = col_character(),
                        premium = col_character(),
                        partd_deductible = col_character(),
                        drug_type = col_character(),
                        gap_coverage = col_character(),
                        drug_type_detail = col_character(),
                        contractid = col_character(),
                        planid = col_double(),
                        segmentid = col_double(),
                        moop = col_character(),
                        star_rating = col_character()
                      )) %>%
  mutate_at(c('premium','partd_deductible'), ~str_replace(.,"-","0")) %>%
  mutate_at(c('premium','partd_deductible'), ~parse_number(.))
  


ma.path.b <- paste0("data/input/raw/ma/landscape/Extracted Data/2022LandscapeSource file MA_NtoW 10262021.csv")
ma.data.b <- read_csv(ma.path.b,
                      skip=6,
                      col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                  "drug_type","gap_coverage","drug_type_detail","contractid",
                                  "planid","segmentid","moop","star_rating"),
                      col_types = cols(
                        state = col_character(),
                        county = col_character(),
                        org_name = col_character(),
                        plan_name = col_character(),
                        plan_type = col_character(),
                        premium = col_character(),
                        partd_deductible = col_character(),
                        drug_type = col_character(),
                        gap_coverage = col_character(),
                        drug_type_detail = col_character(),
                        contractid = col_character(),
                        planid = col_double(),
                        segmentid = col_double(),
                        moop = col_character(),
                        star_rating = col_character()
                      )) %>%
  mutate_at(c('premium','partd_deductible'), ~str_replace(.,"-","0")) %>%
  mutate_at(c('premium','partd_deductible'), ~parse_number(.))


ma.data <- rbind(ma.data.a,ma.data.b)


mapd.path.a <- paste0("data/input/raw/ma/landscape/Extracted Data/PartCD/2022/Medicare Part D 2022 Plan Report 10262021.xls")
mapd.data.a <- read_xls(mapd.path.a,
                        range="A5:Z41314",
                        sheet="Alabama to Montana",
                        col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                    "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                    "national_pdp","premium_partc",
                                    "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                    "partd_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                    "partd_deductible","deductible_exclusions","increase_coverage_limit",
                                    "gap_coverage"))



mapd.path.b <- paste0("data/input/raw/ma/landscape/Extracted Data/PartCD/2022/Medicare Part D 2022 Plan Report 10262021.xls")
mapd.data.b <- read_xls(mapd.path.b,
                        range="A5:Z43732",
                        sheet="Nebraska to Wyoming",
                        col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                    "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                    "national_pdp","premium_partc",
                                    "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                    "partd_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                    "partd_deductible","deductible_exclusions","increase_coverage_limit",
                                    "gap_coverage"))
mapd.data <- rbind(mapd.data.a,mapd.data.b)

final.landscape <- mapd.clean.merge(ma.data=ma.data, mapd.data=mapd.data, y) %>%
  filter(state == "Georgia", planid < 800 | planid > 899) %>%
  ungroup() %>%
  select(contractid, planid, premium, premium_partc, premium_partd=premium_partd_total, year, county)

write_csv(final.landscape, "data/output/ma-snippets/ga-landscape-2022.csv")

# MA Penetration data for 2022 --------------------------------------------

  y <- 2022
  # Month list --------------------------------------------------------------
  monthlist <- if (y == 2008) sprintf("%02d", 6:12) else sprintf("%02d", 1:12)

  # Reader: read numerics as text, then parse --------------------------------
  read_penetration <- function(path) {
    raw <- read_csv(
      path,
      skip = 1,
      col_names = c(
        "state","county","fips_state","fips_cnty","fips",
        "ssa_state","ssa_cnty","ssa","eligibles","enrolled","penetration"
      ),
      # read potential problem columns as character first
      col_types = cols(
        state      = col_character(),
        county     = col_character(),
        fips_state = col_integer(),
        fips_cnty  = col_integer(),
        fips       = col_double(),
        ssa_state  = col_integer(),
        ssa_cnty   = col_integer(),
        ssa        = col_double(),
        eligibles  = col_character(),
        enrolled   = col_character(),
        penetration= col_character()
      ),
      na = c("", "NA", "*", "-", "--"),
      show_col_types = FALSE,
      progress = FALSE
    )

    # robust numeric parsing (handles commas, %, stray text)
    raw %>%
      mutate(
        eligibles   = parse_number(eligibles),
        enrolled    = parse_number(enrolled),
        penetration = parse_number(penetration)
      )
  }

  # One-month loader --------------------------------------------------------
  load_month_pen <- function(m, y) {
    path <- paste0("data/input/raw/ma/penetration/Extracted Data/State_County_Penetration_MA_",y, "_", m, ".csv")

    read_penetration(path) %>%
      mutate(month = as.integer(m), year = y)
  }

  # Read all months, then tidy once ----------------------------------------
  ma.penetration <- map_dfr(monthlist, ~ load_month_pen(.x, y)) %>%
    arrange(state, county, month) %>%
    group_by(state, county) %>%
    fill(fips, .direction = "downup") %>%
    ungroup()

  # Collapse to yearly (safe summaries; avoid NaN/Inf) ----------------------
  final.penetration <- ma.penetration %>%
    group_by(fips, state, county, year) %>%
    arrange(month, .by_group = TRUE) %>%
    summarize(
      n_elig  = sum(!is.na(eligibles)),
      n_enrol = sum(!is.na(enrolled)),

      avg_eligibles   = ifelse(n_elig  > 0, mean(eligibles, na.rm = TRUE), NA_real_),
      sd_eligibles    = ifelse(n_elig  > 1,  sd(eligibles,  na.rm = TRUE), NA_real_),
      min_eligibles   = ifelse(n_elig  > 0, min(eligibles,  na.rm = TRUE), NA_real_),
      max_eligibles   = ifelse(n_elig  > 0, max(eligibles,  na.rm = TRUE), NA_real_),
      first_eligibles = ifelse(n_elig  > 0, first(na.omit(eligibles)),     NA_real_),
      last_eligibles  = ifelse(n_elig  > 0,  last(na.omit(eligibles)),     NA_real_),

      avg_enrolled    = ifelse(n_enrol > 0, mean(enrolled,   na.rm = TRUE), NA_real_),
      sd_enrolled     = ifelse(n_enrol > 1,  sd(enrolled,    na.rm = TRUE), NA_real_),
      min_enrolled    = ifelse(n_enrol > 0, min(enrolled,    na.rm = TRUE), NA_real_),
      max_enrolled    = ifelse(n_enrol > 0, max(enrolled,    na.rm = TRUE), NA_real_),
      first_enrolled  = ifelse(n_enrol > 0, first(na.omit(enrolled)),       NA_real_),
      last_enrolled   = ifelse(n_enrol > 0,  last(na.omit(enrolled)),       NA_real_),

      ssa = last(ssa),
      .groups = "drop"
    )

  ga.penetration.2022 <- final.penetration %>% 
    filter(state == "Georgia") %>%
    ungroup() %>%
    select(fips, county, year, avg_eligibles, avg_enrolled, ssa)

write_csv(ga.penetration.2022, "data/output/ma-snippets/ga-penetration-2022.csv")

# FFS Costs data for 2022 --------------------------------------------

ffs.data <- read_xlsx("data/input/raw/ffs-costs/Extracted Data/FFS2022/FFS22.xlsx",
                  skip=2,
                  col_names=c("ssa","state","county_name","parta_enroll",
                              "parta_reimb","parta_percap","parta_reimb_unadj",
                              "parta_percap_unadj","parta_ime","parta_dsh",
                              "parta_gme","partb_enroll",
                              "partb_reimb","partb_percap"), na=c("*","."))


ga.ffs.costs <- ffs.data %>%
  select(ssa,state,county_name,parta_enroll,parta_reimb,
         partb_enroll,partb_reimb) %>%
  filter(state == "GEORGIA") %>%
  mutate(ssa=as.numeric(ssa)) %>%
  mutate(across(c(parta_enroll, parta_reimb, partb_enroll, partb_reimb),
              ~ parse_number(as.character(.))))

write_csv(ga.ffs.costs, "data/output/ma-snippets/ga-ffs-costs-2022.csv")


# Star Ratings for 2022 --------------------------------------------

source("analysis/rating_variables.R")
ma.path.a <- "data/input/raw/ma/star-ratings/Extracted Star Ratings/2022/2022 Star Ratings Data Table - Measure Stars (Oct 06 2021).csv"
star.data.a <- fread(
  ma.path.a,
  skip = 4,
  stringsAsFactors=FALSE,
  select=c(1:33),
  col.names=rating.vars.2022,
  na = c("", "NA", "*")
) %>%
  mutate(across(
    -any_of(c("contractid","org_type","contract_name","org_marketing","org_parent")),
    ~ parse_number(as.character(.))
  ))


ma.path.b <- "data/input/raw/ma/star-ratings/Extracted Star Ratings/2022/2022 Star Ratings Data Table - Summary Rating (Oct 06 2021).csv"
star.data.b <- fread(
  ma.path.b,
  skip = 2,
  stringsAsFactors=FALSE, 
  select=c(1:11),
  col.names=c("contractid","org_type","contract_name","org_marketing","org_parent","snp","disaster_2019","disaster_2020","partc_score","partd_score","partcd_score"),
  na = c("", "NA", "*")
) %>%
  mutate(
    new_contract=ifelse(partc_score=="Plan too new to be measured",1, ifelse(partcd_score=="Plan too new to be measured",1,0)),
    partc_score  = ifelse(new_contract == 1, NA_real_, parse_number(as.character(partc_score))),
    partcd_score = ifelse(new_contract == 1, NA_real_, parse_number(as.character(partcd_score)))
  ) %>%
  select(contractid, new_contract, partc_score, partcd_score)

ratings.2022 <- as_tibble(star.data.a) %>%
  select(-contract_name, -org_type, -org_marketing) %>%  
  left_join(star.data.b, by=c("contractid")) 

write_csv(ratings.2022, "data/output/ma-snippets/ga-ratings-2022.csv")

# Final GA data ------------------------------------------------------------

ma.data.2022 <- read_tsv("data/input/processed/ma/ma_data_2022.txt") %>%
  filter(state=="GA", planid < 800 | planid > 899, snp=="No",
         !is.na(avg_enrollment)) %>%
  select(contractid, planid, fips, plan_type, partd, avg_enrollment,
         avg_eligibles, avg_enrolled, premium, premium_partc, premium_partd=premium_partd_total,
         rebate_partc, ma_rate, bid, avg_ffscost, partc_score)

write_csv(ma.data.2022, "data/output/ma-snippets/ga-ma-data-2022.csv")
