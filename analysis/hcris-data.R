# Meta --------------------------------------------------------------------
# Author:        Ian McCarthy
# Date Created:  1/6/2025
# Date Edited:   1/15/2026
# Notes:         R file to extract snippets of HCRIS data for in-class examples


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, purrr)


# Simplified data for single hospitals --------------------------------------------

hcris.emory <- read_tsv("data/input/processed/hcris/HCRIS_Data.txt") %>%
    filter(provider_number %in% c(110010, 110078)) %>%
    select(provider_number, fy_end, name, beds, tot_charges, net_pat_rev, tot_discounts, 
           tot_operating_exp, ip_charges, icu_charges, ancillary_charges, tot_discharges, 
           mcare_discharges, mcaid_discharges, tot_mcare_payment, hrrp_payment, year, uncomp_care)

write_csv(hcris.emory, "data/output/hcris-snippets/hcris-emory.csv")


# Full data for selected years and fewer variables ---------------------------- 
hcris.data <- read_tsv("data/input/processed/hcris/HCRIS_Data.txt") %>%
    filter(year>=2010, year<=2015) %>%
    select(provider_number, year, state, beds, tot_charges, net_pat_rev, tot_discounts, 
           tot_operating_exp, ip_charges, icu_charges, ancillary_charges, tot_discharges, 
           mcare_discharges, mcaid_discharges, tot_mcare_payment, hrrp_payment, uncomp_care) %>%
    mutate(discount_factor = 1-tot_discounts/tot_charges,
           price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
           price_denom = tot_discharges - mcare_discharges,
           price = price_num/price_denom) %>%
    select(provider_number, state, price, mcare_discharges, year, hrrp_payment, beds, tot_mcare_payment, uncomp_care) %>%
       filter(price>=100, price<=500000, beds>30)


# KFF Medicaid Expansion Data -----------------------------------------------
kff.dat <- read_csv('data/input/raw/KFF/KFF_medicaid_expansion_2019.csv') %>%
  mutate(expanded = (`Expansion Status` == 'Adopted and Implemented'),
         Description = str_replace_all(Description,c("\n"='','"'='')))

kff.dat$splitvar <- kff.dat %>% select(Description) %>% as.data.frame() %>%
  separate(Description, sep=" ", into=c(NA, NA, NA, "date"))

kff.final <- kff.dat %>%
  mutate(date_adopted = mdy(splitvar$date)) %>%
  select(state=State, expanded, date_adopted)


# HCRIS merged with KFF data --------------------------------------------

state_xwalk <- tibble(
  state_abb  = state.abb,
  state_name = state.name
)

hcris.mcaid <- hcris.data %>%
  left_join(state_xwalk, by = c("state" = "state_abb")) %>%
  left_join(kff.final, by=c("state_name"="state")) %>%
  filter(! state %in% c("DC", "GU", "PR", "MP", "VI"))

write_csv(hcris.mcaid, "data/output/hcris-snippets/hcris-data.csv")



