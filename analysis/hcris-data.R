# Meta --------------------------------------------------------------------
# Author:        Ian McCarthy
# Date Created:  1/6/2025
# Date Edited:   1/6/2025
# Notes:         R file to extract snippets of HCRIS data for in-class examples


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, purrr)


# Simplified data for single hospitals --------------------------------------------

hcris.emory <- read_tsv("data/input/processed/hcris/HCRIS_Data.txt") %>%
    filter(provider_number %in% c(110010, 110078)) %>%
    select(provider_number, fy_end, name, beds, tot_charges, net_pat_rev, tot_discounts, 
           tot_operating_exp, ip_charges, icu_charges, ancillary_charges, tot_discharges, 
           mcare_discharges, mcaid_discharges, tot_mcare_payment, hrrp_payment, year)

write_csv(hcris.emory, "data/output/hcris-snippets/hcris-emory.csv")


# Full data for selected years and fewer variables ---------------------------- 
hcris.data <- read_tsv("data/input/processed/hcris/HCRIS_Data.txt") %>%
    filter(year>=2010, year<=2015) %>%
    select(provider_number, year, beds, tot_charges, net_pat_rev, tot_discounts, 
           tot_operating_exp, ip_charges, icu_charges, ancillary_charges, tot_discharges, 
           mcare_discharges, mcaid_discharges, tot_mcare_payment, hrrp_payment) %>%
    mutate(discount_factor = 1-tot_discounts/tot_charges,
           price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
           price_denom = tot_discharges - mcare_discharges,
           price = price_num/price_denom) %>%
    select(provider_number, price, mcare_discharges, year, hrrp_payment, beds, tot_mcare_payment)

write_csv(hcris.data, "data/output/hcris-snippets/hcris-data.csv")    
 

