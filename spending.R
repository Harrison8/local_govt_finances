library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(magrittr)

# Read in microdata and government labels. Join the data frames.

finances <- read_fwf("2016FinEstDAT_12042018modp_pu.txt", 
                     fwf_widths(c(2, 1, 3, 3, 5, 3, 12, 4, 1),
                                c("state", "type", "county", "unit_id", "subset", "item", "amount", "year", "imputation")))

finances %<>% mutate(id = paste0(state, type, county, unit_id, subset))

finances_names <- read_fwf("Fin_GID_2016.txt",
                           fwf_widths(c(14, 64, 35),
                                      c("id", "name", "county_name")))

finances %<>% left_join(finances_names, by = "id")


# Read in revenue and expense codes

summary_codes <- readxl::read_excel("methodology_for_summary_tabulations.xls", skip = 1)

summary_codes %<>%
  select(-Line, -...9, -...10, -...11) %>%
  mutate_at(vars(Description:...8), replace_na, "..") %>%
  mutate(category = paste0(Description, ...3, ...4, ...5, ...6, ...7, ...8),
         item_codes = str_split(`Item Codes`, ", "))

summary_code

# Subset finances df to Louisville and summarise by revenue and expenses. 

finances %<>% 
  filter(
    state == "18",
    county == "056") %>%
  mutate(
    category = if_else(item %in% summary_codes[[1,"item_codes"]], "revenue", "other"),
    category = replace(category, item %in% summary_codes[[54,"item_codes"]], "expense"))

finances %<>%
  group_by(name, category) %>%
  summarise(total = sum(amount))

finances %<>%
  mutate(total = scales::comma(total * 1000))
