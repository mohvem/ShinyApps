library(shiny)
library(tidyverse)
library(stringr)
library(lubridate)
library(readr)
library(ggplot2)
library(plotly)
library(DT)
rm(list =ls())
setwd("/Users/mohinivembu/Documents/mutual-funds-and-etfs")
# Load Data ---------------------------------------------------------------
mf <- read_csv("Mutual Funds.csv")
etf <- read_csv("ETFs.csv")
# Create Analysis Datasets ------------------------------------------------

#### Portfolio Names ---------------------------------------------------------
port_names <- mf %>% 
  select(fund_extended_name) %>% 
  mutate(port_name = ifelse(str_detect(pattern = "Fund II", fund_extended_name),
                            str_replace_all(pattern = "(?<=Fund II).*", fund_extended_name, ""),
                            str_replace_all(string = fund_extended_name, 
                                            "(?<=Fund ).*|Class I-2|I-2|Class [[:digit:]]|Class [[:alpha:]]|Advisor Class|Institutional|Institutional Class|Value Adv|Value Inv", "")),
         port_name = ifelse(str_detect(pattern = "Equity A|Equity C", fund_extended_name),
                            str_replace_all(string = fund_extended_name, 
                                            "Equity A|Equity C", "Equity"), port_name),
         port_name = ifelse(str_detect(pattern = "Strat [[:alpha:]]", fund_extended_name),
                            str_replace_all(string = fund_extended_name, 
                                            "Strat [[:alpha:]]*", "Strat"), port_name),
         port_name = ifelse(str_detect(pattern = "AB High Yield", fund_extended_name),"AB High Yield", port_name),
         port_name = ifelse(str_detect(pattern = "Alger Mid Cap Focus", fund_extended_name),"Alger Mid Cap Focus", port_name)) %>% 
  arrange(port_name) ### still need to look and see if this is implemented right 
### Asset Allocation --------------------------------------------------------
alloc <- mf %>% 
  select(fund_name, fund_extended_name, currency, category, size, net_assets, investment,fund_family,
         inception_date, basic_materials, consumer_cyclical,financial_services,real_estate,
         consumer_defensive,healthcare, utilities, communication_services,energy,
         industrials, technology, starts_with("portfolio_")) %>% 
  rowwise() %>% 
  mutate(total_alloc = round(sum(basic_materials, consumer_cyclical,financial_services,real_estate,
                                 consumer_defensive,healthcare, utilities, communication_services,energy,
                                 industrials, technology, na.rm = T)),
         other = max(0,100 - total_alloc)) %>% 
  select(-total_alloc) %>% 
  gather(alloc_cat, percent, basic_materials:other) %>% 
  arrange(fund_extended_name,alloc_cat) %>% 
  mutate(alloc_cat_grp = ifelse(str_detect(pattern = "^portfolio", alloc_cat),"Asset Class", "Industry"),
         alloc_cat = case_when(alloc_cat =="total_alloc" ~ "Total",
                               str_detect(string = alloc_cat, "basic") ~ "Basic Materials",
                               str_detect(string = alloc_cat, "cycl") ~ "Consumer - Cyclical",
                               str_detect(string = alloc_cat, "fin") ~ "Financial Services",
                               str_detect(string = alloc_cat, "estate") ~ "Real Estate",
                               str_detect(string = alloc_cat, "def") ~ "Consumer - Defensive",
                               str_detect(string = alloc_cat, "health") ~ "Healthcare",
                               str_detect(string = alloc_cat, "util") ~ "Utilities",
                               str_detect(string = alloc_cat, "comm") ~ "Communication",
                               str_detect(string = alloc_cat, "ener") ~ "Energy",
                               str_detect(string = alloc_cat, "ind") ~ "Industrials",
                               str_detect(string = alloc_cat, "tech") ~ "Technology",
                               str_detect(string = alloc_cat, "convert") ~ "Convertable",
                               str_detect(string = alloc_cat, "bond") ~ "Bond",
                               str_detect(string = alloc_cat, "stock") ~ "Stocks",
                               str_detect(string = alloc_cat, "cash") ~ "Cash",
                               str_detect(string = alloc_cat, "convert") ~ "Convertible",
                               str_detect(string = alloc_cat, "pref") ~ "Preferred",
                               str_detect(string = alloc_cat, "portfolio_other") ~ "Asset - Other",
                               T ~ "Industry - Other")) %>% 
  arrange(fund_extended_name, alloc_cat_grp, desc(percent)) %>% 
  left_join(port_names) %>% 
  distinct(port_name, alloc_cat_grp,alloc_cat,percent, .keep_all = T) %>% 
  select(-fund_name)
#### Performance -------------------------------------------------------------
returns <- mf %>% 
  select(fund_name, fund_extended_name, currency, category, size, net_assets, investment,fund_family,
         inception_date,starts_with("fund_return_")) %>% 
  select(-contains("month"), -contains("ytd")) %>% 
  gather(period,return,contains("fund_return_")) %>% 
  mutate(end_year = ifelse(str_detect(pattern = "[[:digit:]]*year", period), 2018, 
                                      as.numeric(str_extract_all(pattern = "[[:digit:]]{4}",
                                                                 period))),
         start_year = ifelse(str_detect(pattern = "[[:digit:]]*year", period), 
                             2018 - as.numeric(str_extract(pattern = "(?<=return_)[[:digit:]]*",period)) + 1,
                             end_year),
         end_date = make_date(end_year, 12,31),
         start_date = make_date(start_year,1,1)) %>% 
  filter(!is.na(return), period != "fund_return_2018") %>% 
  left_join(port_names)

returns_tbl <- returns %>% 
  filter(str_detect(pattern = "[[:digit:]]*year", period)) %>% 
  arrange(port_name, period, desc(return)) %>% 
  distinct(port_name, period, .keep_all = T) %>% 
  select(-fund_name,-fund_extended_name)

spaghetti_rets <- returns %>% 
  filter(end_year == start_year) %>% 
  select(-inception_date) %>% 
  arrange(fund_extended_name,start_year, end_year) %>% 
  group_by(fund_extended_name, category,currency,investment,size,port_name,fund_name,net_assets,fund_family) %>% 
  complete(start_year = 2009:2018) %>% 
  mutate(return = coalesce(return,0),
         value = 10000,
         date = make_date(start_year, 12,31))%>% 
  select(-start_date,-end_date,-end_year) %>% 
  mutate(value = value * cumprod(1 + return/100))
#### Expenses ----------------------------------------------------------------
fees <-mf %>% 
  select(fund_name, fund_extended_name, currency, category, size, net_assets, investment,fund_family,
              inception_date,starts_with("net_annual_expense_ratio_fund")) %>% 
  left_join(port_names) %>% 
  distinct()

