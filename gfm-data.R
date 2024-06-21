library(tidyverse)
library(googledrive)
library(googlesheets4)
library(rvest)
library(bslib)
library(gt)
library(gtExtras)

# for local
# drive_auth(path = ".secrets/client_secret.json")
# gs4_auth(path = ".secrets/client_secret.json")

drive_auth(path = Sys.getenv("GOOGLE_AUTHENTICATION_CREDENTIALS"))

gs4_auth(path = Sys.getenv("GOOGLE_AUTHENTICATION_CREDENTIALS"))

ss <- drive_get(id = Sys.getenv("GOOGLE_SHEET_ID") 
)

sheet_gaza <- read_sheet(ss)

all_gfm_links <- sheet_gaza %>% 
  # removing fund with broken link for now
  janitor::clean_names() %>% 
  dplyr::filter(person_family != "Jenna/Hashem",
                status != "CLOSED / NOT ACCEPTING DONATIONS") %>%   
  dplyr::select(links) %>% 
  dplyr::filter(str_detect(links, "gofund"))


test_tib <- tibble(
  # campaign_title = NULL,
  link = NULL,
  money_raised = NULL,
  total_goal_amount = NULL,
  total_num_donations = NULL,
  total_num_donations_24_hours = NULL,
  money_raised_24_hours = NULL,
  total_num_donations_last_20 = NULL,
  money_raised_last_20 = NULL,
  num_days_last_20 = NULL)



num_funds <- nrow(all_gfm_links)



### for last 20 donations
start_word <- '\\,\\\\\"donations\\\\\"\\:\\['
end_word <- 'suggested_donation_stats'

for(i in 1:10{
  
  read_html_func <- read_html(as.character(all_gfm_links[i,1]))
  
  link <- as.character(all_gfm_links[i,1])
  
  test_tib[i, "link"] = link
  
  # test_tib[i, "campaign_title"] = read_html_func %>%
  #   html_elements(css = ".p-campaign-header") %>%
  #   html_text()
  
  mr_td =  read_html_func %>%
    html_elements(css = ".hrt-disp-inline") %>%
    html_text()
  
  test_tib[i, "money_raised"] = mr_td[1]
  
  test_tib[i, "total_num_donations"] = stringr::str_sub(mr_td[2], start = 2L, end = -2L)
  
  total_goal = read_html_func %>%
    html_elements(css = ".hrt-text-body-sm") %>%
    html_text()
  
  test_tib[i, "total_goal_amount"] = readr::parse_number(total_goal[1])
  
  ### 20 most recent donations
  most_recent_donations = read_html_func %>%
    html_text() %>%
    enframe() %>%
    dplyr::mutate(result =
                    str_match_all(value,
                                  paste0("(?s)",
                                         start_word,
                                         "(.*?)",
                                         end_word))) %>%
    dplyr::select(result) %>%
    unlist()   
  
  twenty_most_recent_donations = most_recent_donations[[2]][1] %>% 
    tibble::enframe() %>%
    tidyr::separate_longer_delim(value, delim = "donation_id") %>%
    dplyr::filter(row_number() != 1) %>%
    tidyr::separate(value, c("donation_id", "other"), sep = "amount") %>%
    dplyr::mutate(donation_id = readr::parse_number(donation_id)) %>%
    tidyr::separate(other, c("amount", "other"), sep = "is_offline") %>%
    dplyr::mutate(amount = readr::parse_number(amount)) %>%
    tidyr::separate(other, c("name", "other"), sep = "created_at") %>%
    dplyr::select(-name) %>%
    tidyr::separate(other, c("created_at", "other"), sep = "name") %>%
    dplyr::mutate(created_at = readr::parse_datetime(str_sub(
      created_at, start = 6L, end = -12L
    ))) %>%
    tidyr::separate(other, c("name", "other"), sep = "profile_url") %>%
    dplyr::mutate(name = str_sub(name, start = 6L, end = -6L)) %>%
    dplyr::select(-other) 
  
  test_tib[i, "total_num_donations_24_hours"] = twenty_most_recent_donations %>%
    dplyr::filter((now(tzone = "UTC") - hours(5)) - hours(24) <= created_at) %>%
    dplyr::summarise(num_donations = n()) %>% 
    dplyr::pull(num_donations)
  
  test_tib[i, "money_raised_24_hours"] = twenty_most_recent_donations %>%
    dplyr::filter((now(tzone = "UTC") - hours(5)) - hours(24) <= created_at) %>%
    dplyr::summarise(total_donations_amount = sum(amount)) %>% 
    dplyr::pull(total_donations_amount)
  
  # get the spread from the 20 most recent donations
  test_tib[i, "total_num_donations_last_20"] = twenty_most_recent_donations %>%
    dplyr::summarise(
      num_donations = n()
    ) %>% 
    dplyr::pull(num_donations)
  
  test_tib[i, "money_raised_last_20"] = twenty_most_recent_donations %>%
    dplyr::summarise(
      total_donations_amount = sum(amount)
    ) %>% 
    dplyr::pull(total_donations_amount)
  
  test_tib[i, "num_days_last_20"] = twenty_most_recent_donations %>%
    dplyr::summarise(
      most_recent_donation = max(created_at),
      date_of_20th_most_recent_donation = min(created_at)
    ) %>% 
    dplyr::mutate(days = floor(most_recent_donation - 
                                 date_of_20th_most_recent_donation)) %>% 
    dplyr::mutate(days = as.numeric(days)) %>% 
    dplyr::pull(days) 
  
  
}