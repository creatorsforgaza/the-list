library(tidyverse)
library(googledrive)
library(googlesheets4)
library(rvest)
library(bslib)
library(gt)
library(gtExtras)
library(polite)

# drive_auth(path = ".secrets/client_secret.json")
# gs4_auth(path = ".secrets/client_secret.json")


drive_auth(path = Sys.getenv("GOOGLE_AUTHENTICATION_CREDENTIALS"))

gs4_auth(path = Sys.getenv("GOOGLE_AUTHENTICATION_CREDENTIALS"))

ss1 <- drive_get(id = Sys.getenv("GOOGLE_SHEET_ID") 
)

sheet_gaza <- read_sheet(ss1)

# new one
all_gfm_links <- sheet_gaza %>% 
  # removing fund with broken link for now
  janitor::clean_names() %>% 
  dplyr::filter(!person_family %in% c("Jenna/Hashem"),
                status != "CLOSED / NOT ACCEPTING DONATIONS") %>%   
  dplyr::filter(str_detect(links, "gofund"))  %>% 
  dplyr::mutate(fund_parameter = str_squish(fund_parameter)) %>% 
  dplyr::select(fund_parameter)


###### test tib -----
test_tib <- tibble(
  # campaign_title = NULL,
  link = NULL,
  # join_code = NULL,
  money_raised = NULL,
  total_goal_amount = NULL,
  is_canadian = NULL,
  total_num_donations = NULL,
  total_num_donations_24_hours = NULL,
  money_raised_24_hours = NULL
  # total_num_donations_last_20 = NULL,
  # money_raised_last_20 = NULL,
  # num_days_last_20 = NULL
  )



num_funds <- nrow(all_gfm_links)

### for last 20 donations
start_word <- '\\,\\\\\"donations\\\\\"\\:\\['
end_word <- 'suggested_donation_stats'




# function for polite scraping
polite_scraping <- politely(rvest::read_html)  



#### trying out polite ----
for(i in 1:num_funds){

    url = paste0("https://www.gofundme.com/f/",
                 all_gfm_links[i,1])
  
  
    read_html_func <- polite_scraping(url)
    
  # # Add a random delay between 1 to 15 seconds
  random_delay <- runif(1, 10, 20)
  Sys.sleep(random_delay)

  
  link <- as.character(url)
  
  test_tib[i, "link"] = link
  
  
  # mr_td =  read_html_func %>%
  #   html_elements(css = ".hrt-text-body-lg") %>%
  #   html_text()
  # 
  # test_tib[i, "money_raised"] = mr_td[1]
  # 
  # test_tib[i, "total_num_donations"] = stringr::str_sub(mr_td[2], start = 2L, end = -2L)
  # 
  # total_goal = read_html_func %>%
  #   html_elements(css = ".hrt-text-body-sm") %>%
  #   html_text()
  # 
  # test_tib[i, "is_canadian"] = stringr::str_detect(total_goal[1], "CAD")
  # 
  # test_tib[i, "total_goal_amount"] = readr::parse_number(total_goal[1])
  
  
  mr_td =  read_html_func %>%
    html_elements(css = ".hrt-disp-inline") %>%
    html_text()
  
  # changed and good
  test_tib[i, "total_num_donations"] = stringr::str_sub(mr_td[1], start = 2L, end = -2L)
  
  total_goal = read_html_func %>%
    html_elements(css = ".hrt-text-body-sm") %>%
    html_text()
  
  # changed and good
  test_tib[i, "money_raised"] = stringr::str_extract(total_goal[1], "^[^ ]+")
  
  # didnt change and good
  test_tib[i, "is_canadian"] = stringr::str_detect(total_goal[1], "CAD")
  
  # changed and good
  # test_tib[i, "total_goal_amount"] = readr::parse_number(str_extract(total_goal[1], "(?<=raised of ).*?(?= goal)"))  
  
  # changed and good in order to capture all the funds
  test_tib[i, "total_goal_amount"] = readr::parse_number(str_extract_all(total_goal[1], "\\d{1,3}(?:,\\d{3})*")[[1]][2])
  
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
    dplyr::mutate(name = str_sub(name, start = 6L, end = -6L)) 
  
  test_tib[i, "total_num_donations_24_hours"] = twenty_most_recent_donations %>%
    dplyr::filter((now(tzone = "UTC") - hours(7)) - hours(24) <= created_at) %>%
    dplyr::summarise(num_donations = n()) %>% 
    dplyr::pull(num_donations)
  
  test_tib[i, "money_raised_24_hours"] = twenty_most_recent_donations %>%
    dplyr::filter((now(tzone = "UTC") - hours(7)) - hours(24) <= created_at) %>%
    dplyr::summarise(total_donations_amount = sum(amount)) %>% 
    dplyr::pull(total_donations_amount)
  

  
}


# 
# # # Add a random delay between 1 to 15 seconds
# random_delay <- runif(1, 10, 20)
# Sys.sleep(random_delay)
# 
# 
# link <- as.character(url)
# 
# test_tib[i, "link"] = link
# 
# 
# # mr_td =  read_html_func %>%
# #   html_elements(css = ".hrt-text-body-lg") %>%
# #   html_text()
# # 
# # test_tib[i, "money_raised"] = mr_td[1]
# # 
# # test_tib[i, "total_num_donations"] = stringr::str_sub(mr_td[2], start = 2L, end = -2L)
# # 
# # total_goal = read_html_func %>%
# #   html_elements(css = ".hrt-text-body-sm") %>%
# #   html_text()
# # 
# # test_tib[i, "is_canadian"] = stringr::str_detect(total_goal[1], "CAD")
# # 
# # test_tib[i, "total_goal_amount"] = readr::parse_number(total_goal[1])
# 
# 
# mr_td =  read_html_func %>%
#   html_elements(css = ".hrt-disp-inline") %>%
#   html_text()
# # 
# # # changed and good
# # test_tib[i, "total_num_donations"] = stringr::str_sub(mr_td[1], start = 2L, end = -2L)
# # 
# total_goal = read_html_func %>%
#   html_elements(css = ".hrt-text-body-sm") %>%
#   html_text()
# # 
# # # changed and good
# # test_tib[i, "money_raised"] = stringr::str_extract(total_goal[1], "^[^ ]+")
# # 
# # # didnt change and good
# # test_tib[i, "is_canadian"] = stringr::str_detect(total_goal[1], "CAD")
# # 
# # # changed and good
# # # test_tib[i, "total_goal_amount"] = readr::parse_number(str_extract(total_goal[1], "(?<=raised of )\\$[\\d,]+|(?<=raised of )\\€[\\d,]+|(?<=raised of )\\£[\\d,]+|(?<=raised of )\\kr[\\d,]+"))  
# # 
# # test_tib[i, "total_goal_amount"] = readr::parse_number(str_extract(total_goal[1], "(?<=raised of ).*?(?= goal)"))  


# parse_number(str_extract_all(total_goal[1], "\\d{1,3}(?:,\\d{3})*")[[1]][2])
