library(tidyverse)
library(googledrive)
library(googlesheets4)
library(rvest)
library(bslib)
library(gt)
library(gtExtras)
library(polite)


# for local

# drive_auth(path = ".secrets/client_secret.json")
# gs4_auth(path = ".secrets/client_secret.json")

#
drive_auth(path = Sys.getenv("GOOGLE_AUTHENTICATION_CREDENTIALS"))

gs4_auth(path = Sys.getenv("GOOGLE_AUTHENTICATION_CREDENTIALS"))

ss <- drive_get(id = Sys.getenv("GOOGLE_SHEET_ID") 
)

sheet_gaza <- read_sheet(ss)

# all_gfm_links <- sheet_gaza %>% 
#   # removing fund with broken link for now
#   janitor::clean_names() %>% 
#   dplyr::filter(person_family != "Jenna/Hashem",
#                 status != "CLOSED / NOT ACCEPTING DONATIONS") %>%   
#   dplyr::select(links) %>% 
#   dplyr::filter(str_detect(links, "gofund"))

# new one
all_gfm_links <- sheet_gaza %>% 
  # removing fund with broken link for now
  janitor::clean_names() %>% 
  dplyr::filter(person_family != "Jenna/Hashem",
                status != "CLOSED / NOT ACCEPTING DONATIONS") %>%   
  dplyr::filter(str_detect(links, "gofund"))  %>% 
  dplyr::mutate(fund_parameter = str_squish(fund_parameter)) %>% 
  dplyr::select(fund_parameter)


test_tib <- tibble(
  # campaign_title = NULL,
  link = NULL,
  # join_code = NULL,
  money_raised = NULL,
  total_goal_amount = NULL,
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



# response_temp <- map(funds_trial[1:3],
#                      ~scrape(session,
#                              query = list(f = .x)))

# stringr::str_sub("https://www.gofundme.com/f/blah", start = 28L)

# function for polite scraping
polite_scraping <- politely(rvest::read_html)  

# 
# session <- bow("https://www.gofundme.com")
# 
# fund_names <- c("4-months-separates-between-life-and-death-and-we",
#                 "support-khamis-medical-care-for-spine-injury-after-bombing")
# 
# # this is only to illustrate the example.
# letters <- letters[1:3] # delete this line to scrape all letters
# funds <- all_gfm_links %>% 
#   dplyr::mutate(fund = str_sub(links, start = 19L)) %>% 
#   dplyr::pull(fund) %>% 
#   head()
# 
# responses <- map(fund_names, ~scrape(session, query = list(f=.x)) ,
#                  accept = "xml")
# results <- map(responses, ~html_nodes(.x, "#id_page li") %>% 
#                  html_text(trim = TRUE) %>% 
#                  as.numeric() %>%
#                  tail(1) ) %>% 
#   map(~pluck(.x, 1, .default=1))
# 
# responses[[2]] %>% 
#   # html_elements(css = ".hrt-disp-inline") %>%
#   html_text()
# results[[1]] 
# 
# read_html_test <- rvest::read_html("https://gateway.gofundme.com/web-gateway/v1/feed/support-khamis-medical-care-for-spine-injury-after-bombing/campaign")
# 
# start_word <- '\\,\\\\\"donations\\\\\"\\:\\['
# end_word <- 'suggested_donation_stats'
# 
# 
# read_html_test %>%
#   html_text() %>%
#   enframe() %>% 
#   dplyr::mutate(result =
#                   str_match_all(value,
#                                 paste0("currencycode",
#                                        "(.*?)",
#                                        "current_amount"))) %>%
#   dplyr::select(result) %>%
#   unlist()   
# 
# 
# 
# #### trying out gateway
# 
# #### trying out polite ----
# for(i in 1:num_funds){
#   
#   
# 
#   url = paste0("https://gateway.gofundme.com/web-gateway/v1/feed/",
#                all_gfm_links[i,1],
#                "/campaign")
#   
#   read_html_func <- rvest::read_html(url)
#   #   
#   # }
#   # 
#   # # Add a random delay between 1 to 15 seconds
#   random_delay <- runif(1, 1, 5)
#   Sys.sleep(random_delay)
#   
#   
#   link <- as.character(url)
#   
#   test_tib[i, "link"] = link
#   
#   
#   mr_td =  read_html_func %>%
#     html_elements(css = ".hrt-disp-inline") %>%
#     html_text()
#   
#   test_tib[i, "money_raised"] = mr_td[1]
#   
#   test_tib[i, "total_num_donations"] = stringr::str_sub(mr_td[2], start = 2L, end = -2L)
#   
#   total_goal = read_html_func %>%
#     html_elements(css = ".hrt-text-body-sm") %>%
#     html_text()
#   
#   test_tib[i, "total_goal_amount"] = readr::parse_number(total_goal[1])
#   
#   ### 20 most recent donations
#   most_recent_donations = read_html_func %>%
#     html_text() %>%
#     enframe() %>%
#     dplyr::mutate(result =
#                     str_match_all(value,
#                                   paste0("(?s)",
#                                          start_word,
#                                          "(.*?)",
#                                          end_word))) %>%
#     dplyr::select(result) %>%
#     unlist()   
#   
#   twenty_most_recent_donations = most_recent_donations[[2]][1] %>% 
#     tibble::enframe() %>%
#     tidyr::separate_longer_delim(value, delim = "donation_id") %>%
#     dplyr::filter(row_number() != 1) %>%
#     tidyr::separate(value, c("donation_id", "other"), sep = "amount") %>%
#     dplyr::mutate(donation_id = readr::parse_number(donation_id)) %>%
#     tidyr::separate(other, c("amount", "other"), sep = "is_offline") %>%
#     dplyr::mutate(amount = readr::parse_number(amount)) %>%
#     tidyr::separate(other, c("name", "other"), sep = "created_at") %>%
#     dplyr::select(-name) %>%
#     tidyr::separate(other, c("created_at", "other"), sep = "name") %>%
#     dplyr::mutate(created_at = readr::parse_datetime(str_sub(
#       created_at, start = 6L, end = -12L
#     ))) %>%
#     tidyr::separate(other, c("name", "other"), sep = "profile_url") %>%
#     dplyr::mutate(name = str_sub(name, start = 6L, end = -6L)) %>%
#     dplyr::select(-other) 
#   
#   test_tib[i, "total_num_donations_24_hours"] = twenty_most_recent_donations %>%
#     dplyr::filter((now(tzone = "UTC") - hours(7)) - hours(24) <= created_at) %>%
#     dplyr::summarise(num_donations = n()) %>% 
#     dplyr::pull(num_donations)
#   
#   test_tib[i, "money_raised_24_hours"] = twenty_most_recent_donations %>%
#     dplyr::filter((now(tzone = "UTC") - hours(7)) - hours(24) <= created_at) %>%
#     dplyr::summarise(total_donations_amount = sum(amount)) %>% 
#     dplyr::pull(total_donations_amount)
#   
#   # get the spread from the 20 most recent donations
#   # test_tib[i, "total_num_donations_last_20"] = twenty_most_recent_donations %>%
#   #   dplyr::summarise(
#   #     num_donations = n()
#   #   ) %>% 
#   #   dplyr::pull(num_donations)
#   # 
#   # test_tib[i, "money_raised_last_20"] = twenty_most_recent_donations %>%
#   #   dplyr::summarise(
#   #     total_donations_amount = sum(amount)
#   #   ) %>% 
#   #   dplyr::pull(total_donations_amount)
#   # 
#   # test_tib[i, "num_days_last_20"] = twenty_most_recent_donations %>%
#   #   dplyr::summarise(
#   #     most_recent_donation = max(created_at),
#   #     date_of_20th_most_recent_donation = min(created_at)
#   #   ) %>% 
#   #   dplyr::mutate(days = floor(most_recent_donation - 
#   #                                date_of_20th_most_recent_donation)) %>% 
#   #   dplyr::mutate(days = as.numeric(days)) %>% 
#   #   dplyr::pull(days) 
#   
#   
# }


#### trying out polite ----
for(i in 1:num_funds){

    url = paste0("https://www.gofundme.com/f/",
                 all_gfm_links[i,1])
  
  # if(i == 1){
  
    read_html_func <- polite_scraping(url)
    
  # read_html_func <- polite_scraping(as.character(all_gfm_links[i,1]))
  
  # }
  # else{
  #   
    # read_html_func <- rvest::read_html(as.character(all_gfm_links[i,1]))
  #   
  # }
  # 
  # # Add a random delay between 1 to 15 seconds
  random_delay <- runif(1, 10, 20)
  Sys.sleep(random_delay)

  
  link <- as.character(url)
  
  test_tib[i, "link"] = link
  
  
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
    dplyr::filter((now(tzone = "UTC") - hours(7)) - hours(24) <= created_at) %>%
    dplyr::summarise(num_donations = n()) %>% 
    dplyr::pull(num_donations)
  
  test_tib[i, "money_raised_24_hours"] = twenty_most_recent_donations %>%
    dplyr::filter((now(tzone = "UTC") - hours(7)) - hours(24) <= created_at) %>%
    dplyr::summarise(total_donations_amount = sum(amount)) %>% 
    dplyr::pull(total_donations_amount)
  
  # get the spread from the 20 most recent donations
  # test_tib[i, "total_num_donations_last_20"] = twenty_most_recent_donations %>%
  #   dplyr::summarise(
  #     num_donations = n()
  #   ) %>% 
  #   dplyr::pull(num_donations)
  # 
  # test_tib[i, "money_raised_last_20"] = twenty_most_recent_donations %>%
  #   dplyr::summarise(
  #     total_donations_amount = sum(amount)
  #   ) %>% 
  #   dplyr::pull(total_donations_amount)
  # 
  # test_tib[i, "num_days_last_20"] = twenty_most_recent_donations %>%
  #   dplyr::summarise(
  #     most_recent_donation = max(created_at),
  #     date_of_20th_most_recent_donation = min(created_at)
  #   ) %>% 
  #   dplyr::mutate(days = floor(most_recent_donation - 
  #                                date_of_20th_most_recent_donation)) %>% 
  #   dplyr::mutate(days = as.numeric(days)) %>% 
  #   dplyr::pull(days) 
  
  
}





# polite for scraping
# polite::use_manners(save_as = "polite-scrape.R")
# polite::use_manners()
# source("R/polite-scrape.R")

