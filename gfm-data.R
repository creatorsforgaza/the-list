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


#### testing grabbing data from sheet ----

###### test tib -----
test_tib <- tibble(
  # campaign_title = NULL,
  link = NULL,
  # join_code = NULL,
  money_raised = NULL,
  total_goal_amount = NULL,
  # is_canadian = NULL,
  # total_num_donations = NULL,
  currency = NULL
  # total_num_donations_24_hours = NULL,
  # money_raised_24_hours = NULL
  # total_num_donations_last_20 = NULL,
  # money_raised_last_20 = NULL,
  # num_days_last_20 = NULL
  )



num_funds <- nrow(all_gfm_links)

#### start and end words -----

### for last 20 donations
start_word <- '\\,\\\\\"donations\\\\\"\\:\\['
end_word <- 'suggested_donation_stats'

### for the total goal amount and currencycode
# "currencyCode\":\"USD\",\"amount\":50000},\"goalDeadline\":null,
start_word_total_goal = 'userDefinedGoalAmount'
end_word_total_goal = 'visibleInSearch'


### for the current money raised
# updated start and end words on 12/19/2024
# did not work
# start_word_current_amount = 'amountRaisedUnattributedNumber'
# end_word_current_amount = 'numberOfDonationsUnattributed'

# this should work
# start_word_current_amount = 'currentAmount'
# updated 7/7/25
start_word_current_amount = 'tAmount\\\":\\{\\\"__typename'
end_word_current_amount = 'defaultSlug'



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
  #   html_elements(css = ".hrt-disp-inline") %>%
  #   html_text()
  # 
  # # changed and good
  # test_tib[i, "total_num_donations"] = ifelse(length(mr_td) == 1, stringr::str_sub(mr_td[1], start = 2L, end = -2L), 
  #                                             stringr::str_sub(mr_td[2], start = 2L, end = -2L))
  
  # total_goal = read_html_func %>%
  #   html_elements(css = ".hrt-text-body-sm") %>%
  #   html_text()
  
  # changed and good
  test_tib[i, "money_raised"] = read_html_func %>%
    html_text() %>%
    enframe() %>%
    dplyr::mutate(result =
                    str_match_all(value,
                                  paste0("(?s)",
                                         start_word_current_amount,
                                         "(.*?)",
                                         end_word_current_amount))) %>%
    dplyr::select(result) %>%
    unlist() %>%
    enframe() %>% 
    dplyr::mutate(current_amount = as.numeric(str_extract_all(value, "[0-9]+"))) %>% 
    dplyr::select(current_amount) %>% 
    head(n = 1) %>% 
    dplyr::pull(current_amount)
  
  # didnt change and good
  test_tib[i, "currency"] = read_html_func %>%
    html_text() %>%
    enframe() %>%
    dplyr::mutate(result =
                    str_match_all(value,
                                  paste0("(?s)",
                                         start_word_total_goal,
                                         "(.*?)",
                                         end_word_total_goal))) %>%
    dplyr::select(result) %>%
    unlist() %>%
    enframe() %>% 
    dplyr::mutate(currency = case_when(
      str_detect(value, "USD") ~ "USD",
      str_detect(value, "GBP") ~ "GBP",
      str_detect(value, "DKK") ~ "DKK",
      str_detect(value, "EUR") ~ "EUR",
      str_detect(value, "SEK") ~ "SEK",
      TRUE ~ "Other"
    )) %>% 
    head(n = 1) %>% 
    dplyr::pull(currency)
  
  

  
  # changed and good in order to capture all the funds
  test_tib[i, "total_goal_amount"] = read_html_func %>%
    html_text() %>%
    enframe() %>%
    dplyr::mutate(result =
                    str_match_all(value,
                                  paste0("(?s)",
                                         start_word_total_goal,
                                         "(.*?)",
                                         end_word_total_goal))) %>%
    dplyr::select(result) %>%
    # unlist() %>%
    # enframe() %>%
    # dplyr::mutate(result =
    #                 str_match_all(value,
    #                               paste0("(?s)",
    #                                      'currencyCode',
    #                                      "(.*?)",
    #                                      end_word_total_goal))) %>%  
    # dplyr::select(result) %>%
    unlist() %>%
    enframe()  %>% 
    dplyr::mutate(total_goal_amount = as.numeric(str_extract_all(value, "[0-9]+"))) %>% 
    dplyr::select(total_goal_amount) %>% 
    head(n = 1) %>% 
    dplyr::pull(total_goal_amount)


  
}

##### testing -----


