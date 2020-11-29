# Load local authority data and clean

get_la_data <- function(){
  
  
  la_pops <- tibble::tribble(
                        ~ca_name,       ~pop,
            "Aberdeen City",    228670,
            "Aberdeenshire",    261210,
                    "Angus",    116200,
          "Argyll and Bute",     85870,
        "City of Edinburgh",    524930,
         "Clackmannanshire",     51540,
    "Dumfries and Galloway",    148860,
              "Dundee City",    149320,
            "East Ayrshire",    122010,
      "East Dunbartonshire",    108640,
             "East Lothian",    107090,
        "East Renfrewshire",     95530,
                  "Falkirk",    160890,
                     "Fife",    373550,
             "Glasgow City",    633120,
                 "Highland",    235830,
               "Inverclyde",     77800,
               "Midlothian",     92460,
                    "Moray",     95820,
       "Na h-Eileanan Siar",     26720,
           "North Ayrshire",    134740,
        "North Lanarkshire",    341370,
           "Orkney Islands",     22270,
        "Perth and Kinross",    151950,
             "Renfrewshire",    179100,
         "Scottish Borders",    115510,
         "Shetland Islands",     22920,
           "South Ayrshire",    112610,
        "South Lanarkshire",    320530,
                 "Stirling",     94210,
      "West Dunbartonshire",     88930,
             "West Lothian",    183100
    )

  
  readr::read_csv("https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/427f9a25-db22-4014-a3bc-893b68243055/download/trend_ca.csv") %>% 
    janitor::clean_names() %>% 
    mutate(date = ymd(date)) %>% 
    mutate(ca_name = str_replace_all(ca_name, "&", "and")) %>% 
    left_join(la_pops, by = "ca_name") %>% 
    group_by(ca_name) %>% 
    arrange(date) %>% 
    mutate(positive_7d = roll_sumr(daily_positive,
                                   n = 7)) %>%
    mutate(positive_test_rate_7d = roll_mean(positive_percentage,
                                             n = 7,
                                             fill = NA,
                                             align = "right")) %>% 
    mutate(positive_7d_rate = positive_7d / pop * 100000) %>% 
    ungroup()
}

