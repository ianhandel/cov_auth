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

  
  readr::read_csv("https://www.opendata.nhs.scot/datastore/dump/427f9a25-db22-4014-a3bc-893b68243055?bom=True") %>% 
    janitor::clean_names() %>% 
    mutate(date = ymd(date)) %>% 
    mutate(ca_name = str_replace_all(ca_name, "&", "and")) %>% 
    left_join(la_pops, by = "ca_name") %>% 
    group_by(ca_name) %>% 
    arrange(date) %>% 
    mutate(positive_7d = roll_sum(daily_positive, n = 7, fill = "left")) %>% 
    mutate(positive_7d_rate = positive_7d / pop * 100000) %>% 
    ungroup()
}

x <- get_la_data()

authorities <- c("City of Edinburgh", "Argyll and Bute", "Scottish Borders")

x %>% 
  filter(date > now() - days(30) & date < now() - days(7)) %>% 
  filter(ca_name %in% authorities) %>%
  group_by(ca_name) %>% 
  mutate(label = if_else(date == max(date, na.rm = TRUE),
                         ca_name, NA_character_)) %>% 
  ungroup() %>% 
ggplot() +
  aes(x = date,y = positive_7d_rate,
      group = ca_name, colour = ca_name) +
  geom_line(lwd = 2) +
  geom_text_repel(aes(label = label), hjust = -0.05) +
  scale_x_date(expand = expand_scale(mult = c(0, 0.35))) +
  labs(title = "Seven day cases per 100,000",
       x = "Date",
       y = "Rate",
       colour = "Authority") +
  ggthemes::theme_hc(base_size = 16)
