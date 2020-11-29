plot_cases <- function(dat){
dat %>% 
  ggplot() +
  aes(x = date,y = positive_7d_rate,
      group = ca_name, colour = ca_name) +
  geom_line(lwd = 1.5) +
  geom_text_repel(aes(label = label),
                  hjust = -0.1, segment.colour = NA,
                  show.legend = FALSE, size = 5,
                  direction = "y") +
  scale_x_date(expand = expansion(mult = c(0, 0.5))) +
  labs(title = "COVID 19 case rate by Council Area",
       subtitle = str_glue("To {format(now() - days(3), format = '%d-%m-%Y')})"),
       x = "Date",
       y = "Cases (7 day sum per 100,000)",
       colour = "Authority") +
  scale_color_manual(values = c("Scottish Borders" = swatch()[1],
                                "Argyll and Bute" = swatch()[2],
                                "City of Edinburgh" = swatch()[3],
                                "Midlothian" = swatch()[4])) +
  theme(legend.position = "none")
}
