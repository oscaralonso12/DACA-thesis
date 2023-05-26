
# importing library
library(tidyverse)


### add USCIS source for data ####
# source: https://www.uscis.gov/tools/reports-and-studies/immigration-and-citizenship-data?topic_id%5B%5D=33602&ddt_mon=&ddt_yr=&query=&items_per_page=10

# set wd
setwd("~/Desktop/thesis/notebooks/")

# import data for active DACA recipients
active_daca <- read_csv("data/uscis_data/active_DACA_2022.csv")

# importing States pop in 2022
states <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/state/totals/NST-EST2022-ALLDATA.csv")

# getting desired states
states <- states %>% 
  select(NAME, POPESTIMATE2022) %>% 
  slice(15:66) %>% 
  rename('State' = 'NAME',
         'Total' = 'POPESTIMATE2022') %>% 
  slice(1:(n()-1))


# combing with US State Pop data
active_daca_with_pop <- inner_join(active_daca, states, by = 'State')

# renaing columns
active_daca_with_pop <- active_daca_with_pop %>% 
  rename('total_daca' = 'Total.x',
         'total_pop' = 'Total.y')

active_daca_with_pop <- active_daca_with_pop %>% 
  mutate(pct_of_pop = (total_daca / total_pop) * 100)

### create map ###
map <- active_daca_with_pop %>% 
  mutate(State = str_to_lower(State)) %>%
  inner_join(map_data("state"), by = c(State = "region")) %>%
  ggplot(aes(long, lat, group = group, fill = pct_of_pop)) +
  geom_polygon() +
  coord_map() +
  scale_fill_viridis_c(labels = scales::percent_format(scale = 1),
                       name = 'DACA % of State Population') +
  ggthemes::theme_map() +
  theme(legend.position = 'top',
        legend.key.width = unit(1, 'cm')) +
  labs(title = 'Active DACA Recipients as a Percent of State Population',
       subtitle = "Data as of September 30, 2022",
       caption = "USCIS Source: https://www.uscis.gov/tools/reports-and-studies/immigration-and-citizenship-data")

map +
  theme(legend.text = element_text(size = 10))


###### SAVING HIGH RESOLUTION GRAPHS ########
# ggsave("/Users/oscaralonso/Desktop/cool_map.png",
#       last_plot(), dpi = 5000)


### create bar chart with flags of COB for DACA recipients ###

# import COB data
cob_daca <- read_csv("data/uscis_data/COB_DACA_2022.csv")

# create bar graph
cob_daca_plot <- cob_daca %>%
  mutate(pct_share = total / sum(total)) |> 
  head(5) %>% 
  ggplot(aes(fct_reorder(country, total), pct_share)) + 
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab(NULL) +
  ylab('Total') +
  labs(title = 'DACA Country of Birth Totals',
       subtitle = "Total DACA recipients as of September 30, 2022: 589,230",
       caption = "Source: https://www.uscis.gov/tools/reports-and-studies/immigration-and-citizenship-data") +
  coord_flip() +
  ggthemes::theme_clean() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18))

cob_daca_plot
