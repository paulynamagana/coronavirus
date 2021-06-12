# install.packages("devtools")
# The coronavirus package provides a tidy format dataset of the 2019
#Novel Coronavirus COVID-19 (2019-nCoV) epidemic
#The raw data pulled from the Johns Hopkins University Center for Systems
#Science and Engineering (JHU CCSE) Coronavirus repository.
devtools::install_github("RamiKrispin/coronavirus")

# Load the readr, ggplot2, and dplyr packages
library(readr)
library(dplyr)
library(ggplot2)
library(coronavirus)

#update_dataset keep the installed version with the most recent data 
#available on the Github version:
update_dataset()

# Read dataset and rename
data("coronavirus")
coronavirus_total <- coronavirus
head(coronavirus_total)

#The table above shows the  confirmed cases of COVID-19 worldwide by date
#let's arrange and plot this data

#TOTAL CASES PER COUNTRY
summary_df <- coronavirus_total %>% 
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)
summary_df %>% head(20) 

p <- ggplot(summary_df, aes(country,total_cases))+
  geom_bar(stat="identity", fill="darkred") +
  scale_y_continuous(name="Total Cases", labels = scales::comma)+
  scale_x_discrete(name="Country",guide = guide_axis(angle = 90)) +
  theme_classic() +
labs(title="Coronavirus Cases Per Country",
         caption="caption=Source: Johns Hopkins University")
p

#not the best plot as it's all cramped, let's zoom in the data

# LET'S FIND THE TOP 20 COUNTRIES WITH COVID-19 CASES
top_countries <- summary_df %>%
  arrange(total_cases) %>%
  top_n(20) %>%
  slice(1:20)

p <- ggplot(top_countries, aes(x=reorder(country,-total_cases), y=total_cases))+
geom_bar(stat="identity", fill="darkred") +
  scale_y_continuous(name="Total Cases", labels = scales::comma) +
  scale_x_discrete(name="Country", guide = guide_axis(angle = 90)) +
  theme_classic() +
  labs(title="Countries With The Highest COVID-19 Death Toll",
         caption="Source: Johns Hopkins University")
p

# NOW LOOK AT THE NEW CASES DURING THE LAST 24 HOURS
library(tidyr)

corona_24h <-coronavirus %>% 
  filter(date == max(date)) %>%
  select(country, type, cases) %>%
  group_by(country, type) %>%
  summarise(total_cases = sum(cases), .groups = "drop") %>%
  pivot_wider(names_from = type,
              values_from = total_cases)  %>%
  arrange(-confirmed) %>%
  top_n(20) %>%
  slice(1:20)

p <- ggplot(corona_24h, aes(x=reorder(country,-confirmed),y=confirmed))+
  geom_bar(stat="identity", fill="darkblue") +
  scale_y_continuous(name="Total Cases", labels = scales::comma)+
  scale_x_discrete(name="Country",guide = guide_axis(angle = 90)) +
  theme_classic() +
  labs(title = "New Cases During The Past 24 hours",
         caption="Source: Johns Hopkins University")
p

#LE'S PLOT A MAP VISUALISATION OF NEW CASES DURING THE LAST 24 H
#get the package
library(maps)
library(viridis)
## get the world map
world <- map_data("world")

corona_24world <- coronavirus_total %>% 
  select(country, date,type, cases, lat, long) %>%
  filter(date == max(date), type == "confirmed") %>%
  group_by(country, type)


# cutoffs based on the number of cases
mybreaks <- c(1, 20, 100, 1000, 50000)

ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(data=corona_24world, aes(x=long, y=lat, colour=cases),size=3, stroke=F, alpha=I(0.7)) +
  scale_size_continuous(name="Cases", trans="log", range=c(1,7),breaks=mybreaks, labels = c("1-19", "20-99", "100-999", "1,000-49,999", "50,000+")) +
  # scale_alpha_continuous(name="Cases", trans="log", range=c(0.1, 0.9),breaks=mybreaks) +
  scale_color_viridis_c(option="inferno",name="Cases", trans="log",breaks=mybreaks, labels = c("1-19", "20-99", "100-999", "1,000-49,999", "50,000+")) +
  theme_void() + 
  guides( colour = guide_legend()) +
  labs(caption = "Data Repository provided by Johns Hopkins CSSE. Visualization by DataScience+ ") +
  theme(
    legend.position = "bottom",
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#ffffff", color = NA), 
    panel.background = element_rect(fill = "#ffffff", color = NA), 
    legend.background = element_rect(fill = "#ffffff", color = NA)
  )

# AND TOTAL CASES BY TYPE WORLDWIDE
library(plotly)

coronavirus %>% 
  group_by(type, date) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(date) %>%
  mutate(active = confirmed - death - recovered) %>%
  mutate(active_total = cumsum(active),
         recovered_total = cumsum(recovered),
         death_total = cumsum(death)) %>%
  plot_ly(x = ~ date,
          y = ~ active_total,
          name = 'Active', 
          fillcolor = '#1f77b4',
          type = 'scatter',
          mode = 'none', 
          stackgroup = 'one') %>%
  add_trace(y = ~ death_total, 
            name = "Death",
            fillcolor = '#E41317') %>%
  add_trace(y = ~recovered_total, 
            name = 'Recovered', 
            fillcolor = 'forestgreen') %>%
  layout(title = "Distribution of Covid19 Cases Worldwide",
         legend = list(x = 0.1, y = 0.9, text = "Source: data I found somewhere."),
         yaxis = list(title = "Number of Cases"),
         xaxis = list(title = "Source: Johns Hopkins University"))
