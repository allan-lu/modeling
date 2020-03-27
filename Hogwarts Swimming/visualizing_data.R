library(tidyverse)
library(knitr)
library(ggplot2)
library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)


dat <- "Hogwarts_swimming_file.csv"
df <- read_csv(dat)


# Manage the data 
swim_spells <- drop_na(df)

# Create the plot
ggplot(swim_spells, aes(as_datetime(Times), Spells)) +
  geom_point() +
  geom_smooth() +
  scale_x_datetime(date_breaks = "3 min", date_labels = "%M:%S") +
  labs(x = "Performance Time (min:sec)",
       y = "Average Number of Spells Used",
       title = "Number of Spells Used to Swimming Time",
       subtitle = "Are the best swimmers from physical training or spellwork?") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"))

# Save plot1 
ggsave("plot1.png", plot = last_plot(), device = 'png', dpi = 400)

# Create plot2
ggplot(swim_spells, aes(as_datetime(Times), Spells)) +
  geom_point(aes(color = House.name)) +
  geom_smooth(aes(color = House.name), se = F) +
  scale_color_manual(values=c("firebrick", "gold", "royalblue", "darkgreen")) +
  scale_x_datetime(date_breaks = "3 min", date_labels = "%M:%S") +
  labs(x = "Performance Time (min:sec)",
       y = "Average Number of Spells Used",
       color = "Houses",
       title = "Number of Spells Used to Swimming Time",
       subtitle = "Which houses are relying too much on magic to win?") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"))

# Save plot2
ggsave("plot2.png", plot = last_plot(), device = 'png', dpi = 400)


# Manage the data 
swim_100 <- df %>%
  drop_na %>%
  select(Event, Date, TIME, Swimmers, House = House.name, Times) %>%
  filter(grepl("100", Event))

# Create the plot
ggplot(swim_100, aes(Date, as_datetime(Times))) +
  geom_point(aes(color = House)) +
  geom_smooth(aes(color = House)) +
  facet_wrap(~Event, nrow = 2, ncol = 2) +
  scale_color_manual(values=c("firebrick", "gold", "royalblue", "darkgreen")) +
  scale_y_datetime(date_breaks = "1 min", date_labels = "%M:%S") +
  labs(y = "Performance Time (min:sec)",
       color = "Houses",
       title = "100 Yard Swim Event Times",
       subtitle = "Hogwarts House Performance",
       caption = "Data from Oct 2019 to Jan 2020") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        strip.background = element_rect(fill = "plum", color = "black"))

# Save the plot 
ggsave("plot3.png", plot = last_plot(), device = 'png', dpi = 400) 


# Manage the data 
swim_100 <- df %>%
  drop_na %>%
  select(Event, Month, TIME, Swimmers, House = House.name, Times) %>%
  filter(grepl("100", Event) & Month == "January")

# Analysis 
swim_fast <- swim_100 %>%
  group_by(Event) %>%
  top_n(-1, Times) %>%
  # Make the table 
  select(-Month, -Times)

swim_dq <- df %>%
  select(Swimmers, PL) %>%
  filter(grepl("DQ", PL)) %>%
  group_by(Swimmers) %>%
  count(PL, name="DQs") %>%
  ungroup() %>%
  top_n(1, DQs)

sprintf("%s had the most disqualifications with %i.", swim_dq$Swimmers, swim_dq$DQs)


df2 <- df %>%
  drop_na() %>%
  rename(House = House.name) %>%
  filter(grepl("100", Event))

ggplot(df2, aes(as_datetime(Times), color = House, fill = House)) +
  geom_density(position = "stack") + 
  facet_grid(Event ~ Against) +
  scale_fill_manual(values=c("firebrick", "yellow", "royalblue", "darkgreen")) +
  scale_color_manual(values=c("gold2", "black", "darkgoldenrod", "gray")) +
  scale_x_datetime(date_breaks = "1.5 min", date_labels = "%M:%S") +
  scale_y_continuous(breaks=c(0, 0.03, 0.06, 0.09)) + 
  labs(x = "Performance Time (min:sec)",
       y = "Density",
       title = "Hogwarts 100-Yard Event Swim Times",
       subtitle = "By Event and Opposing School",
       caption = "Density Plot of Swim Times") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        strip.background = element_rect(fill = "cornsilk", color = "black"),
        legend.direction = "horizontal",
        legend.position = "bottom")

ggsave("plot4.png", plot = last_plot(), device = 'png', dpi = 400) 


df3 <- df %>%
  drop_na() %>%
  rename(House = House.name) %>%
  filter(!grepl("RELAY", Event) & grepl("FREE", Event)) %>%
  mutate(Event = factor(Event, levels=c("50 FREE", "100 FREE", "200 FREE", "500 FREE")))

ggplot(df3, aes(House, Spells, color = House, fill = House)) +
  geom_violin() +
  facet_grid(Event ~ Against, scale = "free") +
  scale_fill_manual(values=c("firebrick", "yellow", "royalblue", "darkgreen")) +
  scale_color_manual(values=c("gold2", "black", "darkgoldenrod", "gray")) +
  scale_x_discrete(labels = c("G", "H", "R", "S")) +
  labs(y = "Number of Spells",
       color = "Houses",
       fill = "Houses",
       title = "Hogwarts Swim Team's Spell Usage in Freestyle Events",
       subtitle = "By Distance and Opposing School",
       caption = "Violin Plot of Spell Usage at Swimming Events") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        strip.background = element_rect(fill = "cornsilk", color = "black"),
        legend.direction = "horizontal",
        legend.position = "bottom")

ggsave("plot5.png", plot = last_plot(), device = 'png', dpi = 400)

