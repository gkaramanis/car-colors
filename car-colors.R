library(tidyverse)
library(readxl)
library(janitor)
library(magick)

bg_col <- "grey90"

car <- image_read(here::here("img", "volvo.png")) %>% 
  image_fill(color = bg_col, "+10+10", fuzz = 0) %>%
  image_fill(color = bg_col, "+750+220", fuzz = 35) %>% 
  image_fill(color = bg_col, "+950+220", fuzz = 30) %>% 
  image_fill(color = bg_col, "+750+600", fuzz = 30) %>% 
  image_fill(color = "transparent", "+600+350", fuzz = 35) %>% 
  image_flop() %>% 
  image_trim()

car_colors_raw <- read_xlsx(here::here("data", "farger-personbilar-2006-2018.xlsx"))
  
car_colors <- car_colors_raw %>%
  clean_names() %>% 
  select(color = x1, everything()) %>% 
  pivot_longer(cols = x2006:x2018, names_to = "year", values_to = "n") %>% 
  mutate(
    year = as.numeric(str_remove(year, "x")),
    color = fct_recode(color, 
      purple = "LILA", silver = "SILVER", white = "VIT", lightyellow = "LGUL", gray = "GRÅ", darkbrown = "MBRUN", multicolored = "FLERF", darkgray = "MGRÅ", lightgray = "LGRÅ", unknown = "OKÄND", darkgreen = "MGRÖN", black = "SVART", red = "RÖD", lightgreen = "LGRÖN", green = "GRÖN", lightbrown = "LBRUN", blue = "BLÅ", darkred = "MRÖD", lightblue = "LBLÅ", yellow = "GUL", darkblue = "MBLÅ", brown = "BRUN", orange = "ORANGE", lightred = "LRÖD"
    )
    ) %>% 
  mutate(
    fill_color = case_when(
      str_detect(color, "red") ~ "#ff2800",
      str_detect(color, "green") ~ "darkgreen",
      str_detect(color, "blue") ~ "blue3",
      str_detect(color, "yellow") ~ "#fdcc0d",
      str_detect(color, "silver|gray") ~ "gray",
      str_detect(color, "brown") ~ "brown4",
      str_detect(color, "purple") ~ "purple4",
      str_detect(color, "orange") ~ "darkorange2",
      TRUE ~ as.character(color)
    ),
    fill_color = fct_relevel(fill_color, c("white", "gray", "black", "#ff2800", "blue3", "brown4", "darkgreen",  "#fdcc0d", "darkorange2", "purple4"))
    ) %>% 
  filter(color != "multicolored" & color != "unknown") %>% 
  filter(year == 2006 | year == 2012 | year == 2018)

ggplot(car_colors) + 
  geom_bar(aes(x = 0, y = n, fill = fill_color), width = 2, position = "fill", stat = "identity") +
  annotation_raster(car, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  scale_fill_identity() +
  scale_x_continuous(expand  = c(0, 0.0045), limits = c(-1.2, 1)) +
  scale_y_continuous(expand  = c(0, 0.0045)) +
  coord_flip() +
  facet_wrap(vars(year), ncol = 1) +
  labs(
    title = "Colors of newly registered cars in Sweden",
    subtitle = "Similar colors were grouped · Data by Transportstyrelsen · Graphic: Georgios Karamanis"
       ) +
  theme_void(base_family = "Publico Headline Black Italic", base_size = 24) +
  theme(
    aspect.ratio = 0.2977,
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.margin = margin(20, 35, 20, 35),
    strip.text = element_text(margin = margin(10, 0, 20, 0)),
    panel.spacing.y = unit(3, "lines"),
    plot.title = element_text(size = 22, hjust = 0.5, margin = margin(0, 0, 0, 0), family = "Quotes Caps Regular"),
    plot.subtitle = element_text(size = 9, margin = margin(10, 0, 20, 0), hjust = 0.5)
  ) +
  ggsave(here::here("plots", "car-colors.png"), dpi = 320, width = 6, height = 9)
