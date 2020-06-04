
library(tidyverse)
library(lubridate)
library(sf)
library(ggmap)
library(RColorBrewer)
library(extrafont)

loadfonts(device = "win")

style_string <- '&style=element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road.arterial%7Celement:labels%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels%7Cvisibility:off&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Cvisibility:off&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x9e9e9e'
basemap <- get_googlemap(center = c(lon = 48.033574, lat = 46.347869), zoom = 12, inject = style_string, maptype = 'roadmap')

maps_theme <-   theme(
  axis.ticks = element_blank(), 
  axis.text = element_blank(), 
  panel.grid = element_blank(),
  strip.background = element_blank(),
  title = element_text(family = 'Roboto Medium'), 
  plot.subtitle = element_text(family = 'Roboto Light'), 
  plot.caption = element_text(family = 'Roboto Light'), 
  legend.title = element_text(family = 'Roboto'),
  text = element_text(family = 'Roboto')
)

plots_theme <-   theme(
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  strip.background = element_blank(),
  title = element_text(family = 'Roboto Medium'), 
  plot.subtitle = element_text(family = 'Roboto Light'), 
  plot.caption = element_text(family = 'Roboto Light'), 
  legend.title = element_text(family = 'Roboto'),
  text = element_text(family = 'Roboto')
)

df_astra <- readRDS('df_astra.rds')

# 1. Distribution by year-month ----

# version with monthly granularity
# df_gg1 <- df_astra %>% 
#   mutate(
#     mn = month(date, label = TRUE),
#     yr = year(date)
#   ) %>%
#   group_by(yr, mn) %>%
#   summarise(n = n()) %>% 
#   mutate(yr_mn = paste(mn, yr))
# df_gg1$yr_mn <- as_factor(df_gg1$yr_mn)

# version with quarterly granularity
# df_gg1 <- df_astra %>% 
#   mutate(
#     qrt = quarter(date),#, label = TRUE),
#     yr = year(date)
#   ) %>%
#   group_by(yr, qrt) %>%
#   summarise(n = n()) %>% 
#   mutate(yr_qrt = paste(qrt, yr))
# df_gg1$yr_qrt <- as_factor(df_gg1$yr_qrt)

# version with yearly granularity
df_gg1 <- df_astra %>%
  filter(date < '2020-01-01') %>% 
  mutate(yr = year(date)) %>%
  group_by(yr) %>%
  summarise(n = n())

ggplot(df_gg1, aes(x = yr, y = n)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = -90, hjust = 0))

# 2. Distribution by day time ----

df_gg2 <- df_astra %>% 
  mutate(
    hour_id = hour(start),
    month_id = month(start),
    fill_category = case_when(
      hour_id %in% c(7, 9) ~ 'low',
      hour_id == 17 ~ 'high',
      TRUE ~ 'other'
    ),
    fill_Category = factor(fill_category, levels = c('low', 'other', 'high'), ordered = T)
  )

xlabels <- unique(str_c(df_gg2$hour_id, ':00'))

gg2 <- ggplot(df_gg2, aes(x = hour_id) ) +
  geom_histogram(binwidth = 1, aes(fill = fill_category)) +
  geom_text(stat = "count", aes(label = stat(count)), vjust = -0.5, family = 'Roboto') +
  scale_x_continuous(breaks = unique(df_gg2$hour_id), labels = xlabels, expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  labs(
    title = 'Кол-во возгораний по часам',
    subtitle = 'данные пресс-центра МЧС за январь 2015 г. - март 2020 г.',
    x = '',
    y = '',
    caption = 'Автор - Кирилл Гудков / Инструмент - R / vk.com/astr.city.data'
  ) +
  theme(
    panel.background = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none',
    title = element_text(family = 'Roboto Medium'), 
    plot.subtitle = element_text(family = 'Roboto Light'), 
    plot.caption = element_text(family = 'Roboto Light'), 
    legend.title = element_text(family = 'Roboto'),
    text = element_text(family = 'Roboto'),
    strip.background = element_blank()
  ) +
  scale_fill_manual(values = c('#EF233C', '#5CD697', 'grey60'))

gg2
ggsave(filename = 'gg2.png', plot = gg2, device = 'png', path = 'plots', dpi = 450, width = 10, height = 5)

rm(gg2, df_gg2); gc()



# 3. Map with area in fire ----

exclude_coords <- df_astra %>% 
  group_by(lat, lon) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

df_gg3 <- df_astra %>% 
  filter(
    lon > 47.67, lon < 48.42, lat > 45.29, lat < 46.7, 
    !is.na(lon), !is.na(lat), !is.na(area), 
    lat != exclude_coords$lat[1] & lon != exclude_coords$lon[1]
  ) %>% 
  filter(area > 50)

map1 <- ggmap(basemap) +
  stat_density_2d(data = df_gg3, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = 'polygon', h = NULL, adjust = c(0.2, 0.2)) +
  geom_point(data = df_gg3, aes(x = lon, y = lat), alpha = 0.5, shape = 20, col = 'black') +
  maps_theme +
  theme(legend.position = 'none') +
  labs(
    title = 'Карта пожаров Астрахани',
    subtitle = 'данные пресс-центра МЧС за январь 2015 г. - март 2020 г. // \nучитывались пожары с площадью загорания > 50 м²',
    x = '',
    y = '',
    caption = 'Автор - Кирилл Гудков / Инструмент - R / vk.com/astr.city.data'
  ) + 
  scale_fill_gradient(low = 'red', high = 'yellow')

map1
ggsave(filename = 'map1.png', plot = map1, device = 'png', path = 'plots', dpi = 400, width = 7, height = 7)

rm(df_gg3, exclude_coords, map1); gc()

# 4. Duration-area relation ----

df_gg4 <- df_astra %>% 
  filter(!is.na(fire_duration)) %>% 
  mutate(
    duration_category = case_when(
      fire_duration <= 30 ~ 1,
      fire_duration > 30 & fire_duration < 60 ~ 2,
      TRUE ~ 3
    ),
    duration_category = factor(duration_category, levels = c(1,2,3), labels = c('до получаса', 'от получаса до 1 часа', 'свыше 1 часа'))
  )

gg4 <- ggplot(df_gg4, aes(x = duration_category)) + 
  geom_bar(width = 0.98, aes(fill = duration_category)) +
  geom_text(stat = "count", aes(label = stat(count)), vjust = -0.5, family = 'Roboto') +
  labs(
    x = '',
    y = '',
    title = 'Продолжительность пожаров до ликвидации',
    subtitle = 'данные пресс-центра МЧС за январь 2015 г. - март 2020 г.',
    caption = 'Автор - Кирилл Гудков / Инструмент - R / vk.com/astr.city.data'
  ) +
  plots_theme +
  theme(legend.position = 'none') +
  scale_y_continuous(breaks = seq(0, 1000, 500), limits = c(0, 1500), expand = c(0, 0)) +
  scale_fill_manual(values = c('#878787', '#EF233C', '#D90429'))

gg4
ggsave(filename = 'gg4.png', plot = gg4, device = 'png', path = 'plots', dpi = 400, width = 7, height = 5)

rm(gg4, df_gg4); gc()


