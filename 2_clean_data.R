
library(tidyverse)
library(lubridate)

# - First we bind data together -

df1 <- readRDS('result_pozhar.rds')
df2 <- readRDS('result_pozharno_spas_1_500.rds')
df3 <- readRDS('result_pozharno_spas_500_1628.rds')
df4 <- readRDS('result_summary_1_500.rds')

df <- df1 %>% mutate(id = '1') %>% 
  rbind(df2 %>% mutate(id = '2')) %>% 
  rbind(df3 %>% mutate(id = '2')) %>% 
  rbind(df4 %>% mutate(id = '3'))

ggplot(df, aes(x = date, fill = id)) + geom_bar()

# From the plot above we see that regular reports about fires start to appear on the website from 2015
# And from 2019 there are duplicates of fires, because of the appearence of summary reports

rm(df1, df2, df3, df4); gc()

# - Now let's clean the data as much as we can -

df_drop_dup <- df %>% 
  filter(date >= '2015-01-01') %>% 
  mutate(
    # format area of fire
    area = text %>% str_replace_all(pattern = '(кв\\. м\\.|кв\\.м\\.|кв м|кв\\. м|кв м\\.).*', replacement = '') %>% 
      str_replace_all(pattern = '(.*Площадь пожара составляет |.*площади)', replacement = '') %>% 
      str_replace_all(pattern = ',', replacement = '.'),
    area = ifelse(area == text, NA, area),
    area = str_trim(area) %>% as.numeric(),
    
    # obtain address
    address = text %>% str_replace_all(pattern = '(произошло.*|загорание.*)', replacement = '') %>% 
      str_replace_all(pattern = '.*[0-9]{2}[:space:]мин', replacement = '') %>% 
      str_trim() %>% 
      str_replace_all(pattern = '^.*([:space:]в|\\.в|[:space:]на)[:space:]', replacement = '') %>% 
      str_trim(),
    
    # get time of fire start
    start_time = text %>% str_replace_all(pattern = 'мин.*', replacement = '') %>% 
      str_replace_all(pattern = '^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}.*([0-9]{2}).*([0-9]{2}).*', replacement = '\\1:\\2'),
    start = ymd_hm(str_c(date, ' ', start_time)),
    
    # time of extinguishing the fire
    end_time = text %>% str_replace_all(pattern = '(.*Ликвидация|.*ликвидация)', replacement = '') %>%
      str_replace_all(pattern = '.*([0-9]{2}).*([0-9]{2}).*\\(.*', replacement = '\\1:\\2') %>% str_trim(),
    end_time = str_replace_all(end_time, pattern = '^24:', replacement = '00:'),
    end_time = ifelse(str_detect(text, pattern = 'Ликвидация до прибытия'), NA, end_time),
    
    # format special cases
    end_time = str_replace_all(end_time, pattern = '.*([0-9]{2}).*([0-9]{2}).*мин.*', replacement = '\\1:\\2'),
    end_time = if_else(condition = str_detect(end_time, pattern = '[0-9]{2}:[0-9]{2}'), true = end_time, false = NULL),
    end = ymd_hm(str_c(date, ' ', end_time)),
    end = if_else(start > end, end + days(1), end),
    
    fire_duration = (end - start)/60 %>% as.numeric(),
    
    # information about number of firefighters and fire cars
    firefighters = text %>% 
      str_replace_all(pattern = '.*от[:space:]МЧС[:space:](работали|работала|работают)[:space:]([0-9]{1,3})[:space:](единицы|единиц|единица)[:space:]техники[:space:]и[:space:]([0-9]{1,3})[:space:](человек|человека).*', replacement = '\\4') %>% 
      str_trim(),
    firefighters = ifelse(firefighters == text, NA, firefighters) %>% as.integer(),
    firecars = text %>% 
      str_replace_all(pattern = '.*от[:space:]МЧС[:space:](работали|работала|работают)[:space:]([0-9]{1,3})[:space:](единицы|единиц).*', replacement = '\\2') %>% 
      str_trim(),
    firecars = ifelse(firecars == text, NA, firecars) %>% as.integer()
  ) %>% 
  select(text, address, area, date, start, end, fire_duration, firefighters, firecars, id) %>% 
  filter(fire_duration > 0, fire_duration < 360) %>% 
  distinct(start, .keep_all = T) %>% 
  distinct(address, firefighters, .keep_all = T)

# Check regular expressions:
#
# df_drop_dup$text[2375]
# df_drop_dup$text[2375] %>% str_replace_all(pattern = '.*от[:space:]МЧС[:space:](работали|работала|работают)[:space:]([0-9]{1,3})[:space:](единицы|единиц|единица)[:space:]техники[:space:]и[:space:]([0-9]{1,3})[:space:](человек|человека).*', replacement = '\\4') %>% 
#   str_trim()
# 
# df_drop_dup$text[1545]
# df_drop_dup$text[1545] %>% str_replace_all(pattern = '(произошло.*|загорание.*)', replacement = '') %>% 
#   str_replace_all(pattern = '.*[0-9]{2}[:space:]мин', replacement = '') %>% 
#   str_trim() %>% 
#   str_replace_all(pattern = '^.*[:space:]в[:space:]', replacement = '') %>% 
#   str_trim()
# 
# ggplot(df_drop_dup, aes(x = date, fill = id)) + geom_bar()
# ggplot(df_drop_dup, aes(x = fire_duration, y = area)) + geom_point()

# - Now let's subset data to only include fires in Astrakhan -

df_astra <- df_drop_dup %>% 
  filter(str_detect(string = address, pattern = '(Астрахань|Астрахани)')) %>% 
  mutate(
    address = address %>% str_replace_all(pattern = '.*районе', replacement = '') %>% str_trim() %>% 
      str_replace_all(pattern = 'Астрахани', replacement = 'Астрахань') %>% 
      str_replace_all(pattern = '[:space:]по[:space:]', replacement = ', ') %>% str_trim() %>% 
      str_replace_all(pattern = '^[:punct:].*г\\.', replacement = 'г.') %>% str_trim()
  ) %>% 
  filter(
    str_detect(string = address, pattern = '(г.Астрахань|г. Астрахань)'), 
    address != 'г. Астрахань',
    address != 'г.Астрахань'
  )

# - Geocode data -

library(rjson)
library(XML)

df_astra <- readRDS('df_astra.rds')

api_key <- "insert your api-key here"

geocode <- function(address) {
  
  location <- str_replace_all(string = address, pattern = ',', replacement = '') %>% 
    str_replace_all('[:space:]', '+')
  
  url <- str_c('http://geocode-maps.yandex.ru/1.x/?apikey=', api_key, '&geocode=', location) %>% 
    URLencode()
  
  xml_data <- readLines(url) %>% 
    paste(sep = "\n", collapse="") %>% 
    xmlParse(asText = TRUE) %>% 
    xmlToList()
  
  pos <- xml_data$GeoObjectCollection$featureMember$GeoObject$Point$pos
  
  lon <- str_replace(string = pos, pattern = '[:space:].*', replacement = '') %>% as.numeric()
  lat <- str_replace(string = pos, pattern = '.*[:space:]', replacement = '') %>% as.numeric()
  
  result <- data.frame(address = address, lon = lon, lat = lat)
  return(result)
}

df_coords <- map_df(.x = df_astra$address, .f = possibly(geocode, otherwise = NULL))
df_coords <- df_coords %>% distinct(address, .keep_all = T)
saveRDS(df_coords, 'df_coords.rds')
df_coords <- readRDS('df_coords.rds')

df <- df_astra %>% left_join(df_coords, by = 'address') %>% mutate(fire_duration = as.numeric(fire_duration))
saveRDS(df, 'df_astra.rds')

# - Fix wrong time of extinguishing -

df <- readRDS('df_astra.rds')

# try to fix
df2 <- df %>% 
  mutate(
    is_dirty = str_detect(text, 'объявлен'),
    fire_duration = ifelse(is_dirty, NA, fire_duration),
    end = start + fire_duration * 60
  )

# check fix
df2 %>% filter(is_dirty == T) %>% head()
df2 %>% filter(is_dirty == F) %>% head()

#implement fix
df <- df %>% 
  mutate(
    is_dirty = str_detect(text, 'объявлен'),
    fire_duration = ifelse(is_dirty, NA, fire_duration),
    end = start + fire_duration * 60
  ) %>% 
  select(-is_dirty)

saveRDS(df, 'df_astra.rds')






