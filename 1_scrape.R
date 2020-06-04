
library(tidyverse)
library(rvest)
library(httr)

# - First we get all the articles and links -

get_titles_and_links <- function(page) {
  
  url <- str_c('https://30.mchs.gov.ru/deyatelnost/press-centr/operativnaya-informaciya?page=', page)
  
  page_data <- read_html(url) %>% 
    html_nodes(css = '.articles-item__title')
  
  df <- data.frame(
    title = html_text(x = page_data),
    link = html_attr(x = page_data, name = 'href')
  )
  return(df)
}

df <- map_df(.x = 1:500, .f = possibly(get_titles_and_links, otherwise = NULL))

# -------------------------------------
# - 1. Articles with daily summary -
# -------------------------------------

# - Let's subset only valueble articles -

df_val <- df %>% filter(str_detect(string = title, pattern = 'Информация о пожарах'))

# - Then let's copy all the text from articles -

extract_information_from_article <- function(url) {
  
  article <- read_html(str_c('https://30.mchs.gov.ru', url))
  p <- article %>% 
    html_nodes(css = 'p') %>% 
    html_text()
  
  ind <- p %>% str_detect(pattern = '^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}')
  
  result <- data.frame(
    text = p[ind]
  ) %>% 
    mutate(
      date = text %>% str_extract(pattern = '^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}') %>% as.Date(format = '%d.%m.%Y'),
      time_of_registration = text %>% str_replace_all(pattern = ' мин. мск.\\).*', replacement = '') %>% 
        str_replace_all(pattern = '.* \\(', replacement = '') %>% 
        str_replace_all(pattern = ' час. ', replacement = ':'),
      address = text %>% str_replace_all(pattern = ' произошло.*', replacement = '') %>% 
        str_replace_all(pattern = '.*мск.\\) в ', replacement = ''),
      area = text %>% str_replace_all(pattern = '.*площади ', replacement = '') %>% 
        str_replace_all(pattern = ' кв.м..*', replacement = ''),
      end_time = text %>% str_replace_all(pattern = '.*Ликвидация в ', replacement = '') %>% 
        str_replace_all(pattern = ' мин. мск.\\).*', replacement = '') %>% 
        str_replace_all(pattern = '.* \\(', replacement = '') %>% 
        str_replace_all(pattern = ' час. ', replacement = ':'),
      units = text %>% str_replace_all(pattern = '.*работали ([0-9]{1,3}) (единица|единицы|единиц) техники.*', replacement = '\\1'),
      firefighters = text %>% str_replace_all(pattern = '.* ([0-9]{1,3}) (человек|человека) личного состава.*', replacement = '\\1'),
      is_injured = text %>% str_replace_all(pattern = '.*Ликвидация в ', replacement = '') %>% 
        str_replace_all(pattern = '.*мск.\\). ', replacement = '')
    )
  
  return(result)
}

result <- map_df(.x = df_val$link, .f = extract_information_from_article)



# --------------------------------------------
# - 2. Articles related to a particular fire -
# --------------------------------------------

df1 <- readRDS('df1_500.rds')
df2 <- readRDS('df500_1628.rds')
df <- rbind(df1, df2)
rm(df1, df2)

# - Subset articles -

df_val <- df %>% filter(str_detect(string = title, pattern = 'Пожарно-спасательные подразделения ликвидировали пожар'))
result <- map_df(.x = df_val$link, .f = possibly(extract_information_from_article, otherwise = NULL))

df_val <- df %>% filter(str_detect(string = title, pattern = 'Пожар '))
result <- map_df(.x = df_val$link, .f = possibly(extract_information_from_article, otherwise = NULL))


