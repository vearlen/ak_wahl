library(tidyverse)
library(DatawRappr)

# import data and merge ---------------------------------------------------

# df_major <- read_csv("data/AK_Wahl_49-19_long_major.csv",
#                                      locale = locale(encoding = "WINDOWS-1252"))
# 
# df_minor <- read_delim("data/AK_Wahl_49-19_long_special.csv",
#                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","),
#                        trim_ws = TRUE)
# #
# df <- bind_rows(df_major,df_minor)
# 
# 
# df %>%
#   # distinct(Label) %>%
#   write.csv('data/AK_Wahl_49_19.csv',row.names = FALSE)



# import data -------------------------------------------------------------
df <- read.csv('data/AK_Wahl_49_19.csv')

df_nationalrat <- read_delim(
  "data/nationalratwahl.csv",
  delim = ";",
  escape_double = FALSE,
  locale = locale(decimal_mark = ","),
  trim_ws = TRUE
)

df_eu <- read_delim(
  "data/EU.csv",
  delim = ";",
  escape_double = FALSE,
  locale = locale(decimal_mark = ","),
  trim_ws = TRUE
)

ATavailable_keys <- read_csv("data/ATavailable-keys.csv")

# Wahlbeteilung/turnout ---------------------------------------------------

options(scipen = 999)

tmp1 <- df %>% 
  filter(Label == "Wahlberechtigte",Land == "Gesamt") %>% 
  mutate(votes_incr =round( (number - lag(number))/lag(number)*100,2)) %>% 
  select(year = Jahr,`Change in AK voters` = votes_incr)
  
tmp2 <- df %>% 
  filter(Label == "Wahlbeteiligung", Land == "Gesamt") %>% 
  
  select(year=Jahr,AK = ratio)

tmp3 <- df_nationalrat %>% 
  select(Wahl,abgegeben_ratio) %>% 
  mutate(abgegeben_ratio = round(abgegeben_ratio*100,2)) %>% 
  separate(Wahl,into = c("day","month","year")) %>% 
  mutate(year = as.integer(year)) %>% 
  select(year,Parlament = abgegeben_ratio)

df_turn <- tmp3 %>% 
  full_join(tmp2) %>% 
  full_join(tmp1) %>% 
  arrange(year) %>% 
  mutate(year_label = paste0('\'',str_sub(year,3)))#%>% 
  # fill(AK) %>% 
  # fill(`Change in AK voters`) %>% 
  # fill(Parlament)

# turnout graph -----------------------------------------------------------
# https://munichrocker.github.io/DatawRappr/reference/dw_edit_chart.html

dw_test_key()
dw_create_chart(title="Turnout no interpolation",type='d3-lines')

# no interpolation
dw_data_to_chart(select(df_turn,year_label,Parlament,AK,`Change in AK voters`),chart_id = 'bYhOq' ) 
dw_edit_chart('bYhOq',title="Turnout",
              byline = '<a href="https://t.me/vearlen">Ilya Tishchenko</a>')

# interpolated chart
# dw_data_to_chart(df_turn,chart_id = '7A2ow' )
# dw_edit_chart('7A2ow',title="Turnout interpolated",
              # byline = '<a href="https://t.me/vearlen">Ilya Tishchenko</a>')


# data compare parties ----------------------------------------------------

df %>% 
  filter(Label %in% c("FSG","ÖAAB","FA"),Land == "Gesamt") %>% 
  select(Jahr,Label,ratio) %>% 
  View()

df_nationalrat %>% 
  select(FPOE_ratio,SPOE_ratio,OEVP_ratio,Wahl) %>% 
  separate(Wahl,into = c("day","month","year")) %>% 
  mutate(year = as.integer(year)) %>% 
  select(-c(day,month)) %>% 
  View()
