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
# https://www.wahldatenbank.at/#dbst_head
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

tmp1 <- df %>% 
  filter(Label %in% c("FSG","ÖAAB","FA"),Land == "Gesamt") %>% 
  select(Jahr,Label,ratio) %>% 
  mutate(Label = case_when(
    Label == "FSG" ~ "SPOE AK",
    Label == "ÖAAB" ~ "OEVP AK",
    Label == "FA" ~ "FPOE AK"
  )) %>% 
  pivot_wider(id_cols = Jahr,names_from = Label,values_from = ratio) %>% 
  rename(year=Jahr)

tmp2 <- df_nationalrat %>% 
  select(`FPOE Prlm` = FPOE_ratio,`SPOE Prlm` = SPOE_ratio,`OEVP Prlm` = OEVP_ratio,Wahl) %>% 
  separate(Wahl,into = c("day","month","year")) %>% 
  mutate_if(is.numeric,~100*.) %>% 
  
  mutate(year = as.integer(year)) %>% 
  select(-c(day,month)) 

df_comparison <- full_join(tmp1,tmp2) %>% arrange(year) %>% 
  mutate_if(is.numeric,round,0) %>% 
  select(year,"SPOE Prlm","SPOE AK","OEVP Prlm","OEVP AK","FPOE Prlm","FPOE AK")


# create comparison chart -------------------------------------------------
# table
# dw_create_chart(title="Comparison of parties results",type='d3-lines')
dw_data_to_chart(df_comparison,chart_id = 'xoCr1')
dw_edit_chart('xoCr1',
              byline = '<a href="https://t.me/vearlen">Ilya Tishchenko</a>')


# dw_create_chart(title="Comparison of parties results",type='d3-lines')
df_comp_fill <- fill_(df_comparison,names(df_comparison))

dw_data_to_chart(df_comp_fill,chart_id = 'tl1Cf')
dw_edit_chart('tl1Cf',
              byline = '<a href="https://t.me/vearlen">Ilya Tishchenko</a>')
