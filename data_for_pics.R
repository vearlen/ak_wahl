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

# http://db.arbeitsklima.at/en
AKIndex <- read_delim("data/AKI-Overall index-Feb1997-Feb2023.csv",
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)


df_TUD <- read_csv("data/TradeUnionDensity_ILR_TUMT_NOC_RT_A-filtered-2024-04-05.csv")

# data for trade union density --------------------------------------------

df_tud_sel <- df_TUD %>% 
  select(ref_area.label,time,obs_value,source.label) %>% 
  left_join(cntr_codes,by=join_by( ref_area.label == Country)) %>% 
  mutate(Code = case_when(
    ref_area.label == "Viet Nam" ~ ':vn:',
    ref_area.label == "Taiwan, China" ~ ':tw:',
    ref_area.label == "Russian Federation" ~ ':ru:',
    ref_area.label == "Hong Kong, China" ~ ':hk:',
    ref_area.label == "Eswatini" ~ ':sz:',
    ref_area.label == "Occupied Palestinian Territory" ~ ':ps:',
    ref_area.label == "Moldova, Republic of" ~ ':md:',
    ref_area.label == "North Macedonia" ~ ':mk:',
    ref_area.label == "Lao People's Democratic Republic" ~ ':la:',
    ref_area.label == "Congo, Democratic Republic of the" ~ ':cd:',
    ref_area.label == "Korea, Republic of" ~ ':kr:',
    ref_area.label == "Bolivia" ~ ':bo:',
    ref_area.label == "Czechia" ~ ':cz:',
    ref_area.label == "United States" ~ ':us:',
    ref_area.label == "Türkiye" ~ ':tr:',
    ref_area.label == "Venezuela, Bolivarian Republic of" ~ ':ve:',
    ref_area.label == "Tanzania, United Republic of" ~ ':tz:',
    TRUE ~Code
  )) %>% 
  mutate(country = paste0(Code,' ',ref_area.label)) %>% 
  select(-c(ref_area.label,Code)) %>% 
  # filter(obs_value > 26.24) %>% 
  select(country,year=time,`%` = obs_value,source = source.label) %>% 
  arrange(-`%`)



# dw_create_chart(title = "Trade Union Density",type = 'd3-bars')
dw_data_to_chart(df_tud_sel,chart_id = 'm3Nwc')
dw_edit_chart('m3Nwc',intro="", source_name = "ILOSTAT",
              source_url = 'https://rshiny.ilo.org/dataexplorer41/?lang=en&segment=indicator&id=EAP_2WAP_SEX_AGE_RT_A',
              byline = '<a href="https://www.linkedin.com/in/itishchenko/">Ilya Tishchenko</a>')

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

tmp4 <- df_eu %>% 
  select(Wahl,abgegeben_ratio) %>% 
  mutate(abgegeben_ratio = round(abgegeben_ratio*100,2)) %>% 
  separate(Wahl,into = c("day","month","year")) %>% 
  mutate(year = as.integer(year)) %>% 
  select(year,EU = abgegeben_ratio)

df_turn <- tmp3 %>% 
  full_join(tmp2) %>% 
  full_join(tmp1) %>% 
  full_join(tmp4) %>% 
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
dw_data_to_chart(select(df_turn,year_label,Parlament,AK,`Change in AK voters`,EU),chart_id = 'bYhOq' ) 
dw_edit_chart('bYhOq',source_name = "AK, author calculations",
              source_url = 'https://vearlen.github.io/ak_wahl/',
              byline = '<a href="https://www.linkedin.com/in/itishchenko/">Ilya Tishchenko</a>')

# interpolated chart
# dw_data_to_chart(df_turn,chart_id = '7A2ow' )
# dw_edit_chart('7A2ow',title="Turnout interpolated",
              # byline = '<a href="https://t.me/vearlen">Ilya Tishchenko</a>')


# data compare parties ----------------------------------------------------

tmp1 <- df %>% 
  filter(Label %in% c("FSG","ÖAAB","FA"),Land == "Gesamt") %>% 
  select(Jahr,Label,ratio) %>% 
  mutate(Label = case_when(
    Label == "FSG" ~ "SPÖ AK",
    Label == "ÖAAB" ~ "ÖVP AK",
    Label == "FA" ~ "FPÖ AK"
  )) %>% 
  pivot_wider(id_cols = Jahr,names_from = Label,values_from = ratio) %>% 
  rename(year=Jahr)

tmp2 <- df_nationalrat %>% 
  select(`FPÖ Prlm` = FPÖ_ratio,`SPÖ Prlm` = SPÖ_ratio,`ÖVP Prlm` = ÖVP_ratio,Wahl) %>% 
  separate(Wahl,into = c("day","month","year")) %>% 
  mutate_if(is.numeric,~100*.) %>% 
  mutate(year = as.integer(year)) %>% 
  select(-c(day,month)) 

df_comparison <- full_join(tmp1,tmp2) %>% arrange(year) %>% 
  mutate_if(is.numeric,round,0) %>% 
  select(year,"SPÖ Prlm","SPÖ AK","ÖVP Prlm","ÖVP AK","FPÖ Prlm","FPÖ AK")


# create comparison chart -------------------------------------------------
# table
# dw_create_chart(title="Comparison of parties results",type='d3-lines')
dw_data_to_chart(df_comparison,chart_id = 'xoCr1')
dw_edit_chart('xoCr1',source_name = "AK, author calculations",
              source_url = 'https://vearlen.github.io/ak_wahl/',
              byline = '<a href="https://www.linkedin.com/in/itishchenko/">Ilya Tishchenko</a>')


# dw_create_chart(title="Comparison of parties results",type='d3-lines')
df_comp_fill <- fill_(df_comparison,names(df_comparison))

dw_data_to_chart(df_comp_fill,chart_id = 'tl1Cf')
dw_edit_chart('tl1Cf',source_name = "AK, author calculations",
              source_url = 'https://vearlen.github.io/ak_wahl/',
              byline = '<a href="https://www.linkedin.com/in/itishchenko/">Ilya Tishchenko</a>')

cntr_codes <- read_csv('data/Country flag lookup table.csv')

# data for comparison parties lately with minor ---------------------------
df_nationalrat %>% 
  View()

tmp1 <- df %>% 
  filter(!is.na(ratio), Land == "Gesamt", Jahr >= 1989,
         Label != "Wahlbeteiligung") %>% 
  select(Jahr,Label,ratio) %>% 
  mutate(Label = case_when(
    Label == "FSG" ~ "SPÖ AK",
    Label == "ÖAAB" ~ "ÖVP AK",
    Label == "FA" ~ "FPÖ AK",
    Label == "AUGE/UG" ~ "GRÜN AK",
    Label == "GLB" ~ "KPÖ AK",
    TRUE ~ Label
  )) %>% 
  pivot_wider(id_cols = Jahr, names_from = Label, values_from = ratio) %>% 
  rename(year=Jahr) 

tmp2 <-
  df_nationalrat %>% 
  select(`FPÖ Prlm` = FPÖ_ratio,`SPÖ Prlm` = SPÖ_ratio,`ÖVP Prlm` = ÖVP_ratio,
        `GRÜN Prlm` = GRÜN_ratio, `KPÖ Prlm` = KPÖ_ratio,  Wahl) %>%
  separate(Wahl,into = c("day","month","year")) %>% 
  mutate_if(is.numeric,~100*.) %>% 
  mutate(year = as.integer(year)) %>% 
  filter(year >=1989) %>% 
  select(-c(day,month)) 

df_comp_1989 <- full_join(tmp1,tmp2) %>% arrange(year) %>% 
  mutate_if(is.numeric,round,0)

df_comp_1989_fill <- fill_(df_comp_1989,names(df_comp_1989))

# create comparison chart from 1989 ---------------------------------------
# dw_create_chart(title="Comparison of parties results from 1989",type='d3-lines')



dw_data_to_chart(df_comp_1989_fill,chart_id = 'gVlhu')
dw_edit_chart('gVlhu',source_name = "AK, author calculations",
              source_url = 'https://vearlen.github.io/ak_wahl/',
              byline = '<a href="https://www.linkedin.com/in/itishchenko/">Ilya Tishchenko</a>')


# data for map ------------------------------------------------------------

df2014_win <- 
  df %>% 
  # filter(Jahr == 2014 ) %>% 
  filter(!Label %in% c("Wahlbeteiligung","Wahlberechtigte","AbgegebeneStimmen",
                       "UngültigeStimmen","GültigeStimmen")) %>% 
  filter(ratio > 50) %>% 
  left_join(ATavailable_keys,by=join_by(Land==Name)) %>% 
  filter(Land != "Gesamt") %>% 
  View()

# map chart
dw_data_to_chart(df2014_win,chart_id = "q3GUV")
dw_edit_chart('q3GUV',intro="", source_name = "AK, author calculations",
              source_url = 'https://vearlen.github.io/ak_wahl/',
              byline = '<a href="https://www.linkedin.com/in/itishchenko/">Ilya Tishchenko</a>')


# create multiple maps ----------------------------------------------------
ak_years <- df$Jahr %>% unique()
ak_years <- ak_years[-16]
i = ak_years[1]
for (i in ak_years){
tmp_map <-
  df %>% 
  filter(Jahr == i ) %>%
  filter(!Label %in% c("Wahlbeteiligung","Wahlberechtigte","AbgegebeneStimmen",
                       "UngültigeStimmen","GültigeStimmen")) %>% 
  group_by(Land) %>% 
  top_n(1,ratio) %>% 
  left_join(ATavailable_keys,by=join_by(Land==Name)) %>% 
  filter(Land != "Gesamt") %>% 
  arrange(Jahr) %>% 
  ungroup(Land) %>% 
  select(Jahr,  Label, Postal, ratio)

map_new_label <- paste0("AK elections results in ",i)

map_new_name <- dw_create_chart(title=map_new_label,type='d3-maps-choropleth')

dw_data_to_chart(tmp_map,chart_id = map_new_name$id)

dw_edit_chart(map_new_name,
                title=map_new_label,
                axes = list(
                keys = "Postal",
                values = "Label"),
                visualize = list(basemap = "austria-states",
                               "map-key-attr"="Postal",
                               tooltip = list(
                                 body = "{{ ROUND(ratio) }}%",
                                 title = "{{ postal }} | {{label}}"
                               )),
              source_name = "AK, author calculations",
              source_url = 'https://vearlen.github.io/ak_wahl/',
              byline = '<a href="https://www.linkedin.com/in/itishchenko/">Ilya Tishchenko</a>',
              )
}
png_chart <- dw_export_chart(map_new_name,type = 'png')
magick::image_write(png_chart,path = paste0('maps/',map_new_label,'.png'))

