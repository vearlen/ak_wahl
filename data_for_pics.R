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
