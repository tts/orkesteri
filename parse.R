library(tidyverse)
library(lubridate)

df <- read_delim(
  file = "Helsingin_kaupunginorkesterin_konsertit_1882-2020-csv.txt",
  col_types = cols(
    col_date(format = "%d.%m.%Y"),
    col_character(), col_character(),
    col_character(), col_character()),
  delim = "%"
)

floor_decade <- function(value){ return(value - value %% 10) }

data <- df %>% 
  mutate(Vuosikymmen = floor_decade(year(Päivämäärä))) %>% 
  group_by(Vuosikymmen, Säveltäjä) %>%  
  mutate(Konsertteja = n()) %>% 
  filter(Konsertteja >= 30) %>% 
  select(Säveltäjä, Kapellimestari, Vuosikymmen)

data_df <- data.frame(data, stringsAsFactors = FALSE)
  
saveRDS(data_df, "data_df.RDS")
