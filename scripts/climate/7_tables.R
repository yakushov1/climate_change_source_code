library(tidyverse)
library(writexl)
# Стандартная ошибка - стандартное отклонение/квадратный корень из числа наблюдений
std <- function(x){ sd(x)/sqrt(length(x)) } 


# Таблица для вставки в диссертацию ---------------------------------------
# работает только после запуска всех скриптов в директории, то есть последней
monthly_anomaly <- read_csv2("initial_data/climate/cleaned/monthly_anomaly.csv")
monthly_tavg_paths <- list.files("initial_data/climate/cleaned/trends/monthly_tavg/", full.names = T)
monthly_tanomaly_paths <- list.files("initial_data/climate/cleaned/trends/monthly_t_anomaly/", full.names = T)

monthly_tavg_trends <- read_csv(monthly_tavg_paths) |> 
  pivot_wider(id_cols = c(Station, Month), values_from = c(RR, P, b),
              names_from = Parametr)

monthly_tanomaly_trends <- read_csv(monthly_tanomaly_paths) |> 
  pivot_wider(id_cols = c(Station, Month), values_from = c(RR, P, b),
              names_from = Parametr)

#Общая таблица со значениями и характериситками трендов
for_text <- monthly_anomaly |> 
  group_by(Station, Month) |> 
  summarise(across(c(Tavg, T_anomaly), list(
    SE = \(x) round(std(x), 2), 
    mean = \(x) round(mean(x), 1))),
    T_diff = round(max(T_diff), 1)) |> 
  left_join(monthly_tavg_trends, by = c("Station", "Month")) |> 
  left_join(monthly_tanomaly_trends, by = c("Station" ,"Month"))

# Топ-3 тренда по станциям (средние температуры)
top_b_tavg <- for_text |> 
  group_by(Station) |> 
  slice_max(b_tavg, n=3) |> 
  mutate(Tavg_and_SE = str_c(Tavg_mean, Tavg_SE, sep = "±")) |> 
  mutate(Station = case_when(
    Station == "average_by_all_station" ~ "Все станции",
    Station == "bakhta" ~ "Бахта",
    Station == "bor" ~ "Бор",
    Station == "igarka" ~ "Игарка",
    Station == "turukhansk" ~ "Туруханск",
    Station == "verkhneimbatsk" ~ "Верхнеимбатск",
    Station == "vorogovo" ~ "Ворогово",
    Station == "yartsevo" ~ "Ярцево"
  )) |> 
  select(Станция = Station,
         Месяц = Month,
         Ср.температуры=Tavg_and_SE,
         b=b_tavg,
         Rsqr = RR_tavg,
         P = P_tavg,
         T_diff)
  
# Топ-3 тренда по станциям (температурные аномалии)
top_b_tanomaly <- for_text |> 
  group_by(Station) |> 
  slice_max(b_t_anomaly, n=3) |> 
  mutate(T_anomaly_and_SE = str_c(T_anomaly_mean, T_anomaly_SE, sep = "±")) |> 
  mutate(Station = case_when(
    Station == "average_by_all_station" ~ "Все станции",
    Station == "bakhta" ~ "Бахта",
    Station == "bor" ~ "Бор",
    Station == "igarka" ~ "Игарка",
    Station == "turukhansk" ~ "Туруханск",
    Station == "verkhneimbatsk" ~ "Верхнеимбатск",
    Station == "vorogovo" ~ "Ворогово",
    Station == "yartsevo" ~ "Ярцево"
  )) |> 
  select(Станция = Station,
         Месяц = Month,
         Аномалии=T_anomaly_and_SE,
         b=b_t_anomaly,
         Rsqr = RR_t_anomaly,
         P = P_t_anomaly)

write_xlsx(top_b_tanomaly, "initial_data/climate/cleaned/tables_for_text/top_3_t_anomaly_by_b.xlsx")
write_xlsx(top_b_tavg, "initial_data/climate/cleaned/tables_for_text/top_3_tavg_by_b.xlsx")
