# Импорт и очистка данных -------------------------------------------------
library(tidyverse)
library(readxl)
library(plotly)
library(VIM) # использовал для визуализации пропусков
library(imputeTS) # Для интерполяции пропусков

# Функции -----------------------------------------------------------------
read_files <- function(x){
  df <- read_excel(x) |>  
    mutate(Local_time = as_datetime(Local_time, format = "%d.%m.%Y %R")) |> 
    mutate(Year = year(Local_time),
           Month = as.factor(month(Local_time)),
           Day = mday(Local_time)) |> 
    select(Year, Month, Day, Tavg=T, Pr=RRR, Sn=sss, Sn_description = `E'`) |> 
    mutate(Pr = case_when(Pr == "Осадков нет" | Pr == "Следы осадков" ~ "0",
                          .default = Pr)) |> 
    arrange(Year, Month, Day) |>
    mutate(across(c(Year:Day, Sn_description), as.factor),
           across(Tavg:Sn, as.numeric)) |> 
    group_by(Year, Month, Day) |>
    summarise(
      Tavg = mean(Tavg, na.rm = T),
      Pr = sum(Pr, na.rm = T), # Потому что это накопленные осадки, нужно суммировать
      Sn = mean(Sn, na.rm = T),
      Sn_description = as.factor(str_flatten(Sn_description, ", ", na.rm = T))
    )
  return(df)
}
interactive_graph <- function(df, parametr, title) {
  plot_ly(df, type = "scatter", mode = "lines") |>
    add_trace(x = ~Date, y = ~df[[parametr]], name = title) |>
    layout(
      showlegend = F, title = title,
      xaxis = list(rangeslider = list(visible = T))
    )
}

# Данные из Обнинска ------------------------------------------------------
obninsk_temperature <- read_excel("initial_data/climate/1961_2005/23776_TTTR.xlsx") |>
  select(
    Year = год,
    Month = месяц,
    Day = день,
    Tavg = Тср,
    Pr = осадки
  )

obninsk_snow <- read.csv2("initial_data/climate/1961_2005/23776_snow.csv", fileEncoding = "windows-1251") |>
  select(
    Station = Станция,
    Year = Год,
    Month = Месяц,
    Day = День,
    Sn = Высота_снежного_покрова,
    Sn_description = Снежный_покров_.степень_покрытия
  )
obninsk <- obninsk_temperature |>
  left_join(obninsk_snow, by = c("Year", "Month", "Day")) |>
  select(-Station) |> 
  filter(Year<2005 | (Year==2005 & Month == 1))

nrow(obninsk |> 
  group_by(Year) |> 
  summarise(N = n()) |> 
  filter(N>366)) == 0 #Все годы нормальной продолжительности

rm(obninsk_snow, obninsk_temperature)

# Данные rp5.ru -----------------------------------------------------------
paths <- list.files("initial_data/climate/2005_2023", pattern = "[.]xls$", full.names = TRUE) # Просканировали все файлы в директории

rp5 <- paths |> 
  map_df(read_files)

rm(paths)

# Объединенные данные -----------------------------------------------------
climate <- rbind(obninsk, rp5)
nrow(climate |> 
  group_by(Year) |> 
  summarise(N = n()) |> 
  filter(N>366))==0 #Все годы нормальной продолжительности

rm(obninsk, rp5)

# Очистка суточных измерений из Бахты ----------------------------------------------------

summary(climate) # 271 пропуск средней температуры, 3423 пропуса снега, 1458 пропуск осадков
aggr(climate, prop = F, numbers = T) # Визуализация пропусков из пакета VIM

# Если снег равен 9999.0, его надо заменить на NA
# Снега в летние месяцы быть не может
# Если осадки NA, а снег и температура не пропущены, то, скорее всего, осадков не было (=0)
# Оставшиеся пропуски удалим методом линейной интерполяции
# (замена с помощью уравнения линейной регрессии y = kx+b)
# (na_interpolation из пакета imputeTS)

climate <- climate |> 
  mutate(Sn = case_when(
    Sn == 9999 ~ NA,
    Sn == "NaN" ~ NA,
    .default = Sn
  )) |> 
  mutate(Sn = case_when(
    Month %in% c(6:8) & is.na(Sn) ~ 0,
    .default = Sn
  )) |> 
  mutate(Pr = case_when(
    is.na(Pr) & is.na(Tavg) == F & is.na(Sn) == F ~ 0,
    .default = Pr
  )) |> 
  mutate(across(Tavg:Sn, na_interpolation))

summary(climate)
aggr(climate, prop = F, numbers = T)


# Проверка на выбросы -------------------------------------------------------
climate <- climate |>
  mutate(Date = make_date(Year, Month, Day)) # датафрейм с датами в формате дат,чтобы plot_ly смог построить интерактивный график

interactive_graph(climate, parametr = 'Tavg', "Temperature") # Есть проблемы с летними периодами за 1966-1976

interactive_graph(climate, parametr = 'Pr', "Precipitations") # Есть выбросы
# Совсем нереальные пики заменил значениями, похожими на соседние
climate <- climate |> 
  mutate(Pr = case_when(
    Pr %in% c(204.3, 202) ~ 3,
    Pr == 813 ~ 2,
    .default = Pr
  ))
interactive_graph(climate, parametr = 'Pr', "Precipitations") # стало лучше
interactive_graph(climate, parametr= 'Sn', "Snow depth")

aggr(climate, prop = F, numbers = T) # Пропусков больше нет

write_csv2(climate, "initial_data/climate/cleaned/Bakhta_daily_problem_with_summer_1966-1976.csv")


# Бахта расчет среднемесячных температур (1966-1976 исходные) -------------

Bakhta_monthly <- climate |>
  group_by(Year, Month) |>
  summarise(across(c(Tavg, Sn), mean),
            Pr = sum(Pr)) |> # Накопленное количество осадков
  mutate(across(is.character, as.numeric)) |> 
  arrange(Year, Month) |> 
  mutate(Date = make_date(Year, Month))

interactive_graph(Bakhta_monthly, "Tavg", "Temperature")


# Бор и Верхнеимбатск месячные данные -------------------------------------
Bor_monthly <- read_delim("initial_data/climate/monthly/Bor_monthly.txt",
  delim = ";", col_types = "n",
  col_names = c("Station", "Year", 1:12)) |> 
  select(-Station) |> 
  mutate_all(as.numeric) |> 
  pivot_longer(cols = -c(Year), names_to = "Month", values_to = "Tavg") |> 
  mutate(Date = make_date(Year, Month))

interactive_graph(Bor_monthly, parametr = 'Tavg', "Temperature in Bor")

Verkhneimbatsk_monthly <- read_delim("initial_data/climate/monthly/Verkhneimbatsk_monthly.txt",
  delim = ";", col_types = "n",
  col_names = c("Station", "Year", 1:12)) |> 
  select(-Station) |> 
  mutate_all(as.numeric) |> 
  pivot_longer(cols = -c(Year), names_to = "Month", values_to = "Tavg") |> 
  mutate(Date = make_date(Year, Month))

interactive_graph(Verkhneimbatsk_monthly, parametr = 'Tavg', "Temperature in Verkhneimbatsk")


# 1966-1976 Бахта замена на оср Бор - Верхнеимбатск --------
Bor_Verkhn <- Bor_monthly |>
  left_join(Verkhneimbatsk_monthly, by = c("Year", "Month")) |>
  select(Year, Month, Bor_tavg = Tavg.x, Verkhn_tavg = Tavg.y) |>
  filter(Year %in% c(1966:1976), Month %in% c(6:9)) |>
  mutate(across(c(Year, Month), as.integer)) |> 
  group_by(Year, Month) |>
  summarise(Tavg = mean(Bor_tavg, Verkhn_tavg))

Bakhta_monthly_new <- Bakhta_monthly |>
  ungroup() |> 
  mutate(across(c(Year, Month), as.integer)) |> 
  left_join(Bor_Verkhn, by = c("Year", "Month")) |>
  mutate(Tavg = ifelse(is.na(Tavg.y) == F, Tavg.y, Tavg.x)) |>
  select(Year, Month, Tavg, Sn, Pr, Date)

# Среднемесячные температуры в Бахте (исходные данные)
interactive_graph(Bakhta_monthly, "Tavg", "Temperature in Bakhta")
# График среднемесячных температур в Бахте (1966-1976гг 6-8 мес заменены на оср Бор-Верхнеимбатск)
interactive_graph(Bakhta_monthly_new, "Tavg", "Temperature in Bakhta")

rm(Bor_monthly, Bor_Verkhn, Verkhneimbatsk_monthly, climate)

write_csv2(Bakhta_monthly_new, "initial_data/climate/cleaned/Bakhta_monthly_1966-1976_replaced_by_average_Bor_Verkhn.csv")
