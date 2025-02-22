library(tidyverse)
library(readxl)

# Function ----------------------------------------------------------------
#Посчитаем количество "плохого" снежного покрова за весь зимний период
# df - датафрейм со структурой Year, Month, Day, Sn_description
# quality - фильтр значений качества снега (на вход одно значение или вектор из значений)
# на выходе датафрейм с количеством дней, когда снег был заданного качества, за зимний период 
# объединяет сентябрь-декабрь прошлого года с январем-маем текущего года
bad_snow_count <- function(df, quality){
  # датасет-шаблон, вдруг в какие-то годы не будет "плохого" снега
  #Year_template <- data.frame(Year = c(min(df$Year):max(df$Year)))
  sep_dec <- df |> 
    filter(Month %in% c(9:12), Sn_description %in% quality) |> 
    group_by(Year, Station) |> 
    summarise(N = n()) |> 
    mutate(Year =Year+1)
  
  jan_may <- df |> 
    filter(Month %in% c(1:5), Sn_description %in% quality) |> 
    group_by(Year, Station) |> 
    summarise(N = n())
  
  total <-  sep_dec |> 
    full_join(jan_may, by = c("Year", "Station")) |> 
    mutate(N.x=ifelse(is.na(N.x),0, N.x)) |> 
    mutate(N.y=ifelse(is.na(N.y),0, N.y)) |> 
    mutate(count = (N.x+N.y)) |> 
    rename(First_half = N.x, Last_half = N.y)
  
  return(total)
  
}

# Import ------------------------------------------------------------------
snow_quality_data <- read_csv2("initial_data/climate/cleaned/Bakhta_daily_problem_with_summer_1966-1976.csv") |> 
  select(Year, Month, Day, Tavg, Sn_description)

# 2005 - 2023 -------------------------------------------------------------
snow_quality_2005_2023 <- snow_quality_data |> 
  filter(Year > 2005 | (Year == 2005 & Month >1))



s <- levels(as.factor(snow_quality_data$Sn_description))[c(16,20,17)] # Отбор уровней фактора, когда снег = лед, или снег не покрывает всю поверхность почвы

snow_quality_2005_2023 <- snow_quality_2005_2023 |> 
  mutate(Sn_description = ifelse(Sn_description %in% s,1,0)) |>  # 1 - если снег "плохой"
  select(Year, Month, Day,Tavg,  Sn_description) |> 
  mutate(Station = "bakhta")
rm(s, snow_quality_data)

# 1961-2005 ---------------------------------------------------------------
tavg_1961_2004 <- read_excel("initial_data/climate/1961_2005/23776_TTTR.xlsx") |> 
  select(
    Year = год,
    Month = месяц,
    Day = день,
    Tavg = Тср)

snow_quality_1961_2004 <- read.csv2("initial_data/climate/1961_2005/23776_snow.csv", fileEncoding = "windows-1251") |>
  select(
    Station = Станция,
    Year = Год,
    Month = Месяц,
    Day = День,
    Sn = Высота_снежного_покрова,
    Sn_description = Снежный_покров_.степень_покрытия,
    Q1,
    Q2,
    Q3
  )|> 
  filter(Q1 == 0) |> # по описанию данных 0 - данные о снежном покрове верные
  filter(Sn_description != 99) |>
  mutate(Sn_description = ifelse(Sn_description <= 5,1,0)) |> 
  left_join(tavg_1961_2004, by = c("Year", "Month", "Day")) |> 
  select(Year, Month, Day, Tavg, Sn_description) |> 
  mutate(Station = "bakhta")



bad_snow_bakhta <- rbind(bad_snow_count(snow_quality_1961_2004, quality = 1),
                         bad_snow_count(snow_quality_2005_2023, quality = 1)) |> 
  mutate(Station = "bakhta")


rm(snow_quality_1961_2004, snow_quality_2005_2023, tavg_1961_2004)

# Количество "плохих" снежных дней по всем станциям -----------------------

# в колонке Sn описание снежного покрова для каждого дня
# Бывает, что в один и тот же день было несколько разных описаний
# если встречается в пределах одного дня покрытие меньше половины и больше половины, значит снег выпал, появилось укрытие
# в течение 1 дня, поэтому этот день, теоретически, пережить проще, чем в течение целого дня плохой снег
# Отберем уровни, когда строго меньше половины поверхности, или лед (без вариаций)
six_station_snow <- read_csv2("initial_data/climate/cleaned/snow_quality_six_station.csv")

#21 век 6 станций
bad_condition <- levels(as.factor(six_station_snow$Sn_description))[c(6,8,11,
                                                                      19, 21, 24)]
six_station_snow <- six_station_snow |> 
  filter(Month %in% c(1:5, 9:12)) |> 
  mutate(Sn_description = ifelse(Sn_description %in% bad_condition, 1, 0)) |> 
  mutate(Station = factor(Station, levels = c("igarka", "turukhansk", "verkhneimbatsk",
                                              "bor", "vorogovo", "yartsevo"))) |> 
  group_by(Station) |> 
  bad_snow_count(quality = 1)

seven_station <- rbind(six_station_snow, bad_snow_bakhta) |> 
  ungroup() |> 
  mutate(Station = case_when(
    Station == "igarka" ~ "Игарка",
    Station == "turukhansk" ~ "Туруханск",
    Station == "verkhneimbatsk" ~ "Верхнеимбатск",
    Station == "bakhta" ~ "Бахта",
    Station == "bor" ~"Бор",
    Station == "vorogovo" ~ "Ворогово",
    Station == "yartsevo" ~ "Ярцево"
  )) |> 
  mutate(Station = factor(Station, levels = c("Игарка", "Туруханск", "Верхнеимбатск",
                                              "Бахта", "Бор", "Ворогово", "Ярцево")))

write_csv2(seven_station, "initial_data/climate/cleaned/all_station_bad_snow.csv")