library(tidyverse)


# Import ------------------------------------------------------------------


Bakhta_daily <- read_csv2("initial_data/climate/cleaned/Bakhta_daily_problem_with_summer_1966-1976.csv") |>
  filter(Year > 1976) |> #Потому что до 1977 года проблемы с летними месяцами, => нельзя выделить сезоны точно
  select(-Date)

# Function ----------------------------------------------------------------
means_smooth <- function(x, n) {
  pracma::movavg(x, n=10, type=c("s")) #для подсчета скользящего среднего
}
std <- function(x){ sd(x)/sqrt(length(x)) } # Стандартная ошибка - стандартное отклонение/квадратный корень из числа наблюдений

season_by_crossing_stop_level <- function(input_data, parametr, stop_level){
  
  first_half_of_year <- input_data |>
    filter(Month %in% c(1:6)) #только  первая половина года
  last_half_of_year <- input_data |>
    filter(Month %in% c(7:12)) #только вторая половина года
  
  check_more_then_stop_level <- function(df){
    df |> 
      group_by(Year) |>
      mutate(
        more_then_zero = {{parametr}} > {{stop_level}}, # логический столбец, параметр больше 0? (TRUE | FALSE)
        chng1 = cumsum(more_then_zero != lag(more_then_zero, def = first(more_then_zero)))
      )
    # lag(more_then_zero, def = first(more_then_zero))
    # lag сдвигает вектор на +1 (в начале вектора будет добавлено значение)
    # по умолчанию это значение NA, но def переопределяет его на первое в исходном столбце more_then_zero
    # cumsum - кумулятивная сумма
    # в итоговом столбце будет цифра, обозначающая, какой это переход через 0 (в любую сторону) в текущем году
    # в столбце chng1 - порядковый номер перехода температур через 0 в конкретном году
   }
  
  beginning_year <- first_half_of_year |>
    check_more_then_stop_level() |> 
    group_by(Year) |> 
    mutate(Season = ifelse(chng1==0, "Winter_first", 
                           ifelse(max(chng1)==1 & chng1 !=0, "Summer", 
                                  ifelse(chng1 %in% c(1:(max(chng1)-1)), "Spring",
                                         "Summer")
                           )))
  end_year <- last_half_of_year |> 
    check_more_then_stop_level() |> 
    group_by(Year) |> 
    mutate(Season = ifelse(chng1==0, "Summer", 
                           ifelse(max(chng1)==1 & chng1 !=0, "Winter_last", 
                                  ifelse(chng1 %in% c(1:(max(chng1)-1)), "Autumn",
                                         "Winter_last")
                           )))
  
  all_year <- rbind(beginning_year, end_year) |> 
    arrange(Year, Month, Day)
  
  
  #Аггрегации по сезонам, кроме зимы
  aggregate_by_season <- all_year |> 
    filter(Season!="Winter_last", Season!="Winter_first") |> 
    group_by(Year, Season) |> 
    rename(T_=Tavg) |> # не менять, иначе не будут считаться SE
    summarise(Tavg = mean(T_),
              Tavg_SE = std(T_),
              Pr_sum = sum(Pr),
              Sn_avg = mean(Sn),
              Sn_avg_SE = std(Sn),
              Duration = n())
  
  winter_end <- all_year |> 
    filter(Season == "Winter_last") |> 
    group_by(Year) |>
    rename(T_=Tavg) |> # не менять, иначе не будут считаться SE
    summarise(Tavg = mean(T_),
              Tavg_SE = std(T_),
              Pr_sum = sum(Pr),
              Sn_avg = mean(Sn),
              Sn_avg_SE = std(Sn),
              Duration=n()) |> 
    mutate(Year = Year+1) #Сдвигаем год на 1 вперед, так как рассчитывать среднее значение будем уже с будущим годом
  
  winter_start <- all_year |> 
    filter(Season == "Winter_first") |> 
    group_by(Year) |> 
    rename(T_=Tavg) |> # не менять, иначе не будут считаться SE
    summarise(Tavg = mean(T_),
              Tavg_SE = std(T_),
              Pr_sum = sum(Pr),
              Sn_avg = mean(Sn),
              Sn_avg_SE = std(Sn),
              Duration=n())
  
  all_winter <- winter_start |> 
    full_join(winter_end, by="Year") |> 
    mutate(Tavg = (Tavg.x+Tavg.y)/2,
           Tavg_SE = (Tavg_SE.x+Tavg_SE.y)/2,
           Pr_sum = Pr_sum.x+Pr_sum.y,
           Sn_avg = (Sn_avg.x+Sn_avg.y)/2,
           Sn_avg_SE = (Sn_avg_SE.x+Sn_avg_SE.y)/2,
           Duration = Duration.x+Duration.y) |> 
    select(Year, Tavg, Tavg_SE, Pr_sum, Sn_avg,Sn_avg_SE, Duration) |> 
    mutate(Season="Winter")
  
  
  # Найдем количество дней от начала года, когда начался тот или иной сезон
  Spring_start <- all_year |>
    group_by(Year) |>
    filter(Season %in% c("Winter_first")) |> 
    summarise(start = n()+1) |> 
    mutate(Season = "Spring")
  
  Summer_start <- all_year |>
    group_by(Year) |>
    filter(Season %in% c("Winter_first", "Spring")) |> 
    summarise(start = n()+1) |> 
    mutate(Season = "Summer")
  
  Autumn_start <- all_year |>
    group_by(Year) |> 
    filter(Season %in% c("Winter_first", "Spring", "Summer")) |> 
    summarise(start = n()+1) |> 
    mutate(Season = "Autumn")
  
  Winter_start <- all_year |>
    group_by(Year) |> 
    filter(Season %in% c("Winter_first", "Spring", "Summer", "Autumn")) |> 
    summarise(start = n()+1) |> 
    mutate(Season = "Winter")
  
  start_of_season <- rbind(Spring_start, Summer_start, Autumn_start, Winter_start)

  result <- rbind(aggregate_by_season, all_winter) |> 
    arrange(Year, Season) |> 
    left_join(start_of_season, by = c("Year", "Season"))
  return(list("season" = result, 
              "crossing_zero" = all_year))
} #выделить сезоны по переходу parametr через stop_level

# Циклы оттаивания-замерзания - melt_freeze
# на вход принимает результат выполнения функции season_by_crossing_stop_level
# crossing zero
# на выходе датафрейм с количеством переходов температуры через 0
# выводит количество для каждого года суммарно
melt_freeze <- function(df){
  melt_freeze_jan_may <- df |> 
    filter(Month %in% c(1:5)) |> 
    group_by(Year) |> 
    summarise(Melt_freeze = max(chng1))
  melt_freeze_sep_dec <- df |> 
    filter(Month %in% c(9:12)) |> 
    mutate(Year = Year+1) |>  # сдвигаем на +1 год, так как суммировать будем уже со следующим годом
    group_by(Year) |> 
    summarise(Melt_freeze = max(chng1))
  
  melt_freeze_all_winter <- melt_freeze_jan_may |> 
    full_join(melt_freeze_sep_dec, by = "Year") |> 
    mutate(Melt_freeze.y = ifelse(is.na(Melt_freeze.y), 0 , Melt_freeze.y)) |> 
    mutate(Melt_freeze = Melt_freeze.x + Melt_freeze.y) |> 
    select(Year, Melt_freeze)
  
  return(melt_freeze_all_winter)
  
  
}

# Calculations ------------------------------------------------------------
season_by_snow <- season_by_crossing_stop_level(Bakhta_daily, Sn, 0)$season
 
season_by_temperature <- season_by_crossing_stop_level(Bakhta_daily, Tavg, 0)$season |> 
  group_by(Season) |>
  filter(Year > 1977) |> 
  mutate(across(c(Tavg, Pr_sum, Sn_avg, Duration, start), list(
    roll_mean = means_smooth
  )))

write_csv2(season_by_snow, "initial_data/climate/cleaned/season_by_snow.csv")
write_csv2(season_by_temperature, "initial_data/climate/cleaned/season_by_temperature.csv")

# Ежегодное количество циклов "оттаивание-замерзание" 
melt_freeze_all_winter <- melt_freeze(season_by_crossing_stop_level(Bakhta_daily, Tavg, 0)$crossing_zero)
write_csv2(melt_freeze_all_winter, "initial_data/climate/cleaned/melt_freeze_all_winter.csv")

# Количество циклов "замерзание-оттаивание" по месяцам
melt_freeze_by_month <- season_by_crossing_stop_level(Bakhta_daily, Tavg, 0)$crossing_zero |> 
  group_by(Year, Month) |> 
  summarise(MAX = max(chng1)) |> 
  ungroup() |> 
  mutate(Last_max = lag(MAX),
         differ = MAX - Last_max) |> 
  filter(Month %in% c(1:5, 9:12)) |> 
  filter(MAX != 0) |> 
  filter(Month != 12) |> 
  select(Year, Month, Melt_freeze = differ)
  
write_csv2(melt_freeze_by_month, "initial_data/climate/cleaned/melt_freeze_by_month.csv")



