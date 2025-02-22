#От чего зависит возникновение неблагоприятных условий
library(tidyverse)
library(readxl)
library(MuMIn)

# Functions ---------------------------------------------------------------
read_files  <- function(x){
  read_excel(x) |> 
    select(Local_time, Temp = T, Sn_description = `E'`, sss) |> 
    mutate(Local_time = as_datetime(Local_time, format = "%d.%m.%Y %R")) |> 
    mutate(Year = year(Local_time),
           Month = as.factor(month(Local_time)),
           Day = mday(Local_time)) |> 
    filter(is.na(Temp) == F) |> 
    mutate(
      more_then_zero = Temp > 0, # логический столбец, параметр больше 0? (TRUE | FALSE)
      chng1 = cumsum(more_then_zero != lag(more_then_zero, def = first(more_then_zero)))
    ) |> 
    group_by(Year, Month, Day) |> 
    mutate(max_chng = max(chng1),
           min_chng = min(chng1),
           diff = max_chng-min_chng) |> 
    summarise(Sn = mean(sss, na.rm = T),
              Sn_description = as.factor(str_flatten(Sn_description, ", ", na.rm = T)),
              Tavg = mean(Temp, na.rm = T),
              diff = max(diff))
}

daily_temp <- function(x){
  read_excel(x) |> 
    select(Local_time, Temp = T, Sn_description = `E'`, sss) |> 
    mutate(Local_time = as_datetime(Local_time, format = "%d.%m.%Y %R")) |> 
    mutate(Year = year(Local_time),
           Month = as.factor(month(Local_time)),
           Day = mday(Local_time)) |> 
    filter(is.na(Temp) == F)
}



# Import ------------------------------------------------------------------
# 21 век

paths <- list.files("initial_data/climate/2005_2023", pattern = "[.]xls$", full.names = TRUE) # Просканировали все файлы в директории

total_2005_2023 <- paths |> 
  map_df(read_files)

s <- levels(as.factor(total_2005_2023$Sn_description))[c(4, 5,6)] # Отбор уровней фактора, когда снег = лед, или снег не покрывает всю поверхность почвы


total_2005_2023 <- total_2005_2023 |> 
  mutate(Sn_description = ifelse(Sn_description %in% s,1,0)) |> 
  mutate(Sn = case_when(is.na(Sn) ~ 0, .default = Sn))

# Logreg ------------------------------------------------------------------
for_log_reg <- total_2005_2023 |> 
  ungroup() |> 
  select(Sn_description, Sn, Tavg, diff) 

model <- glm(Sn_description~., for_log_reg, family = "binomial")  
summary(model)

confint(model)


a <- total_2005_2023 |> 
  filter(Year > 2007,
         Sn_description == 1) |>
  filter(diff>1) |> 
  group_by(Year, Month) |> 
  summarise(N = n()) |> 
  mutate(cond = case_when(Year > 2012 ~ "good", .default = "bad"))


variability <- ggplot(a, aes(Year, N))+
  geom_col() +
  theme_minimal(base_size = 18)+
  labs(y = "Дней",
       x = "Год")+
  scale_y_continuous(breaks = c(2,5,7))

ggsave(variability, file= "images/climate/bad_snow_and_cross_zero_count.png", device = png,
       width = 2500, height = 1500, units = "px")




# Для XX века -------------------------------------------------------------

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
  select(Year, Month, Day, Tavg, Sn, Sn_description) |> 
  filter(Year %in% c(1976:1994)) |> 
  filter(Month %in% c(4,5,10,11)) |> 
  select(Sn_description, Sn, Tavg) |> 
  mutate(Sn_description = as.factor(Sn_description)) |> 
  filter(is.na(Tavg)==F)





model_XX <- glm(Sn_description~., snow_quality_1961_2004, family = "binomial")  
summary(model_XX)
confint(model_XX)

