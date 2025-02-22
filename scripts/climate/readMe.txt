


-------
Требуется обновление!!!!
-------







Порядок запуска файлов
1_import&cleaning.R
Импорт, аггрегация до суточного и месячного разрешения, очистка от пропусков
На вход:
		1. Данные о температуре и осадках суточного разрешения с 1961 по февраль 2005 года
		initial_data/climate/1961_2005/23776_TTTR.xlsx
		Структура:
		2. Данные о глубине и качестве снежного покрова суточного разрешения с 1961 по февраль 2005 года
		initial_data/climate/1961_2005/23776_snow.csv
		Структура:
		3. Данные о различных климатических параметрах с сайта rp5 
		Все файлы из каталога initial_data/climate/2005_2023
		5. Данные месячного разрешения (Бор и Верхнеимбатск)
		initial_data/climate/monthly/Bor_monthly.txt
		initial_data/climate/monthly/Verkhneimbatsk_monthly.txt

На выходе:
		1. Данные суточного разрешения из Бахты. Есть проблемы с летними месяцами 1966-1976 гг.
		initial_data/climate/cleaned/Bakhta_daily_attention1966-1976(problems_with_summer).csv
		Структура: Year, Month, Day, Tavg, Pr, Sn, Date (в формате даты)
		2. Данные месячного разрешения из Бахты. Летние месяцы заменены на осредненные по Бору и Верхнеимбатску.
		initial_data/climate/cleaned/Bakhta_monthly_1966-1976_replaced_by_average_Bor_Verkhn.csv
		Структура: Структура: Year, Month, Tavg, Pr, Sn, Date (в формате даты)

2_all_station_Tavg&Sn_quality.R
Среднемесячные температуры по 7 метеостанциям.
Качество снега по 6 метеостанциям (кроме Бахты)
На входе:
		1. Все файлы из каталога. 2005-2023, все метеостанции кроме Бахты, суточное разрешение
		initial_data/climate/2005_2023_seven_station
		2. Все файлы из каталога. 1961-2004. 4 станции. Месячное разрешение
		"initial_data/climate/monthly_all_station/"
		3. Парсинг страниц для Ворогово и Ярцево.
		4. Предваритально очищенные данные месячного разрешения для Бахты
		"initial_data/climate/cleaned/Bakhta_monthly_1966-1976_replaced_by_average_Bor_Verkhn.csv"
На выходе:
		1. Среднемесячная температура для всех 7 станций
		initial_data/climate/cleaned/all_station_tavg.csv
		2. Среднемесячные температуры, осредненные по всей территории
		initial_data/climate/cleaned/average_temp_by_7_station.csv
		3. Качественные характеристики снега по 6 метеостанциям (2005-2023)
		initial_data/climate/cleaned/snow_quality_six_station.csv


3_bad_snow.R
Подсчет количества дней с "плохим" снегом (когда покрывал менее половины поверхности почвы или почва была покрыта льдом)
Вход:
		1. Данные суточного разрешения из Бахты. Есть проблемы с летними месяцами 1966-1976 гг.
		"initial_data/climate/cleaned/Bakhta_daily_attention1966-1976(problems_with_summer).csv"
		2.Данные о снеге из Обнинска (неочищенные)
		"initial_data/climate/1961_2005/23776_snow.csv"
Выход:
		1. Количество дней с "плохим" снегом
		initial_data/climate/cleaned/all_station_bad_snow.csv

4_Tavg_Sn_Pr_anomaly.R
	Расчет аномалий для всех станций + для температур, осредненных по всем метеостанциям.
на вход:
		1. Среднемесячные данные по Бахте
		initial_data/climate/cleaned/Bakhta_monthly_1966-1976_replaced_by_average_Bor_Verkhn.csv
		2. Среднемесячные температуры со всех метеостанций
		"initial_data/climate/cleaned/all_station_tavg_monthly.csv"

на выход:
		1. Годовые аномалии на всех метеостанциях
		"initial_data/climate/cleaned/annualy_anomaly.csv"
		2. Месячные аномалии на всех метеостанциях
		"initial_data/climate/cleaned/smonthly_anomaly.csv"

5.crossing_zero.R
Выделение сезонов по переходу параметров (температуры или глубины снежного покрова) через какое-то значение (например, температур через 0).
Подсчет количества циклов оттаивание-замерзание.
На вход:
		1. Данные из Бахты суточного разрешения
		 "initial_data/climate/cleaned/Bakhta_daily_attention1966-1976(problems_with_summer).csv"
На выход:
		1. Сезоны по снегу: ср. температура, сумма осадков, глубина снега, продолжительность, день старта сезона
		"initial_data/climate/cleaned/season_by_snow.csv"
		2. Сезоны по температуре (те же параметры)
		"initial_data/climate/cleaned/season_by_temperature.csv"
		3. Ежегодное количество циклов "оттаивание-замерзание"
		"initial_data/climate/cleaned/melt_freeze_all_winter.csv"
		4. Ежемесячное (для каждого года) количество циклов "оттаивание-замерзание"
		"initial_data/climate/cleaned/melt_freeze_by_month.csv"
6_graphs.R
Все графики по климатическим расчетам строятся здесь.

7_tables.R
Таблицы для с коэффициентами трендов для раздела про климатические изменения (кроме характеристик
годовых трендов для всех станций отдельно)


