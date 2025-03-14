**Статус проекта: завершен.**

Репозиторий содержит R-проект c исходным кодом анализа климатических изменений в среднем течении р. Енисей. Анализ выполнен в рамках подготовки [диссертации](https://sev-in.ru/node/4003) на соискание ученой степени кандидата биологических наук. Если вы не планируете запускать код самостоятельно, рекомендую ознакомиться с более удобным форматом в виде [html-отчета](https://yakushov1.github.io/climate_change_report/).

**Структура проекта**

Рекомендуется запускать проект *climate_for_github.Rproj* в RStudio для автоматического определения рабочего каталога и обеспечения работоспособности ссылок для импорта и экспорта данных без внесения дополнительных изменений.

| Папка         | Описание                                          |
| ------------- | ------------------------------------------------- |
| scripts       | все файлы скриптов для анализа. Описание см. ниже |
| initial_data  | исходники и предобработанные данные               |
| images        | построенные графики                               |
| report_quarto | Quarto-проект для генерации html-отчета           |

### Scripts

В субдиректории climate содержатся пронумерованные скрипты. Необходимо запускать их в предложенном порядке, чтобы воспроизвести весь анализ от этапа очистки сырых данных до построения графиков и таблиц. Комментарии к коду и результат выполнения удобнее смотреть [здесь](https://yakushov1.github.io/climate_change_report/).

| Файл                          | Описание                                                                                                                                                                                                                                                                                                                              |
| ----------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1-import & cleaning.R         | Импорт и очистка данных метеостанции п. Бахта                                                                                                                                                                                                                                                                                         |
| 2-all-station-Tavg&Sn-quality | Расчет среднемесячных показателей для всех использованных в анализе метеостанций                                                                                                                                                                                                                                                      |
| 3-bad-snow.R                  | Расчет количества дней с неблагоприятным (для млекопитающих) снежным покровом. Биологическое обоснование "неблагоприятности" приведено в [тексте](https://sev-in.ru/sites/default/files/2024-12/%D0%AF%D0%BA%D1%83%D1%88%D0%BE%D0%B2-%D0%B4%D0%B8%D1%81%D1%81%D0%B5%D1%80%D1%82%D0%B0%D1%86%D0%B8%D1%8F-6.12.pdf) диссертации         |
| 4-why-bad-snow.R              | С какими метеопараметрами ассоциированы неблагоприятные характеристики снежного покрова?                                                                                                                                                                                                                                              |
| catboost                      | Немного "вне очереди" можно запустить jupyter-ноутбук cat.ipynb из директории catboost. В нем построена модель градиентного бустинга для сопоставления данных о снежном покрове из различных источников. Подробности удобнее посмотреть в [отчете](https://yakushov1.github.io/climate-change-report/4-1-why-bad-snow(catboost).html) |
| 5-crossing-zero.R             | Вычисление количества оттаиваний-замерзаний а также продолжительности климатических сезонов, выделенных по устойчивому переходу среднесуточных температур через 0 градусов.                                                                                                                                                           |
| 6-graphs.R                    | Исходный код для всех графиков из главы диссертации, посвященной климатическим изменениям.                                                                                                                                                                                                                                            |
| 7-tables.R                    | Таблицы с коэффициентами трендов                                                                                                                                                                                                                                                                                                      |


### Images

В субдиректории climate содержатся следующие компоненты:

| Папка/файл                    | Описание                                     |
| ----------------------------- | -------------------------------------------- |
| pr                            | графики среднегодовых и среднемесячных сумм осадков (только 1 график по данным метеостанции п. Бахта, так как по другим метеостанциям эта информация недоступна) |
| pr_anomaly                    | графики среднегодовых и среднемесячных аномалий (отклонений от среднего за 1961-1990 гг.) сумм осадков (только 1 график по данным метеостанции п. Бахта, так как по другим метеостанциям эта информация недоступна) |
| season                        | графики различных параметров (продолжительности - duration, средних температур - tavg, сумм осадков - pr, глубины снежного покрова - sn: префикс смотри в названии файла) для климатических сезонов, выделенных по устойчивому переходу среднесуточных температур через 0 градусов. |
| sn                            | график среднегодовой и среднемесячной глубины снежного покрова |
| sn_anomaly                    | график среднегодовых и среднемесячных аномалий (отклонений от среднего за 1961-1990 гг.) глубины снежного покрова | 
| t_anomaly                     | графики среднегодовых и среднемесячных аномалий (отклонений от среднего за 1961-1990 гг.) температур приземного воздуха для всех метеостанций (см префикс в имени файла)          |
| tavg                          | графики среднегодовых и среднемесячных температур приземного воздуха для всех метеостанций (см префикс в имени файла)      |
| bad_snow_by_station.png       | график количества дней с неблагоприятным снежным покровом по всем метеостанциям   |
| melt_freeze_by_month_graph.png| количество оттаиваний замерзаний по данным метеостанции п. Бахта    |


### Initial data

Каталог с исходными и предобработанными файлами.

---

#### **`1961_2005`** - данные за 1961-2005 гг. по метеостанции п. Бахта:

##### `23776_snow.csv`  - Информация о снежном покрове на метеостанции п. Бахта 
`Станция` - Индекс метеостанции ВМО  <br />
`Год`  <br />
`Месяц`  <br />
`День` <br />
`Высота_снежного_покрова` (в см) <br />
`Снежный_покров_степень_покрытия` - степень покрытия окрестности станции снегом оценивается по 10-бальной шкале. Например, отсутствие снега – 0 баллов, 20% окрестности станции покрыто снегом -2 балла, 50% окрестности станции покрыто снегом -5 баллов и т.д.  <br />
`Q1 - Q3` [Дополнительная информация о снежном покрове](http://meteo.ru/data/snow-cover/)  <br />

#####  `23776_TTTR.xlsx`  - Информация о среднесуточных температурах и количестве осадков по данным метеостанции п. Бахта.
Содержимое интуитивно понятно по названию столбцов. [Дополнительная информация](http://meteo.ru/data/temperature-precipitation/) на сайте-источнике данных. <br />       

---

#### `2005_2023` - данные за 2005-2023 гг. по метеостанции п. Бахта.
В названии файла указаны временные промежутки, к которым относятся данные. В анализе использовано несколько столбцов: <br />
`Местное время` <br />
`Т` - температура воздуха (градусы Цельсия) на высоте 2 метра над уровнем земли <br />
`RRR` - количество выпавших осадков (миллиметры). <br />
`E\`\` - состояние поверхности почвы со снегом или измеримым ледяным покровом. <br />
`sss` - высота снежного покрова (см). <br />

Описание остальных столбцов см. [на сайте-источнике информации](https://rp5.ru/%D0%90%D1%80%D1%85%D0%B8%D0%B2_%D0%BF%D0%BE%D0%B3%D0%BE%D0%B4%D1%8B_%D0%B2_%D0%91%D0%B0%D1%85%D1%82%D0%B5)

--- 

#### `2005_2023_seven_station`  - данные по остальным метеостанциям за 2005-2023 гг.
В названии файла указаны названия населенных пунктов, в которых установлены метеостанции.<br />
В анализе использовано несколько столбцов: <br />
`Местное время` <br />
`Т` - температура воздуха (градусы Цельсия) на высоте 2 метра над уровнем земли <br />
`RRR` - количество выпавших осадков (миллиметры). <br />
`E` - состояние поверхности почвы со снегом или измеримым ледяным покровом. <br />
`sss` - высота снежного покрова (см). <br />
Описание остальных столбцов см. [на сайте-источнике информации](https://rp5.ru/%D0%90%D1%80%D1%85%D0%B8%D0%B2_%D0%BF%D0%BE%D0%B3%D0%BE%D0%B4%D1%8B_%D0%B2_%D0%91%D0%B0%D1%85%D1%82%D0%B5)

---

#### `cleaned`  - предобработанные данные
Все процедуры обработки приведены в папке scripts.
##### `all_station_bad_snow.csv` - данные о количестве дней с неблагоприятным (для мелких млекопитающих) снежным покровом.
`Year` - год <br />
`Station` - название метеостанции <br />
`First_half` - количество дней с неблагоприятными условиями в первой половине зимы (фактически, в предыдущем году) <br />
`Last_half` - то же, но для второй половины зимы <br />
`count` - то же, но для всей зимы (то есть для января-апреля текущего года плюс октябрь-декабрь предыдущего) <br />
##### `annualy_anomaly.csv` - среднегодовые данные:
`Station` - название метеостанции <br />
`Year` - год <br />
`Tavg` - среднегодовая температура на метеостанции <br />
`Tavg_SE` стандартная ошибка среднего для `Tavg` <br />
`Pr` - годовая сумма осадков <br />
`Sn` - средняя глубина снежного покрова <br />
`Sn_SE` - стандартная ошибка среднего для `Sn` <br />
`Tavg_base`- средняя температура приземного воздуха за 1961-1990 гг.<br />
`Tavg_base_SE` - стандартная ошибка среднего для `Tavg_base`<br /> 
`Sn_base` - средняя глубина снега за 1961-1990 гг. <br /> 
`Sn_base_SE` - стандартная ошибка для `Sn_base`<br /> 
`Pr_base`- средняя сумма осадков <br /> 
`Pr_base_SE` - стандартная ошибка для `Pr_base` <br /> 
`T_anomaly`- аномалии (отклонения от среднего за 1961-1990) температур приземного воздуха <br /> 
`Sn_anomaly` - аномалии глубины снежного покрова <br /> 
`Pr_anomaly` - аномалии сумм осадков <br /> 
`Tavg_last_decade` - средняя температура за 2013-2023 гг. <br /> 
`Tavg_last_decade_SE` - стандартная ошибка среднего для `Tavg_last_decade` <br /> 
`Sn_last_decade`  - средняя глубина снежного покрова за 2013-2023 гг.<br /> 
`Sn_last_decade_SE` - стандартная ошибка среднего для `Sn_last_decade` <br /> 
`Pr_last_decade`- средняя сумма осадков за 2013-2023 гг. <br /> 
`Pr_last_decade_SE` - стандартная ошибка среднего для `Pr_last_decade` <br /> 
`T_diff` - разница между `Tavg_base` и `Tavg_last_decade`<br /> 
`Sn_diff`- разница между `Sn_base` и `Sn_last_decade`<br /> 
`Pr_diff` - разница между `Pr_base` и `Pr_last_decade`<br /> 
`T_anomaly_roll_mean` - 10-ти летнее скользящее среднее для аномалий температур <br /> 
`Pr_anomaly_roll_mean` - 10-ти летнее скользящее среднее для аномалий сумм осадков<br /> 
`Sn_anomaly_roll_mean` - 10-ти летнее скользящее среднее для аномалий глубины снежного покрова<br /> 
`Tavg_roll_mean` - 10-ти летнее скользящее среднее для температуры приземного воздуха <br /> 
`Pr_roll_mean`- 10-ти летнее скользящее среднее для сумм осадков <br /> 
`Sn_roll_mean` - 10-ти летнее скользящее среднее для глубин снежного покрова <br /> 
`T_anomaly_roll_mean_from_1976` - 10-ти летнее скользящее среднее для аномалий температур приземного воздуха (с 1976 года) <br /> 
`Pr_anomaly_roll_mean_from_1976`- 10-ти летнее скользящее среднее для аномалий сумм осадков (с 1976 года)  <br /> 
`Sn_anomaly_roll_mean_from_1976` - 10-ти летнее скользящее среднее для аномалий глубины снежного покрова (с 1976 года) <br /> 
`Tavg_roll_mean_from_1976`- 10-ти летнее скользящее среднее для  температур приземного воздуха (с 1976 года)  <br /> 
`Pr_roll_mean_from_1976`- 10-ти летнее скользящее среднее для  сумм осадков (с 1976 года)  <br /> 
`Sn_roll_mean_from_1976` - 10-ти летнее скользящее среднее для  глубин снежного покрова (с 1976 года)  <br /> 
##### `monthly_anomaly.csv` - то же, что `annualy_anomaly.csv`, но среднемесячные значения для каждого года и метеостанции
##### `Bakhta_daily_problem_with_summer_1966-1976.csv` - очищенные данные суточного разрешения для метеостанции п. Бахта
Есть проблемы для летних месяцев с 1966 по 1976 годы, заменить нечем, поэтому эти месяцы лучше не использовать для анализа <br />
`Year` - год <br />
`Month`- месяц <br />
`Day` - день <br />
`Tavg` - среднесуточная температура <br />
`Pr` - суточная сумма осадков <br />
`Sn` - глубина снежного покрова <br />
`Sn_description` - описание снежного покрова <br />
`Date` - сведенная в один столбец дата (для работы с plotly)

##### `Bakhta_monthly_1966-1976_replaced_by_average_Bor_Verkhn.csv` - сренемесячные температуры по данным метеостанции п. Бахта
`Year` - год <br />
`Month`- месяц <br />
`Tavg` - среднемесячная температура <br />
`Pr` - месячная сумма осадков <br />
`Sn` - среднемесячная глубина снежного покрова <br />
`Date` - сведенная в один столбец дата (для работы с plotly)
##### `melt_freeze_all_winter.csv` - количество оттаиваний-замерзаний за всю зиму (то есть до лета текущего года (столбец `Year` ) и за октябрь-декабрь `Year`-1)

##### `melt_freeze_by_month.csv` - количество оттаиваний-замерзаний, распределенное по месяцам

##### `season_by_temperature.csv` - данные о сезонах, выделенных по устойчивому переходу среднесуточных температур через 0 градусов
`Year`-  год <br />
`Season`- сезон <br />
`Tavg`- среднесезонная температура <br />
`Tavg_SE`- стандартная ошибка среднего для `Tavg` <br />
`Pr_sum`- сумма осадков за сезон <br />
`Sn_avg`- средняя глубина снежного покрова за сезон <br />
`Sn_avg_SE`- стандартная ошибка среднего для `Sn_avg` <br />
`Duration`- продолжительность сезона (в днях) <br />
`start`- номер дня (от начала года) когда сезон начался <br />
`Tavg_roll_mean`- 10-ти летнее скользящее среднее для `Tavg` <br />
`Pr_sum_roll_mean`- 10-ти летнее скользящее среднее для `Pr_sum`<br />
`Sn_avg_roll_mean`- 10-ти летнее скользящее среднее для `Sn_avg`<br />
`Duration_roll_mean`- 10-ти летнее скользящее среднее для `Duration`<br />
`start_roll_mean`- 10-ти летнее скользящее среднее для `start`<br />
##### `snow_quality_six_station.csv` - данные о снежном покрова для  станций (кроме Бахты) с 2005 по 2023 годы
`Year` - год <br />
`Month`- месяц <br />
`Day` - день <br />
`Sn_description`  - описание снежного покрова <br />
`Station` - метеостанция <br />

##### *tables_for_text* - таблицы для текста (том 3 станции по темпу роста температур и их аномалий)
##### *trends* - Характеристики трендов из уравнений, отображенных на графиках.
Отсортированы по папкам (названия интуитивно понятны). В префиксах названий файлов указаны названия метеостанций.
Столбцы в файлах:<br />
`Station` - станция<br />
`Parametr`- параметр <br />
`Month`- месяц <br />
`RR`- R^2 (коэффициент детерминации линейного тренда) <br />
`P`- p-уровень значимости <br />
`b`- коэффициент наклона регрессионной прямой (slope) <br />



---

#### `monthly`  - данные месячного разрешения по метеостанциям в п. Бор и Верхнеимбатск
##### `Bor_monthly` - данные по Бору. Столбцы по порядку: `Индекс ВМО`, `Год` ,`месяцы` (с 1 по 12)
##### `Verkhneimbatsk_monthly` - данные по Верхнеимбатску
##### `fld197337.txt` - описание колонок в файлах 
##### `statlist197337.txt` - код метеостанций Бор и Верхнеимбатск

---

#### `monthly_all_station`  -  данные месячного разрешения для четырех метеостанций
Столбцы по порядку (в файле названия отсутствуют): код метеостанции, год, январь (3 столбец) - декабрь (последний столбец)
---

##### `snow__quality_with_other_parametrs.csv` - подготовленный для [модели градиентного бустинга](https://yakushov1.github.io/climate_change_report/4_1_why_bad_snow(catboost).html) файл (сопоставление данных из разных источников)
