**Статус проекта: завершен.**

Репозиторий содержит R-проект c исходным кодом анализа климатических изменений в среднем течении р. Енисей. Анализ выполнен в рамках подготовки [диссертации](https://sev-in.ru/node/4003) на соискание ученой степени кандидата биологических наук. Если вы не планируете запускать код самостоятельно, рекомендую ознакомиться с более удобным форматом в виде [html-отчета](https://yakushov1.github.io/climate_change_report/).

**Структура проекта**

Рекомендуется запускать проект *climate_for_github.Rproj* в RStudio для автоматического определения рабочего каталога и обеспечения работоспособности ссылок для импорта и экспорта данных без внесения дополнительных изменений.

| Папка         | Описание                                          |
|---------------|---------------------------------------------------|
| scripts       | все файлы скриптов для анализа. Описание см. ниже |
| initial_data  | исходники и предобработанные данные               |
| images        | построенные графики                               |
| report_quarto | Quarto-проект для генерации html-отчета           |

### Scripts

В субдиректории climate содержатся пронумерованные скрипты. Необходимо запускать их в предложенном порядке, чтобы воспроизвести весь анализ от этапа очистки сырых данных до построения графиков и таблиц. Комментарии к коду и результат выполнения удобнее смотреть [здесь](https://yakushov1.github.io/climate_change_report/).

+-----------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Файл                              | Описание                                                                                                                                                                                                                                                                                                                              |
+===================================+=======================================================================================================================================================================================================================================================================================================================================+
| 1_import & cleaning.R             | Импорт и очистка данных метеостанции п. Бахта                                                                                                                                                                                                                                                                                         |
+-----------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| 2_all_station_Tavg&Sn_quality     | Расчет среднемесячных показателей для всех использованных в анализе метеостанций                                                                                                                                                                                                                                                      |
+-----------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| 3_bad_snow.R                      | Расчет количества дней с неблагоприятным (для млекопитающих) снежным покровом. Биологическое обоснование "неблагоприятности" приведено в [тексте](https://sev-in.ru/sites/default/files/2024-12/%D0%AF%D0%BA%D1%83%D1%88%D0%BE%D0%B2_%D0%B4%D0%B8%D1%81%D1%81%D0%B5%D1%80%D1%82%D0%B0%D1%86%D0%B8%D1%8F_6.12.pdf) диссертации         |
+-----------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| 4_why_bad_snow.R                  | С какими метеопараметрами ассоциированы неблагоприятные характеристики снежного покрова?                                                                                                                                                                                                                                              |
+-----------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| catboost                          | Немного "вне очереди" можно запустить jupyter-ноутбук cat.ipynb из директории catboost. В нем построена модель градиентного бустинга для сопоставления данных о снежном покрове из различных источников. Подробности удобнее посмотреть в [отчете](https://yakushov1.github.io/climate_change_report/4_1_why_bad_snow(catboost).html) |
+-----------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| 5_crossing_zero.R                 | Вычисление количества оттаиваний-замерзаний а также продолжительности климатических сезонов, выделенных по устойчивому переходу среднесуточных температур через 0 градусов.                                                                                                                                                           |
+-----------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| 6_graphs.R                        | Исходный код для всех графиков из главы диссертации, посвященной климатическим изменениям.                                                                                                                                                                                                                                            |
+-----------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| 7_tables.R                        | Таблицы с коэффициентами трендов                                                                                                                                                                                                                                                                                                      |
+-----------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

### Images

В субдиректории climate содержатся следующие компоненты:

+-----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Папка/файл                        | Описание                                                                                                                                                                                                                                                                            |
+===================================+=====================================================================================================================================================================================================================================================================================+
| pr                                | графики среднегодовых и среднемесячных сумм осадков (только 1 график по данным метеостанции п. Бахта, так как по другим метеостанциям эта информация недоступна)                                                                                                                    |
+-----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| pr_anomaly                        | графики среднегодовых и среднемесячных аномалий (отклонений от среднего за 1961-1990 гг.) сумм осадков (только 1 график по данным метеостанции п. Бахта, так как по другим метеостанциям эта информация недоступна)                                                                 |
+-----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| season                            | графики различных параметров (продолжительности - duration, средних температур - tavg, сумм осадков - pr, глубины снежного покрова - sn: префикс смотри в названии файла) для климатических сезонов, выделенных по устойчивому переходу среднесуточных температур через 0 градусов. |
+-----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| sn                                | график среднегодовой и среднемесячной глубины снежного покрова                                                                                                                                                                                                                      |
+-----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| sn_anomaly                        | график среднегодовых и среднемесячных аномалий (отклонений от среднего за 1961-1990 гг.) глубины снежного покрова                                                                                                                                                                   |
+-----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| t_anomaly                         | графики среднегодовых и среднемесячных аномалий (отклонений от среднего за 1961-1990 гг.) температур приземного воздуха для всех метеостанций (см префикс в имени файла)                                                                                                            |
+-----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| tavg                              | графики среднегодовых и среднемесячных температур приземного воздуха для всех метеостанций (см префикс в имени файла)                                                                                                                                                               |
+-----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| bad_snow_by_station.png           | график количества дней с неблагоприятным снежным покровом по всем метеостанциям                                                                                                                                                                                                     |
+-----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| melt_freeze_by_month_graph.png    | количество оттаиваний замерзаний по данным метеостанции п. Бахта                                                                                                                                                                                                                    |
+-----------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

### Initial data

Каталог с исходными и предобработанными файлами.

`1961_2005` - данные за 1961-2005 гг. по метеостанции п. Бахта

+-----------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Файл            | Описание                                                                                                                                                                                                                                                          |
+=================+===================================================================================================================================================================================================================================================================+
| 23776_snow.csv  | Информация о снежном покрове на метеостанции п. Бахта                                                                                                                                                                                                             |
|                 |                                                                                                                                                                                                                                                                   |
|                 | `Станция` - Индекс метеостанции ВМО\                                                                                                                                                                                                                              |
|                 | `Год`\                                                                                                                                                                                                                                                            |
|                 | `Месяц`\                                                                                                                                                                                                                                                          |
|                 | `День`\                                                                                                                                                                                                                                                           |
|                 | `Высота_снежного_покрова` (в см)\                                                                                                                                                                                                                                 |
|                 | `Снежный_покров_степень_покрытия` - степень покрытия окрестности станции снегом оценивается по 10-бальной шкале. Например, отсутствие снега – 0 баллов, 20% окрестности станции покрыто снегом -2 балла, 50% окрестности станции покрыто снегом -5 баллов и т.д.\ |
|                 | `Q1` - `Q3` [Дополнительная информация о снежном покрове](http://meteo.ru/data/snow-cover/)                                                                                                                                                                       |
+-----------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| 23776_TTTR.xlsx | Информация о среднесуточных температурах и количестве осадков по данным метеостанции п. Бахта. Содержимое интуитивно понятно по названию столбцов. [Дополнительная информация](http://meteo.ru/data/temperature-precipitation/) на сайте-источнике данных.        |
+-----------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

`2005_2023` - данные за 2005-2023 гг. по метеостанции п. Бахта. В названии файла указаны временные промежутки, к которым относятся данные. В анализе использовано несколько столбцов:\
Местное время\
Т - температура воздуха (градусы Цельсия) на высоте 2 метра над уровнем земли\
RRR - количество выпавших осадков (миллиметры).\
E\` - состояние поверхности почвы со снегом или измеримым ледяным покровом.\
sss - высота снежного покрова (см).

Описание остальных столбцов см. [на сайте-источнике информации](https://rp5.ru/%D0%90%D1%80%D1%85%D0%B8%D0%B2_%D0%BF%D0%BE%D0%B3%D0%BE%D0%B4%D1%8B_%D0%B2_%D0%91%D0%B0%D1%85%D1%82%D0%B5)

`2005_2023_seven_station`  - данные по остальным метеостанциям за 2005-2023 гг. В названии файла указаны названия населенных пунктов, в которых установлены метеостанции.

`cleaned`  - предобработанные данные
