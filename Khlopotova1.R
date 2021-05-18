## Хлопотова К.М. 124 ПАЭ - 13 вариант
#для региона 32 рассчитайте урожайность пшеницы в 1999 году, взяв для рассчета средние суммы активных температур за указанный год, 
#с 13 ближайших метеостанций но рассчитав колонку di самостоятельно, как долю месяца, когда среднедневные температуры были выше 7 градусов, 
#но учитывая, что посев не может начаться раньше середины апреля, а вегетация составляет 4 месяца
#32 регион - Брянская область 
# проверяем рабочую дирректорию
rm(list = ls())
setwd("D:/Khlopotova/mathmod1")
getwd()
#устанавливаем пакеты
install.packages("tidyverse")
install.packages("rnoaa")
#открываем нужные пакеты
library("tidyverse")
library("rnoaa")
library("lubridate")


#Ввод констант для расчета урожайности
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)


#Коэффициент использования ФАР посевом
Kf = 300
#Калорийность урожая культуры
Qj = 1600
#Коэффициент "сумма частей основной и побочной продукции"
Lj = 2.2
#Коэффициент "стандартная влажность культуры"
Ej = 25

# скачиваем список метеостанций
station_data = ghcnd_stations() 
write.csv(station_data,"station_data.csv")
station_data = read.csv("station_data.csv")


# Зададим название вектора и координаты столицы региона:
Bryansk = data.frame(id = "Bryansk", latitude = 53.25, longitude = 34.37 )

Bryansk_around = meteo_nearby_stations(lat_lon_df = Bryansk, station_data = station_data,
                                       limit = 13, var = "TAVG",
                                       year_min = 1999, year_max = 1999)

# Создадим таблицу
all_data = tibble()
for (i in 1:13)
{
# Определим станцию из 13 ближайших:
Bryansk_id = Bryansk_around[["Bryansk"]][["id"]][i]
# Загрузим данные для станции:
data = meteo_tidy_ghcnd(stationid = Bryansk_id,
                          var="TAVG",
                          date_min="1999-01-01",
                          date_max="1999-12-31")
# Занесем данные в таблицу, объединив их:
  all_data = bind_rows(all_data, data)
}

# Изменения в таблице сохранятся в векторе 
clean_data = all_data %>%
# Группируем и находим cумму активных температур :
mutate(year = year(date), month = month(date)) %>%
mutate(tavg = tavg/10) %>%
filter(tavg > 5) %>%
group_by(year, month, id) %>%
#Находим сумму активных температур, вычисляем di для температур выше 7
summarize(summ = sum(tavg, na.rm=TRUE), di = length(tavg[tavg>7])/length(tavg) ) %>%
group_by(month) %>%
summarize(s = mean(summ, na.rm = TRUE), di= mean(di)) %>%
#Нужны определенные месяцы - оставим только их
filter(month>=4 & month <=7) %>%
# Добавим колонки для расчета:
mutate (a = af[4:7], b = bf[4:7]) %>%
mutate (fert = ((a + b * 1.0 * s) * di * Kf) / (Qj * Lj * (100-Ej)) )

#Согласно расчету, урожайность пшеницы
Yield = sum(clean_data$fert); Yield
# Результат 18,53 ц/га

