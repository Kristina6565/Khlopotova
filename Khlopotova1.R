## ��������� �.�. 124 ��� - 13 �������
#��� ������� 32 ����������� ����������� ������� � 1999 ����, ���� ��� �������� ������� ����� �������� ���������� �� ��������� ���, 
#� 13 ��������� ������������ �� ��������� ������� di ��������������, ��� ���� ������, ����� ������������� ����������� ���� ���� 7 ��������, 
#�� ��������, ��� ����� �� ����� �������� ������ �������� ������, � ��������� ���������� 4 ������
#32 ������ - �������� ������� 
# ��������� ������� �����������
rm(list = ls())
setwd("D:/Khlopotova/mathmod1")
getwd()
#������������� ������
install.packages("tidyverse")
install.packages("rnoaa")
#��������� ������ ������
library("tidyverse")
library("rnoaa")
library("lubridate")


#���� �������� ��� ������� �����������
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)


#����������� ������������� ��� �������
Kf = 300
#������������ ������ ��������
Qj = 1600
#����������� "����� ������ �������� � �������� ���������"
Lj = 2.2
#����������� "����������� ��������� ��������"
Ej = 25

# ��������� ������ ������������
station_data = ghcnd_stations() 
write.csv(station_data,"station_data.csv")
station_data = read.csv("station_data.csv")


# ������� �������� ������� � ���������� ������� �������:
Bryansk = data.frame(id = "Bryansk", latitude = 53.25, longitude = 34.37 )

Bryansk_around = meteo_nearby_stations(lat_lon_df = Bryansk, station_data = station_data,
                                       limit = 13, var = "TAVG",
                                       year_min = 1999, year_max = 1999)

# �������� �������
all_data = tibble()
for (i in 1:13)
{
# ��������� ������� �� 13 ���������:
Bryansk_id = Bryansk_around[["Bryansk"]][["id"]][i]
# �������� ������ ��� �������:
data = meteo_tidy_ghcnd(stationid = Bryansk_id,
                          var="TAVG",
                          date_min="1999-01-01",
                          date_max="1999-12-31")
# ������� ������ � �������, ��������� ��:
  all_data = bind_rows(all_data, data)
}

# ��������� � ������� ���������� � ������� 
clean_data = all_data %>%
# ���������� � ������� c���� �������� ���������� :
mutate(year = year(date), month = month(date)) %>%
mutate(tavg = tavg/10) %>%
filter(tavg > 5) %>%
group_by(year, month, id) %>%
#������� ����� �������� ����������, ��������� di ��� ���������� ���� 7
summarize(summ = sum(tavg, na.rm=TRUE), di = length(tavg[tavg>7])/length(tavg) ) %>%
group_by(month) %>%
summarize(s = mean(summ, na.rm = TRUE), di= mean(di)) %>%
#����� ������������ ������ - ������� ������ ��
filter(month>=4 & month <=7) %>%
# ������� ������� ��� �������:
mutate (a = af[4:7], b = bf[4:7]) %>%
mutate (fert = ((a + b * 1.0 * s) * di * Kf) / (Qj * Lj * (100-Ej)) )

#�������� �������, ����������� �������
Yield = sum(clean_data$fert); Yield
# ��������� 18,53 �/��

