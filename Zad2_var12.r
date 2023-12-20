library(factoextra)
library(readr)
library(dplyr)
#считали таблицу
table = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1",
                skip=1, comment = "[")
#убрали самую первую строку значений, тк она пустая
table = table[-1,]
#приравняли значения -9999 к NA
table[table == -9999] = NA
#оставили строки ночные
table <- table %>% filter(daytime == "F")
#оставили строки 2013 года
table <- table %>% filter(year(date) == 2013)
#оставили строки осени
table <- table %>% filter(month(date) %in% c(09, 10, 11))
#оставили строки только числовые
table = table %>% select(where(is.numeric))
#оставили столбцы без тех что относятся к co2
table =-table %>% select(-contains("co2"), co2_flux)
#создали таблицу var_data со значениями дисперсии каждого показателя
var_data = table %>% summarise_all( ~var(.x,na.rm=T))
#там где результат получился NA, заменяем на 0
var_data[is.na(var_data)] = 0
#создаем таблицу cpa_data используя var_data чтобы исключить показатели с нулевой дисперсией
cpa_data = table[,as.logical(var_data != 0)]
#создали корреляционную таблицу на основе cpa_data исключив столбцы содержащие NA
cor_matrix = cor(na.exclude(cpa_data))
#создали тепловую карту корреляционной матрицы
heatmap(cor_matrix)
#сделали выборку значений из матрицы для CO2
co2_cor = as.numeric(cor_matrix[84,])
#проименовали эту выборку
names(co2_cor) = names(cor_matrix[84,])
#выбрали только те значения, что входят в диапазон коэффициента влияния
cpa_dataf = cpa_data[,co2_cor > 0.1 | co2_cor < -.1]
#произвели анализ основных компонентов
data_pca = prcomp(na.exclude(cpa_dataf),scale=TRUE)
#отобразили розу ветров для влияния различных показателей
fviz_pca_var(data_pca,repel = TRUE, col.var = "steelblue")
#создали модель линейной регрессии исходя из показателей, максимально приближенных к co2_flux по влиянию
model = lm(co2_flux ~ air_heat_capacity+air_pressure+u_unrot+flowrate+air_density+DOY, table)
#вывели сводную по получившейся модели
summary(model)
#Далее последовательно несколько раз проводим создания линейных моделей, убирая переменные из формулы, оказывающие наименьшее воздействие на модель 
#Начиная с третьей итерации опираемся на дисперсионный анализ ANOVA для исключения переменных

formula = paste(c("co2_flux ~ ",
                  paste(names(cpa_dataf)[-36], collapse = "+")),
                collapse = "")
formula = as.formula(formula)


model_first = lm(formula, cpa_dataf)
summary(model_first)
formula2 = formula(co2_flux ~ DOY + wind_dir + u_spikes + h2o_signal_strength_7200 + 
                     h2o...128  + w_spikes + u_rot + air_molar_volume + h2o_var + 
                     ts_spikes + yaw + qc_LE)
model2 = lm(formula2, cpa_dataf)
summary(model2)
formula3 = formula(co2_flux ~ wind_dir + u_spikes + h2o_signal_strength_7200 + 
                     h2o...128  + w_spikes + u_rot + h2o_var + ts_spikes + yaw + qc_LE)
model3 = lm(formula3, cpa_dataf)
summary(model3)
anova(model3)
formula4 = formula(co2_flux ~ wind_dir + u_spikes + h2o_signal_strength_7200 + 
                     h2o...128 + h2o_var + yaw + qc_LE)
model4 = lm(formula4, cpa_dataf)
summary(model4)
anova(model4)
formula5 = formula(co2_flux ~ wind_dir + u_spikes + h2o...128 + h2o_var + yaw + qc_LE)
model5 = lm(formula5, cpa_dataf)
summary(model5)
anova(model5)
