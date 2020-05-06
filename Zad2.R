# создайте модель множественной линейной регрессии потоков потоков паров воды за осенний период 2013 года 
# по данным измерений методом турбулентной пульсации
# Для выбора нужных суток используйте переменную DOY - день года (1 января - DOY = 1)

#подключаем tidyverse
library(tidyverse)
library(lubridate)
#читаем данные из файла, пропускаем первую строку, заменяем текстовые 'NA', пустые и сгенерированные пороговые значения на NA, игнорируем строки с "[" 
data = read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
data = data[-1,]
#убираем ненужные колонки
data = data[, c(-1, -3, -9, -12, -15, -18, -21, -30, -35, -63 , -70, -88:-99) ]
#преобразуем строковые значения в факторные
data = data %>% mutate_if(is.character, factor)
#заменяем конфликтующие знаки колонок
names(data) = names(data) %>% str_replace_all("[!]","_emph_") %>%
  str_replace_all("[?]","_quest_") %>%  
  str_replace_all("[*]","_star_") %>%  
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
#Посмотрим, что получилось
glimpse(data)
#оставим данные только по осеннему периоду 2013 года:
data = data[data$DOY>=243 & data$DOY<=334 & year(data$date) == 2013, c(1:ncol(data))] 
#выберем все переменные типа numeric
data_numeric = data[,sapply(data,is.numeric) ]
#все остальные переменные:
data_non_numeric = data[,!sapply(data,is.numeric) ]
# создадим матрицу для корелляционного анализа и преобразуем ее в таблицу, выбрав нужный столбец (потоки паров воды)
cor_td = cor(drop_na(data_numeric)) %>% as.data.frame %>% select(h2o_flux)
#выберем имена переменных (строк) с коэффициентом детерминации больше 0.2
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .2] %>% na.exclude; vars
#соберем переменные из вектора в одну формулу:
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep="")); formula
#создадим обучающую и тестирующую выборки:
row_numbers = 1:length(data$date)
teach = sample(row_numbers, floor(length(data$date)*.7))
test = row_numbers[-teach]
#непересекающиеся подвыборки:
teaching_tbl_unq = data[teach,]
testing_tbl_unq = data[test,]
# МОДЕЛЬ 1
#создаем модель линейной регрессии
model = lm(formula, data = data);model
#коэффициенты
coef(model)
#остатки
resid(model)
#доверительный интервал
confint(model)
#P-значения по модели
summary(model)
#дисперсионный анализ
anova(model)
#графическое представление модели:
plot(model)

# МОДЕЛЬ 2
formula = as.formula(paste("h2o_flux~", "(", paste(vars,collapse = "+"), ")^2", sep="", collapse = NULL));formula
#создаем модель линейной регрессии
model2 = lm(formula, data = data);model
#коэффициенты
coef(model2)
#остатки
resid(model2)
#доверительный интервал
confint(model2)
#P-значения по модели
summary(model2)
#дисперсионный анализ
anova(model2)
#графическое представление модели:
plot(model2)

