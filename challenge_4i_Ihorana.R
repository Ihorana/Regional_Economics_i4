## Data Analysis Skill Test
## Ihorana Aguilar Cuco
# 04/12/2020

setwd("~/4intell")

# Carregando os pacotes requeridos
library(ggplot2)
library(tibble)
library(tidyr)
library(readr)
library(purrr)
library(dplyr)
library(stringr)
library(forcats)
library(forestmangr)
library("knitr")
library("rmarkdown")
library(lubridate)
library(forecast)
library(tidyverse)
library(ggthemes)

# Carregando as bases de dados
brasil_comex <- read.csv("data_comexstat.csv", sep = ",", encoding = "latin1")
TFP <- read.csv("TFP.csv", sep = ",", encoding = "latin1")

####################### CASE 1 ######################################
# Questão 1 e 2
TFP <- read.csv("TFP.csv")
TFP$data <- paste(TFP$year,"-01-01",sep = "")
TFP$data <- ymd(TFP$data)
TFP$isocode <- as.factor(TFP$isocode)

USA <- TFP %>%
  filter(isocode == "USA")
USA <- ts(USA$rtfpna, start = 1950,frequency = 1)
USA <- forecast(USA)
USA <- as.data.frame(USA)
USA$data <- rownames(USA)
USA$isocode = "USA"

CAN <- TFP %>%
  filter(isocode == "CAN")
CAN <- ts(CAN$rtfpna, start = 1950,frequency = 1)
CAN <- forecast(CAN)
CAN <- as.data.frame(CAN)
CAN$data <- rownames(CAN)
CAN$isocode = "CAN"

MEX <- TFP %>%
  filter(isocode == "MEX")
MEX <- ts(MEX$rtfpna, start = 1950,frequency = 1)
MEX <- forecast(MEX)
MEX <- as.data.frame(MEX)
MEX$data <- rownames(MEX)
MEX$isocode = "MEX"

fcast <- rbind(CAN, MEX, USA) %>%
  rename(rtfpna = `Point Forecast`,
         year = data) %>%
  select(rtfpna, year, isocode, `Lo 80`, `Hi 80`) %>%
  mutate(status = "projeÃ§Ã£o")


datafinal <- TFP %>%
  select(rtfpna, year, isocode) %>%
  mutate(`Lo 80` = NA,
         `Hi 80` = NA,
    status = "observado") %>%
  rbind(fcast) %>%
  mutate(data = paste(year,"-01-01",sep = ""),
         data = ymd(data))


ggplot(datafinal, aes(x = data, y = rtfpna)) +
  geom_line(aes(color = isocode)) +
  geom_ribbon(aes(ymin = `Lo 80`, ymax = `Hi 80`), 
              fill = "#596DD5", color = NA, size = 0, alpha = .5) +
  facet_grid(isocode ~.) +
  theme_minimal() +
  theme(legend.position = "none")
  

# Questao 3
# Como a produtividade mede o grau de eficiência com que determinada 
#economia utiliza seus recursos para produzir bens e serviços de consumo, 
#entre tais medidas, a mais elementar é, sem dúvida, Índice de capital humano 
# por pessoa, com base em anos de escolaridade e retornos à educação 
#(poderia ser uma proxy para capital humano) e a produtividade do trabalho, 
#que expressa o produto gerado por cada hora de trabalho (ou por alguma outra 
#medida do insumo trabalho) na economia em questão e o estoque de capital.

####################### CASE 2 ######################################
# Explorando os dados
str(brasil_comex)

head(brasil_comex)
summary(brasil_comex)
brasil_comex$date <- as.Date(brasil_comex$date, format = "%Y-%m-%d")
brasil_comex$year <- format(as.Date(brasil_comex$date), format = "%Y")
brasil_comex$month <- format(as.Date(brasil_comex$date), format = "%m")
brasil_comex$usd <- format(brasil_comex$usd, digits=2)
brasil_comex$usd <- as.numeric(brasil_comex$usd) / 1000000 # US$ milhão

# Questao 1
expo_ano_prod <- brasil_comex %>% 
  filter(product %in% c("soybean_meal", "soybean_oil", "soybeans")) %>%
  filter(type %in% "Export")

# Questao 2
years_5 <- brasil_comex %>%
  group_by(product) %>%
  filter(year >= 2015, type == 'Export')%>%
  summarise(Total_expo = sum(usd)) %>%
  arrange(desc(Total_expo))

# Questao 3
rotas <- brasil_comex %>%
  group_by(route) %>%
  filter(year >= 2015, product == 'corn', type == 'Export')%>%
  summarise(Total_expo = sum(usd)) %>%
  arrange(desc(Total_expo))

rotas_produto <- brasil_comex %>%
  group_by(route, product) %>%
  filter(year >= 2015, type == 'Export')%>%
  summarise(Total_expo = sum(usd)) %>%
  arrange(desc(Total_expo))

# Questao 4
rotas_3years <- brasil_comex %>%
  group_by(route) %>%
  filter(year >= 2017, product == 'corn'| product == 'sugar', type == 'Export')%>%
  summarise(Total_expo = sum(usd)) %>%
  arrange(desc(Total_expo))

# Questao 5
import_exp <- brasil_comex %>%
  group_by(state, product) %>%
  filter(type == 'Export')%>%
  summarise(Total_expo = sum(usd)) %>%
  arrange(desc(Total_expo))

# Graficos questao 1

# Graficos Brasil
brasil_comex %>%
  filter(product %in% c("soybean_meal", "soybean_oil", "soybeans")) %>%
  
  filter(type %in% "Export") %>%
  ggplot() +
  aes(x = year, weight = usd) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Ano", y = "US$ milhão", title = "Evolução das exportações de soja, óleo de soja e farelo de soja", subtitle = "Brasil (Avumulado no ano)") +
  ggthemes::theme_calc()

brasil_comex %>%
  filter(product %in% c("soybean_meal", "soybean_oil", "soybeans")) %>%
  
  filter(type %in% "Export") %>%
  ggplot() +
  aes(x = month, weight = usd) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Ano", y = "US$ milhão", title = "Evolução das exportações de soja, óleo de soja e farelo de soja", subtitle = "Brasil (Acumulado no mês 1997-2019)") +
  ggthemes::theme_calc()

# Graficos estados
brasil_comex %>%
  filter(product %in% c("soybean_meal", "soybean_oil", "soybeans")) %>%
  
  filter(type %in% "Export") %>%
  filter(year %in% c("2015", "2016", "2017", "2018", 
                     "2019")) %>%
  ggplot() +
  aes(x = year, weight = usd) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Ano", y = "US$ milhão", title = "Evolução das exporEvolução das exportações de soja, óleo de soja e farelo de soja", subtitle = "Estados (Acumulado nos últimos anos 2015-2019)") +
  theme_minimal() +
  facet_wrap(vars(state))

brasil_comex %>%
  filter(product %in% c("soybean_meal", "soybean_oil", "soybeans")) %>%
  
  filter(type %in% "Export") %>%
  filter(year %in% c("2015", "2016", "2017", "2018", 
                     "2019")) %>%
  ggplot() +
  aes(x = month, weight = usd) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Ano", y = "US$ milhão", title = "Evolução das exporEvolução das exportações de soja, óleo de soja e farelo de soja", subtitle = "Estados (Acumulado mês 2015-2019)") +
  theme_minimal() +
  facet_wrap(vars(state))

# Graficos rotas
brasil_comex %>%
  filter(product %in% c("soybean_meal", "soybean_oil", "soybeans")) %>%
  
  filter(type %in% "Export") %>%
  ggplot() +
  aes(x = year, fill = route, colour = route, weight = usd) +
  geom_bar() +
  scale_fill_brewer(palette = "Blues") +
  scale_color_brewer(palette = "Blues") +
  labs(x = "Ano", y = "US$ milhão", title = "Evolução das exportações de soja, óleo de soja e farelo de soja", subtitle = "Principais rotas: Brasil (Acumulado no ano)") +
  ggthemes::theme_calc()

brasil_comex %>%
  filter(product %in% c("soybean_meal", "soybean_oil", "soybeans")) %>%
  
  filter(type %in% "Export") %>%
  ggplot() +
  aes(x = month, fill = route, colour = route, weight = usd) +
  geom_bar() +
  scale_fill_brewer(palette = "Blues") +
  scale_color_brewer(palette = "Blues") +
  labs(x = "Ano", y = "US$ milhão", title = "Evolução das exportações de soja, óleo de soja e farelo de soja", subtitle = "Principais rotas: Brasil (Acumulado no mês)") +
  ggthemes::theme_calc()

brasil_comex %>%
  filter(product %in% c("soybean_meal", "soybean_oil", "soybeans")) %>%
  
  filter(type %in% "Export") %>%
  filter(year %in% c("2015", "2016", "2017", "2018", 
                     "2019")) %>%
  ggplot() +
  aes(x = month, y = usd, fill = route, colour = route) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Blues") +
  scale_color_brewer(palette = "Blues") +
  labs(x = "Ano", y = "US$ milhão", title = "Evolução das exportações de soja, óleo de soja e farelo de soja", subtitle = "Principais rotas: Estados (Acumulado no mês 2015-2020)") +
  ggthemes::theme_calc() +
  facet_wrap(vars(state))

brasil_comex %>%
  filter(product %in% c("soybean_meal", "soybean_oil", "soybeans")) %>%
  
  filter(type %in% "Export") %>%
  filter(year %in% c("2015", "2016", "2017", "2018", 
                     "2019")) %>%
  ggplot() +
  aes(x = year, y = usd, fill = route, colour = route) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Blues") +
  scale_color_brewer(palette = "Blues") +
  labs(x = "Ano", y = "US$ milhão", title = "Evolução das exportações de soja, óleo de soja e farelo de soja", subtitle = "Principais rotas: Estados (Acumulado no ano 2015-2020)") +
  ggthemes::theme_calc() +
  facet_wrap(vars(state))


# Graficos questao 2
brasil_comex %>%
  filter(product %in% c("soybean_meal", "soybeans", "sugar")) %>%
  
  filter(type %in% "Export") %>%
  filter(year %in% c("2015", "2016", "2017", "2018", 
                     "2019")) %>%
  ggplot() +
  aes(x = product, fill = product) +
  geom_bar() +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Produto", y = "US$ milhão", title = "Evolução dos três principais produtos da pauta de exportação brasileira nos últimos anos", subtitle = "2015-2019") +
  ggthemes::theme_calc() +
  facet_grid(vars(), vars(year))


# Graficos questao 3
# Principais rotas
brasil_comex %>%
  filter(product %in% "corn") %>%
  filter(type %in% "Export") %>%
  filter(year %in% 
           c("2015", "2016", "2017", "2018", "2019")) %>%
  ggplot() +
  aes(x = year, y = usd, fill = route) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Accent") +
  labs(x = "Ano", y = "US$ milhão", title = "Exportações de milho ", subtitle = "2015-2019") +
  ggthemes::theme_calc()

# Rotas por produto
brasil_comex %>%
  filter(type %in% "Export") %>%
  filter(year %in% c("2015", "2016", 
                     "2017", "2018", "2019")) %>%
  ggplot() +
  aes(x = year, fill = route) +
  geom_bar() +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Ano", y = "US$ milhão", title = "Importancia das rotas por produto", subtitle = "2015-2019") +
  ggthemes::theme_calc() +
  facet_wrap(vars(product))


# Graficos questao 4
brasil_comex %>%
  filter(product %in% c("sugar", "wheat")) %>%
  filter(type %in% "Export") %>%
  
  filter(year %in% c("2017", "2018", "2019")) %>%
  ggplot() +
  aes(x = route) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Rotas", title = "Principais parceiros comerciais em termos de milho e açúcar", subtitle = "2017-2019", caption = "US$") +
  ggthemes::theme_calc()

# Graficos questao 5
library(rpivotTable)
data(brasil_comex)
rpivotTable(data = brasil_comex, rows = "state",cols="product", vals = "sum", aggregatorName = "usd", rendererName = "Table", width="100%", height="400px")







