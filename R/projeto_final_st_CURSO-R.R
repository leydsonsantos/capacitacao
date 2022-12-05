#Geracao mensal de eletricidade nos EUA.

#Administracao de Informacao de Energia dos EUA.

#Consumo de combustiveis para geracao de eletricidade:

#Consumo de Gas Natural para Geracao de Eletricidade,
#Setor de Energia Eletrica. Em bilhões de pes cubicos.



# carregar base de dados --------------------------------------------------

setwd("C:/Users/Leydson/Desktop/DSA/learn")

files <- dir(pattern = "NG_2022.csv$")

dados <- as.tibble(files %>%
  map_dfr(fread))

dados$Gas_Consumption <- str_replace(dados$Gas_Consumption,",",".")

dados$Gas_Consumption <- as.numeric(dados$Gas_Consumption)
