#Geracao mensal de eletricidade nos EUA.

#Administracao de Informacao de Energia dos EUA.

#Consumo de combustiveis para geracao de eletricidade:

#Consumo de Gas Natural para Geracao de Eletricidade,
#Setor de Energia Eletrica. Em bilhões de pes cubicos.



# carregar base de dados --------------------------------------------------

setwd("C:/Users/Leydson/Desktop/DSA/learn")

files <- dir(pattern = "NG_2022.csv$")

dados <- as.tibble(files %>%
  map_dfr(fread,encoding = "Latin-1"))

dados$Gas_Consumption <- str_replace(dados$Gas_Consumption,",",".")

dados$Gas_Consumption <- as.numeric(dados$Gas_Consumption)


# Transformação em tsibble. -----------------------------------------------

dados <- dados %>%
  mutate(ano = as.numeric(str_split(Month, " ",simplify = TRUE)[,1]),
         mes = str_split(Month, " ",simplify = TRUE)[,2],
         mes_n = case_when(mes == "janeiro" ~ 01,
                           mes == "fevereiro" ~ 02,
                           mes == "março" ~ 03,
                           mes == "abril" ~ 04,
                           mes == "maio" ~ 05,
                           mes == "junho" ~ 06,
                           mes == "julho" ~ 07,
                           mes == "agosto" ~ 08,
                           mes == "setembro" ~ 09,
                           mes == "outubro" ~ 10,
                           mes == "novembro" ~ 11,
                           mes == "dezembro" ~ 12))

dados_ts <- dados %>%
  select(Gas_Consumption,ano,mes_n) %>%
  mutate(data = yearmonth(paste(ano,mes_n,sep = " "))) %>%
         as_tsibble(index = data)

# Plot da série. ----------------------------------------------------------

dados_ts %>%
  autoplot(y = Gas_Consumption)


# Verificação da sazonalidade. --------------------------------------------

dados_ts %>%
  gg_season(Gas_Consumption,labels = "both")

# Verificação da sub-sazonalidade. ----------------------------------------

dados_ts %>%
  gg_subseries(Gas_Consumption)


# Verificação da autocorrelação. ------------------------------------------

dados_ts %>%
  gg_lag(lag = 1:24, geom = "point")

dados_lag <-  dados_ts %>%
  mutate(l_12 = lag(Gas_Consumption, n= 12))

dados_ts %>%
  ACF(Gas_Consumption, lag_max = 24) %>%
  autoplot()

# Decomposição da série. --------------------------------------------------

dcmp <- dados_ts %>%
  model(stl = STL(Gas_Consumption))
components(dcmp)


# Plot da tendência da série. ------------------------------------------

components(dcmp) %>%
  as_tsibble() %>%
  autoplot(Gas_Consumption, colour = "gray70") +
  geom_line(aes(y=trend),col = "darkorange2")+
  theme_minimal()+scale_y_continuous(limits = c(100,1400),
                                     breaks = seq(100,1400,200))


