#Geracao mensal de eletricidade nos EUA.

#Administracao de Informacao de Energia dos EUA.

#Consumo de combustiveis para geracao de eletricidade:

#Consumo de Gas Natural para Geracao de Eletricidade,
#Setor de Energia Eletrica. Em bilhões de pes cubicos.



# carregar base de dados --------------------------------------------------
install.packages("urca")
pacotes <- c("tsibble","tidyverse","DT","data.table","feasts","fable","urca")
lapply(pacotes,library,character.only = TRUE)
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
  autoplot()


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



dados_ts %>%
  gg_tsdisplay(difference(Gas_Consumption, 12),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced", y="")

dados_ts %>%
  gg_tsdisplay(difference(Gas_Consumption, 12) %>% difference(),
               plot_type='partial', lag=36) +
  labs(title = "Double differenced", y="")


fit <- dados_ts%>%
  model(arima210110 = ARIMA(Gas_Consumption ~ pdq(2,1,0) + PDQ(1,1,0)),
        arima210011 = ARIMA(Gas_Consumption ~ pdq(2,1,0) + PDQ(0,1,1)),
        arima213011 = ARIMA(Gas_Consumption ~ pdq(2,1,3) + PDQ(0,1,1)),
        arima212011 = ARIMA(Gas_Consumption ~ pdq(2,1,2) + PDQ(0,1,1)),
        arima211011 = ARIMA(Gas_Consumption ~ pdq(2,1,1) + PDQ(0,1,1)),
        arima012011 = ARIMA(Gas_Consumption ~ pdq(0,1,2) + PDQ(0,1,1)),
        arima301110 = ARIMA(Gas_Consumption ~ pdq(3,0,1) + PDQ(1,1,0)),
        arima301210 = ARIMA(Gas_Consumption ~ pdq(3,0,1) + PDQ(2,1,0)),
        arima300210 = ARIMA(Gas_Consumption ~ pdq(3,0,0) + PDQ(2,1,0)),
        auto = ARIMA(Gas_Consumption, stepwise = FALSE, approx = FALSE))

fit %>% pivot_longer(everything(), names_to = "Model name",
                     values_to = "Orders")
glance(fit) %>% arrange(AICc) %>% select(.model:BIC)

fit %>% select(auto) %>% gg_tsresiduals(lag=36)

augment(fit) %>%
  filter(.model == "auto") %>%
  features(.innov, ljung_box, lag=24, dof=4)


dados_teste <- dados_ts %>%
  select(Gas_Consumption) %>%
  filter_index("2000 jan" ~ .)

#train <- dados_teste %>% filter_index(. ~ "2020 jan")

fit_arima <- dados_teste %>% model(arima210110 = ARIMA(Gas_Consumption ~ pdq(2,1,0) + PDQ(1,1,0)),
                             arima210011 = ARIMA(Gas_Consumption ~ pdq(2,1,0) + PDQ(0,1,1)),
                             arima213011 = ARIMA(Gas_Consumption ~ pdq(2,1,3) + PDQ(0,1,1)),
                             arima212011 = ARIMA(Gas_Consumption ~ pdq(2,1,2) + PDQ(0,1,1)),
                             arima211011 = ARIMA(Gas_Consumption ~ pdq(2,1,1) + PDQ(0,1,1)),
                             arima012011 = ARIMA(Gas_Consumption ~ pdq(0,1,2) + PDQ(0,1,1)),
                             arima301110 = ARIMA(Gas_Consumption ~ pdq(3,0,1) + PDQ(1,1,0)),
                             arima301210 = ARIMA(Gas_Consumption ~ pdq(3,0,1) + PDQ(2,1,0)),
                             arima300210 = ARIMA(Gas_Consumption ~ pdq(3,0,0) + PDQ(2,1,0)),
                             auto = ARIMA(Gas_Consumption, stepwise = FALSE, approx = FALSE))

fit_arima %>% pivot_longer(everything(), names_to = "Model name",
                     values_to = "Orders")

glance(fit_arima) %>% arrange(AICc) %>% select(.model:BIC)

fit_arima %>% select(arima211011) %>% gg_tsresiduals(lag=36)

fit_arima %>% select(auto) %>% gg_tsresiduals(lag=36)

augment(fit_arima) %>%
  filter(.model == "arima211011") %>%
  features(.innov, ljung_box, lag=24, dof=5)

augment(fit_arima) %>%
  filter(.model == "auto") %>%
  features(.innov, ljung_box, lag=24, dof=5)


#fit_ets <- dados_teste %>% model(ETS(Gas_Consumption))
#report(fit_ets)

#fit_ets %>%
 # gg_tsresiduals(lag_max = 16)

#augment(fit_ets) %>%
  #features(.innov, ljung_box, lag = 16, dof = 6)



fit_arima_211011 <- fit_arima %>% select(arima211011)

fit_arima_auto <- fit_arima %>% select(auto)

report(fit_arima_212011)

bind_rows(
  fit_arima_211011 %>% accuracy(),
  fit_arima_auto %>% accuracy()) %>%
  select(-ME, -MPE, -ACF1)

forecast(fit_arima_auto,h=12) %>%
  autoplot(dados_teste) +
  labs(title = "Cement production in Australia",
       y = "Tonnes ('000)")
