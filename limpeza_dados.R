library(tidyverse)
library(tmap)
library(tidygeocoder)
library(tmaptools)
library(mapview)
library(leaflet)
library(sf)
library(readxl)
library(kableExtra)
library(ggiraph)
library(highcharter)
library(patchwork)
library(ggiraphExtra)
library(ggmap)
library(gridExtra)
library(grid)





homicidio17 <- read_xlsx("C:/Users/Caio/Desktop/APRIORI/Homicidio_2017_2022.xlsx", sheet = '2017')

homicidio18 <- read_xlsx("C:/Users/Caio/Desktop/APRIORI/Homicidio_2017_2022.xlsx", sheet = '2018')

homicidio19 <- read_xlsx("C:/Users/Caio/Desktop/APRIORI/Homicidio_2017_2022.xlsx", sheet = '2019')

homicidio20 <- read_xlsx("C:/Users/Caio/Desktop/APRIORI/Homicidio_2017_2022.xlsx", sheet = '2020')

homicidio21 <- read_xlsx("C:/Users/Caio/Desktop/APRIORI/Homicidio_2017_2022.xlsx", sheet = '2021')

homicidio22 <- read_xlsx("C:/Users/Caio/Desktop/APRIORI/Homicidio_2017_2022.xlsx", sheet = '2022')



# Sumarizando dados por mes e ano

# primeiro vamos fazer gráficos e sumarizar os dados de homicidio. Depois filtra as latitudes e longitudes



# DADOS GERAIS PARA OS PRIMEIROS GRAFICOS E TABELAS-----------------------------------------------



filtragem <- function(dado){
  
  provisorio <- dado %>% 
    dplyr::filter(DEPARTAMENTO_CIRCUNSCRICAO == 'DECAP') %>% 
    dplyr::mutate(DATA = format(DATA_FATO, format = '%Y-%m-%d'),
                  ano = lubridate::year(DATA),
                  mes = lubridate::month(DATA),
                  mes = lubridate::month(mes, label = TRUE)) %>% 
    dplyr::group_by(ano,mes) %>%
    dplyr::summarise(vitimas = sum(`Nº DE VÍT HD`))
  
  
}



homicidio <- list(homicidio17,homicidio18,homicidio19,homicidio20,homicidio21,homicidio22) %>% 
  map(filtragem) %>% 
  reduce(full_join)



# rodar de novo o codigo abaixo

homicidio <- homicidio %>% group_by(ano,mes) %>% summarise(vitimas = sum(vitimas))



homicidio$ano <- as.factor(homicidio$ano)



# Exportar o arquivo para só importar no Rmarkdown e não precisar fazer grandes transformações


write_csv(homicidio,"C:\\Users\\Caio\\Desktop\\Acessibilidade Urbana\\homicidio_geral.csv")





# Dados por ano


homicidio %>% 
  group_by(ano) %>% 
  summarise(vitimas = sum(vitimas)) %>% 
  ggplot(aes(x = ano, y = vitimas))+
  geom_col()+
  geom_text(aes(label = vitimas), position = position_stack(vjust = 0.5) )+
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        rect = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Número de Vítimas de Homicídio Doloso na Cidade de SP")



# dados por ano utilizando highchart

homicidio %>% 
  group_by(ano) %>% 
  summarise(vitimas = sum(vitimas)) %>% 
  hchart('column', hcaes(x = 'ano', y = 'vitimas')) %>% 
  highcharter::hc_title(text = 'Número de Vítimas de Homicídio Doloso na Cidade de SP')





# Dados por mês/ano


homicidio %>% hchart('line', hcaes(x = mes, y = vitimas, group = ano))



homicidio %>% ggplot(aes(x = mes, y = vitimas))+
  geom_bar(position="dodge", stat="identity", fill = '#7cb5ec')+
  facet_wrap(~ano, scales = 'free')+
  labs(title = "Comparativo do Nº de Vítimas de Homicídios Dolosos na cidade de SP")+
  theme_minimal()+
  theme(axis.title = element_blank(),
        #axis.text.y = element_blank(),
        #axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.title.position = "plot",
        strip.background=element_rect(colour="black"),
        axis.text.x = element_text(angle = 45))






# Tabela mes/ano

homicidio %>% pivot_wider(names_from = ano, values_from = vitimas) %>% kbl() %>%  kable_styling(fixed_thead = TRUE)



homicidio %>% pivot_wider(names_from = ano, values_from = vitimas) %>% kbl()





homicidio %>% 
  mutate(mes = dplyr::case_when(mes == "jan" ~ "Janeiro",
                                mes == "fev" ~ "Fevereiro",
                                mes == "mar" ~ "Março",
                                mes == "abr" ~ "Abril",
                                mes == "mai" ~ "Maio",
                                mes == "jun" ~ "Junho",
                                mes == "jul" ~ "Julho",
                                mes == "ago" ~ "Agosto",
                                mes == "set" ~ "Setembro",
                                mes == "out" ~ "Outubro",
                                mes == "nov" ~ "Novembro",
                                mes == "dez" ~ "Dezembro")) %>% 
  rename('Mês' = 'mes',
         'Ano' = 'ano') %>% 
  pivot_wider(names_from = Ano, values_from = vitimas) %>% 
  kable('html', table.attr = "style='width:74%;'", align = "c")





#---------------------------------------------------------------------------------------







# Algumas limpezas para a tabela com os distritos que mais/menos registram---------------------------------

zonas_elec_sp <- read_sf("C:\\Users\\Caio\\Desktop\\Furtos - Dados\\Furtos por bairro\\arquivo shape\\DISTPOL2021_MSP_CEM_V1.shp")





filtragem_mapa <- function(dado){
  
  provisorio <- dado %>% 
    dplyr::filter(DEPARTAMENTO_CIRCUNSCRICAO == 'DECAP') %>% 
    dplyr::mutate(DATA = format(DATA_FATO, format = '%Y-%m-%d'),
                  ano = lubridate::year(DATA),
                  mes = lubridate::month(DATA))
}




homicidio20 <- homicidio20 %>% mutate(NUM_BO = as.character(NUM_BO),
                                      NUMERO_LOGRADOURO = as.character(NUMERO_LOGRADOURO))



homicidio21 <- homicidio21 %>% mutate(NUM_BO = as.character(NUM_BO),
                                      NUMERO_LOGRADOURO = as.character(NUMERO_LOGRADOURO))




homicidio_mapa <- list(homicidio17,homicidio18,homicidio19,homicidio20,homicidio21,homicidio22) %>% 
  map(filtragem_mapa) %>% 
  reduce(full_join) %>% 
  mutate(DP_COD = str_extract(DP_CIRCUNSCRICAO, "[0-9]+"),
         DP_COD = as.integer(DP_COD),
         ocorrencia = lubridate::ymd(DATA_FATO),
         mes_ocorrencia = lubridate::month(ocorrencia),
         ano_ocorrencia = lubridate::year(ocorrencia),
         DP_CIRCUNSCRICAO = dplyr::case_when(DP_CIRCUNSCRICAO =="003 DP - Campos Elísios" ~ "003 DP - Campos Elíseos", TRUE~ DP_CIRCUNSCRICAO))







homicidio_mapa <- homicidio_mapa %>% 
  add_row(DP_COD = 18, ano_ocorrencia = 2018, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 31, ano_ocorrencia = 2018, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 56, ano_ocorrencia = 2018, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 36, ano_ocorrencia = 2019, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 51, ano_ocorrencia = 2019, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 52, ano_ocorrencia = 2019, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 57, ano_ocorrencia = 2019, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 93, ano_ocorrencia = 2019, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 26, ano_ocorrencia = 2020, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 27, ano_ocorrencia = 2020, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 36, ano_ocorrencia = 2020, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 57, ano_ocorrencia = 2020, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 102, ano_ocorrencia = 2020, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 103, ano_ocorrencia = 2020, `Nº DE VÍT HD` = 0) %>%
  add_row(DP_COD = 96, ano_ocorrencia = 2020, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 18, ano_ocorrencia = 2021, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 27, ano_ocorrencia = 2021, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 36, ano_ocorrencia = 2021, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 57, ano_ocorrencia = 2021, `Nº DE VÍT HD` = 0)%>% 
  add_row(DP_COD = 81, ano_ocorrencia = 2021, `Nº DE VÍT HD` = 0)%>% 
  add_row(DP_COD = 93, ano_ocorrencia = 2021, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 99, ano_ocorrencia = 2022, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 57, ano_ocorrencia = 2022, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 13, ano_ocorrencia = 2022, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 29, ano_ocorrencia = 2022, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 7, ano_ocorrencia = 2022, `Nº DE VÍT HD` = 0) %>% 
  add_row(DP_COD = 4, ano_ocorrencia = 2022, `Nº DE VÍT HD` = 0)







# Regiões de mais casos para colocar no RMarkdown


homicidio_regioes <- homicidio_mapa %>% 
  group_by(DP_CIRCUNSCRICAO, ano_ocorrencia) %>% 
  summarise(vitimas = sum(`Nº DE VÍT HD`)) %>% 
  pivot_wider(names_from = ano_ocorrencia, values_from = vitimas) %>% 
  replace_na(list('2017' = 0,'2018' = 0,'2019' = 0,'2020' = 0,'2021' = 0,'2022' = 0))



write.csv(homicidio_regioes,"C:\\Users\\Caio\\Desktop\\Dados Criminais  São Paulo\\Dados utilizados na análise (já modificados)\\homicidio_regioes.csv")





# Mapas interativos ---------------------------------





homicidio_mapa <- homicidio_mapa %>% group_by(DP_COD, ano_ocorrencia) %>% summarise(vitimas = sum(`Nº DE VÍT HD`))


write.csv(homicidio_mapa,"C:/Users/Caio/Desktop/Projeto - Homicidios Dolosos/dados_prontos/homicidio_mapa.csv")




homicidio_mapa <- zonas_elec_sp %>% left_join(homicidio_mapa, by = 'DP_COD') 



homicidio_mapa <- homicidio_mapa %>% mutate(tooltip = paste("Delegacia:",DP_NOM_C,"\nDP_Num:",DP_COD,"\nN de Vítimas:",vitimas))


homicidio_mapa$homicidios <-  cut(homicidio_mapa$vitimas, 
                                  breaks=c(-1, 4, 10, 20, 30,40), 
                                  labels=c("0 - 4", "5 - 10", "11 - 20", 
                                           "21 - 30", "31 - 40"), right = TRUE) 




dps_id <- readRDS("C:\\Users\\Caio\\Desktop\\Learning Shiny\\aprendendo_shiny\\dps_id.rds")


# teste dia 24/10
homicidio_mapa %>% view()

write.csv(homicidio_mapa,"C:/Users/Caio/Desktop/Projeto - Homicidios Dolosos/dados_prontos/homicidio_mapa.csv")


homicidio_mapa <- read_csv("C:/Users/Caio/Desktop/Projeto - Homicidios Dolosos/dados_prontos/homicidio_mapa.csv")

# colocar varios interativos lado a lado



b = homicidio_mapa %>% filter(ano_ocorrencia == 2022) %>% 
  ggplot(aes()) + 
  geom_sf_interactive(color = 'gray50', size = 0.2, data_id = dps_id, # preciso arrumar isso
                      aes(fill = homicidios,tooltip = tooltip))+
  #scale_fill_continuous(high = "#132B43", low = "#56B1F7", name = '')+
  #scale_fill_distiller_interactive(name = "Score", palette = 1)+
  scale_fill_manual(values = c("#F8F3B4", "#F9B31D","#F9841D","#F9521D","#B60F01"))+
  theme_void() + 
  theme(panel.grid.minor = element_blank(),
        rect = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size = 7))



tooltip_css = paste0("border-radius: 4px;",
                     "border: 1px solid gray;",
                     "background: white;",
                     "padding: 5px;",
                     "text-align: center;",
                     "opactiy: 0.5")


i = girafe(ggobj =  b, 
           options = list(
             opts_tooltip(css = tooltip_css),
             opts_zoom(max = 5),
             opts_hover_inv(css = "opacity:0.5;"),
             opts_hover(css = "stroke:red;stroke-width:1.0;")
           ) )


i

# verificar os faltantes

dps_id %>% as_data_frame() %>% filter(!value %in% teste$DP_COD)

teste <- homicidio_mapa %>% filter(ano_ocorrencia == '2022') %>% select(DP_COD)


# salvar os graficos

library(htmlwidgets)

widget_i <- saveWidget(i, file = "widget_i.html")







# LEAFLET / CASOS GEOREFERENCIADOS---------------------------




homicidio_georeferenciado <- homicidio22 %>% 
  filter(DEPARTAMENTO_CIRCUNSCRICAO == 'DECAP' & !str_detect(LOGRADOURO, "VEDAÇÃO")) %>% 
  select(NUM_BO,LOGRADOURO,DATA_FATO ,LATITUDE, LONGITUDE) %>%
  filter(!str_detect(LATITUDE,"NULL")) # precisa coletar alguns latitudes e longitudes




# precisa transformar pra numérico



homicidio_georeferenciado <- homicidio_georeferenciado %>% 
  mutate(LATITUDE = as.numeric(LATITUDE),
         LONGITUDE = as.numeric(LONGITUDE)) 


homicidio_georeferenciado <- homicidio_georeferenciado %>% mutate(label = paste("Endereço: ",LOGRADOURO, ", Data: ",DATA_FATO))


# exportando
write.csv(homicidio_georeferenciado,"C:\\Users\\Caio\\Desktop\\Acessibilidade Urbana\\homicidio_georreferenciado.csv")



leaflet(homicidio_georeferenciado) %>% 
  fitBounds(lng1 = -46.89, 
            lat1 = -23.77, 
            lng2 = -46.34, 
            lat2 = -23.35) %>% 
  addTiles() %>% addMarkers(clusterOptions = markerClusterOptions(),label = ~as.character(label))



# Mapas georreferenciados mais simples(graficos lado a lado)------------------------------------



homic_sf <- homicidio_georeferenciado %>% st_as_sf(coords = c('LONGITUDE', 'LATITUDE'), crs = 4326)


distritos_policiais <- read_sf("C:\\Users\\Caio\\Desktop\\Acessibilidade Urbana\\arquivo_shapefile_distritos\\Distritos_Policiais_SHP.shp")




# 2022
mapa_22 <- ggplot(distritos_policiais) + 
  geom_sf() + 
  geom_sf(data = homic_sf$geometry,  size = 2, shape = 18, color = "red",alpha = 0.7)+ # gosto do 16 e 18 e 20
  theme_minimal()+
  theme(axis.title = element_blank(),
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5))+
  labs(title = "2022")




# 2021

homic_21 <- homicidio21 %>% 
  filter(DEPARTAMENTO_CIRCUNSCRICAO == 'DECAP' & !str_detect(LOGRADOURO, "VEDAÇÃO")) %>%
  filter(!str_detect(LATITUDE,"NULL")) %>% 
  st_as_sf(coords = c('LONGITUDE', 'LATITUDE'), crs = 4326) 


homic_21 <- homic_21 %>% filter(!str_detect(LOGRADOURO, "RUA PARANAIBA"))


mapa_21 <- ggplot(distritos_policiais)+
  geom_sf() + 
  geom_sf(data = homic_21$geometry,  size = 2, shape = 18, color = "red",alpha = 0.7)+ # gosto do 16 e 18 e 20
  theme_minimal()+
  theme(axis.title = element_blank(),
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5))+
  labs(title = "2021")





# 2020



homic_20 <- homicidio20 %>% 
  filter(DEPARTAMENTO_CIRCUNSCRICAO == 'DECAP' & !str_detect(LOGRADOURO, "VEDAÇÃO")) %>%
  filter(!str_detect(LATITUDE,"NULL")) %>% 
  st_as_sf(coords = c('LONGITUDE', 'LATITUDE'), crs = 4326) 




mapa_20 <- ggplot(distritos_policiais)+
  geom_sf() + 
  geom_sf(data = homic_20$geometry,  size = 2, shape = 18, color = "red",alpha = 0.7)+ # gosto do 16 e 18 e 20
  theme_minimal()+
  theme(axis.title = element_blank(),
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5))+
  labs(title = "2020")




# 2019 - precisa arrumar


homic_19 <- homicidio19 %>% 
  filter(DEPARTAMENTO_CIRCUNSCRICAO == 'DECAP' & !str_detect(LOGRADOURO, "VEDAÇÃO")) %>%
  filter(!str_detect(LATITUDE,"NULL")) %>% 
  st_as_sf(coords = c('LONGITUDE', 'LATITUDE'), crs = 4326) 


homic_19 <- homic_19 %>% filter(!str_detect(LOGRADOURO, "RUA OXOSSI") & (!str_detect(LOGRADOURO, "ALAMEDA DOS LÍRIOS")))



mapa_19 <- ggplot(distritos_policiais)+
  geom_sf() + 
  geom_sf(data = homic_19$geometry,  size = 2, shape = 18, color = "red",alpha = 0.7)+ # gosto do 16 e 18 e 20
  theme_minimal()+
  theme(axis.title = element_blank(),
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5))+
  labs(title = "2019")



# 2018



homic_18 <- homicidio18 %>% 
  filter(DEPARTAMENTO_CIRCUNSCRICAO == 'DECAP' & !str_detect(LOGRADOURO, "VEDAÇÃO")) %>%
  filter(!str_detect(LATITUDE,"NULL")) %>%
  filter(!is.na(LATITUDE)) %>%
  filter(!str_detect(LONGITUDE, "NULL")) %>% 
  st_as_sf(coords = c('LONGITUDE', 'LATITUDE'), crs = 4326)




mapa_18 <- ggplot(distritos_policiais)+
  geom_sf() + 
  geom_sf(data = homic_18$geometry,  size = 2, shape = 18, color = "red",alpha = 0.7)+ # gosto do 16 e 18 e 20
  theme_minimal()+
  theme(axis.title = element_blank(),
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5))+
  labs(title = "2018")


# 2017



homic_17 <- homicidio17 %>% 
  filter(DEPARTAMENTO_CIRCUNSCRICAO == 'DECAP' & !str_detect(LOGRADOURO, "VEDAÇÃO")) %>%
  filter(!str_detect(LATITUDE,"NULL")) %>%
  filter(!is.na(LATITUDE)) %>%
  filter(!str_detect(LONGITUDE, "NULL")) %>% 
  st_as_sf(coords = c('LONGITUDE', 'LATITUDE'), crs = 4326)




mapa_17 <- ggplot(distritos_policiais)+
  geom_sf() + 
  geom_sf(data = homic_17$geometry,  size = 2, shape = 18, color = "red",alpha = 0.7)+ # gosto do 16 e 18 e 20
  theme_minimal()+
  theme(axis.title = element_blank(),
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5))+
  labs(title = "2017")




gridExtra::grid.arrange(mapa_22,mapa_21,mapa_20,mapa_19,mapa_18,mapa_17, ncol = 3,nrow = 2)





