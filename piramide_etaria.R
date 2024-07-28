library(censobr)
library(arrow)
library(dplyr)
library(ggplot2)
library(readxl)
library(purrr) 
library(sidrar)
library(showtext) 
library(tidyverse)
library(janitor) 
library(grid)
library(patchwork)

#Vamos agilizar já importando a fonte que queremos para o gráfico
font_add_google("Montserrat", "montserrat")
showtext_auto()

options(scipen = 99999)

#####
#Rações de envelhecimento, sexo e idade mediana
razoes22abl <- get_sidra( x= "9515",
                           geo = "City",
                           geo.filter = "4200101",
                           format = 2) 

razoes22sc <- get_sidra( x= "9515",
                          geo = "State",
                          geo.filter = "42",
                          format = 2) 

razoes22br <- get_sidra(x = "9515",
                        geo = "Brazil",
                        format = 2)


# Definir o caminho para os arquivos
caminho <- "caminho/para/a/sua/base/de/dados/"
idade_sc <- read_excel(paste0(caminho, "idade_sc.xlsx"))
colnames(idade_sc)


#Vamos manipular as variáveis de idade com pivot_longer, mas primeiro vamos converter para numérica
idade_sc <- idade_sc %>%
  mutate(across(starts_with("Masculino - ") | starts_with("Feminino - "), as.numeric))

idade_long <- idade_sc %>%
  pivot_longer(cols = starts_with("Masculino - ") | starts_with("Feminino - "),
               names_to = "Faixa_Etaria_Sexo", 
               values_to = "Populacao") %>%
  separate(Faixa_Etaria_Sexo, into = c("Sexo", "Faixa_Etaria"), sep = " - ") %>%
  mutate(Faixa_Etaria = case_when(
    Faixa_Etaria %in% c("0 a 4 anos", "5 a 9 anos") ~ "0-9 anos",
    Faixa_Etaria %in% c("10 a 14 anos", "15 a 19 anos") ~ "10-19 anos",
    Faixa_Etaria %in% c("20 a 24 anos", "25 a 29 anos") ~ "20-29 anos",
    Faixa_Etaria %in% c("30 a 34 anos", "35 a 39 anos") ~ "30-39 anos",
    Faixa_Etaria %in% c("40 a 44 anos", "45 a 49 anos") ~ "40-49 anos",
    Faixa_Etaria %in% c("50 a 54 anos", "55 a 59 anos") ~ "50-59 anos",
    Faixa_Etaria %in% c("60 a 64 anos", "65 a 69 anos") ~ "60-69 anos",
    Faixa_Etaria %in% c("70 a 74 anos", "75 a 79 anos") ~ "70-79 anos",
    Faixa_Etaria %in% c("80 a 84 anos", "85 a 89 anos") ~ "80-89 anos",
    Faixa_Etaria %in% c("90 a 94 anos", "95 a 99 anos", "100 anos ou mais") ~ "90 anos ou mais"))

#Vamos garantir que estamos pegando os valores corretos
idade_agg <- idade_long %>%
  group_by(Municipio, Faixa_Etaria, Sexo) %>%
  summarise(Total_Populacao = sum(Populacao, na.rm = TRUE)) %>%
  ungroup()

idade_agg <- idade_agg %>%
  group_by(Municipio) %>%
  mutate(Percent_Populacao = Total_Populacao / sum(Total_Populacao) * 100) %>%
  ungroup()

#Agora vamos filtrar os municipios desejados
abelardo <- idade_agg %>%
  filter(Municipio == "Abelardo Luz (SC)")

fronteira <- idade_agg %>%
  filter(Municipio %in% c("São Domingos (SC)", "Ipuaçu (SC)", "Bom Jesus (SC)", "Ouro Verde (SC)", "Faxinal dos Guedes (SC)", "Vargeão (SC)", "Passos Maia (SC)"))

amai <- idade_agg %>%
  filter(Municipio %in% c("Abelardo Luz (SC)", "Bom Jesus (SC)", "Entre Rios (SC)", "Faxinal dos Guedes (SC)", "Ipuaçu (SC)", "Lajeado Grande (SC)", "Marema (SC)", "Ouro Verde (SC)", "Passos Maia (SC)", "Ponte Serrada (SC)", "São Domingos (SC)", "Vargeão (SC)", "Xanxerê (SC)", "Xaxim (SC)"))

oeste_sc <- idade_agg %>%
  filter(Municipio %in% c("Abelardo Luz (SC)", "Água Doce (SC)", "Águas de Chapecó (SC)", "Águas Frias (SC)", "Alto Bela Vista (SC)", "Anchieta (SC)", "Arabutã (SC)", "Arroio Trinta (SC)", "Arvoredo (SC)", "Bandeirante (SC)", "Barra Bonita (SC)", "Belmonte (SC)", "Bom Jesus (SC)", "Bom Jesus do Oeste (SC)", "Caçador (SC)", "Caibi (SC)", "Calmon (SC)", "Campo Erê (SC)", "Capinzal (SC)", "Catanduvas (SC)", "Caxambu do Sul (SC)", "Chapecó (SC)", "Concórdia (SC)", "Cordilheira Alta (SC)", "Coronel Freitas (SC)", "Coronel Martins (SC)", "Cunha Porã (SC)", "Cunhataí (SC)", "Descanso (SC)", "Dionísio Cerqueira (SC)", "Entre Rios (SC)", "Erval Velho (SC)", "Faxinal dos Guedes (SC)", "Flor do Sertão (SC)", "Formosa do Sul (SC)", "Fraiburgo (SC)", "Galvão (SC)", "Guaraciaba (SC)", "Guarujá do Sul (SC)", "Guatambu (SC)", "Herval d'Oeste (SC)", "Ibiam (SC)", "Ibicaré (SC)", "Iomerê (SC)", "Ipira (SC)", "Iporã do Oeste (SC)", "Ipuaçu (SC)", "Ipumirim (SC)", "Iraceminha (SC)", "Irani (SC)", "Irati (SC)", "Itá (SC)", "Itapiranga (SC)", "Jaborá (SC)", "Jardinópolis (SC)", "Joaçaba (SC)", "Jupiá (SC)", "Lacerdópolis (SC)", "Lajeado Grande (SC)", "Lebon Régis (SC)", "Lindóia do Sul (SC)", "Luzerna (SC)", "Macieira (SC)", "Maravilha (SC)", "Marema (SC)", "Matos Costa (SC)", "Modelo (SC)", "Mondaí (SC)", "Nova Erechim (SC)", "Nova Itaberaba (SC)", "Novo Horizonte (SC)", "Ouro (SC)", "Ouro Verde (SC)", "Paial (SC)", "Palma Sola (SC)", "Palmitos (SC)", "Paraíso (SC)", "Passos Maia (SC)", "Peritiba (SC)", "Pinhalzinho (SC)", "Pinheiro Preto (SC)", "Piratuba (SC)", "Planalto Alegre (SC)", "Ponte Serrada (SC)", "Presidente Castello Branco (SC)", "Princesa (SC)", "Quilombo (SC)", "Rio das Antas (SC)", "Riqueza (SC)", "Romelândia (SC)", "Saltinho (SC)", "Salto Veloso (SC)", "Santa Helena (SC)", "Santa Terezinha do Progresso (SC)", "Santiago do Sul (SC)", "São Bernardino (SC)", "São Carlos (SC)", "São Domingos (SC)", "São João do Oeste (SC)", "São José do Cedro (SC)", "São Lourenço do Oeste (SC)", "São Miguel da Boa Vista (SC)", "São Miguel do Oeste (SC)", "Saudades (SC)", "Seara (SC)", "Serra Alta (SC)", "Sul Brasil (SC)", "Tangará (SC)", "Tigrinhos (SC)", "Treze Tílias (SC)", "Tunápolis (SC)", "União do Oeste (SC)", "Vargeão (SC)", "Vargem Bonita (SC)", "Videira (SC)", "Xanxerê (SC)", "Xavantina (SC)", "Xaxim (SC)"))


#Agora vamos fazer uma função para calcular pra gente
gerar_piramide_etaria <- function(df, titulo) {
   max_val <- max(abs(df$Total_Populacao), na.rm = TRUE)
  
  df_resumo <- df %>%
    group_by(Faixa_Etaria, Sexo) %>%
    summarise(Media_Populacao = mean(Total_Populacao, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    mutate(Media_Populacao = if_else(Sexo == "Masculino", -Media_Populacao, Media_Populacao),
           # Adiciona uma coluna para o ajuste do rótulo
           pos_rótulo = if_else(Media_Populacao < 0, Media_Populacao - 0.1 * max_val, Media_Populacao + 0.1 * max_val))
  
  ggplot(df_resumo, aes(x = Faixa_Etaria, y = Media_Populacao, fill = Sexo)) +
    geom_bar(stat = "identity") +
    geom_text(aes(y = pos_rótulo, 
                  label = scales::number(abs(Media_Populacao), accuracy = 0.01, decimal.mark = ",")), 
              size = 4, fontface = "plain", family = "Montserrat") +  
    scale_fill_manual(values = c("Masculino" = "#9b1627", "Feminino" = "#65b7dd")) +
    coord_flip() +
    scale_y_continuous(labels = function(x) scales::number(abs(x), accuracy = 0.01, decimal.mark = ","),
                       breaks = c(-7.5, -5.0, -2.5, 0, 2.5, 5.0, 7.5)) +  #Rótulos específicos no eixo y
    labs(title = titulo, x = NULL, y = "População", fill = "",  #Remove o título do eixo x
         caption = "Fonte: Censo Demográfico/IBGE (2022). Elaborado por @abelardoluzemdados") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 22, family = "Montserrat"),  #Aumenta o tamanho do título
      legend.position = "top",
      legend.justification = c("center", "center"),
      legend.title = element_text(face = "bold", family = "Montserrat", size = 14),  #Fonte do título da legenda
      legend.text = element_text(face = "bold", family = "Montserrat", size = 14),  #Fonte dos labels da legenda
      plot.caption = element_text(hjust = 0.5, family = "Montserrat", size = 12),  #Ajusta o tamanho do caption
      text = element_text(family = "Montserrat"),
      axis.title.x = element_blank(),  # Remove o título do eixo x
      axis.title.y = element_text(face = "bold", family = "Montserrat", size = 14),  #Ajusta o tamanho do título do eixo y
      axis.text.x = element_text(family = "Montserrat", size = 14),  #Aumenta o tamanho dos rótulos do eixo x
      axis.text.y = element_text(family = "Montserrat", size = 14)  #Aumenta o tamanho dos rótulos do eixo y
    )
}

gerar_piramide_etaria2 <- function(df, titulo) {
  max_val <- max(abs(df$Total_Populacao), na.rm = TRUE)
  
  df_resumo <- df %>%
    group_by(Faixa_Etaria, Sexo) %>%
    summarise(Media_Populacao = mean(Total_Populacao, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    mutate(Media_Populacao = if_else(Sexo == "Masculino", -Media_Populacao, Media_Populacao),
             pos_rótulo = if_else(Media_Populacao < 0, Media_Populacao - 0.1 * max_val, Media_Populacao + 0.1 * max_val))
  
  ggplot(df_resumo, aes(x = Faixa_Etaria, y = Media_Populacao, fill = Sexo)) +
    geom_bar(stat = "identity") +
    geom_text(aes(y = pos_rótulo, 
                  label = scales::number(abs(Media_Populacao), accuracy = 0.01, decimal.mark = ",")), 
              size = 4, fontface = "plain", family = "Montserrat") +  # Remove o negrito dos rótulos de dados
    scale_fill_manual(values = c("Masculino" = "#9b1627", "Feminino" = "#65b7dd")) +
    coord_flip() +
    scale_y_continuous(labels = function(x) scales::number(abs(x), accuracy = 0.01, decimal.mark = ","),
                       breaks = c(-7.5, -5.0, -2.5, 0, 2.5, 5.0, 7.5)) +  #Rótulos específicos no eixo y
    labs(title = titulo, x = NULL, y = "População", fill = "") +  #Remove o título do eixo x
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 22, family = "Montserrat"),  #Aumenta o tamanho do título
      legend.position = "top",
      legend.justification = c("center", "center"),
      legend.title = element_text(face = "bold", family = "Montserrat", size = 14),  #Fonte do título da legenda
      legend.text = element_text(face = "bold", family = "Montserrat", size = 14),  #Fonte dos labels da legenda
      text = element_text(family = "Montserrat"),
      axis.title.x = element_blank(),  # Remove o título do eixo x
      axis.title.y = element_text(face = "bold", family = "Montserrat", size = 14),  #Ajusta o tamanho do título do eixo y
      axis.text.x = element_text(family = "Montserrat", size = 14),  #Aumenta o tamanho dos rótulos do eixo x
      axis.text.y = element_text(family = "Montserrat", size = 14)  #Aumenta o tamanho dos rótulos do eixo y
    )
}

#E por fim, vamos gerando pirâmides etárias
grafico_abelardo <- gerar_piramide_etaria(abelardo, "Gráfico 1 - Pirâmide etária em Abelardo Luz, valores em %")

grafico_fronteira <- gerar_piramide_etaria2(fronteira, "Fronteira")
grafico_amai <- gerar_piramide_etaria2(amai, "AMAI (SC)") +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
grafico_oeste <- gerar_piramide_etaria2(oeste_sc, "Oeste de SC") +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

grafico_combinado <- (grafico_fronteira | grafico_amai | grafico_oeste) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Gráfico 2 - Pirâmides etárias da região",
    caption = "Fonte: Censo Demográfico/IBGE (2022). Elaborado por @abelardoluzemdados",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 22, family = "Montserrat"),  #Tamanho do título geral
      plot.caption = element_text(hjust = 0.5, size = 12, family = "Montserrat"),  #Ajusta o tamanho da nota de rodapé
      legend.position = "top",  #Coloca a legenda em cima
      legend.justification = c("center", "center"),
      legend.title = element_text(face = "bold", family = "Montserrat", size = 14),  #Fonte do título da legenda
      legend.text = element_text(face = "bold", family = "Montserrat", size = 14)  #Fonte dos labels da legenda
      ))

#Agora vamos exibir os gráficos
grafico_abelardo 
grafico_combinado