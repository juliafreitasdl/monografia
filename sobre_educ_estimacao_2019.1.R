# PNADC 2019.1: base, deflator e transformando a variavel de ocupacao em numero
microdados_pnadc2019.1 <- PNADcIBGE::read_pnadc("PNADC_012019.txt", 
                                                "input_PNADC_trimestral.txt", 
                                                vars = c("UPA","V1028","Estrato","UF","V2007",
                                                         "V2009","V2010","VD4002",
                                                         "VD4016","V4010","VD3005",
                                                         "V4029","V4032","V1022", "V1023"))

pnadc_2019.1 <- PNADcIBGE::pnadc_deflator(microdados_pnadc2019.1, deflator.file="deflator_PNADC_2022_trimestral_101112.xls")
pnadc_2019.1$VD4016_def <- pnadc_2019.1$VD4016*pnadc_2019.1$Habitual
pnadc_2019.1 <- dplyr::mutate_all(pnadc_2019.1, as.numeric)

# cbo, cod, fazendo equivalencia entre cbo e cod e por fim, adicionando as escolaridades minimas e max.
equivalencia_cbo_cod <- readxl::read_excel("equivalencia_cbo_cod.xlsx")
escolaridade_cbo <- readxl::read_excel("escolaridade_requerida_cbo.xlsx")

pnadc_2019.1_escolaridade <- full_join(pnadc_2019.1, equivalencia_cbo_cod, by = "V4010") %>%
  full_join(escolaridade_cbo, by = "cbo")

# Recortes na base: idade (18-60), retirada de dirigentes e legisladores
pnadc_sem_pesos <- subset(pnadc_2019.1_escolaridade, V2009 %in% 18:60 & (V4010 %in% 1120:9629 | is.na(V4010)))

## Criando colunas
pnadc_estimacao_2019.1 <- pnadc_sem_pesos %>% mutate(classificacao = ifelse(VD3005 > Maximo, 1, 0),
                                                     mulher = ifelse(V2007 == 2, 1,0),
                                                     negro = ifelse(V2010 %in% c(2,4), 1, 0),
                                                     escolaridade = VD3005,
                                                     idade = V2009,
                                                     carteira = ifelse(V4029 == 1,1,0),
                                                     urbano = ifelse(V1022 == 1,1,0),
                                                     capital = ifelse(V1023 == 1,1,0))
##Estimações e quantis

# Definir o número de quintis desejados
num_quintis <- 5

# Dividir a base de dados em quintis de rendimento
data_quintis <- pnadc_estimacao_2019.1 %>%
  mutate(rend_quintil = ntile(VD4016_def, num_quintis))

# Inicializar uma lista vazia para armazenar os resultados
resultados <- list()

# Loop sobre os quintis de rendimento
for (i in 1:num_quintis) {
  # Selecionar a amostra correspondente ao quintil atual
  amostra <- subset(data_quintis, rend_quintil == i) 
  # Executar o probit para a amostra atual
  modelo <- glm(classificacao ~ mulher + negro + escolaridade + idade + carteira + 
                  urbano + capital, family = binomial(link = "probit"), data = pnadc_estimacao_2019.1)
  summary(modelo)  
  # Calcular as marginais do modelo probit bivariado
  marginais <- mfx::probitmfx(modelo, atmean = FALSE, data = amostra)
  # Armazenar os resultados das marginais
  resultados[[i]] <- marginais
}
