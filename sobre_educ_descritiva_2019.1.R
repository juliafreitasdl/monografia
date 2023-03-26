rm(list=ls(all=TRUE))
############ Bases, recortes e preparação para analise descritiva

#PNADC 2019.1: base, deflator e transformando a variavel de ocupacao em numero
microdados_pnadc2019.1 <- read_pnadc("PNADC_012019.txt", 
                          "input_PNADC_trimestral.txt", 
                          vars = c("UPA","V1028","Estrato","UF","V2007",
                                   "V2009","V2010","VD4002",
                                   "VD4016","V4010","VD3005",
                                   "V4029","V4032","V1022", "V1023"))

pnadc_2019.1 <- pnadc_deflator(microdados_pnadc2019.1, deflator.file="deflator_PNADC_2022_trimestral_101112.xls")

pnadc_2019.1$VD4016_def <- pnadc_2019.1$VD4016*pnadc_2019.1$Habitual
pnadc_2019.1$V4010 <- as.numeric(pnadc_2019.1$V4010)

#cbo, cod, fazendo equivalencia entre cbo e cod e por fim, adicionando as escolaridades minimas e max.
equivalencia_cbo_cod <- readxl::read_excel("equivalencia_cbo_cod.xlsx")
escolaridade_cbo <- readxl::read_excel("escolaridade_requerida_cbo.xlsx")

pnadc_equivalente_2019.1 <- dplyr::full_join(pnadc_2019.1, equivalencia_cbo_cod, by ="V4010")

pnadc_2019.1_escolaridade <- dplyr::full_join(pnadc_equivalente_2019.1, escolaridade_cbo, by ="cbo")

#recortes na base: idade (18-60), retirada de dirigentes e legisladores
pnadc_com_idade <- subset(pnadc_2019.1_escolaridade, V2009 %in% c(18:60))
pnadc_sem_pesos <- subset(pnadc_com_idade, V4010 %in% c(1120:9629)| is.na(V4010))

#adicionando os pesos amostrais a base
pnadc_2019.1_final <- pnadc_sem_pesos %>% srvyr::as_survey_design(ids = UPA, 
                                                       strata = Estrato, 
                                                       weights = V1028, 
                                                       nest = TRUE)

# variavel de ocupação e educação em numero
pnadc_2019.1_final$variables$VD3005 <- as.numeric(pnadc_2019.1_final$variables$VD3005)
pnadc_2019.1_final$variables$VD4002 <- as.numeric(pnadc_2019.1_final$variables$VD4002)

#criando colunas: se o trabalhador é sobre-educado(1) ou não (0), e sobre a região que reside ( 1 - Norte; 
# 2 - Nordeste; 3 - Sudeste; 4 - Sul; 5 - Centro-Oeste)
pnadc_2019.1_final <- pnadc_2019.1_final %>% mutate (classificacao = case_when(
                                                     VD3005 > Maximo ~ 1,
                                                     VD3005 < Minimo ~ 0),
                                                   regiao = case_when(
                                                     UF %in% c(11:17) ~ 1,
                                                     UF %in% c(21:29) ~ 2,
                                                     UF %in% c(31:35) ~ 3,
                                                     UF %in% c(41:43) ~ 4, 
                                                     UF %in% c(50:53) ~ 5))

#Limpando a memoria e deixando apenas o que precisa para a analise a seguir
rm("microdados_pnadc2019.1","pnadc_2019.1","escolaridade_cbo","equivalencia_cbo_cod",
   "pnadc_equivalente_2019.1","pnadc_2019.1_escolaridade","pnadc_sem_idade", "pnadc_sem_pesos")

####################### analise descritiva preliminar

## TOTAL DE OCUPADOS [2]
total_ocupados_2019.1 <- survey::svytotal(x=~VD4002==1, design=pnadc_2019.1_final, na.rm=TRUE)

### TOTAL DE SOBRE-EDUCADOS [2]
total_sobre_educados_2019.1 <- survey::svytotal(x=~classificacao == 1, design=pnadc_2019.1_final, na.rm=TRUE)

### TOTAL DE SUB-EDUCADOS [2]
total_sub_educados_2019.1 <- survey::svytotal(x=~classificacao == 0 , design=pnadc_2019.1_final, na.rm=TRUE)

### PORCENTAGEM SOBRE-EDUCADOS
taxa_sobre_educado_2019.1 <- total_sobre_educados_2019.1[2]/total_ocupados_2019.1[2]

###### POR SEXO
## Mulheres - em ordem: total de mulheres ocupadas, total de mulheres sobre-educadas e a taxa de mulheres sobre-educ.
total_mulheres <- survey::svytotal(x=~VD4002==1, design=subset(pnadc_2019.1_final, V2007 == 2), na.rm=TRUE)
stotal_mulheres <- survey::svytotal(x=~classificacao == 1, design=subset(pnadc_2019.1_final, V2007 == 2), na.rm=TRUE)

taxa_mulheres <- stotal_mulheres[2]/total_mulheres[2]
taxa_mulheres

## Homens- em ordem: total de homens ocupadas, total de homens sobre-educadas e a taxa de homens sobre-educ.
total_homens <- survey::svytotal(x=~VD4002==1, design=subset(pnadc_2019.1_final, V2007 == 1), na.rm=TRUE)
stotal_homens <- survey::svytotal(x=~classificacao == 1, design=subset(pnadc_2019.1_final, V2007 == 1), na.rm=TRUE)

taxa_homens <- stotal_homens[2]/total_homens[2]
taxa_homens

###### COR - ordem em relacao a respectiva cor de analise: total de ocupados, total de sobre-educados e taxa de sobre-educ.
## Branca
total_branca <- survey::svytotal(x=~VD4002==1, design=subset(pnadc_2019.1_final, V2010 == 1), na.rm=TRUE)
stotal_branca <- survey::svytotal(x=~classificacao == 1, design=subset(pnadc_2019.1_final, V2010 == 1), na.rm=TRUE)

taxa_branca <- stotal_branca[2]/total_branca[2]
taxa_branca

## Negra
total_negra <- survey::svytotal(x=~VD4002==1, design=subset(pnadc_2019.1_final, V2010 %in% c(2,4)), na.rm=TRUE)
stotal_negra <- survey::svytotal(x=~classificacao == 1, design=subset(pnadc_2019.1_final, V2010 %in% c(2,4)  ), na.rm=TRUE)

taxa_negra <- stotal_negra[2]/total_negra[2]
taxa_negra

## Amarela
total_amarela <- survey::svytotal(x=~VD4002==1, design=subset(pnadc_2019.1_final, V2010 == 3), na.rm=TRUE)
stotal_amarela <- survey::svytotal(x=~classificacao == 1, design=subset(pnadc_2019.1_final, V2010 == 3), na.rm=TRUE)

taxa_amarela <- stotal_amarela[2]/total_amarela[2]
taxa_amarela

## Indigena
total_indigena <- survey::svytotal(x=~VD4002==1, design=subset(pnadc_2019.1_final, V2010 == 5), na.rm=TRUE)
stotal_indigena <- survey::svytotal(x=~classificacao == 1, design=subset(pnadc_2019.1_final, V2010 == 5), na.rm=TRUE)

taxa_indigena <- stotal_indigena[2]/total_indigena[2]
taxa_indigena

####### RENDIMENTO
### Fazer os quantis e depois o total de pessoas que recebem igual ou menos que o resultado para o intervalo.

# Quantil
quantil_renda <- survey::svyquantile(x=~VD4016_def, design=subset(pnadc_2019.1_final, V2009 %in% c(18:60)), 
                                     quantiles=c(0.2, 0.4, 0.6,0.8), na.rm=TRUE)
quantil_renda

## SOBRE-EDUCAÇÃO POR QUINTIL
#20%

total_20 <- survey::svytotal(x=~VD4002 == 1, design=subset(pnadc_2019.1_final, VD4016_def <= 1229.714), na.rm=TRUE)
stotal_20 <- survey::svytotal(x=~classificacao == 1, design=subset(pnadc_2019.1_final, VD4016_def <= 1229.714), na.rm=TRUE)

taxa_20 <- stotal_20[2]/total_20[2]
taxa_20

#40%
total_40 <- survey::svytotal(x=~VD4002 == 1, design=subset(pnadc_2019.1_final, 
                                                           VD4016_def > 1229.714 & 
                                                             VD4016_def <= 1483.811  ), na.rm=TRUE)
stotal_40 <- survey::svytotal(x=~classificacao == 1, design=subset(pnadc_2019.1_final, 
                                                                   VD4016_def > 1229.714 & 
                                                                     VD4016_def <= 1483.811  ), na.rm=TRUE)

taxa_40 <- stotal_40[2]/total_40[2]
taxa_40

#60%
total_60 <- survey::svytotal(x=~VD4002 == 1, design=subset(pnadc_2019.1_final, 
                                                           VD4016_def > 1483.811 & 
                                                             VD4016_def <= 1971.486 ), na.rm=TRUE)
stotal_60 <- survey::svytotal(x=~classificacao == 1, design=subset(pnadc_2019.1_final, 
                                                                   VD4016_def > 1483.811 & 
                                                                     VD4016_def <= 1971.486  ), na.rm=TRUE)

taxa_60 <- stotal_60[2]/total_60[2]
taxa_60

#80%
total_80 <- survey::svytotal(x=~VD4002 == 1, design=subset(pnadc_2019.1_final, 
                                                           VD4016_def > 1971.486 & 
                                                             VD4016_def <= 3156.737 ), na.rm=TRUE)
stotal_80 <- survey::svytotal(x=~classificacao == 1, design=subset(pnadc_2019.1_final, 
                                                                   VD4016_def > 1971.486 & 
                                                                     VD4016_def <= 3156.737), na.rm=TRUE)

taxa_80 <- stotal_80[2]/total_80[2]
taxa_80

#100%
total_100 <- survey::svytotal(x=~VD4002 == 1, design=subset(pnadc_2019.1_final, VD4016_def > 3156.737 ), na.rm=TRUE)
stotal_100 <- survey::svytotal(x=~classificacao == 1, design=subset(pnadc_2019.1_final, VD4016_def > 3156.737), na.rm=TRUE)

taxa_100 <- stotal_100[2]/total_100[2]
taxa_100
