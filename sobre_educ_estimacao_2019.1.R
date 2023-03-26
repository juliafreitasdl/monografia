rm(list=ls(all=TRUE))
############ Bases, recortes e preparação para estimacoes

#PNADC 2019.1: base, deflator e transformando a variavel de ocupacao em numero
microdados_pnadc2019.1 <- read_pnadc("PNADC_012019.txt", 
                                     "input_PNADC_trimestral.txt", 
                                     vars = c("UPA","V1028","Estrato","UF","V2007",
                                              "V2009","V2010","VD4002",
                                              "VD4016","V4010","VD3005",
                                              "V4029","V4032","V1022", "V1023"))

pnadc_2019.1 <- pnadc_deflator(microdados_pnadc2019.1, deflator.file="deflator_PNADC_2022_trimestral_101112.xls")
pnadc_2019.1$VD4016_def <- pnadc_2019.1$VD4016*pnadc_2019.1$Habitual

#transformando as variaveis em numero
pnadc_2019.1$V4010 <- as.numeric(pnadc_2019.1$V4010)
pnadc_2019.1$UF <- as.numeric(pnadc_2019.1$UF)
pnadc_2019.1$V2007 <- as.numeric(pnadc_2019.1$V2007)
pnadc_2019.1$V2009 <- as.numeric(pnadc_2019.1$V2009)
pnadc_2019.1$V2010 <- as.numeric(pnadc_2019.1$V2010)
pnadc_2019.1$VD4002 <- as.numeric(pnadc_2019.1$VD4002)
pnadc_2019.1$VD4016_def <- as.numeric(pnadc_2019.1$VD4016_def)
pnadc_2019.1$VD3005 <- as.numeric(pnadc_2019.1$VD3005)
pnadc_2019.1$V4029 <- as.numeric(pnadc_2019.1$V4029)
pnadc_2019.1$V4032 <- as.numeric(pnadc_2019.1$V4032)
pnadc_2019.1$V1022 <- as.numeric(pnadc_2019.1$V1022)

#cbo, cod, fazendo equivalencia entre cbo e cod e por fim, adicionando as escolaridades minimas e max.
equivalencia_cbo_cod <- readxl::read_excel("equivalencia_cbo_cod.xlsx")
escolaridade_cbo <- readxl::read_excel("escolaridade_requerida_cbo.xlsx")

pnadc_equivalente_2019.1 <- dplyr::full_join(pnadc_2019.1, equivalencia_cbo_cod, by ="V4010")

pnadc_2019.1_escolaridade <- dplyr::full_join(pnadc_equivalente_2019.1, escolaridade_cbo, by ="cbo")

#recortes na base: idade (18-60), retirada de dirigentes e legisladores
pnadc_com_idade <- subset(pnadc_2019.1_escolaridade, V2009 %in% c(18:60))
pnadc_sem_pesos <- subset(pnadc_com_idade, V4010 %in% c(1120:9629)| is.na(V4010))

## Criando colunas
pnadc_estimacao_2019.1 <- pnadc_sem_pesos %>% mutate(classificacao = ifelse(VD3005 > Maximo, 1, 0),
                                                        mulher = ifelse(V2007 == 2, 1,0),
                                                        negro = ifelse(V2010 %in% c(2,4), 1, 0),
                                                        escolaridade = VD3005,
                                                        idade = V2009,
                                                        carteira = ifelse(V4029 == 1,1,0),
                                                        urbano = ifelse(V1022 == 1,1,0),
                                                        capital = ifelse(V1023 == 1,1,0))
#limpando memória
rm("microdados_pnadc2019.1","pnadc_2019.1","equivalencia_cbo_cod","escolaridade_cbo",
   "pnadc_equivalente_2019.1","pnadc_2019.1_escolaridade","pnadc_com_idade","pnadc_sem_pesos")

########################## Estimando para cada quintil

##################20%
pnadc_2019_renda_20 <- subset(pnadc_estimacao_2019.1, VD4016_def <= 1229.714)

# Probit
probit_2019_renda_20 <- glm(classificacao ~ mulher + negro + escolaridade + idade + carteira + 
                              urbano + capital, family = binomial(link = "probit"), data = pnadc_2019_renda_20)
summary(probit_2019_renda_20)

# McFadden's Pseudo R-squared
probit_2019_renda_20_nulo <- update(probit_2019_renda_20, formula = classificacao ~ 1)

McFadden_2019_renda_20 <- 1-as.vector(logLik(probit_2019_renda_20)/logLik(probit_2019_renda_20_nulo))
McFadden_2019_renda_20

#efeitos marginais
marginais_2019_renda_20 <- mfx::probitmfx(classificacao ~ mulher + negro + escolaridade + idade + carteira + 
                                       urbano + capital, atmean = FALSE, data = pnadc_2019_renda_20)
marginais_2019_renda_20

#para o tamanho das observações
sum(is.na(pnadc_2019_renda_20$classificacao))
str(pnadc_2019_renda_20$classificacao)


###################40%
pnadc_2019_renda_40 <- subset(pnadc_estimacao_2019.1, VD4016_def > 1229.714 & VD4016_def <= 1483.811)

# Probit
probit_2019_renda_40 <- glm(classificacao ~ mulher + negro + escolaridade + idade + carteira + 
                              urbano + capital, family = binomial(link = "probit"), data = pnadc_2019_renda_40)
summary(probit_2019_renda_40)

# McFadden's Pseudo R-squared
probit_2019_renda_40_nulo <- update(probit_2019_renda_40, formula = classificacao ~ 1)

McFadden_2019_renda_40 <- 1-as.vector(logLik(probit_2019_renda_40)/logLik(probit_2019_renda_40_nulo))
McFadden_2019_renda_40

#efeitos marginais
marginais_2019_renda_40 <- mfx::probitmfx(classificacao ~ mulher + negro + escolaridade + idade + carteira + 
                                            urbano + capital, atmean = FALSE, data = pnadc_2019_renda_40)
marginais_2019_renda_40

#para o tamanho das observações
sum(is.na(pnadc_2019_renda_40$classificacao))
str(pnadc_2019_renda_40$classificacao)


##################60%
pnadc_2019_renda_60 <- subset(pnadc_estimacao_2019.1, VD4016_def > 1483.811 & VD4016_def <= 1971.486)

# Probit
probit_2019_renda_60 <- glm(classificacao ~ mulher + negro + escolaridade + idade + carteira + 
                              urbano + capital, family = binomial(link = "probit"), data = pnadc_2019_renda_60)
summary(probit_2019_renda_60)

# McFadden's Pseudo R-squared
probit_2019_renda_60_nulo <- update(probit_2019_renda_60, formula = classificacao ~ 1)

McFadden_2019_renda_60 <- 1-as.vector(logLik(probit_2019_renda_60)/logLik(probit_2019_renda_60_nulo))
McFadden_2019_renda_60

#efeitos marginais
marginais_2019_renda_60 <- mfx::probitmfx(classificacao ~ mulher + negro + escolaridade + idade + carteira + 
                                            urbano + capital, atmean = FALSE, data = pnadc_2019_renda_60)
marginais_2019_renda_60

#para o tamanho das observações
sum(is.na(pnadc_2019_renda_60$classificacao))
str(pnadc_2019_renda_60$classificacao)


##########80%
pnadc_2019_renda_80 <- subset(pnadc_estimacao_2019.1, VD4016_def > 1971.486 & VD4016_def <= 3156.737)

# Probit
probit_2019_renda_80 <- glm(classificacao ~ mulher + negro + escolaridade + idade + carteira + 
                              urbano + capital, family = binomial(link = "probit"), data = pnadc_2019_renda_80)
summary(probit_2019_renda_80)

# McFadden's Pseudo R-squared
probit_2019_renda_80_nulo <- update(probit_2019_renda_80, formula = classificacao ~ 1)

McFadden_2019_renda_80 <- 1-as.vector(logLik(probit_2019_renda_80)/logLik(probit_2019_renda_80_nulo))
McFadden_2019_renda_80

#efeitos marginais
marginais_2019_renda_80 <- mfx::probitmfx(classificacao ~ mulher + negro + escolaridade + idade + carteira + 
                                            urbano + capital, atmean = FALSE, data = pnadc_2019_renda_80)
marginais_2019_renda_80

#para o tamanho das observações
sum(is.na(pnadc_2019_renda_80$classificacao))
str(pnadc_2019_renda_80$classificacao)


############100%
pnadc_2019_renda_100 <- subset(pnadc_estimacao_2019.1, VD4016_def >= 3156.737)

# Probit
probit_2019_renda_100 <- glm(classificacao ~ mulher + negro + escolaridade + idade + carteira + 
                              urbano + capital, family = binomial(link = "probit"), data = pnadc_2019_renda_100)
summary(probit_2019_renda_100)

# McFadden's Pseudo R-squared
probit_2019_renda_100_nulo <- update(probit_2019_renda_100, formula = classificacao ~ 1)

McFadden_2019_renda_100 <- 1-as.vector(logLik(probit_2019_renda_100)/logLik(probit_2019_renda_100_nulo))
McFadden_2019_renda_100

#efeitos marginais
marginais_2019_renda_100 <- mfx::probitmfx(classificacao ~ mulher + negro + escolaridade + idade + carteira + 
                                            urbano + capital, atmean = FALSE, data = pnadc_2019_renda_100)
marginais_2019_renda_100

#para o tamanho das observações
sum(is.na(pnadc_2019_renda_100$classificacao))
str(pnadc_2019_renda_100$classificacao)

