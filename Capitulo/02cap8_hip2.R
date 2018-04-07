#### Dados Cap8
#### Hipótese 2
#### Script 2 - Dados completos
#### Script: Neylson Crepalde
#############################

library(MASS)
library(readxl)
library(dplyr)
library(descr)
library(nnet)
library(texreg)
library(party)
dados = read_excel("Artigo_Sitema_Banco_dedos_Completo.xlsx")
names(dados)

#----------------------------------------------
### HIPÓTESE 2 - Haverá maior ocorrência de justificação onde tem maior 
### incidência de desacordo
#----------------------------

# Duas abordagens:
# 1) Analisar os dados como estão. Y = Complexa
# 2) Criar uma variável numérica Justificação - 
#    Complexa = 2, simples = 1, opinião = 0

#-------------------------------------
# Testando a integridade das variáveis
2 %in% (dados$`P01-Mixed position` + dados$`P02-Contrario`) #OK
2 %in% (dados$`P01-Mixed position` + dados$`P02-Contrario`) #OK


#----------------------------
# Abordagem 1 - Dados como estão. Y = Just. Complexa [20]

# Testando apenas a hipótese

jus_des_total = dados %>% select(1:10, 12:14, 17:24) %>%
  mutate(desacordo = case_when(
    `D01-Bold` == 1 ~ 'bold',
    `D02-Soft` == 1 ~ 'soft',
    `D03-Absence` == 1 ~ 'absence'
  )) %>%
  mutate(justificacao = case_when(
    `J01-Complexa` == 1 ~ 'Complex',
    `J02-Simples` == 1 ~ 'Simple',
    `J03-Opiniao` == 1 ~ 'Opinion'
  )) %>% 
  mutate(desac_num = case_when(
    `D01-Bold` == 1 ~ 2,
    `D02-Soft` == 1 ~ 1,
    `D03-Absence` == 1 ~ 0
  )) %>%
  mutate(just_num = case_when(
    `J01-Complexa` == 1 ~ 2,
    `J02-Simples` == 1 ~ 1,
    `J03-Opiniao` == 1 ~ 0
  )) %>%
  mutate(sexo = case_when(
    `G01-Male` == 1 ~ 'M',
    `G02-Female` == 1 ~ 'F'
  )) %>%
  mutate(posicionamento = case_when(
    `P01-Mixed position` == 1 ~ 'mixed',
    `P02-Contrario` == 1 ~ 'contrario',
    `P03-Favoravel` == 1 ~ 'favoravel'
  )) %>%
  mutate(resposta = case_when(
    `T01-User responds or reacts explicitly to the content of the post` == 1 ~ 'content',
    `T02-User responds to previous speaker` == 1 ~ 'prev_speaker',
    `T03-Not-addressing comment` == 1 ~ 'not_addressing_comment'
  )) %>%
  mutate(plat_comment = case_when(
    `Comentário Facebook` == 1 ~ 'Facebook comment',
    `Comentário Audiência` == 1 ~ 'Hearing comment',
    `Comentário Notícias` == 1 ~ 'News comment'
  )) %>%
  mutate(plataforma = case_when(
    `Texto plataforma (FB)` == 1 ~ 'Facebook',
    `Texto plataforma (audiência)` == 1 ~ 'Hearing',
    `Texto plataforma (Notícias)` == 1 ~ 'News'
  ))

freq(jus_des_total$desacordo, plot=F) # Divergência de 4 casos em absence
freq(jus_des_total$`D03-Absence`, plot = F)

freq(jus_des_total$justificacao, plot = F) # Divergência de 1 caso em opiniao
freq(jus_des_total$`J03-Opiniao`, plot = F)

freq(jus_des_total$plat_comment, plot = F)
freq(jus_des_total$`Comentário Notícias`, plot = F) # OK

freq(jus_des_total$plataforma, plot = F)
freq(jus_des_total$`Texto plataforma (Notícias)`, plot = F) # OK. Só tem audiências

#########################
# Testando a relação entre a justificacao e o desacordo
table(jus_des_total$justificacao, jus_des_total$desacordo)
summary(table(jus_des_total$justificacao, jus_des_total$desacordo)) # São dependentes

# Fazendo um teste de correlação
cor.test(jus_des_total$just_num, jus_des_total$desac_num) # Não sig. Corr baixíssima.

#--------------------------------
# Fazendo uma reg log multinomial
reg_log_multi1 = multinom(relevel(factor(justificacao), 'Opinion') ~ desacordo, 
                          data = jus_des_total)
summary(reg_log_multi1)
z <- summary(reg_log_multi1)$coefficients/summary(reg_log_multi1)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
p     # Atenção aos p-valores

# Transformando em porcentagens de chances
betas = (exp(coef(reg_log_multi1)) - 1) * 100

#--------------------------------
# Fazendo uma reg log multinomial mais complexa
# Multinomial com desacordo nominal
reg_log_multi2 = multinom(relevel(factor(justificacao), 'Opinion') ~ desacordo + 
                            sexo + posicionamento + resposta + plat_comment, 
                          data = jus_des_total)
summary(reg_log_multi2)
z <- summary(reg_log_multi2)$coefficients/summary(reg_log_multi2)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
p     # Atenção aos p-valores

# Transformando em porcentagens de chances
betas = (exp(coef(reg_log_multi2)) - 1) * 100
cbind(t(betas), t(p))

# Multinomial com desacordo ordinal
reg_log_multi3 = multinom(relevel(factor(justificacao), 'Opinion') ~ desac_num + 
                            sexo + posicionamento + resposta + plat_comment, 
                          data = jus_des_total)
summary(reg_log_multi3)
z <- summary(reg_log_multi3)$coefficients/summary(reg_log_multi3)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
p     # Atenção aos p-valores

# Transformando em porcentagens de chances
betas = (exp(coef(reg_log_multi3)) - 1) * 100
cbind(t(betas), t(p))


# Exibindo resultados para as duas regressões
screenreg(list(reg_log_multi2,reg_log_multi3))
htmlreg(list(reg_log_multi2,reg_log_multi3),
        custom.coef.names = c('Intercept','Disagreement - bold', 'Disagreement - soft', 
                              'Sex - M', 'Positioning - Favorable', 'Positioning - Mixed', 
                              'Answer - Not addressing comment', 'Answer - Previous Speaker',
                              'Platform - Hearing comment', 'Platform - News comment',
                              'Disagreement (ordinal)'),
        caption = 'Multinomial Logistic Regression Models',
        caption.above = T,
        file = 'res_log_mult.html')


##################################################
#-------------------------------------------------
# Regressão logística ordinal

# Com desacordo categórico nominal
log_or = polr(factor(justificacao, levels = c('Opinion','Simple','Complex')) ~ 
                desacordo + sexo + posicionamento + resposta + plat_comment,
              data = jus_des_total, Hess = T)
summary(log_or)

ctable <- coef(summary(log_or))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable

betas = (exp(coef(log_or)) - 1) * 100
cbind(betas, p)

# Com desacordo categórico ordinal
log_or2 = polr(factor(justificacao, levels = c('Opinion','Simple','Complex')) ~ 
                desac_num + sexo + posicionamento + resposta + plat_comment,
              data = jus_des_total, Hess = T)
summary(log_or2)

ctable <- coef(summary(log_or2))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable

betas = (exp(coef(log_or2)) - 1) * 100
cbind(betas, p)


screenreg(list(log_or, log_or2), custom.model.names = c('Log Ord 1', 'Log Ord 2'))
htmlreg(list(log_or, log_or2), file = 'res_log_ords.html',
        custom.coef.names = c('Disagreement - bold', 'Disagreement - soft', 'Sex - M', 'Positioning - Favorable',
                              'Positioning - Mixed', 'Answer - Not addressing comment', 'Answer - Previous Speaker', 
                              'Platform - Hearing comment', 'Platform - News comment',
                              'Disagreement (ordinal)'),
        caption = "Ordinal Logistic Regression Models", caption.above = T)

##################################################
#-------------------------------------------------
# Decision tree
# Criando uma justificacao binaria
jus_des_subset = subset(jus_des_total, !is.na(justificacao))
dec_tree = ctree(factor(justificacao, levels = c('Opinion','Simple','Complex')) ~ 
                   desac_num + factor(sexo) + 
                   factor(posicionamento) + factor(resposta),
                 data = jus_des_subset)

plot(dec_tree, main="Classification Tree for Justification")
