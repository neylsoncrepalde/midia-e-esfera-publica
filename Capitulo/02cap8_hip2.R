#### Dados Cap8
#### Hipótese 2
#### Script 2 - Dados completos
#### Script: Neylson Crepalde
#############################

# Justificação foi codificada apenas para comentários, não para textos
# Acordo e desacordo foi codificado apenas para comentários, não para textos
# Resposta do usuário foi codificado apenas para comentários

# OS USUÁRIOS FAZEM USO DOS ARGUMENTOS AOS QUAIS ELES SÃO EXPOSTOS NOS DIFERENTES AMBIENTES
# (PLATAFORMAS)? Quem vem a público e qual argumento ele mobiliza?

library(MASS)
library(readxl)
library(dplyr)
library(descr)
library(nnet)
library(texreg)
library(party)
dados = read_excel("Banco_Dados_Completo_17-05.xlsx")
names(dados)


# Filtrando apenas os dados relevantes para a modelagem, a saber, COMENTÁRIOS
dados = dados %>% filter(`Comentário Facebook` == 1 | 
                           `Comentário Audiência` == 1 |
                           `Comentário Notícias` == 1)

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
which(dados$`P01-Mixed position` + dados$`P02-Contrario` + dados$`P03-Favoravel` + 
        dados$`P04-Non Identifiable` > 1) # Tem 1
which(dados$`Comentário Audiência` + dados$`Comentário Facebook` +
        dados$`Comentário Notícias` > 1) # OK
which(dados$`Texto plataforma (audiência)` + dados$`Texto plataforma (FB)` +
        dados$`Texto plataforma (Notícias)` > 1) #OK
which(dados$`D01-Bold` + dados$`D02-Soft` + dados$`D03-Absence` > 1) # Tem 4
which(dados$`A01-Presence` + dados$`A02-Absence` > 1) #OK
which(dados$`T01-User responds or reacts explicitly to the content of the post` +
        dados$`T02-User responds to previous speaker` + 
        dados$`T03-Not-addressing comment` > 1) # Tem 2
which(dados$`J01-Complexa` + dados$`J02-Simples` + dados$`J03-Opiniao` > 1) # Tem 1
which(dados$`G01-Male` + dados$`G02-Female` + dados$`G03-Non Identifiable` > 1) # Tem 1
which(dados$`R01-Relevant` + dados$`R02-Non-relevant` > 1) # Tem 3

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

# Estimando a mesma regressão com as variaveis originais do banco
reg_log_multi2_complexa = glm(`J01-Complexa` ~ 
                              `D01-Bold` + `D02-Soft` +
                              `P01-Mixed position` + `P02-Contrario` + 
                              `P03-Favoravel`+
                              `T01-User responds or reacts explicitly to the content of the post` +
                              `T02-User responds to previous speaker` +
                              `T03-Not-addressing comment` +
                              `Comentário Facebook` + `Comentário Audiência`+ `Comentário Notícias`,
                               data = dados, family = binomial(link = 'logit'))
res_complexa = summary(reg_log_multi2_complexa)
betas_complexa = (exp(coef(reg_log_multi2_complexa)) - 1) * 100
cbind(betas_complexa, res_complexa$coefficients[,4])

reg_log_multi2_simples = glm(`J02-Simples` ~ 
                                `D01-Bold` + `D02-Soft` +
                                `P01-Mixed position` + `P02-Contrario` + 
                                `P03-Favoravel`+
                                `T01-User responds or reacts explicitly to the content of the post` +
                                `T02-User responds to previous speaker` +
                                `T03-Not-addressing comment` +
                                `Comentário Facebook` + `Comentário Audiência`+ `Comentário Notícias`,
                              data = dados, family = binomial(link = 'logit'))
res_simples = summary(reg_log_multi2_simples)
betas_simples = (exp(coef(reg_log_multi2_simples)) - 1) * 100
cbind(betas_simples, res_simples$coefficients[,4])

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

res = summary(reg_log_multi2)
toplot = cbind(t(res$coefficients[,-1]), t(res$standard.errors[,-1]))
toplot = as.data.frame(toplot)
names(toplot) = c("Complex", "Simple", "Complex - SE", "Simple - SE")
reshape2::melt(toplot)
res_complex = toplot[1]
names(res_complex)[1] = "Coef"
res_complex$up = res_complex[,1] + toplot[,3]
res_complex$down = res_complex[,1] - toplot[,3]
res_complex$name = "Complex"
res_complex$var = factor(c('Disagreement - bold', 'Disagreement - soft', 
                           'Sex - M', 'Positioning - Favorable', 'Positioning - Mixed', 
                           'Answer - Not addressing comment', 'Answer - Previous Speaker',
                           'Platform - Hearing comment', 'Platform - News comment'),
                         labels = c('Disagreement - bold', 'Disagreement - soft', 
                                    'Sex - M', 'Positioning - Favorable', 'Positioning - Mixed', 
                                    'Answer - Not addressing comment', 'Answer - Previous Speaker',
                                    'Platform - Hearing comment', 'Platform - News comment'))

res_simple = toplot[2]
names(res_simple)[1] = "Coef"
res_simple$up = res_simple[,1] + toplot[,4]
res_simple$down = res_simple[,1] - toplot[,4]
res_simple$name = "Simple"
res_simple$var = factor(c('Disagreement - bold', 'Disagreement - soft', 
                    'Sex - M', 'Positioning - Favorable', 'Positioning - Mixed', 
                    'Answer - Not addressing comment', 'Answer - Previous Speaker',
                    'Platform - Hearing comment', 'Platform - News comment'),
                    labels = c('Disagreement - bold', 'Disagreement - soft', 
                               'Sex - M', 'Positioning - Favorable', 'Positioning - Mixed', 
                               'Answer - Not addressing comment', 'Answer - Previous Speaker',
                               'Platform - Hearing comment', 'Platform - News comment'))


res_completo = rbind(res_complex, res_simple)
class(res_completo$var)

library(ggplot2)
library(forcats)
res_completo %>% group_by(name) %>% 
  ggplot(aes(x = fct_rev(var), y = Coef, fill = name, label = round(Coef, 2))) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=down, ymax=up, colour = name), 
                lwd=1, width=0, alpha = .5) +
  geom_point(size=4, pch=21, alpha = .6) +
  coord_flip() +
  theme_bw(base_size = 14) + 
  theme(plot.title=element_text(size=18)) + 
  # Título do gráfico e títulos de x e y
  labs(y='', x='', color='', title = 'Estimates')+
  scale_colour_discrete(guide = F) +
  scale_fill_discrete("Model")
ggsave("reg_multi2_coef_plot.png", height = 5, width = 8, dpi=100)

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
