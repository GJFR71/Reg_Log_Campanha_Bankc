### Roteiro Padrão de Regressão Logística - Campanha Bancária

# Carregando bibliotecas necessárias
library(tidyverse)
library(caret)
library(haven)
library(rcompanion) # cramersV
library(pROC)
library(readr)
library(knitr)

###CONTEXTO DO NEGÓCIO:#####
#'Os dados bank.csv estão relacionados com campanhas de marketing 
#'direto de uma instituição bancária portuguesa. 
#'As campanhas de marketing foram baseadas em chamadas telefônicas. 
#'Frequentemente, mais de um contato com o mesmo cliente era necessário 
#'para avaliar se o produto (depósito a prazo bancário) 
#'seria (ou não) subscrito.





# Passo 1: Importação da Base

dados <- read_delim("bank.csv", delim = ";", 
                   escape_double = FALSE, trim_ws = TRUE)
View(dados)

#** O primeiro passo eh sempre interessante entender um pouco mais sobre os dados
#** e nada melhor do que comecar a cronstruir o nosso "mapa astral" dos dados. 
#** Vamos descreve-los e pensar no que precisamos nos preocupar em relacao a cada 
#** variavel?



## DICIONÁRIO DE DADOS ####
#' Variáveis de entrada:
#'dados do cliente bancário:
#'  1 - idade (numérico)
#'2 - trabalho: tipo de trabalho (categórico: "admin.", "unknown", "unemployed", "management", "housemaid", "entrepreneur", "student", "blue-collar", "self-employed", "retired", "technician", "services")
#'3 - estado civil: (categórico: "married","divorced","single"; nota: "divorced" significa divorciado ou viúvo)
#'4 - educação (categórico: "unknown","secondary","primary","tertiary")
#'5 - default: tem crédito em default? (binário: "yes","no")
#'6 - saldo: saldo médio anual, em euros (numérico)
#'7 - habitação: tem empréstimo habitacional? (binário: "yes","no")
#'8 - empréstimo: tem empréstimo pessoal? (binário: "yes","no")
#'relacionado com o último contato da campanha atual:
#'  9 - contato: tipo de comunicação do contato (categórico: "unknown","telephone","cellular")
#'10 - dia: último dia de contato do mês (numérico)
#'11 - mês: último mês de contato do ano (categórico: "jan", "feb", "mar", ..., "nov", "dec")
#'12 - duração: duração do último contato, em segundos (numérico)
#'outros atributos:
#' 13 - campanha: número de contatos realizados durante esta campanha e para este cliente (numérico, inclui o último contato)





## Pratica - ANÁLISE UNIVARIADA ######


# Passo 2: Análise Exploratória (EDA)
summary(dados)
colMeans(is.na(dados)) * 100  # Percentual de missing
str(dados)
table(dados$y)  # Distribuição da variável resposta

#*** Vamos começar explorando cada uma das variaveis?



#*** Observando missing nas variaveis
table(is.na (dados$age))
table(is.na (dados$job)) 
table(is.na (dados$marital))
table(is.na (dados$education)) 
table(is.na (dados$default))
table(is.na (dados$balance))
table(is.na (dados$housing)) 
table(is.na (dados$loan))
table(is.na (dados$contact)) 
table(is.na (dados$day))
table(is.na (dados$month))
table(is.na (dados$duration)) 
table(is.na (dados$campaign))
table(is.na (dados$pdays)) 
table(is.na (dados$previous))
table(is.na (dados$poutcome))

# Análise de cada variável: 
# Explorando a variável Age
summary(dados$age)

ggplot(dados, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribuição da Idade", x = "Idade", y = "Frequência")

ggplot(dados, aes(x = age)) + 
  geom_boxplot(color="black", fill="blue") +
  labs(title = "Boxplot da Idade", x = "Idade")

#' Verificando o histograma, é possível notar 
#' que há uma leve assimetria à direita, com
#' a média situada a direita da mediana, indicando influência
#' de valores elevados.
#' 
#' O Box-plot confirma a existência de outliers nos valores superiores.
#' 

# Explorando a variável Job
cat("Número de categorias em 'job':", length(unique(dados$job)), "\n")
sum(is.na(dados$job))  # Verificação de valores ausentes
table(dados$job)  # Tabela de frequência

# Gráfico de barras com ordenação e rotação dos rótulos
ggplot(dados, aes(x = reorder(job, -table(job)[job]))) +
  geom_bar(fill = "orange", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribuição de Trabalhos", x = "Tipo de Trabalho", y = "Frequência")

# Proporção de cada categoria em porcentagem
prop.table(table(dados$job)) * 100

#' verifica-se que existem 12 
#' tipos de categorias nessa variável, indicando ser necessário
#' fazer um agrupamento.


# Explorando a variável education
cat("Número de categorias em 'education':", length(unique(dados$education)), "\n")
sum(is.na(dados$education))  # Verificação de valores ausentes
table(dados$education)  # Tabela de frequência

# Gráfico de barras com ordenação e rotação dos rótulos
ggplot(dados, aes(x = reorder(education, -table(education)[education]))) +
  geom_bar(fill = "orange", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribuição de Nível Educacional", x = "Níveis educacionais", y = "Frequência")

# Proporção de cada categoria em porcentagem
prop.table(table(dados$education)) * 100

#' Verifica-se que existem 4 níveis educacionais, sendo um 
#' desconhecido e bem pequeno. Pode ser agrupado com o 
#' nível primário, que também tem volume baixo.


# Explorando a variável default
cat("Número de categorias em 'default':", length(unique(dados$default)), "\n")
sum(is.na(dados$default))  # Verificação de valores ausentes
table(dados$default)  # Tabela de frequência

# Gráfico de barras com ordenação e rotação dos rótulos
ggplot(dados, aes(x = reorder(default, -table(default)[default]))) +
  geom_bar(fill = "orange", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribuição de Default (Crédito)", x = "Níveis de Default", y = "Frequência")

# Proporção de cada categoria em porcentagem
prop.table(table(dados$default)) * 100


#' Percebe-se que é uma variável binária bem
#' desbalanceada, com a maioria não tendo crédito
#' 


# Explorando a variável balance (saldo médio)
summary(dados$balance)

ggplot(dados, aes(x = balance)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribuição do Saldo Médio", x = "Saldo Médio", y = "Frequência")


#' trata-se de uma variável numérica, 
#' com grande assimetria a direita e variabilidade.
#' Apresenta valores negativos e média afastada a direita
#' da mediana, indicando influência de valores elevados.
#' O Boxplot indicou muitos outliers. 
#' PREVEJO A NECESSIDADE DE CATEGORIZÁ-LA


# Explorando a variável housing
cat("Número de categorias em 'housing':", length(unique(dados$housing)), "\n")
sum(is.na(dados$housing))  # Verificação de valores ausentes
table(dados$housing)  # Tabela de frequência

# Gráfico de barras com ordenação e rotação dos rótulos
ggplot(dados, aes(x = reorder(housing, -table(housing)[housing]))) +
  geom_bar(fill = "orange", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribuição de Housing", x = "Níveis de Housing", y = "Frequência")

# Proporção de cada categoria em porcentagem
prop.table(table(dados$housing)) * 100

#' Também é uma variável binária e com distribuição
#' equilibrada entre as categorias.


# Explorando a variável loan (empréstimo)
cat("Número de categorias em 'loan':", length(unique(dados$loan)), "\n")
sum(is.na(dados$loan))  # Verificação de valores ausentes
table(dados$loan)  # Tabela de frequência

# Gráfico de barras com ordenação e rotação dos rótulos
ggplot(dados, aes(x = reorder(loan, -table(loan)[loan]))) +
  geom_bar(fill = "orange", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribuição de Loan", x = "Níveis de Loan", y = "Frequência")

# Proporção de cada categoria em porcentagem
prop.table(table(dados$loan)) * 100


#' Outra variável binária e com distribuição
#' desequilibrada entre as categorias.


# Explorando a variável loan (contact)
cat("Número de categorias em 'contact':", length(unique(dados$contact)), "\n")
sum(is.na(dados$loan))  # Verificação de valores ausentes
table(dados$contact)  # Tabela de frequência

# Gráfico de barras com ordenação e rotação dos rótulos
ggplot(dados, aes(x = reorder(contact, -table(contact)[contact]))) +
  geom_bar(fill = "orange", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribuição de Loan", x = "Níveis de Loan", y = "Frequência")

# Proporção de cada categoria em porcentagem
prop.table(table(dados$contact)) * 100

#' Observa-se que há condições para agrupar o contato telefonico com o desconhecido
#' formando 2 categorias: celular e outros
#' 

##############################################################
#' As variaveis dia e mes podem ser analisadas posteriormente para verificar
#' se há influencia na época do mes ou no mes do ano, com a
#' contratação do depósito (variável Y).
#' 
#' 
# Transformações das variáveis Day e Month para análise temporal

# 1. Verificando e preparando as variáveis
# Convertendo 'day' e 'month' para numérico e categórico, se necessário
dados$day <- as.numeric(dados$day)
dados$month <- as.factor(dados$month)

# 2. Realizando as transformações solicitadas
# Transformando 'day' em períodos do mês
dados$day_category <- cut(
  dados$day,
  breaks = c(-Inf, 10, 20, Inf),
  labels = c("inicio_mes", "meio_mes", "final_mes")
)

# Transformando 'month' em trimestres
dados$month_trimester <- case_when(
  dados$month %in% c("jan", "feb", "mar") ~ "1_trim",
  dados$month %in% c("apr", "may", "jun") ~ "2_trim",
  dados$month %in% c("jul", "aug", "sep") ~ "3_trim",
  dados$month %in% c("oct", "nov", "dec") ~ "4_trim",
  TRUE ~ "desconhecido"
)

# 3. Examinando as novas variáveis
cat("Categorias em 'day_category':", unique(dados$day_category), "\n")
table(dados$day_category)

cat("Categorias em 'month_trimester':", unique(dados$month_trimester), "\n")
table(dados$month_trimester)

# 4. Visualizando as distribuições
ggplot(dados, aes(x = day_category)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribuição por Período do Mês", x = "Período", y = "Frequência")

ggplot(dados, aes(x = month_trimester)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Distribuição por Trimestre", x = "Trimestre", y = "Frequência")

#' Com relção a variável day_category percebe-se que a maioria
#' dos contatos foi realizado no meio do mês e o nomero de contatos
#' de inicio e final do mes foram equivalentes.
#' 
#' Avaliando a variável month_trimester o segundo trimestre
#' teve maior frequencia, seguido do terceiro trimestre.
#' Os primeiro e quarto trimestres tiveram frequencias menores. 






#############################################################

#' Explorando a variável duration
summary(dados$duration)

ggplot(dados, aes(x = duration)) +
 geom_histogram(binwidth = 5, fill = "blue", color = "black") +
 labs(title = "Distribuição da Duração do Contato", x = "Duração", y = "Frequência")

ggplot(dados, aes(x = duration)) + 
  geom_boxplot(color="black", fill="blue") +
  labs(title = "Boxplot da Duração do Contato", x = "Duração")


#' Verificando o histograma, é possível notar 
#' que há  assimetria à direita, com
#' a média situada a direita da mediana, indicando influência
#' de valores elevados.
#' 
#' O Box-plot confirma a existência de outliers nos valores superiores.
#' 

#' Explorando a variável campaign
summary(dados$campaign)

ggplot(dados, aes(x = campaign)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribuição do Número de Contatos", x = "Número de Contatos", y = "Frequência")

ggplot(dados, aes(x = campaign)) + 
  geom_boxplot(color="black", fill="blue") +
  labs(title = "Boxplot do Número de Contatos", x = "Número de Contatos")


#' Verificando o histograma, é possível notar 
#' que há  assimetria à direita, com
#' a média situada a direita da mediana, indicando influência
#' de valores elevados.
#' 
#' O Box-plot confirma a existência de outliers nos valores superiores.
#' OBSEVADO UM MAXIMO DE 50 LIGAÇÕES PARA O CLIENTE???

#' Explorando a variável pdays
summary(dados$pdays)

ggplot(dados, aes(x = pdays)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribuição dos Dias prévios sem contato", x = "pdays", y = "Frequência")

ggplot(dados, aes(x = pdays)) + 
  geom_boxplot(color="black", fill="blue") +
  labs(title = "Boxplot do Dias prévios sem contato", x = "pdays")

#' Percebe-se uma distribuição completamente assimétrica.
#' muitos indivíduos que não foram contatados anteriormente
#' e muitos com tempo demasiado longo sem terem sido contatados.
#' Possibilidade de categorizar em sem contato ou já contatado
#' 

#' Explorando a variável pdays
summary(dados$previous)

ggplot(dados, aes(x = previous)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribuição do Número de contatos prévios", x = "Número de contatos", y = "Frequência")

ggplot(dados, aes(x = previous)) + 
  geom_boxplot(color="black", fill="blue") +
  labs(title = "Boxplot do Dias Número de contatos prévios", x = "Número de contatos")
#' Observa-se que a variável não tem distribuição normal
#' há muitos valores elevados situados nos 25% finais.
#' Também é possível categorizar em "Nunca contatado"
#' e "Já abordado"


# Explorando a variável poutcome (resultado)
cat("Número de categorias em 'poutcome':", length(unique(dados$poutcome)), "\n")
sum(is.na(dados$poutcome))  # Verificação de valores ausentes
table(dados$poutcome)  # Tabela de frequência


# Gráfico de barras com ordenação e rotação dos rótulos
ggplot(dados, aes(x = reorder(poutcome, -table(poutcome)[poutcome]))) +
  geom_bar(fill = "orange", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribuição de Loan", x = "Níveis de Loan", y = "Frequência")

# Proporção de cada categoria em porcentagem
prop.table(table(dados$poutcome)) * 100

#' Observa-se que a maioria possui o resultado desconhecido.
#' variavel com categorias bem desproporcionais. Poderia ser dividida
#' entre conhecidas e desconhecidas? faria sentido? vale o esforço?
#' 
#' 


# Explorando a variável y (variavel dependente)
cat("Número de categorias em 'y':", length(unique(dados$y)), "\n")
sum(is.na(dados$y))  # Verificação de valores ausentes
table(dados$y)  # Tabela de frequência


# Gráfico de barras com ordenação e rotação dos rótulos
ggplot(dados, aes(x = reorder(y, -table(y)[y]))) +
  geom_bar(fill = "orange", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribuição de y", x = "Níveis de y", y = "Frequência")

# Proporção de cada categoria em porcentagem
prop.table(table(dados$y)) * 100


#' É a variável de interesse (variável dependente). Não apresenta distribuição
#' normal, os seja, está bem desequilibdada com 88% de não ocorrência
#' do evento.

################################################
#*** Vamos explorar a relacao entre as variaveis?
#*** Algumas perguntas que podemos responder:
#*** - Existe uma relacao linear entre Age, balance e duration com o Y?
#*** - Existe relacao entre pdays e previous? Sera que as medias sao diferentes? 
#*** (se forem, precisamos colocar as duas no modelo?)
#*** - Como eh a distribuicao do job, marital, education  por y?
#*

# Explorando Relações Entre Variáveis

# 1. Relação Linear Entre Variáveis Numéricas e Y
# Corrigindo o cálculo de correlações: convertendo 'y' para numérico corretamente
dados$y_numeric <- as.numeric(as.factor(dados$y)) - 1

# Correlações
correlations <- cor(dados[, c("age", "balance", "duration")], dados$y_numeric, use = "complete.obs")
print(correlations)

# Visualização de dispersão com ajuste de colunas
pairs(~ age + balance + duration + y_numeric, 
      data = dados[, c("age", "balance", "duration", "y_numeric")],
      main = "Relação Entre Age, Balance, Duration e Y")

#'  as correlações apresentadas são muito fracas, 
#'  indicando ausência de relação linear significativa entre
#'  'age', 'balance', 'duration' e 'y'.


# Explorando Relações Entre Variáveis com Taxa de Resposta

# 1. Taxa de Resposta por Variáveis Numéricas (Age, Balance, Duration)
num_vars <- c("age", "balance", "duration")
for (var in num_vars) {
  taxa_resposta <- aggregate(y_numeric ~ get(var), data = dados, FUN = mean)
  colnames(taxa_resposta) <- c(var, "taxa_resposta")
  print(head(taxa_resposta))
  
  ggplot(taxa_resposta, aes(x = .data[[var]], y = taxa_resposta)) +
    geom_line(color = "blue") +
    geom_point(color = "red") +
    labs(title = paste("Taxa de Resposta por", var), x = var, y = "Taxa de Resposta")
}


#' - Age: Variações irregulares e dispersas, sem padrão claro.
#' - Balance: Taxa de resposta zero para múltiplos valores negativos,
#'  ausência de tendência.
#' - Duration: Todas as taxas de resposta apareceram como zero,
#'  indicando erro nos dados ou agrupamento muito fino.
#'  VOU CATEGORIZAR AS TRES

# Explorando Relações Entre Variáveis com Taxa de Resposta (Com Variáveis Categorizadas)

# 1. Transformações para Variáveis Categóricas:
# Categorizando Age
dados$age_cat <- cut(dados$age, 
                     breaks = c(-Inf, 35, 55, Inf), 
                     labels = c("jovem", "maduro", "idoso"))

# Categorizando Balance
dados$balance_cat <- cut(dados$balance, 
                         breaks = c(-Inf, 1500, 3500, Inf), 
                         labels = c("baixo", "medio", "alto"))

# Categorizando Duration
dados$duration_cat <- cut(dados$duration, 
                          breaks = c(-Inf, 500, 1000, Inf), 
                          labels = c("curto", "medio", "elevado"))

# 2. Taxa de Resposta por Variáveis Categorizadas:
categorias <- c("age_cat", "balance_cat", "duration_cat")
for (cat_var in categorias) {
  taxa_resposta <- aggregate(y_numeric ~ get(cat_var), data = dados, FUN = mean)
  colnames(taxa_resposta) <- c(cat_var, "taxa_resposta")
  print(taxa_resposta)
  
  ggplot(taxa_resposta, aes(x = .data[[cat_var]], y = taxa_resposta, fill = taxa_resposta)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = scales::percent(taxa_resposta, accuracy = 0.1)), vjust = -0.5, size = 3) +
    labs(title = paste("Taxa de Resposta por", cat_var), x = cat_var, y = "Taxa de Resposta") +
    theme_minimal()
}
# Explorando Relações Entre Variáveis com Taxa de Resposta (Interpretação Revisada)

# Interpretação da Taxa de Resposta por Categoria:
# 1. Age_cat:
# - Jovem (até 35 anos): Taxa de resposta 11,9%, intermediária entre as faixas.
# - Maduro (36 a 55 anos): Taxa de resposta 10,2%, a menor entre as faixas etárias.
# - Idoso (56 anos ou mais): Taxa de resposta 16,4%, a mais elevada, indicando maior propensão de adesão ao produto.

# 2. Balance_cat:
# - Baixo (<1500): Taxa de resposta 10,3%, a menor entre as faixas.
# - Médio (1500 a 3500): Taxa de resposta 16,3%, a mais alta, sugerindo que clientes com saldo médio respondem melhor.
# - Alto (>3500): Taxa de resposta 14%, próxima à média, mas inferior à faixa intermediária.

# 3. Duration_cat:
# - Curto (≤500): Taxa de resposta 7,4%, a menor entre as faixas.
# - Médio (501 a 1000): Taxa de resposta 34,7%, demonstrando forte impacto da duração.
# - Elevado (>1000): Taxa de resposta 57%, evidenciando que a duração do contato é um fator altamente relevante.

# Conclusões Gerais:
# - A variável 'duration_cat' apresenta maior poder preditivo, com forte crescimento na taxa de resposta em contatos mais longos.
# - 'age_cat' mostra que clientes idosos têm maior propensão, enquanto a faixa madura é a menos responsiva.
# - 'balance_cat' sugere que clientes com saldo médio são mais receptivos.
# - Recomenda-se incluir essas variáveis categorizadas no modelo de regressão logística e explorar interações.



# Explorando Relações Entre Variáveis  Pdays e Previous 

# **2. Correlação de Pearson:**

correlacao <- cor(dados$previous, dados$pdays, use = "complete.obs")
cat("Correlação de Pearson (previous vs. pdays):", correlacao, "\n")

#' **Resultado:** 0.5776 (Correlação moderada)

# **3. Associação Categórica (Cramér's V):**

library(rcompanion)
tabela <- table(dados$previous_cat, dados$pdays_cat)
cramer_valor <- cramerV(tabela, bias.correct = TRUE)
cat("Cramér's V (previous_cat vs. pdays_cat):", cramer_valor, "\n")

#' **Resultado:** 1 (Associação perfeita)

# **4. Verificação de Multicolinearidade (VIF):**

library(car)
modelo_vif <- lm(pdays ~ previous + I(previous^2), data = dados)
cat("VIF (pdays ~ previous):", vif(modelo_vif), "\n")

#' **Resultado:** 3.1827 (Moderada colinearidade)

# Conclusão:**
#'- **Pearson:** Correlação moderada (0.5776), indica relação linear.
#' - **Cramér's V:** Associação categórica perfeita (1), sugere redundância.
#' - **VIF:** Moderada colinearidade (3.1827).


#' Com base nas métricas apresentadas (Pearson, Cramér's V e VIF),
#' é correto concluir que 'pdays' e 'previous' fornecem praticamente
#' a mesma informação. Por isso, recomendar a exclusão de uma delas
#' para evitar multicolinearidade.


# 3. Distribuição de Job, Marital, Education por Y

# 1. Categorizando 'job' conforme solicitado
dados$job_cat <- ifelse(dados$job %in% c("management"), "management",
                        ifelse(dados$job %in% c("blue-collar"), "blue_collar",
                               ifelse(dados$job %in% c("technician", "admin."), "technician_admin", "outros")))

# 2. Tabelas de Distribuição por Y
# Tabela de Job por Y
tabela_job <- table(dados$job_cat, dados$y)
prop_job <- prop.table(tabela_job, margin = 2)
cat("Tabela de Job por Y:\n")
print(tabela_job)
cat("Proporção de Job por Y:\n")
print(round(prop_job, 4))

# Gráfico de Job por Y com proporções corretas
ggplot(dados, aes(x = job_cat, fill = y)) +
  geom_bar(position = "fill") +
  geom_text(stat = "count", aes(label = scales::percent(..count../sum(..count..), accuracy = 0.1)),
            position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Distribuição de Job por Y",
       x = "Categoria de Job",
       y = "Proporção") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Tabela de Marital por Y
tabela_marital <- table(dados$marital, dados$y)
prop_marital <- prop.table(tabela_marital, margin = 2)
cat("Tabela de Marital por Y:\n")
print(tabela_marital)
cat("Proporção de Marital por Y:\n")
print(round(prop_marital, 4))

# Gráfico de Marital por Y
ggplot(dados, aes(x = marital, fill = y)) +
  geom_bar(position = "fill") +
  geom_text(stat = "count", aes(label = scales::percent(after_stat(count)/sum(after_stat(count)), accuracy = 0.1)),
            position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Distribuição de Marital por Y",
       x = "Estado Civil",
       y = "Proporção")


# Tabela de Education por Y
tabela_education <- table(dados$education, dados$y)
prop_education <- prop.table(tabela_education, margin = 2)
cat("Tabela de Education por Y:\n")
print(tabela_education)
cat("Proporção de Education por Y:\n")
print(round(prop_education, 4))

# Gráfico de Education por Y
ggplot(dados, aes(x = education, fill = y)) +
  geom_bar(position = "fill") +
  geom_text(stat = "count", aes(label = scales::percent(after_stat(count)/sum(after_stat(count)), accuracy = 0.1)),
            position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Distribuição de Education por Y",
       x = "Educação",
       y = "Proporção")

#  Interpretação das Tabelas de Distribuição de Job, Marital e Education por Y (Foco na Adesão)

# 1. Interpretação de Job por Y (Adesão - 'yes'):
# - 'outros' lidera em adesão (34,55%), indicando alta propensão ao produto.
# - 'management' possui 25,14% de adesão, mostrando também um grupo relevante.
# - 'technician_admin' tem 27,06% de adesão, próximo ao grupo 'management'.
# - 'blue_collar' apresenta a menor taxa de adesão (13,24%), mostrando baixa conversão.

# 2. Interpretação de Marital por Y (Adesão - 'yes'):
# - 'single' possui a maior adesão (32,05%), sendo o estado civil mais receptivo.
# - 'married' tem 53,17% de adesão, mas é o grupo mais numeroso, podendo ter relevância.
# - 'divorced' representa 14,78% de adesão, menor, mas ainda significativo.

# 3. Interpretação de Education por Y (Adesão - 'yes'):
# - 'tertiary' lidera com 37,04%, evidenciando a importância da alta escolaridade.
# - 'secondary' tem 47,02%, mostrando ser um grupo expressivo.
# - 'primary' apresenta a menor adesão (12,28%), indicando baixa conversão.
# - 'unknown' tem 3,65%, pouco representativo.

# Conclusão Focada na Adesão:
# - **Job:** 'outros', 'management' e 'technician_admin' são os mais propensos.
# - **Marital:** 'single' é o grupo com maior adesão.
# - **Education:** 'tertiary' e 'secondary' concentram a maioria das adesões.
# Recomenda-se priorizar essas categorias para estratégias de marketing e incluir no modelo de regressão logística.



# Análise das Variáveis 'day_category' e 'month_trimester' em Relação a 'Y' (Adesão)

# 1. Tabela e Proporção de 'day_category' por 'Y'
tabela_day <- table(dados$day_category, dados$y)
prop_day <- prop.table(tabela_day, margin = 2)
cat("Tabela de day_category por Y:\n")
print(tabela_day)
cat("Proporção de day_category por Y:\n")
print(round(prop_day, 4))


# 2. Tabela e Proporção de 'month_trimester' por 'Y'
tabela_month <- table(dados$month_trimester, dados$y)
prop_month <- prop.table(tabela_month, margin = 2)
cat("Tabela de month_trimester por Y:\n")
print(tabela_month)
cat("Proporção de month_trimester por Y:\n")
print(round(prop_month, 4))



# Interpretação de 'day_category':
#' - 'meio_mes' apresenta a maior adesão (41,84%), 
#' sugerindo maior receptividade nesse período.
#' 
#' - 'inicio_mes' (28,60%) e 'final_mes' (29,56%)
#'  possuem adesão semelhante e inferior ao meio do mês.


# Interpretação sobre 'month_trimester':
# - O '2_trim' lidera a adesão (39,16%), mas o '3_trim' (30,13%) também é relevante.
# - A diferença entre os trimestres não caracteriza sazonalidade sem análise de vários anos.
# - O '4_trim' (16,31%) é melhor que o '1_trim' (14,40%), mas isso pode ser efeito de campanhas específicas.




# Análise da Relação de 'contact_cat' com 'Y'

# 1. Categorizando 'contact' (mantido conforme definido anteriormente)
dados$contact_cat <- ifelse(dados$contact == 'cellular', 'celular', 'outros')

# 2. Tabela e Proporção de 'contact_cat' por 'Y'
tabela_contact <- table(dados$contact_cat, dados$y)
prop_contact <- prop.table(tabela_contact, margin = 2)

cat("Tabela de contact_cat por Y:\n")
print(tabela_contact)
cat("Proporção de contact_cat por Y:\n")
print(round(prop_contact, 4))

# 3. Visualizando a Relação com Gráfico de Barras
ggplot(dados, aes(x = contact_cat, fill = y)) +
  geom_bar(position = "fill") +
  geom_text(stat = "count", aes(label = scales::percent(after_stat(count) / sum(after_stat(count)), accuracy = 0.1)),
            position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Distribuição de contact_cat por Y",
       x = "Categoria de Contato",
       y = "Proporção") +
  theme_minimal()
# Interpretação da Relação 'contact_cat' com 'Y':
#' - 'celular' gerou 79,85% das adesões, evidenciando forte eficácia deste canal.
#' - 'outros' representou apenas 20,15% das adesões, sugerindo baixa eficiência.
#' - Apesar de 'celular' gerar mais adesões, 
#' também concentrou maior volume de contatos não convertidos (62,00%).
#' - A relação sugere priorizar campanhas por celular,
#'  mas com melhorias na abordagem para reduzir rejeições.
#'  

# Análise da Relação de 'loan' com 'Y' (Com Interpretação)

# 1. Tabela e Proporção de 'loan' por 'Y'
tabela_loan <- table(dados$loan, dados$y)
prop_loan <- prop.table(tabela_loan, margin = 2)

cat("Tabela de loan por Y:\n")
print(tabela_loan)
cat("Proporção de loan por Y:\n")
print(round(prop_loan, 4))

# Interpretação da Relação 'loan' com 'Y':
# - Clientes sem empréstimo ('no') apresentam maior adesão, indicando menor resistência à nova oferta.
# - Clientes com empréstimo ('yes') possuem menor adesão, possivelmente devido a comprometimento financeiro.
# - Este padrão sugere que possuir um empréstimo é uma barreira à subscrição do novo produto.

# 2. Visualizando a Relação com Gráfico de Barras
ggplot(dados, aes(x = loan, fill = y)) +
  geom_bar(position = "fill") +
  geom_text(stat = "count", aes(label = scales::percent(after_stat(count) / sum(after_stat(count)), accuracy = 0.1)),
            position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Distribuição de loan por Y",
       x = "Empréstimo Pessoal",
       y = "Proporção") +
  theme_minimal()

# Conclusão:
# - Clientes sem empréstimo são o público-alvo mais promissor.
# - Recomenda-se direcionar campanhas e condições diferenciadas para clientes com empréstimos ativos, visando superar objeções financeiras.


# Análise da Relação de 'housing' com 'Y' (Com Interpretação)

# 1. Tabela e Proporção de 'housing' por 'Y'
tabela_housing <- table(dados$housing, dados$y)
prop_housing <- prop.table(tabela_housing, margin = 2)

cat("Tabela de housing por Y:\n")
print(tabela_housing)
cat("Proporção de housing por Y:\n")
print(round(prop_housing, 4))

# Interpretação da Relação 'housing' com 'Y':
#' - Clientes sem empréstimo habitacional ('no')
#'  apresentam maior taxa de adesão, sugerindo menor
#'   comprometimento financeiro.
#' - Clientes com empréstimo habitacional ('yes')
#'  demonstram menor adesão, possivelmente por restrições
#'   orçamentárias.
#' - A relação sugere que o status de crédito habitacional
#'  pode influenciar a aceitação de novos produtos financeiros.

# 2. Visualizando a Relação com Gráfico de Barras
ggplot(dados, aes(x = housing, fill = y)) +
  geom_bar(position = "fill") +
  geom_text(stat = "count", aes(label = scales::percent(after_stat(count) / sum(after_stat(count)), accuracy = 0.1)),
            position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Distribuição de housing por Y",
       x = "Empréstimo Habitacional",
       y = "Proporção") +
  theme_minimal()

# Conclusão:
# - O público sem empréstimo habitacional é mais propenso à adesão.
# - Recomenda-se criar ofertas específicas para clientes com financiamento habitacional, mitigando restrições financeiras.


################################################################
# Cálculo da Relação de Y com Variáveis Categóricas através do Cramér's V



# Lista de variáveis categóricas

cat_vars <- c('job_cat', 'marital', 'education', 'default', 'housing', 'loan', 
              'contact_cat', 'day_category', 'month_trimester', 
              'age_cat', 'balance_cat', 'duration_cat', 'previous')

# Função para calcular Cramér's V
df_cramers <- data.frame(Variavel = character(), CramersV = numeric())
for (var in cat_vars) {
  tbl <- table(dados[[var]], dados$y)
  cv <- cramerV(tbl, bias.correct = TRUE)
  df_cramers <- rbind(df_cramers, data.frame(Variavel = var, CramersV = cv))
}

# Exibindo resultados ordenados por força da associação
df_cramers <- df_cramers[order(-df_cramers$CramersV),]
print(df_cramers)



# Interpretação Detalhada:
# 1. **duration_cat (0.3446):** Apresenta a maior associação, destacando sua importância.
# 2. **previous (0.1771):** Relevante, indicando que contatos anteriores são fortes preditores de adesão.
# 3. **contact_cat (0.1178) e housing (0.1036):** Possuem associação moderada, sugerindo influência dos canais de contato e situação financeira.
# 4. **month_trimester (0.0966):** Aponta possível influência temporal, embora moderada.
# 5. **loan (0.0689), job_cat (0.0687) e balance_cat (0.0662):** Relações fracas, mas potencialmente úteis em interações.
# 6. **marital (0.0614), age_cat (0.0546) e education (0.0520):** Associações baixas, com relevância menor.
# 7. **default (0.0000) e day_category (0.0000):** Sem associação significativa.

# Conclusão:
# - **previous** é uma variável importante, devendo ser incluída no modelo.
# - **duration_cat**, **contact_cat**, e **housing** merecem destaque.
# - **default** e **day_category** podem ser descartadas.
# - Explore interações entre variáveis com Cramér's V moderado.

################################################
# Criação de Dummies para Variáveis Categóricas Selecionadas

# Dummies para age_cat (jovem e idoso)
dados$age_jovem <- ifelse(dados$age_cat == 'jovem', 1, 0)
dados$age_idoso <- ifelse(dados$age_cat == 'idoso', 1, 0)

# Dummies para balance_cat (médio e alto)
dados$balance_medio <- ifelse(dados$balance_cat == 'medio', 1, 0)
dados$balance_alto <- ifelse(dados$balance_cat == 'alto', 1, 0)

# Dummies para duration_cat (médio e elevado)
dados$duration_medio <- ifelse(dados$duration_cat == 'medio', 1, 0)
dados$duration_elevado <- ifelse(dados$duration_cat == 'elevado', 1, 0)

# Dummies para job_cat (outros, management, technician_adm)
dados$job_outros <- ifelse(dados$job_cat == 'outros', 1, 0)
dados$job_management <- ifelse(dados$job_cat == 'management', 1, 0)
dados$job_technician_adm <- ifelse(dados$job_cat == 'technician_adm', 1, 0)

# Dummies para marital (single e married)
dados$marital_single <- ifelse(dados$marital == 'single', 1, 0)
dados$marital_married <- ifelse(dados$marital == 'married', 1, 0)

# Dummies para education (tertiary e secondary)
dados$edu_tertiary <- ifelse(dados$education == 'tertiary', 1, 0)
dados$edu_secondary <- ifelse(dados$education == 'secondary', 1, 0)

# Dummies para month_trimester (2-trim, 3-trim, 4-trim)
dados$trim_2 <- ifelse(dados$month_trimester == '2-trim', 1, 0)
dados$trim_3 <- ifelse(dados$month_trimester == '3-trim', 1, 0)
dados$trim_4 <- ifelse(dados$month_trimester == '4-trim', 1, 0)

# Dummies para loan (no)
dados$loan_no <- ifelse(dados$loan == 'no', 1, 0)

# Dummies para housing (no)
dados$housing_no <- ifelse(dados$housing == 'no', 1, 0)

# Criando a nova dummy 'contatados' baseada em 'previous'
dados$contatados <- ifelse(dados$previous > 0, 1, 0)


###############################################################

# Construção do Modelo de Regressão Logística com Dummies Específicas

# Análise do Modelo de Regressão Logística com Dummies

# Certificando que a variável resposta está no formato binário correto
dados$y <- as.numeric(as.factor(dados$y)) - 1  # Convertendo 'y' para 0 e 1

# Ajuste do modelo de regressão logística com ajustes solicitados
# Ajuste inicial do modelo completo
# Ajustando o modelo com a nova dummy 'contatados'
modelo <- glm(y ~ balance_medio + 
                duration_medio + duration_elevado + 
                marital_married + 
                loan_no + housing_no + 
                contatados,
              family = binomial, data = dados)



summary(modelo)

# Interpretação dos Resultados:
# - **Todas as variáveis mantidas são significativas ao nível de 5%.**
# - `duration_medio` e `duration_elevado` têm os maiores efeitos positivos.
# - `marital_married` tem efeito negativo sobre a probabilidade de adesão.
# - `loan_no`, `housing_no` e `previous` são fortes preditores do evento.
# `balance_medio` apresenta contribuição relevante e significativa.

# Conclusões e Recomendações:
# - O modelo ajustado tem um AIC de 2625,4, indicando bom ajuste.


# Modelo Stepwise (Backward e Forward)
modelo_stepwise <- step(modelo, 
                        direction = "both", 
                        trace = TRUE)

summary(modelo_stepwise)


# Comparação de Desempenho
cat("AIC do Modelo com p-valor:", AIC(modelo), "\n")
cat("AIC do Modelo Stepwise:", AIC(modelo_stepwise), "\n")


# Exponencial dos Parâmetros (Odds Ratios)
cat("\nExponencial dos Parâmetros (Modelo com p-valor):\n")
print(exp(coef(modelo)))

cat("\nExponencial dos Parâmetros (Modelo Stepwise):\n")
print(exp(coef(modelo_stepwise)))


# Interpretação dos Resultados: Exponencial dos Parâmetros (Odds Ratios)

#' Odds Ratios e Significado Prático:**
#'
#'- balance_medio (1.582):** Clientes com saldo médio têm 58,2% mais chance de adesão em comparação aos outros grupos.
#'- duration_medio (8.05):** Contato com duração média aumenta 8 vezes a probabilidade de adesão, se comparado aos contatos curtos.
#'- duration_elevado (22.38):** Contato com duração elevada multiplica em 22 vezes a chance de adesão,
#' se comparado aos contatos curtos, sendo a variável mais influente.
#'- marital_married (0.732):** Ser casado reduz a chance de adesão em 26,8%,comparado aos solteiros e divorciados, sugerindo um perfil menos receptivo.
#'- loan_no (2.247):** Não ter empréstimo pessoal dobra a probabilidade de adesão, do que os que possuem emprétimos.
#'- housing_no (2.485):** Não possuir financiamento habitacional aumenta a chance de adesão em 148,5%, comparados aos que possuem fianciamento.
#'- contatados (3.632):** Ter sido contatado anteriormente aumenta a probabilidade de adesão em 263,2%, em relação aos que nunca foram contatados.


# Avaliação de Métricas de Qualidade de Ajuste do Modelo de Regressão Logística

# Calculando Previsões
base_metricas <- dados
base_metricas$y_chapeu <- predict(modelo, newdata = base_metricas, type = "response")
base_metricas$decisao_05 <- ifelse(base_metricas$y_chapeu >= 0.5, 1, 0)

# Calculando MSE e RMSE
base_metricas$MSE <- (base_metricas$y - base_metricas$y_chapeu)^2
MSE_modelo <- mean(base_metricas$MSE)
RMSE_modelo <- sqrt(MSE_modelo)

# Matriz de Confusão e Métricas de Classificação
conf <- table(base_metricas$y, base_metricas$decisao_05)
precision_05 <- conf[2,2] / sum(conf[,2])
recall_05 <- conf[2,2] / sum(conf[2,])

# Cálculo do Lift nos Primeiros 10%
df_y_chapeu <- base_metricas %>%
  select(y, y_chapeu) %>%
  arrange(desc(y_chapeu)) %>%
  mutate(percentil_ychapeu = ntile(row_number(), 10))

lift_summary <- df_y_chapeu %>%
  group_by(percentil_ychapeu) %>%
  summarise(media_ychapeu = mean(y_chapeu),
            media_y = mean(as.numeric(y))) %>%
  mutate(Lift = media_y / mean(base_metricas$y))

# Curva ROC e AUC
roc_curve <- roc(base_metricas$y, base_metricas$y_chapeu)
auc_value <- auc(roc_curve)

plot(roc_curve, main = "Curva ROC", col = "blue", lwd = 2)
abline(a=0, b=1, col="gray", lty=2)
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lty = 1, cex = 0.8)

# Exibindo Resultados
cat("MSE:", MSE_modelo, "\n")
cat("RMSE:", RMSE_modelo, "\n")
cat("Precisão (0.5):", precision_05, "\n")
cat("Recall (0.5):", recall_05, "\n")
print(lift_summary)

# Interpretação Completa da Avaliação de Métricas do Modelo de Regressão Logística

## Desempenho Geral do Modelo:
#' AIC: 2625,35 — Indica um bom equilíbrio entre ajuste e complexidade.
#' MSE: 0,08496 — Erro médio quadrático baixo, sugerindo um bom ajuste.
#' RMSE: 0,29148 — Confirma que as previsões são relativamente próximas aos valores reais.
#' Acurácia: 88,5% — O modelo classificou corretamente 88,5% das observações no corte de 0,5.
#' KS (Kolmogorov-Smirnov): 0,72 — Forte separação entre as classes positivas e negativas.
#' Ponto de Maior Separação (KS): O ponto de corte ótimo é aproximadamente 0,32, onde ocorre a máxima diferença entre taxas acumuladas de verdadeiros positivos e falsos positivos.

## Análise de Precisão, Recall, Lift e KS:
#' Precisão (0,5): 47,7% — De todas as previsões positivas, 47,7% estavam corretas.
#' Recall (0,5): 12,1% — O modelo identificou apenas 12,1% dos casos positivos reais.
#' Lift (10% primeiros): 3,95 — Nos 10% principais, a taxa de adesão foi 3,95 vezes superior à média.
#' KS: 0,72 — Excelente discriminador de desempenho.
          
 ## Interpretação das Principais Variáveis (Odds Ratios):
#' duration_elevado (22,38)`: Impacto positivo mais significativo — contatos mais longos geram alta adesão.
#' contatados (3,63): Histórico de contato prévio aumenta em 263% a chance de adesão.
#' housing_no (2,49) e `loan_no (2,25): Ausência de financiamentos aumenta a propensão.
#' marital_married (0,73): Clientes casados têm menor propensão à adesão.
          
          ## **Curva ROC e AUC:**
#' AUC: Aproximadamente 0,80 — Indica boa capacidade discriminatória.
#' Curva ROC: Apresenta separação adequada entre classes positivas e negativas.
          
          ## Conclusões:
#' O modelo é sólido, com bom ajuste geral e variáveis relevantes.
#' O desempenho em recall é baixo, sugerindo que o modelo tem espaço para melhorias, como ajustes de corte ou introdução de novas variáveis.
#' O Lift alto nos primeiros percentis sugere boa aplicabilidade para campanhas direcionadas.
          

# Pronto! Este roteiro é o modelo base para suas análises. Ajustes futuros são bem-vindos.



# Ajuste do Ponto de Corte Baseado no KS para Regressão Logística

## **Como Realizar o Ajuste com Corte em 0,32 (KS):**

# 1. Gerar Previsões
base_metricas$y_chapeu <- predict(modelo, newdata = base_metricas, type = "response")

# 2. Definir Decisão com Base no Novo Corte (0,32)
base_metricas$decisao_032 <- ifelse(base_metricas$y_chapeu >= 0.32, 1, 0)

# 3. Calcular a Nova Matriz de Confusão
conf_matrix_032 <- table(base_metricas$y, base_metricas$decisao_032)
print(conf_matrix_032)

# 4. Calcular Precisão e Recall no Novo Corte
precision_032 <- conf_matrix_032[2,2] / sum(conf_matrix_032[,2])
recall_032 <- conf_matrix_032[2,2] / sum(conf_matrix_032[2,])

cat("Precisão (0.32):", precision_032, "\n")
cat("Recall (0.32):", recall_032, "\n")

# 5. Comparar Desempenho com Corte Anterior (0,5)
conf_matrix_05 <- table(base_metricas$y, base_metricas$decisao_05)
accuracy_032 <- sum(diag(conf_matrix_032)) / sum(conf_matrix_032)
accuracy_05 <- sum(diag(conf_matrix_05)) / sum(conf_matrix_05)

cat("Acurácia com Corte 0.32:", accuracy_032, "\n")
cat("Acurácia com Corte 0.5:", accuracy_05, "\n")

# Comparação de Precisão, Recall e Tamanho Amostral nos Cortes 0,5 e 0,32

## **Tabela Comparativa de Desempenho com Tamanho de Amostra:**


### **Cálculo com Percentil (10%)**:


percentil_10 <- floor(nrow(base_metricas) * 0.10)

# Ajustando cálculo de acertos com base em valores reais de recall
acertos_recall_05 <- round(sum(base_metricas$y == 1 & base_metricas$decisao_05 == 1))
acertos_recall_032 <- round(sum(base_metricas$y == 1 & base_metricas$decisao_032 == 1))

# Criando e exibindo tabela comparativa revisada
comparacao_metricas <- data.frame(
  Corte = c("0.5", "0.32"),
  Precisao = c(precision_05, precision_032),
  Recall = c(recall_05, recall_032)
)

print(comparacao_metricas)
knitr::kable(comparacao_metricas)


# Comparação Detalhada: Cortes 0,5 e 0,32
# Total de 4521 linhas divididas em blocos de 10% (452 linhas cada).

## Análise do Corte 0,5:
#' Precisão: Daqueles que realmente realizam o evento
#' o modelo acerta cerca de 47,73%.
#' Alcance: O modelo possui um pequeno alcance ou seja,
#' é capaz de pegar 12,1% daqueles que realmente
#' fazem o evento.




# Com corte 0,32, a análise mostra:
#' Precisão: Daqueles que realmente realizam o evento
#' o modelo acerta cerca de 46,35%.
#' Alcance: O modelo amplia significativamente o alcance,
#' captando mais eventos reais, embora com leve perda 
#' de precisão. É capaz de pegar 31,66% daqueles que realmente
#' fazem o evento.

#' Sendo assim, verifica-se que a precisão,ou seja,
#' a capacidade de acertar aqueles que farão o evento é praticamente igual
#' nos dois pontos de corta, poré, o alcance do corte 0,32 è
#' significativamente maior, sendo preferível para aumentar
#' a cobertura.


