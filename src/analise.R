#======INICIO======

# Instalar pacotes necessários (caso não estejam instalados)
install.packages(c("readxl", "dplyr", "ggplot2", "corrplot"))

# Carregar pacotes
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)

#======LEITURA======

# Ler o arquivo
dados_bruto <- read_excel('Documentos/github/Trabalho-01-Aprendizado-de-M-quina-e-Reconhecimento-de-Padr-es/dados/DataSet_18_10_2023-Depurado.xlsx')

# Selecionar as colunas desejadas (as primeiras 26 colunas)
dados <- dados_bruto[, 1:26]

#ver todas as variáveis
summary(dados)
# ver a dispersão em cada variável
desv_p <- lapply(dados[,1:26], sd)
desv_p
#boxplot
ggplot(dados, aes(x = has_scholarship, y = dropout, group = cut(age, breaks = 5))) + 
  geom_boxplot() #teste para ver se funciona

#======TRANSFORMAÇÃO======

#Evitar NAs
dados$dropout <- ifelse(dados$dropout == "True", 1, ifelse(dados$dropout == "False", 0, NA))
dados$has_scholarship <- ifelse(dados$has_scholarship == "True", 1, 
                                ifelse(dados$has_scholarship == "False", 0, NA))
dados$moved_student <- ifelse(dados$moved_student == "True", 1, ifelse(dados$moved_student == "False", 0, NA))

# Transformar dados para numéricos:
dados$dropout <- as.integer(dados$dropout)  # 0 para falso e 1 para verdadeiro
dados$has_scholarship <- as.integer(dados$has_scholarship)
dados$working_student <- as.integer(dados$working_student)
dados$moved_student <- as.integer(dados$moved_student)

#======VISUALIZAÇÃO======

#ver variáveis após a transformação
summary(dados)
# ver a dispersão em cada variável após a transformação
desv_p <- lapply(dados[,1:26], sd)
desv_p
#boxplot individual após a transformação
ggplot(dados, aes(x = has_scholarship, y = dropout, group = cut(age, breaks = 5))) + 
  geom_boxplot()
ggplot(dados, aes(x = moved_student, y = dropout, group = cut(age, breaks = 5))) + 
  geom_boxplot()
ggplot(dados, aes(x = working_student, y = dropout, group = cut(age, breaks = 5))) + 
  geom_boxplot()

#ver dados
View(dados)

#======SELEÇÃO======

# Selecionar todas as colunas numéricas:
df_numeros <- dados %>% select(where(is.numeric))

#======MEDIDAS======

# Montar a matriz de correlação
matrix_correlacao <- cor(df_numeros)

# Mostrar a correlação com 'dropout'
dropout_correlacao <- sort(matrix_correlacao[, 'dropout'], decreasing = TRUE)
print(dropout_correlacao)

# Descrever estatística agrupada por 'dropout'
summary_by_dropout <- dados %>%
  group_by(dropout) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    # More numeric columns
    num_registrations_summary = list(summary(num_registrations)),
    .groups = "drop"
  )

# Mostrar a matriz de correlação:
plot.new()
dev.off()

corrplot(matrix_correlacao, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
title('Matriz de Correlação')

# Calcular medidas de Posição (Média, Mediana e Moda) por grupo de 'dropout'
medidas_posicao <- dados %>%
  group_by(dropout) %>%
  summarise(
    media_age = mean(age, na.rm = TRUE),
    mediana_age = median(age, na.rm = TRUE)
  )

# Calcular a moda
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

moda_age_dropout <- dados %>%
  group_by(dropout) %>%
  summarise(
    moda_age = get_mode(age)
  )

medidas_posicao <- left_join(medidas_posicao, moda_age_dropout, by = "dropout")

print("Medidas de Posição por Dropout:")
print(medidas_posicao)

# Calcular medidas de Dispersão por grupo de 'dropout'

medidas_dispersao <- dados %>%
  group_by(dropout) %>%
  summarise(
    amplitude_age = max(age, na.rm = TRUE) - min(age, na.rm = TRUE),
    variancia_age = var(age, na.rm = TRUE),
    desvio_padrao_age = sd(age, na.rm = TRUE)
  )

# Coeficiente de Variação de Pearson (CV)
coeficiente_variacao <- dados %>%
  group_by(dropout) %>%
  summarise(
    cv_age = sd(age, na.rm = TRUE) / mean(age, na.rm = TRUE)
  )

medidas_dispersao <- left_join(medidas_dispersao, coeficiente_variacao, by = "dropout")

print("\nMedidas de Dispersão por Dropout:")
print(medidas_dispersao)

# Calcular Frequência Relativa da variável 'dropout'

frequencia_relativa_dropout <- dados %>%
  group_by(dropout) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    frequencia_relativa = n / sum(n)
  ) %>%
  select(dropout, frequencia_relativa)

print("\nFrequência Relativa de Dropout:")
print(frequencia_relativa_dropout)

# Calcular a matriz de correlação (já feito anteriormente)
print("\nMatriz de Correlação (incluindo correlação com 'dropout'):")
print(dropout_correlacao)

# Análise de Similaridade (Distância Euclidiana)

df_para_distancia <- df_numeros %>% select(age, dropout, moved_student, working_student, has_scholarship)

# Remover linhas com NA para o cálculo da distância
df_para_distancia <- na.omit(df_para_distancia)

# Normalizar os dados para que variáveis com escalas diferentes não dominem a distância
df_normalizado <- scale(df_para_distancia)

# Calcular a matriz de distância euclidiana
distancia_euclidiana <- dist(df_normalizado, method = "euclidean")

print("\nExemplo de Matriz de Distância Euclidiana (primeiras linhas e colunas):")
print(as.matrix(distancia_euclidiana)[1:5, 1:5]) # Exibe as 5 primeiras linhas e colunas para não ser muito extenso

# Identificar Correlações Positivas e Negativas com 'dropout'

print("\nCorrelações Positivas com Dropout (ordenadas):")
correlacoes_positivas <- dropout_correlacao[dropout_correlacao > 0 & names(dropout_correlacao) != "dropout"]
print(sort(correlacoes_positivas, decreasing = TRUE))

print("\nCorrelações Negativas com Dropout (ordenadas):")
correlacoes_negativas <- dropout_correlacao[dropout_correlacao < 0]
print(sort(correlacoes_negativas, decreasing = FALSE))