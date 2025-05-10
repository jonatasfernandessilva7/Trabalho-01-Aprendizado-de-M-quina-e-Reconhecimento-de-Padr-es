import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# Ler o arquivo:
caminho = "../dados/DataSet_18_10_2023-Depurado.xlsx"
dados_bruto = pd.read_excel(caminho)

dados = pd.DataFrame(dados_bruto.iloc[:,:26])   # pega as colunas do dataset
#print(dados)
#print(dados.iloc[:,25:26]) # somente a coluna de dropout

# Transformando dados para numéricos:
dados['dropout'] = dados['dropout'].astype(int)     # 0 para falso e 1 para verdadeiro
dados['has_scholarship'] = dados['has_scholarship'].astype(int)
dados['working_student'] = dados['working_student'].astype(int)
dados['moved_student'] = dados['moved_student'].astype(int)

# Selecionar todas as colunas que possuem números/valores:
df_numeros = dados.select_dtypes(include=['int64', 'float64'])

# Montar a matriz de correlação:
matrix_correlacao = df_numeros.corr()

dropout_correlacao = matrix_correlacao['dropout'].sort_values(ascending = False)
print(dropout_correlacao)

print(dados.groupby('dropout')['frequency_grade_result'].describe())


# Mostrando a matriz de correlação:
plt.figure(figsize = (15,10))
sns.heatmap(matrix_correlacao, annot=True, cmap='coolwarm')
plt.title('Matriz de Correlação')
plt.show()