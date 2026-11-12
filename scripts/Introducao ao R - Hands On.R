# =============================================================================
# PRIMEIROS PASSOS NA LINGUAGEM R — HANDS ON
# Curso SDM 2026 | Dia 2, manhã | Duração sugerida: 30 minutos
# =============================================================================
# Objetivo: aprender a ler, executar e modificar comandos dos scripts do curso.
# Os registros abaixo são INVENTADOS para esta prática. Não são dados de campo
# nem os registros usados na modelagem da espécie virtual.
# A atividade principal usa somente funções que já acompanham o R.
# Não instala pacotes, não baixa dados e não grava nem remove arquivos.

# COMO USAR NO RSTUDIO -------------------------------------------------- 1 min
# Abra este arquivo no editor de scripts.
# Posicione o cursor em uma linha e pressione Ctrl + Enter (Windows/Linux)
# ou Command + Enter (macOS). Também é possível selecionar várias linhas.
# Leia a saída no Console antes de continuar.
# Linhas iniciadas por # são comentários: o R não as executa.
# Nos comandos com várias linhas, selecione a expressão inteira.


# 1. OBJETOS E FUNÇÕES -------------------------------------------------- 3 min

2 + 3                            # O R também funciona como uma calculadora.
10 / 2
2^3                              # Potência: 2 elevado a 3.

x <- c(2, 4, 6)                  # <- guarda o resultado em um objeto.
x                                # Mostra o conteúdo do objeto.

mean(x)                          # mean() calcula a média. Resultado: 4.
sum(x)                           # Soma dos valores.
length(x)                        # Quantidade de elementos.

X <- 99                          # R diferencia maiúsculas e minúsculas.
x
X                                # x e X são objetos diferentes.

# EXPERIMENTE: troque x por c(10, 20, 30) e execute mean(x) novamente.


# 2. TIPOS DE VALORES --------------------------------------------------- 2 min

temperatura <- 28.5              # Número: use ponto como separador decimal.
especie <- "Especie_A"            # Texto: use aspas.
coordenada_conferida <- TRUE      # Valor lógico: TRUE ou FALSE.
valor_ausente <- NA               # Informação ausente.

class(temperatura)
class(especie)
class(coordenada_conferida)

mistura <- c(1, "2", 3)           # Misturar texto e números transforma em texto.
mistura
class(mistura)

# NA não é zero e não significa que a espécie está ausente.


# 3. VETORES E FATORES -------------------------------------------------- 4 min

chuva <- c(1500, 1800, 2100, 1700) # Vetor numérico, em mm/ano.
chuva
chuva[1]                         # Primeiro elemento. A contagem começa em 1.
chuva[c(1, 3)]                   # Primeiro e terceiro elementos.
mean(chuva)

chuva > 1700                     # Comparação: retorna TRUE ou FALSE.
chuva[chuva > 1700]               # Seleciona os valores que atendem à condição.

habitat <- factor(c("lavrado", "mata", "lavrado", "mata"))
habitat                          # Fator: representa categorias com níveis.
levels(habitat)
table(habitat)                   # Frequência de cada categoria.

# EXPERIMENTE: selecione as chuvas menores ou iguais a 1800 mm.


# 4. MATRIZES E LISTAS -------------------------------------------------- 3 min

m <- matrix(1:6, nrow = 2)        # 1:6 produz os inteiros de 1 até 6.
m                                # Por padrão, a matriz é preenchida por colunas.
dim(m)                           # Número de linhas e colunas.
m[1, 2]                          # Linha 1, coluna 2. Resultado: 3.

resultado <- list(dados = chuva, matriz = m, especie = especie)
resultado                        # Uma lista pode reunir objetos diferentes.
resultado$dados                  # Acesso pelo nome do componente.
resultado[[2]]                   # Acesso ao segundo componente: a matriz.

# Uma matriz tem elementos do mesmo tipo.
# Uma lista pode conter vetores, tabelas, matrizes e modelos ajustados.


# 5. TABELAS DE DADOS --------------------------------------------------- 4 min

oc <- data.frame(
  id = 1:6,
  especie = c("Especie_A", "Especie_A", "Especie_B",
              "Especie_A", "Especie_B", "Especie_A"),
  lon = c(-60.7, -61.1, -60.5, -61.3, -60.9, NA),
  lat = c(2.8, 3.2, 3.5, NA, 2.6, 3.0),
  temperatura = c(28.5, 27.8, 29.1, 28.2, NA, 28.8),
  habitat = factor(c("lavrado", "lavrado", "mata",
                     "lavrado", "mata", "lavrado"))
)

oc                               # Cada linha é um registro fictício.
head(oc)                         # Mostra as primeiras linhas.
nrow(oc)                         # Número de registros.
ncol(oc)                         # Número de colunas.
names(oc)                        # Nomes das colunas.

# Em um data.frame, colunas diferentes podem ter tipos diferentes.
# Neste exemplo, temperatura está em graus Celsius e coordenadas em graus.


# 6. SELEÇÃO E CONDIÇÕES ------------------------------------------------ 4 min

oc$lon                           # Seleciona a coluna pelo nome.
oc[1, ]                          # Primeira linha, todas as colunas.
oc[, 2]                          # Segunda coluna, todas as linhas.
oc[, c("especie", "lon", "lat")]  # Seleciona colunas pelos nomes.

oc$especie == "Especie_A"         # == compara; <- atribui um valor.
oc[oc$especie == "Especie_A", ]   # Registros da espécie A.

oc[oc$especie == "Especie_A" & oc$habitat == "lavrado", ]
# & exige que as duas condições sejam verdadeiras.

oc[oc$id == 1 | oc$id == 3, ]
# | exige que pelo menos uma condição seja verdadeira.

# Outros operadores: != (diferente), >, <, >= e <=.


# 7. INSPEÇÃO E INFORMAÇÕES AUSENTES ------------------------------------ 4 min

str(oc)                          # Estrutura e tipos de cada coluna.
summary(oc)                      # Resumo das colunas, incluindo valores ausentes.

is.na(oc$lat)                    # Identifica latitudes ausentes.
sum(is.na(oc$lat))               # Conta quantas estão ausentes.

mean(oc$temperatura)             # Retorna NA porque há uma temperatura ausente.
mean(oc$temperatura, na.rm = TRUE) # Calcula com os valores disponíveis.
# na.rm = TRUE não preenche nem estima a informação ausente.

coordenadas_completas <- complete.cases(oc[, c("lon", "lat")])
coordenadas_completas
oc_com_coordenadas <- oc[coordenadas_completas, ]
oc_com_coordenadas
nrow(oc_com_coordenadas)          # Resultado: 4 registros.

# Mantivemos oc intacto e criamos outro objeto com a seleção.
# Uma coordenada preenchida ainda pode estar errada: este filtro verifica
# apenas se lon e lat estão ausentes, não sua qualidade geográfica.


# 8. PACOTES E AJUDA ---------------------------------------------------- 3 min

# Um pacote reúne funções, documentação e, em alguns casos, dados.
# Instalar e carregar são operações diferentes. Os exemplos ficam comentados
# para evitar downloads ou dependências durante esta introdução.

# install.packages("terra")       # Instala no ambiente de bibliotecas escolhido.
# library(terra)                  # Disponibiliza suas funções na sessão atual.
# terra::rast()                   # Chama uma função pelo pacote de origem.
# ?mean                           # Abre a ajuda de uma função no RStudio.
# help("mean")                    # Outra forma de abrir a mesma ajuda.

# Instale um pacote quando necessário; não repita a instalação a cada sessão.
# O operador :: permite chamar uma função sem executar library() antes,
# mas o pacote precisa estar instalado.


# DESAFIO FINAL -------------------------------------------------------- 2 min
# Execute os comandos que você escrever abaixo de cada questão.
# 1. Quantos registros de Especie_A existem em oc?
# 2. Crie oc_A_coordenadas com os registros de Especie_A que têm lon e lat.
# 3. Calcule a temperatura média de oc_A_coordenadas.

# Confira apenas depois de tentar:
# 1. Resultado esperado: 4 registros.
# 2. Resultado esperado: 2 registros, com id 1 e 2.
# 3. Resultado esperado: 28.15 graus Celsius.

# GABARITO PARA CONSULTA (retire o # inicial para executar):
# sum(oc$especie == "Especie_A")
# oc_A_coordenadas <- oc[oc$especie == "Especie_A" &
#                        complete.cases(oc[, c("lon", "lat")]), ]
# mean(oc_A_coordenadas$temperatura, na.rm = TRUE)

# Antes de passar ao roteiro de SDM, identifique no editor os objetos,
# funções, argumentos, colunas, condições e chamadas de pacotes.

