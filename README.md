# Modelagem Avançada de Distribuição de Espécies e Nicho Ecológico com R

Curso SDM 2026 — repositório de apresentação do curso e organização das atividades práticas.

## Informações do curso

| Informação | Descrição |
|---|---|
| Realização | UFVJM / PPGPV / AgriMe / Embrapa Roraima |
| Local | UFVJM, Campus JK, Diamantina (MG) |
| Endereço | Rodovia MGT 367, km 583, nº 5.000, Alto da Jacuba |
| Datas | 11 a 14 e 16 a 20 de novembro de 2026 |
| Carga horária | 60 horas de atividades teóricas e práticas |

A distribuição da carga horária entre os dias e os horários diários está pendente de confirmação.

## Sobre o curso


O curso integra fundamentos ecológicos, decisões metodológicas e prática em R para desenvolver e interpretar modelos correlativos de distribuição de espécies e nicho ecológico. O percurso acompanha a formulação da pergunta de pesquisa, a preparação dos dados, o ajuste e a avaliação dos modelos, suas projeções no espaço e no tempo e a comunicação dos resultados.

A proposta combina explicações conceituais, discussão de estudos e desenvolvimento guiado de scripts. Os participantes examinam como a biologia da espécie, a qualidade das ocorrências, a escolha dos preditores, a área de calibração e os critérios de avaliação influenciam as conclusões. As aplicações incluem conservação, invasões biológicas e mudanças climáticas.

Nicho ecológico, adequabilidade ambiental e distribuição geográfica são conceitos relacionados, com alcances distintos. As projeções de adequabilidade serão interpretadas à luz das condições abióticas, das interações bióticas e da acessibilidade, sem assumir que indiquem automaticamente ocupação, abundância ou estabelecimento.

## Público e conhecimentos prévios


Estudantes, pesquisadores e profissionais interessados em aplicar e avaliar criticamente modelos de distribuição de espécies. Familiaridade com computadores e conhecimentos básicos de R são recomendados. A introdução ao R e ao RStudio oferece apoio inicial para acompanhar o roteiro, mas o curso aborda também decisões metodológicas avançadas.

## Objetivos de aprendizagem


Ao final do curso, espera-se que os participantes sejam capazes de:

- Formular perguntas e hipóteses coerentes com a biologia da espécie, a escala de análise e o uso pretendido do modelo.
- Avaliar fontes, qualidade, cobertura e vieses dos registros de ocorrência e selecionar preditores ecologicamente pertinentes.
- Justificar a área de calibração, os dados de contraste, os algoritmos e o controle da complexidade.
- Interpretar partições de avaliação, métricas, limiares e diagnósticos, reconhecendo suas limitações.
- Examinar pressupostos, extrapolação e incertezas nas projeções e compreender a abordagem Ensemble of Small Models.
- Organizar um fluxo de trabalho em R e comunicar resultados com mapas, evidências e limites de uso.

## Organização do curso

As unidades seguem a sequência conceitual e prática do material de aula. A prática em R acompanha as etapas de modelagem, com retomadas dos conceitos necessários às decisões de cada etapa.


| Unidade | Questão central | Conteúdo principal |
|---|---|---|
| Fundamentos e pergunta de pesquisa | O que o modelo permite investigar? | Construção do conhecimento científico; observação, inferência e hipótese; nicho ecológico, diagrama BAM e dualidade entre espaços ambiental e geográfico; abordagens correlativas e mecanísticas; aplicações e limites de ENM e SDM. |
| Ambiente de trabalho em R | Como organizar uma análise reprodutível? | Introdução ao R e ao RStudio; pacotes, scripts e diretórios; organização de dados e resultados; visão geral do fluxo de modelagem e documentação das decisões com apoio do protocolo ODMAP. |
| Preparação dos dados | Que dados sustentam a pergunta? | Fontes e produtos de ocorrência; identificação taxonômica, coordenadas, procedência e detectabilidade; tamanho amostral, limpeza e filtros; vieses espaciais; preditores, resolução e escala; colinearidade e relevância ecológica. |
| Ajuste dos modelos | Como representar as relações com o ambiente? | Ausências, pseudoausências e background; definição e justificativa da área de calibração; respostas e pressupostos dos algoritmos; Maxent e outras abordagens; complexidade, sobreajuste, subajuste e ensembles; uso do flexsdm. |
| Avaliação dos modelos | Que evidência sustenta o desempenho estimado? | Particionamento dos dados, incluindo estratégias espaciais; independência da avaliação; matriz de confusão; métricas e curvas ROC; limiares e consequências de omissão e comissão; curvas de resposta e importância das variáveis. |
| Projeções no espaço e no tempo | Quando a transferência é defensável? | Adequabilidade no presente e em outros locais e períodos; pressupostos de transferência; extrapolação e ambientes novos; dinâmica de invasão e comparação de nichos; projeções climáticas CMIP6, cenários SSP e incertezas entre modelos climáticos. |
| Ensemble of Small Models | Como modelar com informação limitada? | Fundamentos e aplicações de ESM; combinação de modelos de baixa dimensão; adequabilidade e concordância entre classificações; limites de interpretação e avaliação com poucas ocorrências. |
| Comunicação e discussão | Como apresentar evidências e limites? | Mapas temáticos com tmap; apresentação do estudo do grupo em cinco slides; relação entre pergunta, métodos, resultados e conclusão; incertezas e limites de uso; leitura crítica e troca de experiências no Café com Paper. |

## Metodologia e atividades

As aulas articulam exposições dialogadas, exemplos de estudos publicados, demonstrações e prática guiada em R. A construção e a interpretação dos resultados acompanham o roteiro de modelagem, com discussão das alternativas e das consequências de cada decisão. R e RStudio constituem o ambiente de trabalho; flexsdm apoia a modelagem e tmap, a elaboração de mapas.

As atividades integram a definição de uma pergunta, a preparação das ocorrências e dos preditores, o ajuste e a avaliação dos modelos e a interpretação de suas projeções. A exploração de algoritmos e configurações deve ser orientada pelo objetivo do estudo, pela informação disponível e pelos pressupostos de cada método.


O curso inclui codificação assistida, atividades interativas, discussões e desenvolvimento de projetos dos participantes. Entre os pacotes trabalhados estão `flexsdm`, `tmap`, `rgbif`, `geodata` e `geobr`.

## Apresentação do estudo do grupo

O material de aula propõe a comunicação do estudo em cinco slides, organizados segundo a lógica de introdução, métodos, resultados e discussão:

1. Pergunta, espécie e decisão que o modelo pode apoiar.
2. Ocorrências, qualidade dos dados, preditores e área de calibração.
3. Modelos selecionados, ensemble e avaliação.
4. Adequabilidade ambiental, distribuição potencial e extrapolação.
5. Conclusão, incertezas e limites de uso.

A discussão considera a coerência ecológica, a justificativa das escolhas e a clareza da comunicação. Métricas elevadas, isoladamente, não demonstram validade ecológica nem capacidade de transferência. No roteiro apresentado, as partições usadas na seleção e no ajuste não constituem automaticamente um teste externo independente.

## Organização das atividades práticas

A estrutura abaixo representa o projeto local utilizado nas atividades; sua inclusão aqui documenta a organização prevista para o trabalho.

```text
Modelagem/
├── Modelagem.Rproj
├── Data/
│   ├── climate/
│   ├── CMIP6/
│   ├── elevation/
│   ├── Ecoregions/
│   ├── Koeppen 1986-2010/
│   ├── monfreda/
│   ├── Natural Earth/
│   ├── Shapes/
│   ├── soil_world/
│   └── spam/
├── Amanoa cupatensis/
└── Zaprionus tuberculatus/
```

Esses diretórios e arquivos permanecem locais nesta etapa.

## Estudos de caso

- *Amanoa cupatensis*: organização de ocorrências, modelagem e análise dos resultados.
- *Zaprionus tuberculatus*: organização de ocorrências, modelagem e análise de projeções, incluindo cenários futuros.

## Materiais e disponibilidade

Este repositório reúne, por enquanto, a apresentação do curso e a descrição de suas práticas. Slides, scripts, dados e resultados serão acrescentados conforme forem liberados para compartilhamento.

O material didático do curso inclui slides, scripts R, exemplos, referências científicas e links para aprofundamento. A descrição desses recursos não implica que já estejam disponíveis para download neste repositório.

## Licenças e fontes

A licença declarada pode ser consultada em [LICENSE.md](LICENSE.md). Recursos externos conservam as condições de uso e atribuição de suas fontes. Consulte os metadados e cite a procedência dos dados utilizados. Cada inclusão de conteúdo deverá indicar a licença aplicável.
