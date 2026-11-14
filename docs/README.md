# Da pergunta ao artigo — Curso SDM 2026

Livro atualizado em 13/11/2026, com foco na simulação do uso do script de Zaprionus tuberculatus, desde o planejamento de campo até a comunicação dos resultados.

## Fontes da atualização
- Slides: Curso SDM 2026 - 2026-11-06.pptx (501 slides na cópia consultada).
- Script: Zaprionus tuberculatus SDM e Futuro 2026-10-12.R (conteúdo conferido nesta atualização).

O nome do script mantém a data nominal. O manifesto em fontes/manifesto.json identifica os arquivos efetivamente consultados por SHA-256. Os slides e o script originais não foram editados.

## Ler e publicar
Abra index.html. Para publicar, envie esta pasta docs ao repositório gcamaro/sdm2026, preservando a estrutura. O GitHub Pages utiliza main → /docs; o arquivo de entrada precisa estar em docs/index.html. Não foi realizado upload nesta atualização.

## Editar e reconstruir
Os textos editáveis estão em capitulos/*.json. Para regenerar o livro:

    python3 tools/build.py

A bibliografia utiliza fontes/referencias.json. Quando seus dados forem alterados, execute primeiro:

    python3 tools/references.py

O construtor usa a biblioteca padrão do Python. Os materiais CSV, Markdown, R e JSON têm versões HTML para leitura no navegador, sem download automático.

## Como acompanhar o roteiro
Os capítulos intercalam narrativa, teoria e blocos pequenos de código. Eles explicam o script completo; não o substituem como arquivo executável. Os comandos originais, compactados e adicionais estão identificados. O RDS dos modelos depende dos dados, preditores e demais objetos para retomar a projeção.

A narrativa de campo é didática. Não foi executado um novo ajuste ecológico para atualizar o livro. Não foram incluídos o exemplo de espécie virtual nem um segundo workflow de ESM. O endereço histórico 15-esm.html foi mantido para conservar os links, mas seu conteúdo trata de executar e retomar o roteiro principal.

## Autoria e licença
George Amaro — george.amaro@embrapa.br.
A versão online deste livro será sempre gratuita e distribuída sob a licença CC BY-NC 4.0: https://creativecommons.org/licenses/by-nc/4.0/.
A página de licença permanece no item 18; o glossário, no item 17; as referências, no item 19.

## Arquivos e referências
O inventário de imagens registra a associação atual aos slides. A bibliografia preserva os artigos de George Amaro e George Correa/Corrêa Amaro já incluídos, com deduplicação. A seção “Outras obras e materiais citados” continua retirada.
