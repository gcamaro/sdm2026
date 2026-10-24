# Da pergunta ao artigo

Livro online do Curso SDM 2026, preparado em 24/10/2026 a partir dos 446 slides da apresentação indicada e dos dois scripts do curso. Narrativa didática com os personagens originais, 18 capítulos, trechos R rastreáveis, exercícios, glossário e materiais de trabalho.

## Abrir
Abra `index.html` em um navegador. Não há instalação, login ou conexão necessária para ler. Busca, navegação, troca de tema, cópia de código e impressão são locais. Links externos precisam de conexão.

## Publicar no GitHub Pages
1. Envie o conteúdo desta pasta para a raiz da branch destinada ao site no repositório do curso, preservando a estrutura.
2. Em Settings → Pages, selecione publicação pela branch e a pasta raiz.
3. Aguarde a publicação e use a URL fornecida pelo GitHub.

Se preferir usar a pasta `docs`, copie o conteúdo para `docs` e selecione essa pasta no Pages. O pacote também funciona como subdiretório de um site estático: todos os links internos são relativos. Não foi realizado upload ou publicação.

## Editar e reconstruir
Os arquivos `capitulos/*.json` contêm título, abertura, referência aos slides e corpo HTML editável de cada capítulo. Edite esses arquivos e execute:

    python3 tools/build.py

O construtor usa somente a biblioteca padrão do Python e regenera os HTML e o índice de busca. Estilos em `assets/book.css`; interações em `assets/book.js`. Não edite apenas o HTML gerado se pretende reconstruir depois.

## Estrutura
- HTML na raiz: livro pronto para publicar.
- capitulos: fontes editáveis do texto.
- assets: ilustrações, estilos e interações.
- fontes: cópias dos scripts, inventários e hashes das fontes.
- materiais: registro ODMAP, registro de refinamentos e roteiro do artigo.
- tools: construtor e verificador.

## Análises e figuras
Não foi executada uma nova modelagem ecológica. Mapas e quadrinhos foram extraídos dos slides, com origem documentada. A conversão do mapa TIFF para PNG não redesenha projeção, cores ou composição. Exemplos adicionais e trechos originais estão identificados. Os trechos são dependentes dos objetos do workflow e não constituem um script linear autossuficiente. Dados ambientais e planilhas de ocorrências não estão incluídos.

Os scripts originais do curso não foram alterados. As cópias fornecidas preservam seus cabeçalhos e licença CC BY-NC-SA 4.0. Créditos e direitos das imagens seguem suas respectivas origens; não se presume que a licença do código se aplique a todo material visual.

## Base metodológica complementar
Foram lidos no Zotero os suplementos de Kass et al. (2024), “Achieving higher standards in species distribution modeling by leveraging the diversity of available software”. A organização geral do fluxo do curso tem correspondência com os exemplos de espécies raras, invasoras e mudanças climáticas. Detalhes no capítulo de fontes. Os PDFs não foram redistribuídos.

## Leitura dos materiais no navegador
Todos os links de consulta abrem versões HTML: o roteiro em Markdown é apresentado como texto formatado, os registros CSV como tabelas e os scripts R e inventários JSON como texto com botão de cópia. Os arquivos-fonte são preservados. Essas páginas são regeneradas por `python3 tools/build.py`.

## Autoria, licença e consultas
George Amaro — george.amaro@embrapa.br. A versão online deste livro será sempre gratuita e distribuída sob a licença CC BY-NC 4.0: https://creativecommons.org/licenses/by-nc/4.0/. A declaração encontra-se no item 18. O glossário foi organizado em ordem alfabética e o item 19 reúne as referências identificadas nos slides. Os arquivos originais de código são preservados.

## Atualização da licença e das referências
O item 18 apresenta somente a licença do livro. Para regenerar a lista de referências a partir dos metadados locais, execute `python3 tools/references.py` e depois `python3 tools/build.py`. A seção “Outras obras e materiais citados” foi retirada da lista.
