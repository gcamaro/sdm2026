# Preparação do computador para o Curso SDM 2026

Instalação do R e do RStudio e conferência dos pacotesAtualizado em 21 de outubro de 2026

Conclua esta preparação antes do início do curso, usando uma conexão estável com a internet. Instale primeiro o R, depois o RStudio Desktop e, por último, os pacotes. Os programas são gratuitos nas versões indicadas aqui.

[GitHub do curso — informações e materiais](https://github.com/gcamaro/sdm2026)

## 1 Instale o R

O R executa os cálculos e as análises. A versão de referência para esta preparação é o R 4.6.1, de 64 bits. Evite versões de desenvolvimento (R-devel). Se já utiliza outra versão em projetos, mantenha-a e procure apoio antes de substituí-la.

### Windows

Acesse o link abaixo, baixe o instalador da versão estável e execute-o. Em geral, as opções padrão são suficientes. Se o computador for institucional, confirme se você tem permissão para instalar programas.

[Baixar R para Windows — CRAN](https://cran.r-project.org/bin/windows/base/)

### macOS

Em “Sobre Este Mac”, identifique o processador. Para chips Apple M1, M2 ou posteriores, escolha arm64; para Intel, escolha x86_64. Baixe o arquivo .pkg correspondente e siga o instalador. Confira a versão mínima do macOS informada na página do R.

[Baixar R para macOS — CRAN](https://cran.r-project.org/bin/macosx/)

### Linux

Siga as instruções específicas para sua distribuição e sua versão. A instalação dos pacotes espaciais pode exigir bibliotecas do sistema e apoio técnico.

[Instalar R em Linux — CRAN](https://cran.r-project.org/bin/linux/)

## 2 Instale e abra o RStudio Desktop

O RStudio é a interface usada para editar scripts e trabalhar com o R. Na página abaixo, escolha RStudio Desktop gratuito para seu sistema, e não RStudio Server ou a edição Pro. A referência atual é RStudio 2026.10.0; a página informa Windows 11, macOS 14 ou superior e distribuições Linux compatíveis. Para sistemas anteriores, consulte a equipe antes de escolher uma versão antiga.

[Baixar RStudio Desktop — Posit](https://docs.posit.co/ide/user/download.html)

Depois da instalação, abra o RStudio. No painel Console, deve aparecer a versão do R e o sinal >. Digite 1 + 1 e pressione Enter: o resultado esperado é [1] 2. Os próximos blocos devem ser executados no Console, e não no Terminal.

## 3 Instale os pacotes do curso

Copie os blocos a seguir na ordem indicada. Aguarde o retorno do sinal > antes de continuar. A primeira instalação pode demorar. Se aparecer uma pergunta sobre atualizar outros pacotes, escolha não atualizar neste momento. Não é necessário carregar todos os pacotes para instalá-los.

### Pacotes do CRAN

Esta lista reúne os pacotes declarados ou chamados pelos dois scripts do curso, além das ferramentas de instalação e da exportação de tabelas para Word. As dependências obrigatórias serão instaladas automaticamente.

```r
options(repos = c(CRAN = "https://cloud.r-project.org"))
options(timeout = max(600, getOption("timeout")))

pacotes_cran <- c(
  "pacman", "remotes", "ggplot2", "sf", "tmap",
  "CoordinateCleaner", "geodata", "dplyr", "rnaturalearth",
  "corrplot", "RColorBrewer", "terra", "scales", "geobr",
  "countrycode", "gt", "formattable", "flextable", "bdc",
  "rgbif", "openxlsx2", "fuzzySim", "sp", "here", "classInt",
  "ragg", "tidyr", "pROC", "rmarkdown"
)
faltantes <- setdiff(pacotes_cran, rownames(installed.packages()))
if (length(faltantes) > 0) install.packages(faltantes)
```

A lista contempla tmap 4. Se você já usa tmap 3, não prossiga com essa versão para os roteiros do curso; solicite apoio para atualizar o ambiente.

### Pacotes do GitHub

Instale as revisões abaixo para usar as mesmas versões de referência: flexsdm 1.4.2, covsel 1.1.1 e rnaturalearthhires 1.0.0.9000. Os identificadores longos selecionam a revisão exata; copie-os por inteiro. Execute antes de carregar esses pacotes na sessão.

```r
remotes::install_github(
  "sjevelazco/flexsdm",
  ref = "f6635814e3387feef27a725e004c0e16d8e2d23d",
  dependencies = NA, upgrade = "never", build_vignettes = FALSE
)
remotes::install_github(
  "ropensci/rnaturalearthhires",
  ref = "e4736f636baa1c013d77d2ba028dd5bc334defee",
  dependencies = NA, upgrade = "never", build_vignettes = FALSE
)
remotes::install_github(
  "antadde/covsel",
  ref = "8aa04643526a7d786da833a6e349c18f485eef51",
  dependencies = NA, upgrade = "never", build_vignettes = FALSE
)
```

Se ocorrer erro de compilação: no Windows, instale o Rtools correspondente à versão do R; no macOS, podem ser necessárias as ferramentas de linha de comando do Xcode. No Linux, os requisitos variam com a distribuição. A seção de ajuda ao final traz os links.

## 4 Confira a instalação

Execute ainda na mesma sessão, depois dos blocos anteriores. O teste deve terminar sem erro. Se você reiniciou o R, execute novamente a definição de pacotes_cran da seção anterior.

```r
pacotes_curso <- c(
  pacotes_cran, "flexsdm", "rnaturalearthhires", "covsel"
)
carregou <- vapply(
  pacotes_curso, requireNamespace, logical(1), quietly = TRUE
)
if (any(!carregou)) {
  stop(paste("Verifique:", paste(pacotes_curso[!carregou],
                              collapse = ", ")))
}
versoes <- vapply(pacotes_curso, function(p) {
  as.character(utils::packageVersion(p))
}, character(1))
print(versoes)
stopifnot(
  getRversion() >= "4.6.1",
  utils::packageVersion("tmap") >= "4.0.0",
  utils::packageVersion("flexsdm") == "1.4.2",
  utils::packageVersion("covsel") == "1.1.1"
)
stopifnot(rmarkdown::pandoc_available())
message("Pacotes e Pandoc disponíveis.")
```

O Pandoc é usado na exportação das tabelas gt para Word. Execute o teste dentro do RStudio, que inclui essa ferramenta. Instalar Microsoft Word não substitui essa dependência.

### Teste rápido dos dados espaciais

Este exemplo usa apenas dados criados no próprio R. O esperado é um mapa simples e a mensagem de conclusão, sem baixar dados ou ajustar modelos.

```r
r <- terra::rast(
  nrows = 2, ncols = 2, xmin = -60, xmax = -58,
  ymin = 0, ymax = 2, crs = "EPSG:4326"
)
terra::values(r) <- 1:4
r_projetado <- terra::project(r, "EPSG:3857")
stopifnot(terra::ncell(r_projetado) > 0)
ponto <- sf::st_as_sf(
  data.frame(x = -60, y = 0), coords = c("x", "y"), crs = 4326
)
stopifnot(sf::st_crs(sf::st_transform(ponto, 3857))$epsg == 3857)
tmap::tmap_mode("plot")
print(tmap::tm_shape(r) + tmap::tm_raster())
message("Teste espacial concluído.")
```

Uma instalação que carrega os pacotes e conclui esse teste está preparada para iniciar as atividades. Isso não substitui a execução e a avaliação dos modelos durante o curso.

### Se aparecer o erro Cannot find proj.db

Se o teste espacial apresentar `Error: [rast] empty srs` acompanhado de `Cannot find proj.db`, o PROJ não está encontrando o banco de definições dos sistemas de coordenadas. Esse problema pode ocorrer por uma configuração de caminhos e não é exclusivo do macOS.

**Aplique o ajuste abaixo somente se esse erro ocorrer.** Se o teste espacial já funcionou, prossiga para a próxima etapa.

Execute este bloco no Console do RStudio, antes de repetir o teste espacial:

```r
# Localiza o banco de definições fornecido com terra
proj.path <- system.file("proj", package = "terra")

if (!file.exists(file.path(proj.path, "proj.db"))) {
  stop("proj.db não foi encontrado nessa instalação do terra. Contate a equipe.")
}

# Configura os caminhos de busca usados pelo PROJ e pelo GDAL
terra::projPaths(proj.path)
terra::projPaths(proj.path, with_proj = FALSE)

# Confere se o banco está acessível
terra::proj_ok()
```

Se a checagem retornar `TRUE`, execute novamente o bloco **Teste rápido dos dados espaciais**, desde a criação de `r`. A correção estará confirmada quando o teste terminar sem erro e o mapa for apresentado.

Esse ajuste vale para a sessão atual do R. Se reiniciar o R e o mesmo erro reaparecer, execute o bloco novamente antes do teste ou das operações espaciais.

Se aparecer a mensagem de que `proj.db` não foi encontrado, ou se o erro persistir, não substitua o caminho por uma pasta de outro programa ou de uma versão antiga do R. Envie a mensagem completa e a saída de `sessionInfo()` para **george.amaro@embrapa.br**.

## 5 Baixe e abra o projeto do curso

Consulte o GitHub do curso para acompanhar as informações e os materiais disponíveis. O projeto das atividades é distribuído pelo OneDrive.

[GitHub — gcamaro/sdm2026](https://github.com/gcamaro/sdm2026)

[OneDrive — pasta com Modelagem.zip](https://1drv.ms/f/c/74559871d74a2001/IgAmmEN_q6H9Rrjbh0edm5OfAdtIXQFh0vaSXA6laG0TJSI?e=qasdnO)

1. Baixe Modelagem.zip e descompacte o arquivo em uma pasta local do computador. Aguarde a conclusão da extração; não trabalhe dentro do ZIP.

2. Preserve os nomes e a estrutura interna das pastas. Não mova os dados ou os scripts individualmente.

3. Abra Modelagem.Rproj no RStudio. Isso permite que here localize a pasta do projeto e os dados corretamente.

4. Execute o bloco abaixo. A primeira linha deve indicar a pasta Modelagem e a segunda deve retornar TRUE.

```r
here::here()
dir.exists(here::here("Data"))
```

Não execute os scripts completos antes da orientação em aula. Para preparar o computador, basta instalar os pacotes, fazer os testes e conferir a pasta do projeto.

## Se encontrar dificuldades

- Erro de instalação ou carregamento: copie a primeira mensagem de erro e as linhas anteriores. Uma mensagem final sobre “non-zero exit status” costuma ser consequência de um erro informado antes.

- Falha no GitHub: confira a conexão e tente novamente mais tarde. Se aparecer limite de acesso da API, informe a equipe; não compartilhe senhas nem tokens.

- Falha em sf, terra ou units: se a mensagem mencionar `Cannot find proj.db`, siga a correção específica acima. Para outros erros, informe o sistema operacional, a versão do R e a mensagem completa; não altere caminhos de PROJ/GDAL por tentativa.

- Pacote antigo já instalado: o primeiro bloco instala apenas os ausentes. Se os testes apontarem uma versão incompatível, reinicie o R e solicite orientação para atualizar o pacote indicado.

- Teste do Pandoc falhou: confirme que o comando foi executado no RStudio Desktop e reinicie o programa. Se persistir, envie o resultado do teste à equipe.

Para diagnóstico, execute sessionInfo() e envie a saída junto com a mensagem de erro para george.amaro@embrapa.br. Informe também seu sistema operacional.

## Links de apoio

[Rtools para Windows — escolha conforme a versão do R](https://cran.r-project.org/bin/windows/Rtools/)

[Ferramentas de compilação para R no macOS](https://mac.r-project.org/tools/)

[Documentação do RStudio — instalação e uso](https://docs.posit.co/ide/user/)

As instruções e os requisitos dos instaladores foram consultados em 21 de outubro de 2026. Para manter a turma com um ambiente consistente, evite atualizar R e pacotes durante o curso sem orientação.
