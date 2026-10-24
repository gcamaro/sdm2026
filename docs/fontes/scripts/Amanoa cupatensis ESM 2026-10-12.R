###
# Curso SDM
# Projeto eNichos
# Embrapa / UFVJM / UFSC
# II SIMC - Diamantina (MG)
# George Amaro
# Licença: CC BY-NC-SA 4.0
# 2026-10-12
# Ensemble of Small Models
###


# Parametrização ------------------------------------------------------------------------------

my.species <- "Amanoa cupatensis"
# Área nativa
# Países considerados nativos nesta análise
native.range <- c("Brazil", "Colombia", "Venezuela")

# Rótulos para áreas nativas e invadidas
native.range.label     <- "native"
non.native.range.label <- "non-native"

# Última atualização das ocorrências (Excel: AAAA-MM-DD)
data.last.update <- ""

# Fontes de dados
use.excel   <- FALSE
use.GBIF    <- TRUE

if (!use.excel && !use.GBIF) stop("Ative pelo menos uma fonte de ocorrências.")

# Variáveis preditoras adicionais
use.ELEV    <- TRUE
use.SOIL    <- TRUE

# Pontos de ocorrências nos mapas dos modelos?
plot.occurrences <- TRUE


# Reprodutibilidade: antes de qualquer particionamento ou amostragem.
set.seed(1968)

# Carrega Pacotes -----------------------------------------------------------------------
# Instala Package Management Toll (pacman)
if (!require("pacman")) install.packages("pacman")

# Carrega os pacotes básicos necessários
# CRAN
pacman::p_load(
  ggplot2, sf, tmap, CoordinateCleaner, geodata, dplyr, rnaturalearth,
  corrplot, RColorBrewer, terra, scales, geobr, countrycode, gt, formattable,
  tidyr, flextable, bdc, rgbif, openxlsx2, sp, here, ragg
)

# GitHub
# remotes::install_github("sjevelazco/flexsdm")
pacman::p_load_gh(
  "sjevelazco/flexsdm",
  "ropensci/rnaturalearthhires",
  "antadde/covsel"
)


# Definições do Ambiente ----------------------------------------------------------------------

# Ajusta terra e GDAL
proj.path <- system.file("proj", package = "terra")
stopifnot(file.exists(file.path(proj.path, "proj.db")))

terra::projPaths(proj.path)
terra::projPaths(proj.path, with_proj = FALSE)

# Projeções para os mapas
wgs84.proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
wgs84.crs.geo <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Opções de formatação de tabelas (flextable)
flextable::set_flextable_defaults(font.size = 10,
                                  padding.top = 1,
                                  padding.bottom = 1,
                                  padding.left = 1,
                                  padding.right = 1)

# Opções cosméticas para Graficos, tabelas e mapas
theme_custom <- function(base_size, ...) {
  ggplot2::theme_light(base_size = base_size, ...) +
    ggplot2::theme(
      plot.title = element_text(face = "bold"),
      legend.position = "right"
    )
}

ggplot2::theme_set(theme_custom(base_size = 12))
ggplot2::update_geom_defaults("line", list(size = 0.5))

tmap_options(
  legend.na.show = FALSE,
  legend.reverse = TRUE,
  component.autoscale = FALSE
)

tmap_mode("plot")

color.basic      <- RColorBrewer::brewer.pal(n = 10, name = "Set3")
color.model      <- colorRampPalette(c("white", "deepskyblue3", "yellow", "darkorange", "red"))
color.bin        <- c("white", "red")
color.barplot    <- c("#FF8C00", "#A034F0", "#159090", "#E16A86", "#909800", "#9183E6", "#440154", "#C800C8", "#B2DF8A", "#FDBF6F")
palette_01       <- c("#E66A00", "#F39C12", "#F7C843", "#8BC34A", "#2E8B6F", "#0B5D4A")

# Variáveis para numerar os resultados
count.plot  <- 1
count.table <- 1
count.map   <- 1

# Pastas de trabalho e variáveis ambientais
work.folder             <- here::here(my.species)
data.folder             <- here::here("Data")
NE.50m.physical.folder  <- file.path(data.folder, "50m_physical")
NE.50m.cultural.folder  <- file.path(data.folder, "50m_cultural")
Soil.folder             <- file.path(data.folder, "soil_world")
CMIP6.folder            <- file.path(data.folder, "CMIP6")
WWFEco.folder           <- file.path(data.folder, "Ecoregions/TerrestrialEcos")
Koeppen.folder          <- file.path(data.folder, "Koeppen 1986-2010")
NE.folder               <- file.path(data.folder, "Natural Earth")
SHP.folder              <- file.path(data.folder, "Shapes")

# Pastas para resultados
results.folder          <- file.path(work.folder, paste0("Results ", format(Sys.time(), "%Y-%m-%d %Hh%Mm%Ss")))
sdm.folder              <- file.path(results.folder, "SDM")

# Arquivos de dados
# Excel com ocorrências NÃO VALIDADAS
# Data da última atualização (AAAA-MM-DD)
data.file        <- paste0(my.species, " Ocorrencia ", data.last.update, ".xlsx")

# Classes KG - https://koeppen-geiger.vu-wien.ac.at 
Koeppen2.file    <- file.path(Koeppen.folder, "KG_1986-2010.grd")

# Ecorregiões
# WWW Ecorregiões 2017
WWFEco.file      <- file.path(WWFEco.folder, "wwf_terr_ecos.shp")

# SHP Natural Earth (https://www.naturalearthdata.com)
ocean.shp <- ne_download(
  scale       = 10,
  type        = "ocean",
  category    = "physical",
  returnclass = "sf",
  destdir     = NE.folder
)

rivers.shp <- ne_download(
  scale       = 10,
  type        = "rivers_lake_centerlines",
  category    = "physical",
  returnclass = "sf",
  destdir     = NE.folder
)

lakes.shp <- ne_download(
  scale       = 10,
  type        = "lakes",
  category    = "physical",
  returnclass = "sf",
  destdir     = NE.folder
)

world <- ne_download(
  scale       = 10,
  type        = "admin_0_countries",
  category    = "cultural",
  returnclass = "sf",
  destdir     = NE.folder
) %>% 
  dplyr::filter(TYPE != "Indeterminate")

estados <- ne_download(
  scale       = 10,
  type        = "admin_1_states_provinces",
  category    = "cultural",
  returnclass = "sf",
  destdir     = NE.folder
)

# Mapas do Brasil
# Levels: 0=Brazil; 1=state; 2=municipality
# SHP IPEA / IBGE

file.name <- file.path(SHP.folder, "br1.br.gpkg")
if (!file.exists(file.name)) {
  BR1 <- geobr::read_state(year = 2020, simplified = TRUE)
} else {
  BR1 <- sf::read_sf(file.name)
}

BR1       <- sf::st_transform(BR1, 4326)
mask.BR   <- terra::vect(BR1$geom)
extent.BR <- as.vector(sf::st_bbox(BR1))[c(1, 3, 2, 4)]

file.name <- file.path(SHP.folder, "br2.br.gpkg")
if (!file.exists(file.name)) {
  BR2 <- geobr::read_municipality(year = 2020, simplified = TRUE)
} else {
  BR2 <- sf::read_sf(file.name)
}

BR2 <- sf::st_transform(BR2, 4326)

# Mapa de fundo para a terra
earth <- terra::rast(file.path(NE.folder, "Earth/NE1_LR_LC_SR_W_DR.tif"))
earth <- terra::aggregate(earth, fact = 2)

# Remove Antárdida das projeções globais
predictors.crop <- terra::ext(-180, 180, -59, 90)

# Verifica e cria diretórios necessários
setwd(work.folder)

dir.create(results.folder, showWarnings = FALSE, recursive = TRUE)
dir.create(sdm.folder,    showWarnings = FALSE, recursive = TRUE)


# Cria dataframe para relatório do modelo
report.occ.df <- NULL
report.occ.df <- data.frame(
  Occurrences = character(),
  Native      = integer(),
  Invasive    = integer(),
  Total       = integer(),
  Obs         = character()
)


# Dados de Ocorrência ------------------------------------------------------------

# Dados compilados a partir de publicações e pesquisas de campo
# Devem seguir o padrão da planilha excel do projeto
if (use.excel) {
  
  Excel.data <- openxlsx2::wb_to_df(data.file, 
                                    row_names = FALSE,
                                    col_names = TRUE,
                                    detect_dates = TRUE,
                                    skip_empty_rows = TRUE)
  
  presence.points.Excel <- as.data.frame(Excel.data)
  
  presence.points.Excel <- presence.points.Excel %>%
    dplyr::mutate(source = "Excel")
  
  colnames(presence.points.Excel) <- c("taxonName", "decimalLongitude", "decimalLatitude", "establishmentMeans", "year", "country", "stateProvince", "countyMunicipality", "locality", "references", "source")
  
  presence.points.Excel$decimalLongitude   <- as.double(presence.points.Excel$decimalLongitude)
  presence.points.Excel$decimalLatitude    <- as.double(presence.points.Excel$decimalLatitude)
  presence.points.Excel$year               <- as.integer(presence.points.Excel$year)
  presence.points.Excel$establishmentMeans <- as.character(presence.points.Excel$establishmentMeans)
  presence.points.Excel$locality           <- as.character(presence.points.Excel$locality)
  presence.points.Excel$country            <- as.character(presence.points.Excel$country)
  presence.points.Excel$countyMunicipality <- as.character(presence.points.Excel$countyMunicipality)
  presence.points.Excel$stateProvince      <- as.character(presence.points.Excel$stateProvince)
  presence.points.Excel$references         <- as.character(presence.points.Excel$references)
  
  # Remove registros sem referências (não deveriam estar aqui!)
  presence.points.Excel <-
    presence.points.Excel %>%
    filter(
      !is.na(references),
      trimws(references) != ""
    )
  
  report.occ.df <- report.occ.df %>% 
    add_row(Occurrences = "RAW Excel data",
            Total = nrow(presence.points.Excel))
  
  rm(Excel.data)
  
}


# Pontos de presença GBIF (rgbif)
if (use.GBIF) {
  
  file.name <- paste0(my.species, " GBIF COL-XR.Rda")
  
  checklist.key <- "7ddf754f-d193-4cc9-b351-99906754a03b" # COL-XR
  
  taxon.match <- rgbif::name_backbone(
    name = my.species,
    rank = "SPECIES",
    checklistKey = checklist.key,
    strict = TRUE
  )
  
  # strict = TRUE impede correspondência com um nível taxonômico superior,
  # mas ainda permite correspondência aproximada (FUZZY).
  
  # Opcional: descomente para aceitar apenas correspondências exatas.
  # if (!identical(taxon.match$matchType, "EXACT")) {
  #   stop("Correspondência taxonômica não exata. Confira taxon.match.")
  # }
  
  if (!identical(taxon.match$matchType, "EXACT")) {
    warning("Correspondência taxonômica não exata: confira taxon.match antes de interpretar os registros.")
  }

  taxon <- taxon.match$usageKey
  
  if (length(taxon) != 1L || is.na(taxon)) {
    stop("Espécie não encontrada no COL-XR. Confira taxon.match.")
  }
  
  if (file.exists(file.name)) {
    
    presence.points.GBIF <- readRDS(file.name)
    
  } else {
    
    # Set user, password and e-mail in .Renviron
    GBIF.download <- as.vector(
      occ_download(
        pred("taxonKey", taxon),
        pred("hasCoordinate",TRUE),
        pred("hasGeospatialIssue",FALSE),
        pred("occurrenceStatus","PRESENT"),
        # pred_gte("year", 1965),
        # pred_lt("coordinateUncertaintyInMeters", 1000),
        # pred_in("basisOfRecord", "PRESERVED_SPECIMEN"),
        pred_in("basisOfRecord",
                c("HUMAN_OBSERVATION",
                  "PRESERVED_SPECIMEN",
                  "MATERIAL_SAMPLE",
                  "OCCURRENCE")),
        # pred_not(pred_in("institutionCode", c("iNaturalist"))),
        # pred_not(pred_in("mediatype", "StillImage")),
        # pred_in("establishmentMeans", "native"),
        checklistKey = checklist.key,
        format = "SIMPLE_CSV"      )
    )
    
    occ_download_wait(GBIF.download)
    GBIF.data <- occ_download_get(GBIF.download, overwrite = TRUE) %>% occ_download_import()
    
    # Histograma de incerteza das coordenadas
    if (length(GBIF.data$coordinateUncertaintyInMeters[!is.na(GBIF.data$coordinateUncertaintyInMeters)]) > 0) {
      plot.name <- paste0("Plot ", count.plot, " - GBIF Coordinate Uncertainty.png")
      plot.name <- file.path(sdm.folder, plot.name)
      
      png(filename = plot.name, units = "cm", width = 26, height = 16, res = 400)
      
      hist(
        GBIF.data$coordinateUncertaintyInMeters[!is.na(GBIF.data$coordinateUncertaintyInMeters)],
        breaks = 20,
        xlab = "Coordinate Uncertainty (m)",
        main = "",
        col = "#6496FF"
      )
      
      dev.off()
      count.plot = count.plot + 1
    }
    
    # Correspondência entre as colunas padronizadas e os campos do GBIF
    gbif.columns <- c(
      taxonName          = "scientificName",
      decimalLongitude   = "decimalLongitude",
      decimalLatitude    = "decimalLatitude",
      establishmentMeans = "establishmentMeans",
      year               = "year",
      country            = "country",
      stateProvince      = "stateProvince",
      countyMunicipality = "county",
      locality           = "locality",
      references         = "references"
    )
    
    presence.points.GBIF <- as.data.frame(GBIF.data)
    
    # Acrescenta campos que não vieram no download
    for (column in setdiff(unname(gbif.columns), names(presence.points.GBIF))) {
      presence.points.GBIF[[column]] <- NA_character_
    }
    
    presence.points.GBIF <- presence.points.GBIF[, unname(gbif.columns)]
    colnames(presence.points.GBIF) <- names(gbif.columns)
    
    presence.points.GBIF <- presence.points.GBIF |>
      dplyr::mutate(
        dplyr::across(
          -c(decimalLongitude, decimalLatitude, year),
          as.character
        ),
        decimalLongitude = as.double(decimalLongitude),
        decimalLatitude  = as.double(decimalLatitude),
        year             = as.integer(year),
        source           = "GBIF"
      )
    
    saveRDS(presence.points.GBIF, file.name)
    
    # Retorno é invisível; usamos apenas pelo efeito de escrever arquivo
    openxlsx2::write_xlsx(GBIF.data, file = paste0(my.species, " GBIF Original Data.xlsx"))
    
    # Se for primeiro download, grava citação em um arquivo TXT
    citation.file <- paste0(my.species, " GBIF COL-XR Citation.txt")
    
    if (file.exists(citation.file)) {
      GBIF.citation <- readLines(citation.file)
    } else {
      GBIF.citation <- gbif_citation(GBIF.download)
      GBIF.citation <- GBIF.citation$download
      file.create(citation.file)
      fileConn <- file(citation.file)
      writeLines(GBIF.citation, fileConn)
      close(fileConn)
    }
    


  }

  # O cache também deve registrar procedência e citação no relatório desta execução.
  citation.file <- paste0(my.species, " GBIF COL-XR Citation.txt")
  GBIF.citation <- if (file.exists(citation.file)) readLines(citation.file, warn = FALSE) else
    "Citação indisponível no cache: recuperar DOI/metadados do download original."
  report.occ.df <- report.occ.df %>%
    add_row(Occurrences = "GBIF data", Total = nrow(presence.points.GBIF),
            Obs = paste("Taxonomy queried: COL-XR; Checklist Key:", checklist.key,
                        "; current matched Taxon Key:", taxon,
                        "; cached records retain their original download taxonomy;",
                        paste(GBIF.citation, collapse = " ")))
}


# Dataset final de ocorrências
presence.points <- NULL

if (use.excel && nrow(presence.points.Excel) > 0) {
  presence.points <- dplyr::bind_rows(presence.points, presence.points.Excel) 
}

if (use.GBIF && nrow(presence.points.GBIF) > 0) {
  presence.points <- dplyr::bind_rows(presence.points, presence.points.GBIF) 
}

report.occ.df <- report.occ.df %>%
  add_row(Occurrences = "RAW initial occurrence dataset",
          Total = nrow(presence.points),
          Obs = "Before cleaning")

if (is.null(presence.points) || nrow(presence.points) == 0L) {
  stop("Nenhuma ocorrência disponível. Confira as fontes de dados.")
}

# Ajuste e Limpeza dos Dados ------------------------------------------------------------------

# # Remove ocorrências sem informação do ano
# presence.points <- presence.points %>%
#   filter(!is.na(year))
# report.occ.df <- report.occ.df %>%
#   add_row(Occurrences = "Cleaning occurrence dataset",
#           Total = nrow(presence.points),
#           Obs = "Removed: occurrences without year")

# # Limite mínimo do ano (Worldclim data)
# first.year <- 1965
# presence.points <- presence.points[presence.points$year >= first.year, ]
# report.occ.df <- report.occ.df %>%
#   add_row(Occurrences = "Cleaning occurrence dataset",
#           Total = nrow(presence.points),
#           Obs = paste("Removed: occurrences before", first.year))

# Ajusta coordenadas
# Remove ocorrências sem coordenadas
flags <- NULL
flags <- bdc_coordinates_empty(
  presence.points,
  lat = "decimalLatitude",
  lon = "decimalLongitude"
)

flags <- bdc_summary_col(data = flags)
presence.points <- presence.points[flags$.summary, ]
report.occ.df <- report.occ.df %>%
  add_row(Occurrences = "Cleaning occurrence dataset",
          Total = nrow(presence.points),
          Obs = "Removed: occurrences without coordinates")

# Remove ocorrências com coordenadas fora dos limites
flags <- NULL
flags <- bdc_coordinates_outOfRange(
  presence.points,
  lat = "decimalLatitude",
  lon = "decimalLongitude"
)

flags <- bdc_summary_col(data = flags)
presence.points <- presence.points[flags$.summary, ]
report.occ.df <- report.occ.df %>%
  add_row(Occurrences = "Cleaning occurrence dataset",
          Total = nrow(presence.points),
          Obs = "Removed: occurrences out of range")

# Detalhe numérico mínimo: 2 casas decimais (~1,11 km em latitude).
# Casas decimais não comprovam acurácia ou incerteza posicional.
# Conferir coordinateUncertaintyInMeters nos dados brutos e revisar os registros.
# A resolução de 2,5 minutos corresponde a ~4,63 km em latitude.
.ndec  <- 1
flags <- NULL
flags <- bdc_coordinates_precision(
  presence.points,
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  ndec = .ndec
)

flags <- bdc_summary_col(data = flags)
presence.points <- presence.points[flags$.summary, ]
report.occ.df <- report.occ.df %>%
  add_row(Occurrences = "Cleaning occurrence dataset",
          Total = nrow(presence.points),
          Obs = paste("Removed: occurrences with precision <", .ndec))

# Adiciona um identificar único a cada registro
presence.points <- presence.points %>% 
  mutate(row_id = row_number()) %>%
  relocate(row_id, .before = taxonName) 


# Ajusta e corrige nomes dos países
# Atribui ocorrências ao limite de país (apenas pontos dentro dos limites)
# Nomes dos países a partir das coordenadas
presence.points$country <- NA

presence.points <- bdc_country_from_coordinates(
  data = presence.points,
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  country = "country"
)

# Padronização do nome e código dos países
presence.points <- bdc_country_standardized(
  data = presence.points,
  country = "country"
)

# Mantem apenas uma informação de país
presence.points$country <- presence.points$country_suggested
presence.points$country_suggested <- NULL

presence.points <- presence.points %>%
  relocate(countryCode, .after = country) 

# Remove ocorrências sem identificação do país
# ou na Antarctica (provável erro!)
presence.points <- presence.points %>%
  filter(!is.na(country) & country != "Antarctica")

report.occ.df <- report.occ.df %>%
  add_row(Occurrences = "Cleaning occurrence dataset",
          Total = nrow(presence.points),
          Obs = "Adjusted: country names")

# Identifica e corrige outros problemas
# Pre-compute o shapefile para evitar download duplo dentro de clean_coordinates()
.ne_countries_large <- rnaturalearth::ne_countries("large", returnclass = "sf")

flags <- NULL
flags <- CoordinateCleaner::clean_coordinates(
  x = presence.points,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  countries = "country",
  species = "taxonName",
  tests = c("capitals",
            "centroids", 
            "duplicates", 
            "equal", 
            "gbif", 
            "validity", 
            # "seas",
            "institutions",
            "zeros"),
  capitals_rad = 2000,
  centroids_rad = 2000,
  inst_rad = 1000,
  centroids_detail = "both",
  seas_scale = 110,
  country_refcol = "countryCode",
  seas_ref    = .ne_countries_large,
  country_ref = .ne_countries_large
)
rm(.ne_countries_large)

flags <- bdc_summary_col(data = flags)
summary(flags)

bdc_quickmap(
  data = dplyr::arrange(flags, .summary),
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  col_to_map = ".summary",
  size = 0.9
)

# Remove pontos de presença com problemas
presence.points.clean.no.filter <- presence.points[flags$.summary, ]
rm(flags, presence.points)

report.occ.df <- report.occ.df %>%
  add_row(Occurrences = "Cleaning occurrence dataset",
          Total = nrow(presence.points.clean.no.filter),
          Obs = "Adjusted: several issues")



if (nrow(presence.points.clean.no.filter) == 0L) {
  stop("Nenhuma ocorrência restou após a limpeza. Rever os dados e os filtros.")
}

# Histograma de ocorrências/ano
n.total   <- nrow(presence.points.clean.no.filter)
n.missing <- sum(is.na(presence.points.clean.no.filter$year))
p.missing <- 100 * n.missing / n.total

if (any(is.finite(presence.points.clean.no.filter$year))) {
# Limites e quebras do eixo X
x.min <- floor(min(presence.points.clean.no.filter$year, na.rm = TRUE) / 20) * 20
x.max <- ceiling(max(presence.points.clean.no.filter$year, na.rm = TRUE) / 20) * 20
x.breaks <- seq(x.min, x.max, by = 20)

# Gráfico
plot.name <- paste0("Plot ", count.plot, " - Histogram Occ per Year.png")
plot.name <- file.path(sdm.folder, plot.name)

png(filename = plot.name, units = "cm", width = 26, height = 16, res = 400)

ggplot(
  presence.points.clean.no.filter %>% filter(!is.na(year)),
  aes(x = year)
) +
  geom_histogram(
    aes(fill = after_stat(count)),
    breaks = pretty(presence.points.clean.no.filter$year, n = 20),
    color = "black"
  ) +
  scale_fill_gradientn(
    colors = palette_01,
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = x.breaks
  ) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    x = "Year", 
    y = "Frequency"
  ) +
  annotate(
    "text",
    x = -Inf, y = Inf,
    hjust = 0, vjust = 1,
    lineheight = 1.1,
    label = paste0(
      "Total records: ", comma(n.total), "\n",
      "Missing year: ", sprintf("%.1f%%", p.missing), " (", comma(n.missing), ")"
    )
  ) +
  theme_minimal()

dev.off()
count.plot = count.plot + 1
} else {
  warning("Sem anos de coleta válidos: histograma temporal não foi gerado.")
}


# Biogeo status -------------------------------------------------------------------
# Aproximação por países (considerando revisão bibliográfica)
lista.nativa <- countrycode::countrycode(native.range, origin = "country.name", destination = "iso2c")

# Classificação geográfica inferida por países, não verificação de estabelecimento/invasão.
# O campo original permanece nos arquivos brutos. Conferir cultivo, identificação e origem.
presence.points.clean.no.filter$establishmentMeans <- NA
presence.points.clean.no.filter$establishmentMeans[presence.points.clean.no.filter$countryCode %in% lista.nativa] <- native.range.label
presence.points.clean.no.filter$establishmentMeans[!presence.points.clean.no.filter$countryCode %in% lista.nativa] <- non.native.range.label


# Adiciona ao relatório
report.occ.df <- report.occ.df %>%
  add_row(Occurrences = "Clean, unfiltered, Biogeo adjusted",
          Native = nrow(filter(presence.points.clean.no.filter, establishmentMeans == native.range.label)),
          Invasive = nrow(filter(presence.points.clean.no.filter, establishmentMeans == non.native.range.label)),
          Total = nrow(presence.points.clean.no.filter),
          Obs = "Adjusted biogeo status")


# Remoção manual de ocorrências -----------------------------------------------------
# # Remove ocorrências identificadas com problemas
# # independente de qual conjunto de dados (Excel ou GBIF)
# presence.points.clean.no.filter <-
#   presence.points.clean.no.filter %>%
#   filter(!country == "CN")
# 
# # Adiciona ao relatório
# report.occ.df <- report.occ.df %>%
#   add_row(Occurrences = "Clean, unfiltered, Biogeo adjusted",
#           Native = nrow(filter(presence.points.clean.no.filter, establishmentMeans == native.range.label)),
#           Invasive = nrow(filter(presence.points.clean.no.filter, establishmentMeans == non.native.range.label)),
#           Total = nrow(presence.points.clean.no.filter),
#           Obs = "Manual adjustments")

# # Grava XLSX e aguarda conferência para prosseguimento
# file.name <- paste0(sdm.folder, "/", my.species, " Ocorrencias Limpas sem Filtro ", format(Sys.time(), "%Y-%m-%d"), ".xlsx")
# openxlsx2::write_xlsx(presence.points.clean.no.filter, file = file.name)


# Mapa Interativo de Verificação --------------------------------------------------------------
# Arquivo HTML gerado pode ser enviado por e-mail
# Variáveis genéricas para elaboração de mapas
points.tmp <- presence.points.clean.no.filter
points.tmp$Longitude <- points.tmp$decimalLongitude
points.tmp$Latitude <- points.tmp$decimalLatitude
points.map.W <- points.tmp
points.map.W <- sf::st_as_sf(points.map.W, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
points.map.W$map.species <- rep(my.species, nrow(points.map.W))

# Pontos do Brasil, preparados uma única vez para todos os mapas
points.map.BR <- terra::mask(terra::vect(points.map.W), mask.BR)

bbox_new <- st_bbox(points.map.W) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[1] <- bbox_new[1] - (0.05 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.05 * xrange) # xmax - right
bbox_new[2] <- bbox_new[2] - (0.05 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.05 * yrange) # ymax - top

occ.map <-
  # ocorrências
  tm_basemap() +
  tm_shape(points.map.W, is.main = TRUE, bbox = bbox_new) +
  tm_symbols(size = 0.5,
             lwd = 0.1,
             col = "black",
             fill = "map.species",
             fill_alpha = 0.8,
             fill.scale = tm_scale_categorical(
               values = "red",
               labels = my.species
             )
  ) 

file.name <- paste0("Map 00 - Species Initial Occurrences.html")
file.name <- file.path(sdm.folder, file.name)

suppressWarnings(
  tmap_save(
    tm = occ.map, filename = file.name)
)



# Variáves Preditoras -------------------------------------------------------------------------

# Environmental variables
# 1 minuto de arco = 1,852 km (uma milha náutica)
# 0,5 ~ 930m ~ 1km 
# 2,5 ~ 4,5 km (4,63 km)

predictors.other <- NULL
predictor.other.names <- NULL
predictors.other.labels.full <- NULL

# Clima presente Worldclim 2.1 (1970-2000) padrão
# A data da coleta pode não coincidir com essa climatologia. Justificar a janela temporal
# na pesquisa; os filtros por ano abaixo/acima são opcionais, não aplicados automaticamente.
predictors <- worldclim_global(var = "bio", res = 2.5, path = data.folder)
predictors <- crop(predictors, predictors.crop)

# Máscara das variáveis ambientais
predictors.mask <- terra::ifel(is.na(predictors[[1]]), NA, 1)

# Ajusta os nomes resumidos das variáveis
predictor.names <- c(
  "Bio01", "Bio02", "Bio03", "Bio04", "Bio05", "Bio06",
  "Bio07", "Bio08", "Bio09", "Bio10", "Bio11", "Bio12",
  "Bio13", "Bio14", "Bio15", "Bio16", "Bio17", "Bio18", 
  "Bio19"
)

predictors.names.full <- predictor.names

names(predictors) <- predictor.names

# Nomes longos das variáveis preditoras
predictors.labels.full <- c(
  "Annual Mean Temperature", "Mean Diurnal Range", "Isothermality", "Temperature Seasonality",
  "Maximum Temperature of Warmest Month", "Minimum Temperature of Coldest Month",
  "Temperature Annual Range", "Mean Temperature of Wettest Quarter",
  "Mean Temperature of Driest Quarter", "Mean Temperature of Warmest Quarter",
  "Mean Temperature of Coldest Quarter", "Annual Precipitation", "Precipitation of Wettest Month",
  "Precipitation of Driest Month", "Precipitation Seasonality", "Precipitation of Wettest Quarter",
  "Precipitation of Driest Quarter", "Precipitation of Warmest Quarter", 
  "Precipitation of Coldest Quarter"
)

# Outras variáveis preditoras
predictors.other <- NULL
predictors.other.labels.full <- NULL


# Elevação (DEM) — controlada por use.ELEV no script raíz
if (!exists("use.ELEV")) use.ELEV <- TRUE
if (use.ELEV) {
  # Usando o {geodata} - alguns problemas com valores negativos
  elevation    <- elevation_global(res = 2.5, path = data.folder)
  elevation    <- terra::crop(elevation, predictors.mask, snap = "near", mask = TRUE, threads = TRUE)
  
  if (!terra::compareGeom(elevation, predictors.mask, stopOnError = FALSE)) {
    elevation <- terra::project(elevation, predictors.mask, method = "bilinear")
  }

  predictors.other             <- elevation
  predictor.other.names        <- "Elev"
  names(predictors.other)      <- predictor.other.names
  predictors.other.labels.full <- "Elevation"
}

# # Produção de banana (SpamCrops)
# banana <- crop_spam("banana", var = "phys_area", path = data.folder) 
# banana <- banana$banana_phys_area_all
# banana <- terra::resample(banana, predictors.mask, method = "sum")
# banana <- terra::mask(banana, predictors.mask)
# 
# # Adiciona a variável ao conjunto de variáveis bioclimáticas
# predictors.other             <- c(predictors.other, banana)
# predictor.other.names        <- c(predictor.other.names, "Banana")
# 
# names(predictors.other)      <- predictor.other.names
# predictors.other.labels.full <- c(predictors.other.labels.full, "Banana Area (ha)")

# Finaliza variáveis adicionais
predictors.other.labels.full <- as.data.frame(cbind(names(predictors.other), predictors.other.labels.full))


# Solo (SoilGrids) — controlado por use.SOIL no script raíz
if (!exists("use.SOIL")) use.SOIL <- FALSE
predictors.soil <- NULL

if (use.SOIL) {
  soil.vars       <- c("bdod", "clay", "silt", "sand", "cfvo")
  soil.vars.names <- c("Bulk Density (kg dm-3", "Clay (%", "Silt (%", "Sand (%", "CFVO (% >2mm")
  depths          <- c(5, 15, 30, 60)
  depths.names    <- c(" 0-5cm)", " 5-15cm)", " 15-30cm)", " 30-60cm)")
  
  soil.list <- list()
  for (v in soil.vars) {
    for (d in depths) {
      message("Baixando: ", v, "-", d)
      r <- geodata::soil_world(var = v, depth = d, stat = "mean", path = data.folder)
      names(r) <- paste0(v, "_", gsub("-", "_", d))
      soil.list[[paste(v, d, sep = "_")]] <- r
    }
  }
  
  predictors.soil <- terra::rast(soil.list)
  predictors.soil <- terra::crop(predictors.soil, predictors.mask, snap = "near", mask = TRUE, threads = TRUE)
  
  predictors.soil <- terra::aggregate(
    predictors.soil,
    fact = 5,
    fun = "mean",
    na.rm = TRUE
  )
  if (!terra::compareGeom(predictors.soil, predictors.mask, stopOnError = FALSE)) {
    predictors.soil <- terra::project(predictors.soil, predictors.mask, method = "bilinear")
  }
  stopifnot(terra::compareGeom(predictors.soil, predictors.mask))
  
}

# Nomes das variáveis de solo
predictors.soil.labels.full <- NULL

if (use.SOIL && !is.null(predictors.soil)) {
  for (v in soil.vars.names) {
    for (d in depths.names) {
      predictors.soil.labels.full <- c(predictors.soil.labels.full, paste0(v, d))
    }
  }
  predictors.soil.labels.full <- as.data.frame(cbind(names(predictors.soil), predictors.soil.labels.full))
}


# Adiciona nomes aos labels das variáveis ambientais
predictors.all.labels.full <- as.data.frame(cbind(names(predictors), predictors.labels.full))
names(predictors.all.labels.full) <- c("Var", "Label")

if (!is.null(predictors.other.labels.full) && ncol(predictors.other.labels.full) > 0) {
  names(predictors.other.labels.full) <- c("Var", "Label")
  predictors.all.labels.full <- rbind(predictors.all.labels.full, predictors.other.labels.full)
}

if (!is.null(predictors.soil.labels.full) && ncol(predictors.soil.labels.full) > 0) {
  names(predictors.soil.labels.full) <- c("Var", "Label")
  predictors.all.labels.full <- rbind(predictors.all.labels.full, predictors.soil.labels.full)
}


# Nomes explícitos evitam depender da posição das colunas de metadados
predictors.env.names <- c(names(predictors), names(predictors.other), names(predictors.soil))

# Extrai dados ambientais (das variáveis preditoras) a partir das ocorrências 
presence.points.model <- presence.points.clean.no.filter %>%
  sdm_extract(
    data = .,
    x = "decimalLongitude",
    y = "decimalLatitude",
    env_layer = predictors,
    filter_na = FALSE
  )

# Variáveis complementares
if (use.ELEV) {
  presence_other <- presence.points.clean.no.filter %>%
    sdm_extract(
      data      = .,
      x         = "decimalLongitude",
      y         = "decimalLatitude",
      env_layer = predictors.other,
      filter_na = FALSE
    )
  presence.points.model <- cbind(presence.points.model, presence_other[, names(predictors.other), drop = FALSE])
}

# Variáveis físicas de solo
if (use.SOIL) {
  presence_soil <- presence.points.clean.no.filter %>%
    sdm_extract(
      data      = .,
      x         = "decimalLongitude",
      y         = "decimalLatitude",
      env_layer = predictors.soil,
      filter_na = FALSE
    )
  presence.points.model <- cbind(presence.points.model, presence_soil[, names(predictors.soil), drop = FALSE])
  
}

# Preserva ocorrências limpas, sem filtros, com as variáveis ambientais
presence.points.clean.env <- presence.points.model

# Remove eventuais NA nas covariáveis
presence.points.model <- presence.points.model %>%
  dplyr::filter(
    if_all(
      dplyr::all_of(predictors.env.names),
      ~ !is.na(.)
    )
  )

# Adiciona ao relatório
report.occ.df <- report.occ.df %>% 
  add_row(Occurrences = "Clean, unfiltered, validated, complete environmental data",
          Native = nrow(filter(presence.points.model, establishmentMeans == native.range.label)),
          Invasive = nrow(filter(presence.points.model, establishmentMeans == non.native.range.label)),
          Total = nrow(presence.points.model),
          Obs = paste0(length(predictors.env.names), " covariates added"))

# Ocorrências com NA que foram removidas (verificar)
# Tabela
tmp.table <- gt(anti_join(presence.points.clean.no.filter, presence.points.model))

file.name <- paste0("Table ", count.table, " - Occurrences without Envimental Data - VERIFY.docx")
file.name <- iconv(file.name, to = "ASCII//TRANSLIT")
file.name <- file.path(sdm.folder, file.name)
gtsave(data = tmp.table, filename = file.name)
count.table = count.table + 1

tmp.table

# Grava XLSX dos dados usados no Modelo
file.name <- paste0(sdm.folder, "/", my.species, " Presence Points Model ", format(Sys.time(), "%Y-%m-%d"), ".xlsx")
openxlsx2::write_xlsx(presence.points.model, 
                      file = file.name,
                      as_table = FALSE,
                      col_names = TRUE,
                      na.strings = "",
                      overwrite = TRUE
) 


# Estatísticas Descritivas e Gráficos de Correlação -------------------------------------------
# Tabela de Estatísticas Descritivas das Variáveis 
# nas ocorrências usados para modelagem (ocorrências limpas antes do filtro ambiental)
bio.stats <- NULL
bio.vars <- predictors.env.names
bio.stats <- sapply(
  presence.points.model[, bio.vars, drop = FALSE],
  function(x) {
    x <- suppressWarnings(as.numeric(x))  # se for factor/character com números
    c(
      min    = min(x, na.rm = TRUE),
      max    = max(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      mean   = mean(x, na.rm = TRUE),
      sd     = sd(x, na.rm = TRUE)
    )
  }
)

bio.stats <- as.data.frame(t(bio.stats))
bio.stats <- cbind(rownames(bio.stats), predictors.all.labels.full$Label, bio.stats)

tmp.table <- NULL
tmp.table <- 
  bio.stats %>%
  gt() %>%
  cols_label(
    "rownames(bio.stats)"	= md("**Variable**"),
    "predictors.all.labels.full$Label" = md("**Variable Name**"),
    min = md("**Minimum**"),
    max = md("**Maximun**"),
    median = md("**Median**"),
    mean = md("**Mean**"),
    sd = md("**SD**")
    # "vif.step@results$VIF[vif.step@results$Variables %in% predictors.keep]" = md("**VIF**")
  ) %>%
  fmt_number(
    columns = c("min", "max", "median", "mean", "sd"),
    decimals = 2
  )

file.name <- paste0("Table ", count.table, " - Occurrences Descriptive Statistics.docx")
file.name <- file.path(sdm.folder, file.name)
gtsave(data = tmp.table, filename = file.name)
count.table = count.table + 1

tmp.table

# Matriz e Grafico de correlações
presence.points.values <- as.data.frame(
  presence.points.model[, predictors.env.names, drop = FALSE]
)
presence.points.values <- na.omit(presence.points.values)

# # Retira variáveis categóricas
# presence.points.values <- within(presence.points.values, rm(HII))

# Matriz de correlações
m <- cor(presence.points.values, method = "pearson")

# Salva Grafico
file.name <- paste0("Plot ", count.plot, " - Pearson Correlation HClust.png")
file.name <- file.path(sdm.folder, file.name)
png(filename = file.name, units = "cm", width = 30, height = 30, res = 400)
count.plot = count.plot + 1

corrplot(m,
         method = "ellipse", type = "full",
         tl.col = "black",
         tl.cex = 1,
         col = COL2("PuOr"),
         # col = COL2('PiYG'),
         cl.pos = 'b', 
         cl.cex = 1.1,
         bg = "white",
         diag = TRUE,
         outline = FALSE,
         order = "hclust",
         hclust.method = "ward.D",
         # addrect = 4,
         rect.col = "navy"
)

dev.off()

file.name <- paste0("Plot ", count.plot, " - Pearson Correlation HClust Numbers.png")
file.name <- file.path(sdm.folder, file.name)
png(filename = file.name, units = "cm", width = 30, height = 30, res = 400)
count.plot = count.plot + 1

corrplot(m,
         method = "number", type = "lower",
         tl.col = "black",
         tl.cex = 1,
         col = COL2("PuOr"),
         cl.pos = 'b', 
         cl.cex = 1.1,
         bg = "white",
         diag = TRUE,
         outline = FALSE,
         order = "hclust",
         hclust.method = "ward.D",
         addrect = 4,
         rect.col = "navy",
         number.cex = 0.9,
)

dev.off()


# Filtro de Ocorrências -----------------------------------------------------------------------
# Redução do viés de amostragem (sampling bias)

# “The uniform sampling assumption does not require a uniformly random sample 
# from geographic space, but instead that environmental conditions are sampled 
# in proportion to their availability, regardless of their spatial pattern” 
# (Merow et al., 2013).

# Referências: 
# Radosavljevic and Anderson (2014)
# Lee‐Yaw et al. (2018) 
# Atwater et al. (2018)

# In general, model accuracy increases with sample size until 
# an asymptote is reached (Hirzel and Guisan, 2002; Stockwell and Peterson, 2002; 
# Hernandez et al., 2006; Wisz et al., 2008), often between, 20 and 30 
# presence records (Stockwell and Peterson, 2002).

# # Sem filtro
# presence.points.clean.filtered <- presence.points.model
# 
# # Adiciona ao relatório
# report.occ.df <- report.occ.df %>%
#   add_row(Occurrences = "Clean, filtered, validated, complete environmental data",
#           Native = nrow(filter(presence.points.clean.filtered, establishmentMeans == native.range.label)),
#           Invasive = nrow(filter(presence.points.clean.filtered, establishmentMeans == non.native.range.label)),
#           Total = nrow(presence.points.clean.filtered),
#           Obs = paste0("Without filter: too few points!"))

# # Filtro geográfico por distância (Km)
# # Diminui um pouco mais a amostra
# # Usa a resolução das variáveis preditoras
# res.km    <- ceiling(res(predictors.mask)*111.32)[1]
# distances <- seq(0, res.km * 5, res.km)[-1]
# 
# presence.points.clean.filtered <- occfilt_geo(
#   data = presence.points.model,
#   x = "decimalLongitude",
#   y = "decimalLatitude",
#   env_layer = c(predictors, predictors.other, predictors.soil),
#   method = c("defined", factor = distances),
#   reps = 10,
# )
# 
# presence.points.clean.filtered <- occfilt_select(
#   occ_list = presence.points.clean.filtered,
#   x = "decimalLongitude",
#   y = "decimalLatitude",
#   env_layer = c(predictors, predictors.other, predictors.soil),
#   filter_prop = TRUE
# )
# 
# selected <- presence.points.clean.filtered$filter_prop$filt_value[grepl("\\*", presence.points.clean.filtered$filter_prop$filt_value)]
# 
# presence.points.clean.filtered <- presence.points.clean.filtered[["occ"]]
# # Adiciona ao relatório
# report.occ.df <- report.occ.df %>%
#   add_row(Occurrences = "Clean, filtered, validated, complete environmental data",
#           Native   = nrow(filter(presence.points.clean.filtered, establishmentMeans == native.range.label)),
#           Invasive = nrow(filter(presence.points.clean.filtered, establishmentMeans == non.native.range.label)),
#           Total    = nrow(presence.points.clean.filtered),
#           Obs      = paste0("Distance selected (km): ", selected[1]))

# # Filtro geográfico por resolução dos preditores
# presence.points.clean.filtered <- occfilt_geo(
#   data = presence.points.model,
#   x = "decimalLongitude",
#   y = "decimalLatitude",
#   env_layer = c(predictors, predictors.other, predictors.soil),
#   method = c("cellsize", factor = seq(1, 15, 5)),
# )
# 
# presence.points.clean.filtered <- occfilt_select(
#   occ_list = presence.points.clean.filtered,
#   x = "decimalLongitude",
#   y = "decimalLatitude",
#   env_layer = c(predictors, predictors.other, predictors.soil),
#   filter_prop = TRUE
# )
# 
# selected <- presence.points.clean.filtered$filter_prop$filt_value[grepl("\\*", presence.points.clean.filtered$filter_prop$filt_value)]
# 
# presence.points.clean.filtered <- presence.points.clean.filtered[["occ"]]
# 
# # Adiciona ao relatório
# report.occ.df <- report.occ.df %>%
#   add_row(Occurrences = "Clean, filtered, validated, complete environmental data",
#           Native   = nrow(filter(presence.points.clean.filtered, establishmentMeans == native.range.label)),
#           Invasive = nrow(filter(presence.points.clean.filtered, establishmentMeans == non.native.range.label)),
#           Total    = nrow(presence.points.clean.filtered),
#           Obs      = paste0("Cell size selected: ", selected[1]))

# Filtro geográfico por resolução dos preditores
# Mantém apenas um registro por pixel (mais simples)
presence.points.clean.filtered <- occfilt_geo(
  data = presence.points.model,
  x = "decimalLongitude",
  y = "decimalLatitude",
  env_layer = c(predictors, predictors.other, predictors.soil),
  method = c("cellsize", factor = 1),
  prj = crs(predictors)
)

# Adiciona ao relatório
report.occ.df <- report.occ.df %>%
  add_row(Occurrences = "Clean, filtered, validated, complete environmental data",
          Native   = nrow(filter(presence.points.clean.filtered, establishmentMeans == native.range.label)),
          Invasive = nrow(filter(presence.points.clean.filtered, establishmentMeans == non.native.range.label)),
          Total    = nrow(presence.points.clean.filtered),
          Obs      = "Cell size: 1 (one record per pixel)")

# Existem ainda filtros no espaço ambiental
# Veja a documentação do {fexsdm}

# Área de Calibração -------------------------------------------------------------

# Definir área de calibração (região M do diagrama BAM)

# # Máscara baseada nos polígonos das zonas KG com ocorrências
# # accessible area approach - biogeographic entities (BE)
# koeppen.rast  <- rast(Koeppen2.file)
# koeppen.vect  <- as.polygons(koeppen.rast)
# # Retirar classe "Ocean" (32)
# KG.zones.mask <- koeppen.vect[!koeppen.vect$layer == 32]
# 
# KG.zones.mask <-
#   calib_area(
#     data = presence.points.clean.no.filter,
#     x = "decimalLongitude",
#     y = "decimalLatitude",
#     method = c("mask", KG.zones.mask, "layer"),
#     crs = wgs84.proj
#   )
# names(KG.zones.mask) <- "Area"
# 
# plot(KG.zones.mask)


# # Máscara baseada nos polígonos WWF ecoregions com ocorrências
# # accessible area approach - biogeographic entities (BE)
# # Interseção com pontos de presença
# # Classificação WWF das ecorregiões
# ecor.vect <- terra::vect(WWFEco.file, layer = "wwf_terr_ecos")
# 
# WWF.zones.mask <-
#   calib_area(
#     data = presence.points.clean.no.filter,
#     x = "decimalLongitude",
#     y = "decimalLatitude",
#     method = c("mask", ecor.vect, "ECO_NAME"),
#     crs = wgs84.proj
#   )
# names(WWF.zones.mask) <- "Area"
# 
# plot(WWF.zones.mask)

# # Adiciona buffer
# # O CRS WGS84 usa graus, então precisamos projetar para metros antes
# # Escolha um CRS métrico adequado à região; EPSG:3857 distorce distâncias.
# # O exemplo abaixo deve ser adaptado antes de uso científico.
# WWF.zones.mask.proj <- terra::project(WWF.zones.mask, "EPSG:3857")
# # Criar buffer
# WWF.zones.mask.buffer <- terra::buffer(WWF.zones.mask.proj, width = 50000)
# # Retornar ao CRS original (WGS84)
# WWF.zones.mask.buffer <- terra::project(WWF.zones.mask.buffer, wgs84.proj)
# # Agregar em apenas um polígono
# WWF.zones.mask.buffer <- terra::aggregate(WWF.zones.mask.buffer, dissolve = T)
# 
# # Visualizar resultado
# plot(WWF.zones.mask, border = "blue")
# plot(WWF.zones.mask.buffer, border = "red", add = TRUE)


# # Cria máscara com retângulo envolvente das ocorrências
# # Considera áreas administrativas (simple approach - political divisions (PD))
# presence.points.mask <- presence.points.clean.no.filter
# coordinates(presence.points.mask) <- c("decimalLongitude", "decimalLatitude")
# proj4string(presence.points.mask) <- wgs84.crs.geo
# 
# presence.points.mask <- as.polygons(ext(presence.points.mask))
# presence.points.mask <- as.polygons(crop(predictors.mask, presence.points.mask) > -Inf)
# crs(presence.points.mask) <- wgs84.proj
# names(presence.points.mask) <- "Area"
# 
# plot(presence.points.mask)


# # Máscara baseada em uma área geográfica
# # Considera áreas administrativas (simple approach - political divisions (PD))
# # Região de ocorrência e principal região de interesse em projetar o modelo
# 
# # continents <- c("Europe", "North America")
# # GEO <- ne_countries(country = countries, scale = 10, returnclass = "sf")
# countries <- c("Brazil")
# GEO <- ne_countries(country = countries, scale = 10, returnclass = "sf")
# GEO.mask <- vect(GEO$geometry)
# GEO.mask <- terra::mask(predictors.mask, GEO.mask)
# GEO.mask <- crop(GEO.mask, GEO)
# GEO.mask <- as.polygons(GEO.mask)
# names(GEO.mask) <- "Area"
# 
# plot(GEO.mask)


# Cria a área de Calibração
# flexsdm
calibration.area <-
  calib_area(
    data = presence.points.clean.no.filter,
    x = "decimalLongitude",
    y = "decimalLatitude",
    # method = c("mask", KG.zones.mask, "Area"),
    # method = c("mask", WWF.zones.mask.buffer, "Area"),
    # method = c("mask", WWF.zones.mask, "Area"),
    # method = c("mask", presence.points.mask, "Area"),
    # method = c("mask", OCC.mask, "Area"),
    # method = c("buffer", width = 300000),
    method = c('bmcp', width = 50000),
    # method = "mcp",
    # groups = "type",
    crs = wgs84.proj
  )

# Define uma máscara com base na área de calibração
calibration.mask <- terra::crop(predictors.mask, calibration.area, mask = T)
calibration.mask.poly <- as.polygons(
  calibration.mask, 
  round = TRUE, 
  aggregate = TRUE,
  values = TRUE,
  na.rm = TRUE
)

# # Máscara final da Área de Calibração que será utilizada no modelo
plot(calibration.mask, legend = FALSE, col = "grey")
points(presence.points.clean.no.filter[, c("decimalLongitude", "decimalLatitude")], pch = 20, cex = 0.8, col = "red")
plot(world$geom, add = TRUE)

# Área da área de calibração em km2
calibration.area.km <- expanse(calibration.mask, "km")
message("Calibration Area: ", format(calibration.area.km, nsmall = 2, big.mark = ","), " Km2")

# Adiciona ao relatório
# Descrever o método em Obs, abaixo
report.occ.df <- report.occ.df %>% 
  add_row(Occurrences = "Calibration área",
          Native = nrow(filter(presence.points.clean.no.filter, establishmentMeans == native.range.label)),
          Invasive = nrow(filter(presence.points.clean.no.filter, establishmentMeans == non.native.range.label)),
          Total = nrow(presence.points.clean.no.filter),
          Obs = paste0("BMCP with 50k buffer ", calibration.area.km, " Kmˆ2."))



# Mapa de Ocorrências -----------------------------------------------------------
# Com pontos limpos, sem filtro
# Variáveis genéricas para elaboração de mapas
bbox.map   <- sf::st_bbox(calibration.mask)
# Margem aditiva mantém a ordem dos limites em ambos os hemisférios.
bbox.map[c(1, 3)] <- bbox.map[c(1, 3)] + c(-1, 1) * 0.05 * (bbox.map[3] - bbox.map[1])
bbox.map[c(2, 4)] <- bbox.map[c(2, 4)] + c(-1, 1) * 0.05 * (bbox.map[4] - bbox.map[2])
asp.map    <- ceiling((bbox.map$xmax - bbox.map$xmin) / (bbox.map$ymax - bbox.map$ymin))

presence.map <-
  # mapa do mundo (vegetação, RGB)
  # com crop para a região 
  tm_shape(crop(earth, bbox.map), is.main = TRUE) +
  # tm_shape(earth) +
  tm_rgb(col = tm_vars(c(1, 2, 3), multivariate = TRUE)) + 
  # Contorno dos países
  tm_shape(world) +
  tm_polygons(col = "black", fill = NULL, lwd = 0.6) +
  # Contorno do Brasil
  tm_shape(BR1) +
  tm_polygons(col = "black", 
              fill = NULL, 
              lwd = 0.6) + 
  tm_text("name_state", 
          size = 0.45, 
          fontface = "bold") +
  # complementos do mapa
  tm_graticules(alpha = 0.5, labels.size = 1) +
  tm_compass(
    type = "arrow", 
    size = 1.5,
    position = tm_pos_in(pos.h = "LEFT", pos.v = "BOTTOM", align.h = "center")
  ) +
  tm_credits(
    "Geographic coordinates\nDatum: WGS84",
    position = tm_pos_in(pos.h = "LEFT", pos.v = "BOTTOM", align.h = "center"),
    size = 0.6,
    fontface = "bold",
    col = "black",
    bg.color = "white",
    bg.alpha = 0.6,
    padding = 0.2
  ) +
  tm_scalebar(
    # breaks = c(0, 200, 400, 1200),
    width = 14,
    position = tm_pos_in(pos.h = "LEFT", pos.v = "BOTTOM", align.h = "center")
  ) +
  tm_layout(
    scale = 1
  ) +
  tm_crs(4326) 

# Adiciona ocorrências, se existirem
if (nrow(points.map.W) != 0 & plot.occurrences) {
  presence.map <- presence.map +
    # Ocorrências 
    tm_shape(points.map.W) +
    tm_symbols(size = 0.25,
               lwd = 0.15,
               col = "black",
               fill = "red",
               fill_alpha = 1
    )
}

file.name <- paste0("Map " , count.map, " - Species Occurrences Data.png")
file.name <- file.path(sdm.folder, file.name)
count.map = count.map + 1

suppressWarnings(
  tmap_save(
    tm = presence.map, filename = file.name,
    units = "cm", dpi = 400,
    device = ragg::agg_png
  )
)


# Particionamento -----------------------------------------------------------------------------

# Particionamento das ocorrências (treinamento/teste do modelo) 
# Adiciona uma coluna com valor 1, indicando serem observações de ocorrências
presence.points.clean.filtered$pr_ab <- 1

# Define o número de partições
n_groups <- 4   

# Define o grid para spatial block cross-validation
# Resolução do raster ambiental: 0.04166667 x 0.04166667
# 1 grau no equador: 111,32 km
# Resultado = cell_size * 111.32
set.seed(1968)
presence.points.partition <- presence.points.clean.filtered %>%
  part_sblock(
    data = .,
    env_layer = c(predictors, predictors.other, predictors.soil),    
    pr_ab = "pr_ab",
    x = "decimalLongitude",
    y = "decimalLatitude",
    n_part = n_groups,
    min_res_mult = 0.5/res(predictors.mask)[1],
    max_res_mult = 10/res(predictors.mask)[1], 
    # min_occ = ceiling(log2(nrow(presence.points.clean.filtered))),
    min_occ = 2,
    num_grids = 20,
    prop = 1
  )

presence.points.model <- presence.points.partition$part

# Cria um raster a partir da melhor divisão de blocos
.env_layers <- c(predictors, predictors.other, predictors.soil)

partition.grid <- get_block(
  env_layer = .env_layers,   
  best_grid = presence.points.partition$grid)

plot(crop(partition.grid, bbox.map), 
     col = RColorBrewer::brewer.pal(n_groups,"BrBG")
)
points(presence.points.clean.filtered[, c("decimalLongitude", "decimalLatitude")], pch = 20, cex = 1, col = "red")
plot(world$geom, add = TRUE)


# Background e Pseudoausências ----------------------------------------------------------------------------------

# Background (com as partições já definidas)
n.pres <- nrow(presence.points.model)

number.bg <- 10000
number.pa <- dplyr::case_when(
  n.pres < 20  ~ 10 * n.pres,
  n.pres < 50  ~ 5 * n.pres,
  n.pres < 200 ~ 3 * n.pres,
  TRUE         ~ 2 * n.pres
)

number.pa <- max(100, min(10000, number.pa))

set.seed(1968)

# Background aleatório dentro de cada fold; quantidade proporcional às presenças do fold.
# Essa alocação não equivale a uma amostra uniforme por área em todo o domínio.
# Random background
background.points <- lapply(1:n_groups, function(k) {
  sample_background(
    data = presence.points.model,
    x = "x",
    y = "y",
    n = round(sum(presence.points.model$.part == k) * number.bg / nrow(presence.points.model)),
    method = "random",
    rlayer = partition.grid,
    maskval = k,
    calibarea = calibration.area
  )
}) %>%
  bind_rows()

# # Random background com distância
# background.points <- lapply(1:n_groups, function(k) {
#   flexsdm::sample_pseudoabs(
#     data = presence.points.model,
#     x = "x",
#     y = "y",
#     n = round(sum(presence.points.model$.part == k) * number.bg / nrow(presence.points.model)),
#     method = c("geoenv_const", width = "10000", env = predictors),
#     rlayer = partition.grid,
#     maskval = k,
#     calibarea = calibration.area
#   )
# }) %>%
#   bind_rows()

# Random pseudoabsences com distâncias
pseudoabs.points <- lapply(1:n_groups, function(k) {
  flexsdm::sample_pseudoabs(
    data = presence.points.model,
    x = "x",
    y = "y",
    n = round(sum(presence.points.model$.part == k) * number.pa / nrow(presence.points.model)),
    method = c("geo_const", width = "10000", env = predictors),
    rlayer = partition.grid,
    maskval = k,
    calibarea = calibration.area
  )
}) %>%
  bind_rows()

# Extrai pontos usando o grid criado anteriormente
# Define pontos de background e pseudo-ausências
background.points <- sdm_extract(
  data = background.points, 
  x = "x", 
  y = "y", 
  env_layer = partition.grid
)

pseudoabs.points <- sdm_extract(
  data = pseudoabs.points, 
  x = "x", 
  y = "y", 
  env_layer = partition.grid
)

# Extrair dados ambientais das coordenadas de background e pseudo-ausências
background.points.model <- background.points %>%
  sdm_extract(
    data = .,
    x = "x",
    y = "y",
    env_layer = .env_layers,
    filter_na = TRUE
  )

pseudoabs.points.model <- pseudoabs.points %>%
  sdm_extract(
    data = .,
    x = "x",
    y = "y",
    env_layer = .env_layers,    
    filter_na = TRUE
  )

# Ajusta dados de ocorrência para o mesmo formato
names(presence.points.model) <- c("decimalLongitude", "decimalLatitude", "pr_ab", ".part")

dados_aux <- presence.points.clean.filtered %>%
  dplyr::select(decimalLongitude, decimalLatitude, pr_ab, dplyr::all_of(predictors.env.names)) %>%
  # dplyr::select(-id) %>%
  dplyr::distinct(decimalLongitude, decimalLatitude, .keep_all = TRUE)

presence.points.model <- presence.points.model %>%
  left_join(dados_aux, by = c("decimalLongitude", "decimalLatitude", "pr_ab"))

presence.points.model <- presence.points.model %>%
  rename(
    x = decimalLongitude,
    y = decimalLatitude
  )

presence.points.model <- na.omit(presence.points.model)

# Apresenta o número de pontos de presença em cada bloco
presence.points.model %>%
  dplyr::group_by(.part) %>%
  dplyr::count()

background.points.model %>%
  dplyr::group_by(.part) %>%
  dplyr::count()

pseudoabs.points.model %>%
  dplyr::group_by(.part) %>%
  dplyr::count()

# Registra quantidades efetivas após arredondamento e remoção de NA.
report.occ.df <- report.occ.df %>%
  add_row(Occurrences = "Effective contrast samples",
          Total = nrow(presence.points.model),
          Obs = paste("Presences =", nrow(presence.points.model),
                      "; background =", nrow(background.points.model),
                      "; pseudoabsences =", nrow(pseudoabs.points.model),
                      "; within-fold random background allocated in proportion to presences."))

# Informações sobre o grid criado com as ocorrências
# spa_auto = Moran's I autocorrelação espacial
# env_sim = similaridade ambiental com base na distância Euclidiana
# sd_p = desvio padrão de presenças
presence.points.partition$best_part_info
presence.points.partition$best_part_info$cell_size*111.32

# Adiciona ao relatório
report.occ.df <- report.occ.df %>% 
  add_row(Occurrences = "Data partitioning",
          Native = nrow(filter(presence.points.clean.filtered, establishmentMeans == native.range.label)),
          Invasive = nrow(filter(presence.points.clean.filtered, establishmentMeans == non.native.range.label)),
          Total = nrow(presence.points.clean.filtered),
          Obs = paste0("Background = ", number.bg, " (random)",
                       "; PseudoAbs = ", number.pa, " (geo_const)",
                       "; Grid with Cell size = ", presence.points.partition$best_part_info$cell_size*111.32,
                       "; Moran's I = ", presence.points.partition$best_part_info$spa_auto,
                       "; Env. Sim. = ", presence.points.partition$best_part_info$env_sim))



# Para ilustrar o processo: Plot de todos os pontos, grid e background
file.name <- paste0("Map " , count.map, " - Partition Area and Model Data.png")
file.name <- file.path(sdm.folder, file.name)
count.map = count.map + 1

png(filename = file.name, width = 30, height = 30, units = "cm" , res = 400)

plot(crop(partition.grid, bbox.map), 
     col = RColorBrewer::brewer.pal(n_groups,"BrBG"))
plot(world$geom, 
     legend = FALSE, 
     axes = FALSE, 
     add = TRUE)
points(background.points[, c("x", "y")], 
       col = "black", 
       pch = 20, 
       cex = 0.5)
points(pseudoabs.points[, c("x", "y")], 
       col = "green", 
       pch = 20, 
       cex = 0.6)
points(presence.points.model[, c("x", "y")], 
       col = "red", 
       pch = 20, 
       cex = 1)

dev.off()

gc()


# Seleção de Variáveis ------------------------------------------------------------------------
# Simplificação didática: seleção feita no conjunto disponível, fora dos folds.
# As métricas seguintes não são validação externa ou aninhada de todo o procedimento.
# Para inferência de transferência, planejar avaliação independente ou ciclo externo.
# # Seleção de variáveis com covsel (Adde et al., 2023)
# # GLM with elastic-net regularization (GLM-EN) (Zou and Hastie, 2005)
# # GAM with null-space penalization (GAM-NP) (Marra and Wood, 2011)
# # guided regularized RF (RF-GR) (Deng and Runger, 2013)
# # remotes::install_github("antadde/covsel")
## 1) Colinearidade
covdata <- rbind(presence.points.model, background.points.model)
pa      <- covdata$pr_ab
covdata <- data.frame(covdata[, 5:ncol(covdata)])

set.seed(1968)

# Retira variáveis
# São removidas variáveis macroclimáticas amplas
# Austin (2007)
# predictors.to.remove <- c("Bio01", "Bio12")

# Remoção de variáveis com artefatos — controlado por remove.artifact.vars no script raíz
# Bio08, Bio09, Bio18, Bio19 apresentam problemas em regiões áridas (Booth 2022)
remove.artifact.vars <- FALSE
if (remove.artifact.vars) {
  predictors.to.remove <- c("Bio08", "Bio09", "Bio18", "Bio19")
  covdata <- covdata %>% dplyr::select(-dplyr::any_of(predictors.to.remove))
}

# Filtro de colinearidade
covdata.filter <- covsel::covsel.filteralgo(
  covdata = covdata,
  pa = pa,
  # force = c("Bio05","Bio06","Bio09","Bio15"),
  corcut = 0.7 # correlação aceita
)

predictors.final.keep <- sort(names(covdata.filter))
predictors.final.keep

# # 2) Embedded selection
# covdata.embed <- covsel::covsel.embed(
#   covdata = covdata.filter,
#   pa = pa,
#   algorithms = c("glm", "gam", "rf"),
#   # force = c("Bio05","Bio06","Bio09","Bio15"),
#   ncov = ceiling(log2(length(which(pa == 1)))),
#   maxncov = ceiling(nrow(presence.points.model)/10) + 1,
#   nthreads = max(1L, parallel::detectCores() - 4L, na.rm = TRUE)
# )
# 
# # Variáveis selecionadas para modelagem
# predictors.final.keep <- sort(covdata.embed$ranks_2$covariate)
# predictors.final.keep

# Variáveis que serão utilizadas na modelagem final
# Pode-se incluir, abaixo aquelas que se deseja manter de qualquer forma
predictors.keep <- character(0)

# Conjunto final de variáveis
predictors.final.keep <- unique(c(predictors.final.keep, predictors.keep))

predictors.model <- .env_layers[[predictors.final.keep]]
# predictors.model <- vars$env_layer[[(names(vars$env_layer) %in% predictors.final.keep)]]
names(predictors.model)

predictors.labels <- predictors.all.labels.full$Label[predictors.all.labels.full$Var %in% predictors.final.keep]

# Adiciona ao relatório
report.occ.df <- report.occ.df %>% 
  add_row(Occurrences = "Clean, filtered, validated, reduced environmental data",
          Native = nrow(filter(presence.points.clean.filtered, establishmentMeans == native.range.label)),
          Invasive = nrow(filter(presence.points.clean.filtered, establishmentMeans == non.native.range.label)),
          Total = nrow(presence.points.clean.filtered),
          Obs = paste0(nlyr(predictors.model), " covariates retained: ", predictors.final.keep))


# Espaço ambiental por PCA --------------------------------------------------------------------
# A PCA resume as variáveis selecionadas em novos eixos (componentes principais).
# O background define os eixos do ambiente disponível na área de calibração.
# As ocorrências são projetadas nesses mesmos eixos, sem recalcular a PCA.
# Esta análise é ilustrativa: não altera as variáveis usadas na modelagem.

# 1) Seleciona as mesmas variáveis ambientais para background e ocorrências
pca.variables <- names(predictors.model)

pca.background <- as.data.frame(
  background.points.model[, pca.variables, drop = FALSE]
)

pca.occurrences <- as.data.frame(
  presence.points.clean.env[, pca.variables, drop = FALSE]
)
pca.groups <- as.character(presence.points.clean.env$establishmentMeans)

# 2) Retira linhas com valores ausentes ou não finitos apenas desta análise
pca.background.keep <- rowSums(!is.finite(as.matrix(pca.background))) == 0
pca.occurrences.keep <- rowSums(!is.finite(as.matrix(pca.occurrences))) == 0

pca.background <- pca.background[pca.background.keep, , drop = FALSE]
pca.occurrences <- pca.occurrences[pca.occurrences.keep, , drop = FALSE]
pca.groups <- pca.groups[pca.occurrences.keep]

# 3) Exclui da PCA variáveis sem variação no background
pca.sd <- sapply(pca.background, sd)
pca.variables <- names(pca.sd)[is.finite(pca.sd) & pca.sd > 0]

pca.background <- pca.background[, pca.variables, drop = FALSE]
pca.occurrences <- pca.occurrences[, pca.variables, drop = FALSE]

# Dois eixos exigem pelo menos duas variáveis e três pontos de background
if (length(pca.variables) >= 2 && nrow(pca.background) >= 3 && nrow(pca.occurrences) > 0) {

  # 4) Ajusta a PCA somente no background
  # Padroniza as variáveis para que unidades diferentes não dominem os eixos
  environment.pca <- prcomp(
    pca.background,
    center = TRUE,
    scale. = TRUE
  )

  # 5) Projeta as ocorrências usando a mesma centralização e padronização
  pca.background.scores <- as.data.frame(environment.pca$x)
  pca.occurrences.scores <- as.data.frame(
    predict(environment.pca, newdata = pca.occurrences)
  )

  # Mantém a identificação dos grupos alinhada com as ocorrências
  pca.groups[is.na(pca.groups) | !pca.groups %in%
               c(native.range.label, non.native.range.label)] <- "Unclassified"
  pca.occurrences.scores$Group <- pca.groups

  # 6) Calcula a porcentagem de variância explicada por cada componente
  pca.variance <- 100 * environment.pca$sdev^2 / sum(environment.pca$sdev^2)
  pca.xlabel <- paste0("PC1 (", round(pca.variance[1], 1), "% explained variance)")
  pca.ylabel <- paste0("PC2 (", round(pca.variance[2], 1), "% explained variance)")

  # 7) Representa o ambiente disponível e as ocorrências nos dois primeiros eixos
  env.plot <- ggplot() +
    geom_point(
      data = pca.background.scores,
      aes(x = PC1, y = PC2),
      color = "grey70",
      size = 0.8,
      alpha = 0.25
    ) +
    geom_point(
      data = pca.occurrences.scores,
      aes(x = PC1, y = PC2, color = Group),
      size = 2.5,
      alpha = 0.80
    ) +
    scale_color_manual(
      values = setNames(
        c("red3", "green4", "black"),
        c(non.native.range.label, native.range.label, "Unclassified")
      ),
      name = "Occurrences"
    ) +
    coord_equal() +
    theme_bw() +
    labs(
      x = pca.xlabel,
      y = pca.ylabel,
      caption = "Grey points: calibration-area background. Occurrences projected onto the background PCA."
    ) +
    theme(
      text = element_text(size = 16),
      panel.grid.minor = element_blank()
    )

  # 8) Grava o gráfico, mantendo o padrão do curso
  file.name <- paste0("Plot ", count.plot, " - Environmental Space PCA.png")
  file.name <- file.path(sdm.folder, file.name)

  ggplot2::ggsave(
    filename = file.name,
    plot = env.plot,
    width = 30,
    height = 22,
    units = "cm",
    dpi = 400,
    device = ragg::agg_png,
    bg = "white"
  )

  count.plot <- count.plot + 1

} else {
  message("PCA não representada: dados insuficientes para dois componentes.")
}


# Espaço ambiental: precipitação e temperatura -----------------------------------------------
# Complementa a PCA com variáveis nas unidades originais, mesmo se não selecionadas.
# Cada ponto representa uma ocorrência limpa, sem filtros espacial ou ambiental.
# Elipses de concentração de 95%: resumo descritivo sob aproximação normal.
# Não são intervalos de confiança da média nem limites do nicho.
# A comparação visual não constitui um teste de diferença entre os grupos.

# 1) Seleciona registros com as duas variáveis e classificação de origem conhecida
climate.data <- presence.points.clean.env %>%
  dplyr::filter(
    is.finite(Bio12),
    is.finite(Bio01),
    establishmentMeans %in% c(native.range.label, non.native.range.label)
  )

climate.native <- climate.data %>%
  dplyr::filter(establishmentMeans == native.range.label)

climate.introduced <- climate.data %>%
  dplyr::filter(establishmentMeans == non.native.range.label)

# 2) Define cores, símbolos e tamanho das amostras na legenda
climate.groups <- c(native.range.label, non.native.range.label)
climate.colors <- setNames(c("green4", "red3"), climate.groups)
climate.shapes <- setNames(c(21, 24), climate.groups)
climate.labels <- setNames(
  paste0(climate.groups, " (n = ", c(nrow(climate.native), nrow(climate.introduced)), ")"),
  climate.groups
)

if (nrow(climate.data) > 0) {

  # 3) Inicia o gráfico nas unidades originais
  climate.plot <- ggplot(climate.data, aes(x = Bio12, y = Bio01))

  # 4) Acrescenta a elipse do grupo nativo, se houver dados suficientes
  # A matriz de covariância deve permitir uma elipse em duas dimensões.
  if (nrow(climate.native) >= 5) {
    climate.native.cov <- cov(climate.native[, c("Bio12", "Bio01")])

    if (all(is.finite(climate.native.cov)) && det(climate.native.cov) > 0) {
      climate.plot <- climate.plot +
        stat_ellipse(
          data = climate.native,
          type = "norm",
          level = 0.95,
          geom = "polygon",
          fill = "green4",
          color = "green4",
          alpha = 0.12,
          linewidth = 0.7,
          show.legend = FALSE
        )
    }
  }

  # 5) Acrescenta a elipse do grupo introduzido/invasivo, se houver dados suficientes
  if (nrow(climate.introduced) >= 5) {
    climate.introduced.cov <- cov(climate.introduced[, c("Bio12", "Bio01")])

    if (all(is.finite(climate.introduced.cov)) && det(climate.introduced.cov) > 0) {
      climate.plot <- climate.plot +
        stat_ellipse(
          data = climate.introduced,
          type = "norm",
          level = 0.95,
          geom = "polygon",
          fill = "red3",
          color = "red3",
          alpha = 0.12,
          linewidth = 0.7,
          show.legend = FALSE
        )
    }
  }

  # 6) Desenha os pontos sobre as elipses, sem deslocar seus valores
  climate.plot <- climate.plot +
    geom_point(
      aes(fill = establishmentMeans, shape = establishmentMeans),
      color = "grey20",
      size = 3,
      stroke = 0.4,
      alpha = 0.80
    ) +
    scale_fill_manual(
      values = climate.colors,
      breaks = climate.groups,
      labels = climate.labels,
      name = "Occurrences"
    ) +
    scale_shape_manual(
      values = climate.shapes,
      breaks = climate.groups,
      labels = climate.labels,
      name = "Occurrences"
    ) +
    scale_x_continuous(labels = scales::label_comma()) +
    labs(
      x = "Annual precipitation (mm)",
      y = "Mean annual temperature (°C)",
      caption = paste(
        "Shaded ellipses: 95% normal data ellipses, where estimable (n >= 5).",
        "Descriptive comparison of occurrence records; not niche boundaries.",
        sep = "\n"
      )
    ) +
    theme_bw() +
    theme(
      text = element_text(size = 16),
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(size = 10, hjust = 0)
    )

  # 7) Grava o gráfico após o da PCA
  file.name <- paste0("Plot ", count.plot, " - Temperature and Precipitation.png")
  file.name <- file.path(sdm.folder, file.name)

  ggplot2::ggsave(
    filename = file.name,
    plot = climate.plot,
    width = 30,
    height = 22,
    units = "cm",
    dpi = 400,
    device = ragg::agg_png,
    bg = "white"
  )

  count.plot <- count.plot + 1

} else {
  message("Gráfico de temperatura e precipitação não gerado: não há registros válidos.")
}


# Modelagem -----------------------------------------------------------------------------------
gc()

# Definições básicas
sample.size <- nrow(presence.points.model)

# Máscara do Brasil
BR.mask <- world %>%
  filter(ADMIN == "Brazil") %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  terra::vect(.)

# Predictors
predictors.names <- names(predictors.model)

# Subset do conjunto de dados apenas com as variáveis mais importantes
presence.points.model   <- cbind(presence.points.model[, 1:4, drop = FALSE], presence.points.model[, names(predictors.model), drop = FALSE])
background.points.model <- cbind(background.points.model[, 1:4, drop = FALSE], background.points.model[, names(predictors.model), drop = FALSE])
pseudoabs.points.model  <- cbind(pseudoabs.points.model[, 1:4, drop = FALSE], pseudoabs.points.model[, names(predictors.model), drop = FALSE])

# ESM requer mais de duas variáveis para formar os modelos bivariados.
if (length(predictors.names) < 3L) {
  stop("ESM requer pelo menos três preditores após a seleção de variáveis.")
}

# Ensemble of Small Models
set.seed(1968)
# Ajuste Maxent
emax <- esm_max(
  data = rbind(presence.points.model, pseudoabs.points.model),
  response = "pr_ab",
  predictors = predictors.names,
  partition = ".part",
  thr = "max_sens_spec",
  background = background.points.model,
  classes = "default",
  regmult = 1.5,
  clamp = TRUE,
  pred_type = "cloglog"
)

if (!is.list(emax) || length(emax$esm_model) == 0L) {
  stop("ESM Maxent não reteve modelos bivariados com peso positivo.")
}

# Projeção Maxent
emax_pred <- sdm_predict(
  models = emax,
  pred = predictors.model,
  predict_area = BR.mask
)

# Ajuste GLM
eglm <- esm_glm(
  data = rbind(presence.points.model,  pseudoabs.points.model),
  response = "pr_ab",
  predictors = predictors.names,
  partition = ".part",
  thr = "max_sens_spec",
  poly = 0,
  inter_order = 0
)

if (!is.list(eglm) || length(eglm$esm_model) == 0L) {
  stop("ESM GLM não reteve modelos bivariados com peso positivo.")
}

# Projeção GLM
eglm_pred <- sdm_predict(
  models = eglm,
  pred = predictors.model,
  predict_area = BR.mask
)

# Ajuste GAM
egam <- esm_gam(
  data = rbind(presence.points.model,  pseudoabs.points.model),
  response = "pr_ab",
  predictors = predictors.names,
  partition = ".part",
  thr = "max_sens_spec",
  k = 2
)

# Gravação e recuperação dos modelos ESM (opcional) ---------------------------
# Execute apenas se desejar guardar os ajustes ou retomar uma análise.
# Os dados, preditores e demais objetos do fluxo continuam sendo necessários.
# Para ler em outra sessão, informe o caminho do RDS gravado anteriormente.
#
# Grava os três modelos ajustados
# file.models <- file.path(sdm.folder, "ESM Models.rds")
# saveRDS(list(Maxent = emax, GLM = eglm, GAM = egam), file = file.models)
#
# Recupera os três modelos, sem ajustá-los novamente
# esm.models <- readRDS(file.models)
# emax <- esm.models$Maxent
# eglm <- esm.models$GLM
# egam <- esm.models$GAM

if (!is.list(egam) || length(egam$esm_model) == 0L) {
  stop("ESM GAM não reteve modelos bivariados com peso positivo.")
}

# Projeção GAM
egam_pred <- sdm_predict(
  models = egam,
  pred = predictors.model,
  predict_area = BR.mask
)

# Mediana das projeções contínuas dos ESM
esm_pred <- c(
  emax_pred[[1]][[1]],
  eglm_pred[[1]][[1]],
  egam_pred[[1]][[1]]
)

esm_median <- terra::app(
  esm_pred,
  fun = median,
  na.rm = FALSE
)

names(esm_median) <- "ESM_median"

# Visualiza Modelos
# Reúne as projeções contínuas
maps.esm <- c(
  emax_pred[[1]][[1]],
  eglm_pred[[1]][[1]],
  egam_pred[[1]][[1]],
  esm_median
)

names(maps.esm) <- c("ESM Maxent", "ESM GLM", "ESM GAM", "ESM Mediana")

terra::plot(
  maps.esm,
  nc = 2,
  nr = 2,
  col = color.model(100),
  range = c(0, 1)
)


# Registra os modelos e as configurações usados nesta execução
# Total indica o número de modelos bivariados retidos em cada ESM.
report.occ.df <- report.occ.df %>%
  add_row(
    Occurrences = "ESM Maxent",
    Total = length(emax$esm_model),
    Obs = "classes = default; regmult = 1.5; clamp = TRUE; pred_type = cloglog"
  ) %>%
  add_row(
    Occurrences = "ESM GLM",
    Total = length(eglm$esm_model),
    Obs = "poly = 0; inter_order = 0"
  ) %>%
  add_row(
    Occurrences = "ESM GAM",
    Total = length(egam$esm_model),
    Obs = "k = 2"
  ) %>%
  add_row(
    Occurrences = "ESM predictors used",
    Total = length(predictors.names),
    Obs = paste(predictors.names, collapse = "; ")
  ) %>%
  add_row(
    Occurrences = "ESM fitting data",
    Total = nrow(presence.points.model),
    Obs = paste0(
      "Presences = ", nrow(presence.points.model),
      "; pseudoabsences = ", nrow(pseudoabs.points.model),
      "; background = ", nrow(background.points.model),
      "; response = pr_ab; partition = .part; partitions = ",
      length(unique(presence.points.model$.part))
    )
  ) %>%
  add_row(
    Occurrences = "ESM combination and projection",
    Obs = paste(
      "Within each ESM: weighted mean using Somers' D;",
      "continuous consensus: median of Maxent, GLM and GAM;",
      "projection area: Brazil; NA preserved."
    )
  ) %>%
  add_row(
    Occurrences = "ESM binary maps and agreement",
    Obs = paste(
      "Each ESM uses its own max_sens_spec threshold;",
      "binary consensus: at least 2 of 3 models;",
      "agreement map: combinations of Maxent, GLM and GAM predicting suitability."
    )
  ) %>%
  add_row(
    Occurrences = "Software versions",
    Obs = paste0(
      "R ", getRversion(),
      "; flexsdm ", packageVersion("flexsdm"),
      "; terra ", packageVersion("terra"),
      "; tmap ", packageVersion("tmap")
    )
  )


# Grava Relatório de Modelagem 
# Tabela do Relatório
tmp.table <- 
  report.occ.df %>%
  gt() %>%
  cols_label(
    Occurrences	= md("**Occurences**"),
    Native      = md("**Native countries (inferred)**"),
    Invasive    = md("**Outside native countries (inferred)**"),
    Total       = md("**Total**"),
    Obs         = md("**Complementar information**")
  ) %>%
  fmt_number(
    decimals = 0
  ) %>%
  sub_missing(
    missing_text = "-"
  )

file.name <- paste0("Table ", count.table, " - Model REPORT.docx")
file.name <- file.path(sdm.folder, file.name)
count.table = count.table + 1
gtsave(data = tmp.table, filename = file.name)

tmp.table

# Pós-Modelagem -----------------------------------------------------
# Métricas 
esm.select.model <- sdm_summarize(list(emax, eglm, egam))

# Tabela de Métricas — ESM

# Métricas por partição
esm.metrics.part <- dplyr::bind_rows(
  emax$performance_part,
  eglm$performance_part,
  egam$performance_part
) %>%
  dplyr::filter(threshold == "max_sens_spec") %>%
  dplyr::mutate(
    FPR  = 1 - TNR,
    bACC = (TPR + TNR) / 2,
    RMSE = sqrt(1 - CRPS)
  )

# Ordem das métricas
metric.codes <- c(
  "TPR", "TNR", "W_TPR_TNR", "TSS",
  "SORENSEN", "JACCARD", "FPB", "OR",
  "BOYCE", "AUC", "IMAE", "FPR",
  "bACC", "CRPS", "MCC", "RMSE"
)

metric.names <- c(
  "True Positive Rate, Sensitivity or Recall (TPR)",
  "True Negative Rate or Specificity (TNR)",
  "Weighted TPR-TNR",
  "True Skill Statistic (TSS)",
  "Sorensen Index",
  "Jaccard Index",
  "F-measure on Presence-Background (FPB)",
  "Omission or False Negative or Underprediction Rate (OR/UPR)",
  "Continuous Boyce Index (CBI)",
  "Area Under ROC Curve (AUC)",
  "Inverse Mean Absolute Error (1-MAE)",
  "False Positive Rate (FPR)",
  "Balanced Accuracy",
  "Inverse Brier Score (1-MSE; CRPS in flexsdm)",
  "Matthews Correlation Coefficient (MCC)",
  "Root-Mean-Square Error (RMSE)"
)

# Média e desvio-padrão entre partições
esm.metrics.summary <- esm.metrics.part %>%
  dplyr::select(model, dplyr::all_of(metric.codes)) %>%
  tidyr::pivot_longer(
    cols = -model,
    names_to = "metric",
    values_to = "value"
  ) %>%
  dplyr::group_by(model, metric) %>%
  dplyr::summarise(
    mean = mean(value),
    sd = sd(value),
    .groups = "drop"
  )

# Organiza uma linha por métrica
metric.df <- esm.metrics.summary %>%
  tidyr::pivot_wider(
    names_from = model,
    values_from = c(mean, sd),
    names_glue = "{model}_{.value}"
  ) %>%
  dplyr::slice(match(metric.codes, metric)) %>%
  dplyr::mutate(metric.names = .env$metric.names) %>%
  dplyr::select(
    metric.names,
    esm_max_mean, esm_max_sd,
    esm_glm_mean, esm_glm_sd,
    esm_gam_mean, esm_gam_sd
  )

# Tabela de métricas
tmp.table <-
  metric.df %>%
  gt::gt() %>%
  gt::tab_header(
    title = "ESM Performance Metrics",
    subtitle = "Mean and standard deviation across validation partitions"
  ) %>%
  gt::tab_spanner(
    label = "ESM Maxent",
    columns = c(esm_max_mean, esm_max_sd)
  ) %>%
  gt::tab_spanner(
    label = "ESM GLM",
    columns = c(esm_glm_mean, esm_glm_sd)
  ) %>%
  gt::tab_spanner(
    label = "ESM GAM",
    columns = c(esm_gam_mean, esm_gam_sd)
  ) %>%
  gt::cols_label(
    metric.names = gt::md("**Metric Names**"),
    esm_max_mean = gt::md("**Mean**"),
    esm_max_sd   = gt::md("**SD**"),
    esm_glm_mean = gt::md("**Mean**"),
    esm_glm_sd   = gt::md("**SD**"),
    esm_gam_mean = gt::md("**Mean**"),
    esm_gam_sd   = gt::md("**SD**")
  ) %>%
  gt::fmt_number(
    columns = c(
      esm_max_mean, esm_max_sd,
      esm_glm_mean, esm_glm_sd,
      esm_gam_mean, esm_gam_sd
    ),
    decimals = 5
  ) %>%
  gt::sub_missing(
    missing_text = "NA"
  ) %>%
  gt::tab_source_note(
    source_note = paste(
      "Internal evaluation of each ESM; these are not metrics of the median map.",
      "Threshold-dependent metrics use max_sens_spec.",
      "Selection reuses the available data/partitions; not external or nested validation.",
      "In the installed flexsdm implementation, CRPS is calculated as 1-MSE.",
      "RMSE is calculated within each partition before summarization.",
      "NA indicates that the summary could not be calculated for all partitions."
    )
  )

# Salva a tabela
file.name <- paste0("Table ", count.table, " - ESM METRICS.docx")
file.name <- file.path(sdm.folder, file.name)

gt::gtsave(data = tmp.table, filename = file.name)
count.table <- count.table + 1

tmp.table

# Histogramas de adequabilidade nas ocorrências — ESM ------------------------
# Avaliação descritiva do ajuste: estas presenças participaram do treinamento.
# Não substitui as métricas de validação por partição.
# A mediana é extraída do mapa mediano, não calculada entre histogramas.

esm.hist.rasters <- c(
  emax_pred[[1]][[1]],
  eglm_pred[[1]][[1]],
  egam_pred[[1]][[1]],
  esm_median
)
names(esm.hist.rasters) <- c("Maxent", "GLM", "GAM", "Median")

presence.points.prob <- sdm_extract(
  data = presence.points.model[, c("x", "y"), drop = FALSE],
  x = "x",
  y = "y",
  env_layer = esm.hist.rasters,
  filter_na = FALSE
)

# Usa as mesmas ocorrências nos quatro painéis.
# Presenças fora da projeção para o Brasil, ou em células NA, são excluídas.
n.hist.total <- nrow(presence.points.prob)
presence.points.prob <- presence.points.prob %>%
  dplyr::filter(
    dplyr::if_all(c(Maxent, GLM, GAM, Median), ~ is.finite(.x))
  )
n.hist.used <- nrow(presence.points.prob)

presence.points.hist.esm <- presence.points.prob %>%
  tidyr::pivot_longer(
    cols = c(Maxent, GLM, GAM, Median),
    names_to = "Model",
    values_to = "Suitability"
  ) %>%
  dplyr::mutate(Model = factor(Model, levels = c("Maxent", "GLM", "GAM", "Median")))

# Limiares próprios dos três ESMs; não há limiar validado para a mediana.
esm.hist.thresholds <- esm.select.model %>%
  dplyr::filter(
    threshold == "max_sens_spec",
    model %in% c("esm_max", "esm_glm", "esm_gam")
  ) %>%
  dplyr::mutate(
    Model = dplyr::recode(model, esm_max = "Maxent", esm_glm = "GLM", esm_gam = "GAM"),
    Model = factor(Model, levels = c("Maxent", "GLM", "GAM", "Median"))
  )

n.breaks <- 10

hist.esm.plot <- ggplot(
  data = presence.points.hist.esm,
  aes(x = Suitability)
) +
  geom_histogram(
    aes(y = after_stat(count), fill = after_stat(x)),
    breaks = seq(0, 1, length.out = n.breaks + 1),
    color = "black"
  ) +
  geom_vline(
    data = esm.hist.thresholds,
    aes(xintercept = thr_value),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  facet_wrap(~ Model, ncol = 2, drop = FALSE) +
  scale_fill_gradientn(
    colours = color.model(100),
    limits = c(0, 1),
    name = "Suitability"
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(breaks = scales::breaks_width(1)) +
  labs(
    x = "Predicted suitability",
    y = "Number of occurrences",
    caption = paste0(
      "Training presences within the projection: ", n.hist.used, " of ", n.hist.total,
      ". Descriptive assessment, not independent validation.\n",
      "Dashed lines: model-specific max_sens_spec thresholds. Median: no threshold assigned."
    )
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    plot.caption = element_text(size = 10)
  )

file.name <- paste0("Plot ", count.plot, " - ESM Suitability at Occurrences.png")
file.name <- file.path(sdm.folder, file.name)
count.plot <- count.plot + 1

ggplot2::ggsave(
  filename = file.name,
  plot = hist.esm.plot,
  units = "cm",
  width = 30,
  height = 24,
  dpi = 400,
  device = ragg::agg_png
)

# Para visualizar no painel Plots, execute:
# hist.esm.plot


# Curvas de resposta e importância das variáveis — ESM ------------------------

esm.models <- list(
  Maxent = emax,
  GLM = eglm,
  GAM = egam
)

# Mesmas variáveis usadas nos ajustes
esm.variables <- predictors.names

# Referência ambiental comum para as curvas
esm.reference <- rbind(
  presence.points.model[, esm.variables, drop = FALSE],
  background.points.model[, esm.variables, drop = FALSE]
)

esm.reference <- esm.reference[complete.cases(esm.reference), ]

# Médias ambientais: demais variáveis permanecerão nesses valores
esm.reference.means <- colMeans(esm.reference)

# Curvas condicionais de resposta (demais variáveis nas médias).
# Não são PDPs marginalizados sobre a distribuição das outras variáveis.
# Curvas de resposta ---------------------------------------------------------

pdp.resolution <- 100
pdp.list <- list()

for (v in esm.variables) {
  
  message("Curva de resposta: ", v)
  
  # Valores da variável focal
  pdp.grid <- seq(
    min(esm.reference[[v]]),
    max(esm.reference[[v]]),
    length.out = pdp.resolution
  )
  
  # Mantém as demais variáveis nas médias
  pdp.data <- as.data.frame(
    matrix(
      esm.reference.means,
      nrow = pdp.resolution,
      ncol = length(esm.variables),
      byrow = TRUE
    )
  )
  
  names(pdp.data) <- esm.variables
  pdp.data[[v]] <- pdp.grid
  
  # Uma coluna para cada ESM
  pdp.predictions <- matrix(
    NA_real_,
    nrow = pdp.resolution,
    ncol = length(esm.models)
  )
  
  colnames(pdp.predictions) <- names(esm.models)
  
  for (i in seq_along(esm.models)) {
    
    # Modelos bivariados e respectivos pesos
    small.models <- esm.models[[i]]$esm_model
    small.weights <- as.numeric(names(small.models))
    
    stopifnot(
      all(is.finite(small.weights)),
      all(small.weights >= 0),
      sum(small.weights) > 0
    )
    
    small.predictions <- matrix(
      NA_real_,
      nrow = nrow(pdp.data),
      ncol = length(small.models)
    )
    
    for (j in seq_along(small.models)) {
      
      if (inherits(small.models[[j]], "maxnet")) {
        
        small.predictions[, j] <- as.numeric(
          predict(
            small.models[[j]],
            newdata = pdp.data,
            type = "cloglog",
            clamp = TRUE
          )
        )
        
      } else {
        
        small.predictions[, j] <- as.numeric(
          predict(
            small.models[[j]],
            newdata = pdp.data,
            type = "response"
          )
        )
        
      }
    }
    
    # Combinação ponderada: mesma regra usada na projeção do ESM
    pdp.predictions[, i] <- as.numeric(
      small.predictions %*% small.weights / sum(small.weights)
    )
  }
  
  # Consenso mediano no mesmo perfil ambiental
  pdp.median <- apply(pdp.predictions, 1, median)
  
  pdp.list[[v]] <- data.frame(
    variable = v,
    value = pdp.grid,
    pdp.predictions,
    Median = pdp.median
  )
}

pdp.df <- dplyr::bind_rows(pdp.list)

pdp.plot.data <- pdp.df %>%
  tidyr::pivot_longer(
    cols = c(Maxent, GLM, GAM, Median),
    names_to = "model",
    values_to = "suitability"
  )

# Valores observados nas presenças
pdp.presences <- presence.points.model %>%
  dplyr::select(dplyr::all_of(esm.variables)) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "variable",
    values_to = "value"
  )

# Gráfico
pdp.plot <- ggplot(
  pdp.plot.data,
  aes(x = value, y = suitability, color = model)
) +
  geom_line(linewidth = 0.8) +
  geom_rug(
    data = pdp.presences,
    aes(x = value),
    inherit.aes = FALSE,
    sides = "b",
    alpha = 0.5
  ) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  scale_color_manual(
    values = c(
      Maxent = "#D95F02",
      GLM = "#1B9E77",
      GAM = "#7570B3",
      Median = "black"
    )
  ) +
  # coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Predictor value",
    y = "Predicted suitability",
    color = "Model",
    caption = paste(
      "Conditional response curves: other predictors held at their reference means.",
      "Marks indicate values observed at presence locations."
    )
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

pdp.plot

# Salva o gráfico
file.name <- paste0("Plot ", count.plot, " - ESM Response Curves.png")
file.name <- file.path(sdm.folder, file.name)

ggsave(
  filename = file.name,
  plot = pdp.plot,
  width = 30,
  height = 25,
  units = "cm",
  dpi = 400
)

count.plot <- count.plot + 1

# Importância das variáveis por permutação -------------------------------------------------

# Base comum para comparar os três ESM e a mediana
varimp.data <- rbind(
  presence.points.model[, c("pr_ab", esm.variables), drop = FALSE],
  pseudoabs.points.model[, c("pr_ab", esm.variables), drop = FALSE]
)

varimp.data <- varimp.data[complete.cases(varimp.data), ]

set.seed(1968)

n.sim <- 30
varimp.list <- list()
counter <- 1

# Primeiro avalia os dados originais; depois, cada variável permutada
for (v in c("Original", esm.variables)) {
  
  message("Importância: ", v)
  
  n.repetitions <- if (v == "Original") 1 else n.sim
  
  for (s in seq_len(n.repetitions)) {
    
    prediction.data <- varimp.data[, esm.variables, drop = FALSE]
    
    if (v != "Original") {
      prediction.data[[v]] <- sample(prediction.data[[v]])
    }
    
    esm.predictions <- matrix(
      NA_real_,
      nrow = nrow(prediction.data),
      ncol = length(esm.models)
    )
    
    colnames(esm.predictions) <- names(esm.models)
    
    for (i in seq_along(esm.models)) {
      
      small.models <- esm.models[[i]]$esm_model
      small.weights <- as.numeric(names(small.models))
      
      small.predictions <- matrix(
        NA_real_,
        nrow = nrow(prediction.data),
        ncol = length(small.models)
      )
      
      for (j in seq_along(small.models)) {
        
        if (inherits(small.models[[j]], "maxnet")) {
          
          small.predictions[, j] <- as.numeric(
            predict(
              small.models[[j]],
              newdata = prediction.data,
              type = "cloglog",
              clamp = TRUE
            )
          )
          
        } else {
          
          small.predictions[, j] <- as.numeric(
            predict(
              small.models[[j]],
              newdata = prediction.data,
              type = "response"
            )
          )
          
        }
      }
      
      esm.predictions[, i] <- as.numeric(
        small.predictions %*% small.weights / sum(small.weights)
      )
    }
    
    esm.predictions <- cbind(
      esm.predictions,
      Median = apply(esm.predictions, 1, median)
    )
    
    for (m in colnames(esm.predictions)) {
      
      evaluation <- flexsdm::sdm_eval(
        p = esm.predictions[varimp.data$pr_ab == 1, m],
        a = esm.predictions[varimp.data$pr_ab == 0, m],
        thr = "max_sens_spec"
      )
      
      varimp.list[[counter]] <- data.frame(
        model = m,
        variable = v,
        repetition = s,
        AUC = evaluation$AUC[1]
      )
      
      counter <- counter + 1
    }
  }
}

varimp.results <- dplyr::bind_rows(varimp.list)

varimp.baseline <- varimp.results %>%
  dplyr::filter(variable == "Original") %>%
  dplyr::select(model, AUC.original = AUC)

varimp.df <- varimp.results %>%
  dplyr::filter(variable != "Original") %>%
  dplyr::left_join(varimp.baseline, by = "model") %>%
  dplyr::mutate(importance = AUC.original - AUC) %>%
  dplyr::group_by(model, variable) %>%
  dplyr::summarise(
    importance.mean = mean(importance),
    importance.sd = sd(importance),
    .groups = "drop"
  )

varimp.plot <- ggplot(
  varimp.df,
  aes(x = variable, y = importance.mean)
) +
  geom_col(fill = "#159090") +
  geom_errorbar(
    aes(
      ymin = importance.mean - importance.sd,
      ymax = importance.mean + importance.sd
    ),
    width = 0.2
  ) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~ model, ncol = 2) +
  coord_flip() +
  labs(
    x = "Predictor",
    y = "AUC decrease after permutation",
    caption = paste("Descriptive importance on fitting data, not independent validation.",
                    "Bars: mean; error bars: SD across permutations, not total predictive uncertainty.")
  ) +
  theme_bw()

varimp.plot

file.name <- paste0("Plot ", count.plot, " - ESM Permutation Importance.png")
file.name <- file.path(sdm.folder, file.name)

ggsave(
  filename = file.name,
  plot = varimp.plot,
  width = 30,
  height = 25,
  units = "cm",
  dpi = 400
)

count.plot <- count.plot + 1

# MAPAS E TIFFs DOS ESM PARA O BRASIL -----------------------------------------

tmap_mode("plot")

# 1) Ocorrências -------------------------------------------------------------

# Usa points.map.BR, preparado no bloco inicial dos mapas.


# 2) Projeções contínuas -----------------------------------------------------

esm.continuous <- c(
  emax_pred[[1]][[1]],
  eglm_pred[[1]][[1]],
  egam_pred[[1]][[1]],
  esm_median
)

names(esm.continuous) <- c(
  "ESM_Maxent",
  "ESM_GLM",
  "ESM_GAM",
  "ESM_Median"
)


# 3) Limiares próprios de cada ESM -------------------------------------------

esm.thresholds <- flexsdm::sdm_summarize(
  list(emax, eglm, egam)
) %>%
  dplyr::filter(threshold == "max_sens_spec")

# Coloca os limiares na mesma ordem dos rasters
esm.thresholds <- esm.thresholds[
  match(
    c("esm_max", "esm_glm", "esm_gam"),
    esm.thresholds$model
  ),
]

stopifnot(
  nrow(esm.thresholds) == 3L,
  all(is.finite(esm.thresholds$thr_value))
)

esm.thresholds[, c("model", "threshold", "thr_value")]


# 4) Mapas binários dos três ESM ---------------------------------------------

esm.binary <- esm.continuous[[1:3]]

for (i in 1:3) {
  
  esm.binary[[i]] <- terra::ifel(
    esm.continuous[[i]] >= esm.thresholds$thr_value[i],
    1,
    0
  )
}

names(esm.binary) <- c(
  "ESM_Maxent_binary",
  "ESM_GLM_binary",
  "ESM_GAM_binary"
)

# Consenso: pelo menos dois modelos classificam a célula como adequada
# NA é preservado quando algum dos três modelos não tem predição
esm.votes <- terra::app(
  esm.binary,
  fun = sum,
  na.rm = FALSE
)

esm.majority <- terra::ifel(esm.votes >= 2, 1, 0)
names(esm.majority) <- "ESM_Majority_binary"

esm.binary <- c(esm.binary, esm.majority)


# 5) Gravação dos GeoTIFFs ---------------------------------------------------

tiff.folder <- file.path(sdm.folder, "GeoTIFF")
dir.create(tiff.folder, recursive = TRUE, showWarnings = FALSE)

# Contínuos
for (i in 1:terra::nlyr(esm.continuous)) {
  
  file.name <- file.path(
    tiff.folder,
    paste0(my.species, " - ", names(esm.continuous)[i], " - BR.tif")
  )
  
  terra::writeRaster(
    esm.continuous[[i]],
    filename = file.name,
    datatype = "FLT4S",
    gdal = "COMPRESS=LZW",
    overwrite = FALSE
  )
}

# Binários: 0 = abaixo do limiar; 1 = acima ou igual ao limiar
for (i in 1:terra::nlyr(esm.binary)) {
  
  file.name <- file.path(
    tiff.folder,
    paste0(my.species, " - ", names(esm.binary)[i], " - BR.tif")
  )
  
  terra::writeRaster(
    esm.binary[[i]],
    filename = file.name,
    datatype = "INT1U",
    NAflag = 255,
    gdal = "COMPRESS=LZW",
    overwrite = FALSE
  )
}

# Registra os limiares usados
write.csv(
  esm.thresholds[, c("model", "threshold", "thr_value")],
  file = file.path(tiff.folder, "ESM - Binary thresholds.csv"),
  row.names = FALSE
)


# 6) Preparação dos oito mapas -----------------------------------------------

esm.map.rasters <- c(esm.continuous, esm.binary)

esm.map.titles <- c(
  "ESM Maxent",
  "ESM GLM",
  "ESM GAM",
  "Median consensus",
  "ESM Maxent - max_sens_spec",
  "ESM GLM - max_sens_spec",
  "ESM GAM - max_sens_spec",
  "Binary consensus - at least 2 of 3"
)

esm.maps <- list()


# 7) Monta os mapas mantendo o mesmo padrão ----------------------------------

for (i in 1:terra::nlyr(esm.map.rasters)) {

  message("Mapa: ", esm.map.titles[i])

  model.predict.BR.map <-
    # Grupo de legendas fora do mapa
    tm_components(
      group_id = "LEG",
      position = tm_pos_out(
        cell.h = "right",
        cell.v = "center",
        pos.h = "left",
        pos.v = "top",
        align.h = "left"
      ),
      stack = "vertical",
      resize_as_group = TRUE,
      offset = 0.5,
      frame = TRUE,
      bg.color = "grey80",
      bg.alpha = 0.6
    ) +
  
    # Oceano
    tm_shape(ocean.shp) +
    tm_polygons(
      fill = "grey90",
      fill_alpha = 0.7,
      col = NULL
    ) +
  
    # Contorno dos países
    tm_shape(world) +
    tm_polygons(
      col = "black",
      fill = "grey80",
      lwd = 0.6
    ) +
    tm_text("SOVEREIGNT",
            size = 0.6,
            fontface = "bold"
    )

  if (i <= 4) {
    model.predict.BR.map <- model.predict.BR.map +
      # Raster do modelo
      tm_shape(esm.map.rasters[[i]], is.main = TRUE) +
      tm_raster(
        col_alpha = 1,
        col.scale = tm_scale_continuous(
          n = 10,
          values = color.model(10),
          midpoint = 0.5,
          limits = c(0, 1)
        ),
        col.legend = tm_legend(
          group_id = "LEG",
          title = "Suitability",
          reverse = TRUE,
          title.size = 0.8,
          title.align = "left",
          title.fontface = "bold",
          title.padding = c(0.5, 0, 0, 0),
          text.size = 0.8,
          show = TRUE,
          orientation = "portrait",
          height = 13,
          margins = c(0.3, 0.5, 0.5, 0.5)
        )
      )
  } else {
    if (i <= 7) {
      esm.binary.legend <- paste0(
        "Suitability >= ",
        formatC(esm.thresholds$thr_value[i - 4], digits = 4, format = "f")
      )
    } else {
      esm.binary.legend <- "Suitable: at least 2 of 3"
    }

    model.predict.BR.map <- model.predict.BR.map +
      # Raster do modelo
      tm_shape(esm.map.rasters[[i]], is.main = TRUE) +
      tm_raster(
        col_alpha = 1,
        col.scale = tm_scale_categorical(
          levels = c(0, 1),
          levels.drop = FALSE,
          values = color.bin,
          labels = c("Unsuitable", "Suitable")
        ),
        col.legend = tm_legend(
          group_id = "LEG",
          title = esm.binary.legend,
          reverse = FALSE,
          title.size = 0.8,
          title.align = "left",
          title.fontface = "bold",
          title.padding = c(0.5, 0, 0, 0),
          text.size = 0.8,
          show = TRUE,
          orientation = "portrait",
          item.height = 1,
          item.width = 1,
          item.space = 0.3,
          margins = c(0.3, 0.5, 0.5, 0.5)
        )
      )
  }

  model.predict.BR.map <- model.predict.BR.map +
    # Contorno do Brasil
    tm_shape(BR1, is.main = TRUE) +
    tm_polygons(col = "black", 
                fill = NULL, 
                lwd = 0.6
    ) + 
    tm_text("name_state", 
            size = 0.45, 
            fontface = "bold"
    ) +
  
    # Complementos
    tm_graticules(alpha = 0.5, labels.size = 1) +
    tm_compass(
      type = "arrow",
      size = 1.5,
      position = tm_pos_in(
        pos.h = "RIGHT",
        pos.v = "BOTTOM",
        align.h = "center"
      )
    ) +
    tm_credits(
      "Geographic coordinates\nDatum: WGS84",
      position = tm_pos_in(
        pos.h = "RIGHT", 
        pos.v = "BOTTOM", 
        align.h = "center"
      ),
      size = 0.6,
      fontface = "bold",
      col = "black",
      bg.color = "white",
      bg.alpha = 0.6,
      padding = 0.2
    ) +
    tm_scalebar(
      position = tm_pos_in(
        pos.h = "RIGHT",
        pos.v = "BOTTOM",
        align.h = "center"
      )
    ) +
    tm_layout(scale = 1) +
    tm_crs(4326)


  model.predict.BR.map <- model.predict.BR.map +
    tm_title(esm.map.titles[i], position = tm_pos_out("center", "top"))

  # Adiciona ocorrências, se existirem
  if (nrow(points.map.BR) > 0 && plot.occurrences) {
    model.predict.BR.map <- model.predict.BR.map +
      tm_shape(points.map.BR) +
      tm_symbols(
        size = 0.15,
        lwd = 0.1,
        col = "black",
        fill = "map.species",
        fill_alpha = 1,
        fill.scale = tm_scale_categorical(
          values = "black",
          labels = my.species
        ),
        fill.legend = tm_legend(
          group_id = "LEG",
          title = "Occurrences",
          title.size = 0.8,
          title.align = "left",
          title.fontface = "bold",
          title.padding = c(0, 0, 0.5, 0),
          text.size = 0.8,
          text.fontface = "italic",
          show = TRUE,
          orientation = "portrait",
          margins = c(0.5, 0.5, 0, 0.5)
        )
      )
  }


  esm.maps[[i]] <- model.predict.BR.map
}

# 8) Painel dos quatro mapas contínuos ---------------------------------------

esm.continuous.panel <- tmap_arrange(
  esm.maps[1:4],
  ncol = 2,
  nrow = 2,
  outer.margins = 0.02
)

file.name <- paste0(
  "Map ", count.map, " - ESM Continuous Models BR.png"
)

file.name <- file.path(sdm.folder, file.name)

suppressWarnings(
  tmap_save(
    tm = esm.continuous.panel,
    filename = file.name,
    width = 48,
    height = 40,
    units = "cm",
    dpi = 400,
    device = ragg::agg_png
  )
)

count.map <- count.map + 1

# 9) Painel dos quatro mapas binários ----------------------------------------

esm.binary.panel <- tmap_arrange(
  esm.maps[5:8],
  ncol = 2,
  nrow = 2,
  outer.margins = 0.02
)

file.name <- paste0(
  "Map ", count.map, " - ESM Binary Models BR.png"
)

file.name <- file.path(sdm.folder, file.name)

suppressWarnings(
  tmap_save(
    tm = esm.binary.panel,
    filename = file.name,
    width = 48,
    height = 40,
    units = "cm",
    dpi = 400,
    device = ragg::agg_png
  )
)

count.map <- count.map + 1

# Mantém o mapa contínuo mediano disponível individualmente
model.predict.BR.map <- esm.maps[[4]]


# Histogramas das variáveis ambientais nas ocorrências -----------------------

# Seleciona as variáveis usadas nos ESM
presence.points.hist <- presence.points.model[
  , predictors.names, drop = FALSE
]

# Associa os nomes das variáveis aos respectivos rótulos
hist.labels <- predictors.all.labels.full$Label[
  match(predictors.names, predictors.all.labels.full$Var)
]

# Histogramas
for (i in seq_along(predictors.names)) {
  
  variable <- predictors.names[i]
  xlabel <- hist.labels[i]
  
  if (is.na(xlabel) || xlabel == "") {
    xlabel <- variable
  }
  
  # Retira valores ausentes ou não finitos
  var.bio <- presence.points.hist[[variable]]
  var.bio <- var.bio[is.finite(var.bio)]
  
  if (length(var.bio) == 0) {
    next
  }
  
  hist.data <- data.frame(value = var.bio)
  
  hist.plot <- ggplot(hist.data, aes(x = value)) +
    geom_histogram(
      aes(y = after_stat(density)),
      color = "blue",
      fill = "lightblue",
      alpha = 0.8,
      bins = 10
    ) +
    geom_vline(
      xintercept = mean(var.bio),
      linetype = "dashed",
      linewidth = 0.6
    ) +
    labs(
      x = xlabel,
      y = "Density"
    ) +
    theme_bw()
  
  # Densidade apenas quando há valores suficientes e distintos
  if (length(var.bio) >= 2 && length(unique(var.bio)) > 1) {
    
    hist.plot <- hist.plot +
      geom_density(
        color = "#FC4E07",
        fill = "#FC4E07",
        alpha = 0.5
      )
  }
  
  file.name <- paste0(
    "Plot ", count.plot,
    " - Histogram and Density - ", variable, ".png"
  )
  
  file.name <- file.path(sdm.folder, file.name)
  
  png(
    filename = file.name,
    units = "cm",
    width = 20,
    height = 20,
    res = 400
  )
  
  print(hist.plot)
  dev.off()
  
  count.plot <- count.plot + 1
  message(variable)
}


# MAPA do modelo para o Brasil
# Usa points.map.BR, preparado no bloco inicial dos mapas.

# Mapa contínuo
model.predict.BR.map <-
  # Grupo de legendas fora do mapa
  tm_components(
    group_id = "LEG",
    position = tm_pos_out(
      cell.h = "right",
      cell.v = "center",
      pos.h = "left",
      pos.v = "top",
      align.h = "left"
    ),
    stack = "vertical",
    resize_as_group = TRUE,
    offset = 0.5,
    frame = TRUE,
    bg.color = "grey80",
    bg.alpha = 0.6
  ) +
  
  # Oceano
  tm_shape(ocean.shp) +
  tm_polygons(
    fill = "grey90",
    fill_alpha = 0.7,
    col = NULL
  ) +
  
  # Contorno dos países
  tm_shape(world) +
  tm_polygons(
    col = "black",
    fill = "grey80",
    lwd = 0.6
  ) +
  tm_text("SOVEREIGNT",
          size = 0.6,
          fontface = "bold"
  ) +
  
  # Raster do modelo
  tm_shape(esm_median, is.main = TRUE) +
  tm_raster(
    col_alpha = 1,
    col.scale = tm_scale_continuous(
      n = 10,
      values = color.model(10),
      midpoint = 0.5,
      limits = c(0, 1)
    ),
    col.legend = tm_legend(
      group_id = "LEG",
      title = "Suitability",
      reverse = TRUE,
      title.size = 0.8,
      title.align = "left",
      title.fontface = "bold",
      title.padding = c(0.5, 0, 0, 0),
      text.size = 0.8,
      show = TRUE,
      orientation = "portrait",
      height = 13,
      margins = c(0.3, 0.5, 0.5, 0.5)
    )
  ) +
  
  # Contorno do Brasil
  tm_shape(BR1, is.main = TRUE) +
  tm_polygons(col = "black", 
              fill = NULL, 
              lwd = 0.6
  ) + 
  tm_text("name_state", 
          size = 0.45, 
          fontface = "bold"
  ) +
  
  # Complementos
  tm_graticules(alpha = 0.5, labels.size = 1) +
  tm_compass(
    type = "arrow",
    size = 1.5,
    position = tm_pos_in(
      pos.h = "RIGHT",
      pos.v = "BOTTOM",
      align.h = "center"
    )
  ) +
  tm_credits(
    "Geographic coordinates\nDatum: WGS84",
    position = tm_pos_in(
      pos.h = "RIGHT", 
      pos.v = "BOTTOM", 
      align.h = "center"
    ),
    size = 0.6,
    fontface = "bold",
    col = "black",
    bg.color = "white",
    bg.alpha = 0.6,
    padding = 0.2
  ) +
  tm_scalebar(
    position = tm_pos_in(
      pos.h = "RIGHT",
      pos.v = "BOTTOM",
      align.h = "center"
    )
  ) +
  tm_layout(scale = 1) +
  tm_crs(4326)

# Adiciona ocorrências, se existirem
if (nrow(points.map.BR) > 0 && plot.occurrences) {
  model.predict.BR.map <- model.predict.BR.map +
    tm_shape(points.map.BR) +
    tm_symbols(
      size = 0.15,
      lwd = 0.1,
      col = "black",
      fill = "map.species",
      fill_alpha = 1,
      fill.scale = tm_scale_categorical(
        values = "black",
        labels = my.species
      ),
      fill.legend = tm_legend(
        group_id = "LEG",
        title = "Occurrences",
        title.size = 0.8,
        title.align = "left",
        title.fontface = "bold",
        title.padding = c(0, 0, 0.5, 0),
        text.size = 0.8,
        text.fontface = "italic",
        show = TRUE,
        orientation = "portrait",
        margins = c(0.5, 0.5, 0, 0.5)
      )
    )
}

file.name <- paste0("Map ", count.map, " - Project Model BR.png")
file.name <- file.path(sdm.folder, file.name)
count.map = count.map + 1

suppressWarnings(
  tmap_save(
    tm = model.predict.BR.map, filename = file.name,
    units = "cm", dpi = 400,
    device = ragg::agg_png
  )
)

# Concordância dos ESMs ------------------------------------------------------
# Cada modelo usa seu próprio limiar max_sens_spec.
# Usa somente os três modelos individuais, sem incluir o consenso majoritário.
# Concordância não representa probabilidade de presença nem validação do acerto.
# NA permanece NA quando algum modelo não possui predição.
# Código binário: Maxent = 1, GLM = 2, GAM = 4.
# As somas identificam quais modelos classificam cada célula como adequada.
esm.agreement <- esm.binary[[1]] +
  2 * esm.binary[[2]] +
  4 * esm.binary[[3]]

names(esm.agreement) <- "ESM_Agreement"
levels(esm.agreement) <- data.frame(
  ID = 0:7,
  Models = c(
    "None", "Maxent", "GLM", "Maxent + GLM",
    "GAM", "Maxent + GAM", "GLM + GAM", "Maxent + GLM + GAM"
  )
)

# Mapa categórico das combinações de modelos
model.agreement.BR.map <-
  # Grupo de legendas fora do mapa
  tm_components(
    group_id = "LEG",
    position = tm_pos_out(
      cell.h = "right",
      cell.v = "center",
      pos.h = "left",
      pos.v = "top",
      align.h = "left"
    ),
    stack = "vertical",
    resize_as_group = TRUE,
    offset = 0.5,
    frame = TRUE,
    bg.color = "grey80",
    bg.alpha = 0.6
  ) +
  
  # Oceano
  tm_shape(ocean.shp) +
  tm_polygons(
    fill = "grey90",
    fill_alpha = 0.7,
    col = NULL
  ) +
  
  # Contorno dos países
  tm_shape(world) +
  tm_polygons(
    col = "black",
    fill = "grey80",
    lwd = 0.6
  ) +
  tm_text("SOVEREIGNT",
          size = 0.6,
          fontface = "bold"
  ) +
  
  # Raster do modelo
  tm_shape(esm.agreement, is.main = TRUE) +
  tm_raster(
    col_alpha = 1,
    col.scale = tm_scale_categorical(
      values = c(
        "white", "#D95F02", "#1B9E77", "#E6AB02",
        "#7570B3", "#E7298A", "#66A61E", "#252525"
      ),
      levels.drop = FALSE
    ),
    col.legend = tm_legend(
      group_id = "LEG",
      title = "Model agreement",
      reverse = FALSE,
      title.size = 0.8,
      title.align = "left",
      title.fontface = "bold",
      title.padding = c(0.5, 0, 0, 0),
      text.size = 0.8,
      show = TRUE,
      orientation = "portrait",
      item.height = 1,
      item.width = 1,
      item.space = 0.3,
      margins = c(0.3, 0.5, 0.5, 0.5)
    )
  ) +
  
  # Contorno do Brasil
  tm_shape(BR1, is.main = TRUE) +
  tm_polygons(col = "black", 
              fill = NULL, 
              lwd = 0.6
  ) + 
  tm_text("name_state", 
          size = 0.45, 
          fontface = "bold"
  ) +
  
  # Complementos
  tm_graticules(alpha = 0.5, labels.size = 1) +
  tm_compass(
    type = "arrow",
    size = 1.5,
    position = tm_pos_in(
      pos.h = "RIGHT",
      pos.v = "BOTTOM",
      align.h = "center"
    )
  ) +
  tm_credits(
    "Geographic coordinates\nDatum: WGS84",
    position = tm_pos_in(
      pos.h = "RIGHT", 
      pos.v = "BOTTOM", 
      align.h = "center"
    ),
    size = 0.6,
    fontface = "bold",
    col = "black",
    bg.color = "white",
    bg.alpha = 0.6,
    padding = 0.2
  ) +
  tm_scalebar(
    position = tm_pos_in(
      pos.h = "RIGHT",
      pos.v = "BOTTOM",
      align.h = "center"
    )
  ) +
  tm_layout(scale = 1) +
  tm_crs(4326)

# Adiciona ocorrências, se existirem
if (nrow(points.map.BR) > 0 && plot.occurrences) {
  model.agreement.BR.map <- model.agreement.BR.map +
    tm_shape(points.map.BR) +
    tm_symbols(
      size = 0.15,
      lwd = 0.1,
      col = "black",
      fill = "map.species",
      fill_alpha = 1,
      fill.scale = tm_scale_categorical(
        values = "black",
        labels = my.species
      ),
      fill.legend = tm_legend(
        group_id = "LEG",
        title = "Occurrences",
        title.size = 0.8,
        title.align = "left",
        title.fontface = "bold",
        title.padding = c(0, 0, 0.5, 0),
        text.size = 0.8,
        text.fontface = "italic",
        show = TRUE,
        orientation = "portrait",
        margins = c(0.5, 0.5, 0, 0.5)
      )
    )
}

file.name <- paste0("Map ", count.map, " - ESM Agreement BR.png")
file.name <- file.path(sdm.folder, file.name)
count.map = count.map + 1

suppressWarnings(
  tmap_save(
    tm = model.agreement.BR.map, filename = file.name,
    units = "cm", dpi = 400,
    device = ragg::agg_png
  )
)


# Registro da execução e limitações para interpretação ----------------------------
writeLines(c(
  "Roteiro didático; configurações precisam de justificativa por espécie e objetivo.",
  "Seleção de variáveis e modelos reutiliza dados/partições: avaliação não aninhada.",
  "Classe non-native é inferência por países e não comprova introdução ou invasão.",
  "Casas decimais não comprovam acurácia. Revisar incerteza e datas nos dados brutos.",
  "Adequabilidade não é probabilidade de ocupação, estabelecimento ou impacto.",
  "Concordância entre modelos não representa toda a incerteza preditiva."
), file.path(sdm.folder, "Interpretation - limitations.txt"))
writeLines(capture.output(sessionInfo()), file.path(sdm.folder, "Session info.txt"))

# EOF
