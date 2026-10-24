###
# Curso SDM
# Projeto eNichos
# Embrapa / UFVJM / UFSC
# II SIMC - Diamantina (MG)
# George Amaro
# Licença: CC BY-NC-SA 4.0
# 2026-10-12
# Ensemble of Eight Algorithms 
###

# Parametrização ------------------------------------------------------------------------------

my.species   <- "Zaprionus tuberculatus"

# Área nativa
native.range <- c("Cabo Verde", "Cameroon", "Central African Republic", "Chad", 
                  "Republic of the Congo", "Democratic Republic of the Congo", 
                  "Côte d'Ivoire", "Gabon", "Kenya", "Madagascar", "Malawi", "Mauritius", 
                  "Mayotte", "Mozambique", "Niger", "Nigeria", "Réunion", "Saint Helena", 
                  "Sao Tome and Principe", "Seychelles", "South Africa", 
                  "Tanzania", "Uganda", "Zambia", "Zimbabwe")

# Rótulos para áreas nativas e invadidas
native.range.label     <- "native"
non.native.range.label <- "non-native"
# Pode usar também: invasive, introduced

# Última atualização das ocorrências (Excel: AAAA-MM-DD)
data.last.update <- "2026-08-28"

# Fontes de dados
use.excel   <- TRUE
use.GBIF    <- TRUE

if (!use.excel && !use.GBIF) stop("Ative pelo menos uma fonte de ocorrências.")

# Variáveis preditoras adicionais
use.ELEV    <- TRUE
use.SOIL    <- FALSE

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
  flextable, bdc, rgbif, openxlsx2, fuzzySim, sp, here, classInt, ragg, pROC
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
future.folder           <- file.path(results.folder, "Future")

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
dir.create(sdm.folder,     showWarnings = FALSE, recursive = TRUE)
dir.create(future.folder,  showWarnings = FALSE, recursive = TRUE)


# Cria dataframe para relatório do modelo
report.occ.df <- NULL
report.occ.df <- data.frame(
  Occurrences = character(),
  Native      = integer(),
  Invasive    = integer(),
  Total       = integer(),
  Obs         = character()
)

# DISTRIBUIÇÃO GEOGRÁFICA POTENCIAL ----------------------------------------------
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
.ndec  <- 2
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

# presence.points.clean.no.filter é o cojunto de ocorrências válidas, compiladas
# das referências da tabela Excel e do GBIF.


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
  tm_shape(points.map.W, is.master = TRUE, bbox = bbox_new) +
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
  presence.points.model <- cbind(presence.points.model, presence_other[, 14:ncol(presence_other)])
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
  presence.points.model <- cbind(presence.points.model, presence_soil[, 14:ncol(presence_soil)])
  
}

# Preserva ocorrências limpas, sem filtros, com as variáveis ambientais
presence.points.clean.env <- presence.points.model

# Remove eventuais NA nas covariáveis
presence.points.model <- presence.points.model %>%
  dplyr::filter(
    if_all(
      dplyr::all_of(names(presence.points.model)[14:ncol(presence.points.model)]),
      ~ !is.na(.)
    )
  )

# Adiciona ao relatório
report.occ.df <- report.occ.df %>% 
  add_row(Occurrences = "Clean, unfiltered, validated, complete environmental data",
          Native = nrow(filter(presence.points.model, establishmentMeans == native.range.label)),
          Invasive = nrow(filter(presence.points.model, establishmentMeans == non.native.range.label)),
          Total = nrow(presence.points.model),
          Obs = paste0(ncol(presence.points.model) - 13, " covariates added"))

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
bio.vars <- names(presence.points.model[,14:ncol(presence.points.model)])
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

# Matriz e Gráfico de correlações
presence.points.values <- as.data.frame(
  presence.points.model[, 14:ncol(presence.points.model)]
)
presence.points.values <- na.omit(presence.points.values)

# # Retira variáveis categóricas
# presence.points.values <- within(presence.points.values, rm(HII))

# Matriz de correlações
m <- cor(presence.points.values, method = "pearson")

# Salva Gráfico
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

# Mapa de Ocorrências -----------------------------------------------------------
# Com pontos limpos, sem filtro
# Variáveis genéricas para elaboração de mapas

presence.map <-
  # mapa do mundo (vegetação, RGB)
  # com crop para a região 
  tm_shape(crop(earth, predictors.crop) , is.main = TRUE) +
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
  # complementos do mapa
  tm_graticules(alpha = 0.5, labels.size = 1) +
  tm_compass(
    type = "arrow", 
    size = 1.5,
    position = tm_pos_in(pos.h = "LEFT", pos.v = "BOTTOM", align.h = "center")
  ) +
  tm_credits(
    "Geographic coordinates\nWGS84 (EPSG:4326)",
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



# Área de Calibração -------------------------------------------------------------

# Definir área de calibração (região M do diagrama BAM)

# Máscara baseada nos polígonos das zonas KG com ocorrências
# accessible area approach - biogeographic entities (BE)
koeppen.rast  <- rast(Koeppen2.file)
koeppen.vect  <- as.polygons(koeppen.rast)
# Retirar classe "Ocean" (32)
KG.zones.mask <- koeppen.vect[!koeppen.vect$layer == 32]

KG.zones.mask <-
  calib_area(
    data = presence.points.clean.no.filter,
    x = "decimalLongitude",
    y = "decimalLatitude",
    method = c("mask", KG.zones.mask, "layer"),
    crs = wgs84.proj
  )
names(KG.zones.mask) <- "Area"

plot(KG.zones.mask)


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
# WWF.zones.mask.buffer <- terra::buffer(WWF.zones.mask.proj, width = 200000)
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
    method = c("mask", KG.zones.mask, "Area"),
    # method = c("mask", WWF.zones.mask.buffer, "Area"),
    # method = c("mask", WWF.zones.mask, "Area"),
    # method = c("mask", presence.points.mask, "Area"),
    # method = c("mask", OCC.mask, "Area"),
    # method = c("buffer", width = 300000),
    # method = c('bmcp', width = 300000),
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
# Com ocorrências *do modelo*
plot(calibration.mask, legend = FALSE, col = "grey")
points(presence.points.clean.filtered[,3:4], pch = 20, cex = 0.8, col = "red")
plot(world$geom, add = TRUE)

# Área da área de calibração em km2
calibration.area.km <- expanse(calibration.mask, "km")
message("Calibration Area: ", format(calibration.area.km, nsmall = 2, big.mark = ","), " Km2")

# Adiciona ao relatório
# Descrever o método em Obs, abaixo
report.occ.df <- report.occ.df %>% 
  add_row(Occurrences = "Calibration área",
          Native = nrow(filter(presence.points.clean.filtered, establishmentMeans == native.range.label)),
          Invasive = nrow(filter(presence.points.clean.filtered, establishmentMeans == non.native.range.label)),
          Total = nrow(presence.points.clean.filtered),
          Obs = paste0("Koppen-Geiger classes containing clean unfiltered occurrences; hypothesis of calibration domain, not demonstrated historical accessibility; ", calibration.area.km, " km2."))



# Particionamento -----------------------------------------------------------------------------
predictors <- c(predictors, predictors.other, predictors.soil)

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
    env_layer = predictors,    
    pr_ab = "pr_ab",
    x = "decimalLongitude",
    y = "decimalLatitude",
    n_part = n_groups,
    min_res_mult = 0.5/res(predictors.mask)[1],
    max_res_mult = 10/res(predictors.mask)[1], 
    min_occ = ceiling(log2(nrow(presence.points.clean.filtered))),
    # min_occ = 2,
    num_grids = 20,
    prop = 1
  )

presence.points.model <- presence.points.partition$part

# Cria um raster a partir da melhor divisão de blocos
partition.grid <- get_block(
  env_layer = predictors,   
  best_grid = presence.points.partition$grid)

plot(partition.grid, 
     col = RColorBrewer::brewer.pal(n_groups,"PRGn")
)
points(presence.points.clean.filtered[,3:4], pch = 20, cex = 1, col = "red")


# Background e Pseudoausências ----------------------------------------------------------------------------------

# Background (com as partições já definidas)
n.pres    <- nrow(presence.points.model)
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
    method = c("geo_const", width = "20000", env = predictors),
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
    env_layer = predictors,
    filter_na = TRUE
  )

pseudoabs.points.model <- pseudoabs.points %>%
  sdm_extract(
    data = .,
    x = "x",
    y = "y",
    env_layer = predictors,    
    filter_na = TRUE
  )

# Ajusta dados de ocorrência para o mesmo formato
names(presence.points.model) <- c("decimalLongitude", "decimalLatitude", "pr_ab", ".part")

dados_aux <- presence.points.clean.filtered %>%
  dplyr::select(3:4, 14:ncol(.)) %>%
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

png(filename = file.name, width = 30, height = 11.5, units = "cm" , res = 400)

plot(partition.grid, 
     col = RColorBrewer::brewer.pal(n_groups,"PRGn"))
points(background.points[, c("x", "y")], 
       col = "black", 
       pch = 20, 
       cex = 0.5)
points(pseudoabs.points[, c("x", "y")], 
       col = "yellow", 
       pch = 20, 
       cex = 0.6)
points(presence.points.model[, c("x", "y")], 
       col = "red", 
       pch = 20, 
       cex = 1)

dev.off()

gc()


# Seleção de Variáveis ------------------------------------------------------------------------

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
  corcut = 0.8 # correlação aceita
)

predictors.final.keep <- sort(names(covdata.filter))
predictors.final.keep

# 2) Embedded selection
covdata.embed <- covsel::covsel.embed(
  covdata = covdata.filter,
  pa = pa,
  algorithms = c("glm", "gam", "rf"),
  # force = c("Bio05","Bio06","Bio09","Bio15"),
  ncov = ceiling(log2(length(which(pa == 1)))),
  maxncov = ceiling(nrow(presence.points.model)/10) + 1,
  nthreads = max(1L, parallel::detectCores() - 4L, na.rm = TRUE)
)

# Variáveis selecionadas para modelagem
predictors.final.keep <- sort(covdata.embed$ranks_2$covariate)
predictors.final.keep

# Variáveis que serão utilizadas na modelagem final
# Pode-se incluir, abaixo aquelas que se deseja manter de qualquer forma
predictors.keep <- c("")

# Conjunto final de variáveis
predictors.final.keep <- unique(c(predictors.final.keep, predictors.keep))

predictors.model <- predictors[[(names(predictors) %in% predictors.final.keep)]]
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

pca.groups <- as.character(
  presence.points.clean.env$establishmentMeans
)

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
# Cada ponto representa uma ocorrência limpa e filtrada.
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
# ENM/SDM com ensemble
gc()

sample.size        <- nrow(presence.points.model)    # Tamanho da amostra
models.threshold   <- "max_sens_spec"                # Limiar de corte dos algoritmos
models.metric      <- "SORENSEN"                     # Métrica para seleção de modelos
ensemble.method    <- "median"                       # Método de ensemble
ensemble.threshold <- "SORENSEN_mean"                # Limiar para seleção dos modelos para o ensemble
ensemble.cut       <- 0.60                           # Valor do limiar de corte para compor o ensemble

# Predictors
predictors.names <- names(predictors.model)
# predictors.names <- names(predictors.model)[!names(predictors.model) == "MODISLC"]

# Subset do conjunto de dados apenas com as variáveis mais importantes
presence.points.model   <- cbind(presence.points.model[, 1:4, drop = FALSE], presence.points.model[, names(predictors.model), drop = FALSE])
background.points.model <- cbind(background.points.model[, 1:4, drop = FALSE], background.points.model[, names(predictors.model), drop = FALSE])
pseudoabs.points.model  <- cbind(pseudoabs.points.model[, 1:4, drop = FALSE], pseudoabs.points.model[, names(predictors.model), drop = FALSE])

# Conjuntos de dados completos
data.model.background <- rbind(presence.points.model, background.points.model)
data.model <- rbind(presence.points.model, pseudoabs.points.model)


# ENM/SDM com ensemble: MAX, RAF, GBM, GAU, NET, SVM, GLM, GAM
# A repetibilidade em paralelo também depende do backend e das versões dos pacotes.
set.seed(1968)

# MAX
# Definição das feature classes com base no tamanho da amostra
# Classes
if (sample.size < 10) {
  classes <- "l"
} else if (sample.size < 15) {
  classes <- c("l", "lq")
} else {
  classes <- c("l", "h", "lq", "lqh")
}

# Sample size
if (sample.size < 20) {
  regmult.values <- seq(1, 5, 1)
} else {
  regmult.values <- seq(0.5, 5, 0.5)
}

tune.grid.max <- expand.grid(
  regmult = regmult.values,
  classes = classes
)

max.fit.model <- tune_max(
  data = data.model,
  response = "pr_ab",
  predictors = predictors.names,
  background = background.points.model,
  partition = ".part",
  grid = tune.grid.max,
  thr = models.threshold,        
  metric = models.metric,         
  clamp = TRUE,
  pred_type = "cloglog",
  n_cores = max(1L, parallel::detectCores() - 2L, na.rm = TRUE)
)

max.select.model <- sdm_summarize(list(max.fit.model))
max.select.model

# Adiciona ao relatório
report.occ.df <- report.occ.df %>%
  add_row(Occurrences = "Dataset for the final model",
          Native = nrow(filter(presence.points.clean.filtered, establishmentMeans == native.range.label)),
          Invasive = nrow(filter(presence.points.clean.filtered, establishmentMeans == non.native.range.label)),
          Total = nrow(presence.points.clean.filtered),
          Obs = paste("MAX: ", max.select.model$regmult, max.select.model$classes))


# Random Forest
tune.grid.raf <- expand.grid(
  mtry = seq_along(predictors.names),
  ntree = c(200, 400, 800, 1200)
)

raf.fit.model <- tune_raf(
  data = data.model,
  response = "pr_ab",
  predictors = predictors.names,
  partition = ".part",
  grid = tune.grid.raf,
  thr = models.threshold,        
  metric = models.metric,         
  n_cores = max(1L, parallel::detectCores() - 2L, na.rm = TRUE)
)

raf.select.model <- sdm_summarize(list(raf.fit.model))
raf.select.model

# Adiciona ao relatório
report.occ.df <- report.occ.df %>%
  add_row(Occurrences = "Dataset for the final model",
          Native = nrow(filter(presence.points.clean.filtered, establishmentMeans == native.range.label)),
          Invasive = nrow(filter(presence.points.clean.filtered, establishmentMeans == non.native.range.label)),
          Total = nrow(presence.points.clean.filtered),
          Obs = paste("RAF: ", raf.select.model$mtry, raf.select.model$ntree))


# Boosted Regression Trees
tune.grid.gbm <- expand.grid(
  n.trees        = c(200, 400, 800, 1200),
  shrinkage      = c(0.005, 0.01),
  n.minobsinnode = c(3, 5, 10)
)

gbm.fit.model <- tune_gbm(
  data = data.model,
  response = "pr_ab",
  predictors = predictors.names,
  partition = ".part",
  grid = tune.grid.gbm,
  thr = models.threshold,        
  metric = models.metric,         
  n_cores = max(1L, parallel::detectCores() - 2L, na.rm = TRUE)
)

gbm.select.model <- sdm_summarize(list(gbm.fit.model))
gbm.select.model

# Adiciona ao relatório
report.occ.df <- report.occ.df %>%
  add_row(
    Occurrences = "Dataset for the final model",
    Native = nrow(filter(presence.points.clean.filtered, establishmentMeans == native.range.label)),
    Invasive = nrow(filter(presence.points.clean.filtered, establishmentMeans == non.native.range.label)),
    Total = nrow(presence.points.clean.filtered),
    Obs = paste("GBM: ", gbm.select.model$n.trees, gbm.select.model$shrinkage, gbm.select.model$n.minobsinnode))


# Neural Network
tune.grid.net <- expand.grid(
  size  = c(2, 4, 6),
  decay = c(0.01, 0.1, 0.5, 1)
)
  
net.fit.model <- tune_net(
  data = data.model,
  response = "pr_ab",
  predictors = predictors.names,
  partition = ".part",
  grid = tune.grid.net,
  thr = models.threshold,        
  metric = models.metric,         
  n_cores = max(1L, parallel::detectCores() - 2L, na.rm = TRUE)
)
  
net.select.model <- sdm_summarize(list(net.fit.model))
net.select.model

# Adiciona ao relatório
report.occ.df <- report.occ.df %>%
  add_row(
    Occurrences = "Dataset for the final model",
    Native = nrow(filter(presence.points.clean.filtered, establishmentMeans == native.range.label)),
    Invasive = nrow(filter(presence.points.clean.filtered, establishmentMeans == non.native.range.label)),
    Total = nrow(presence.points.clean.filtered),
    Obs = paste("NET: ", net.select.model$size, net.select.model$decay))


# SVM
# Support Vector Machine
tune.grid.svm <- expand.grid(
  C     = c(0.5, 2, 8, 32),
  sigma = c(0.001, 0.01, 0.1)
)
  
svm.fit.model <- tune_svm(
  data = data.model,
  response = "pr_ab",
  predictors = predictors.names,
  partition = ".part",
  grid = tune.grid.svm,
  thr = models.threshold,        
  metric = models.metric,         
  n_cores = max(1L, parallel::detectCores() - 2L, na.rm = TRUE)
)

svm.select.model <- sdm_summarize(list(svm.fit.model))
svm.select.model

# Adiciona ao relatório
report.occ.df <- report.occ.df %>%
  add_row(
    Occurrences = "Dataset for the final model",
    Native = nrow(filter(presence.points.clean.filtered, establishmentMeans == native.range.label)),
    Invasive = nrow(filter(presence.points.clean.filtered, establishmentMeans == non.native.range.label)),
    Total = nrow(presence.points.clean.filtered),
    Obs = paste("SVM: ", svm.select.model$C, svm.select.model$sigma))


# GAU
gau.fit.model <- fit_gau(
  data = data.model,
  response = "pr_ab",
  predictors = predictors.names,
  partition = ".part",
  thr = models.threshold,
)

gau.select.model <- sdm_summarize(list(gau.fit.model))
gau.select.model

# Adiciona ao relatório
report.occ.df <- report.occ.df %>%
  add_row(
    Occurrences = "Dataset for the final model",
    Native = nrow(filter(presence.points.clean.filtered, establishmentMeans == native.range.label)),
    Invasive = nrow(filter(presence.points.clean.filtered, establishmentMeans == non.native.range.label)),
    Total = nrow(presence.points.clean.filtered),
    Obs = paste(
      "GAU: default settings;",
      "Predictors:", length(predictors.names),
      "; Threshold criterion:", models.threshold
    )
  )


# GLM
if (sample.size >= length(predictors.names) * 5) {
  glm.fit.model <- fit_glm(
    data = data.model,
    response = "pr_ab",
    predictors = predictors.names,
    partition = ".part",
    thr = models.threshold,
    poly = 2
  )
  
  glm.select.model <- sdm_summarize(list(glm.fit.model))
  glm.select.model
} else {
  glm.fit.model <- NULL
}

# Adiciona ao relatório
report.occ.df <- report.occ.df %>%
  add_row(
    Occurrences = "Dataset for the final model",
    Native = nrow(filter(presence.points.clean.filtered, establishmentMeans == native.range.label)),
    Invasive = nrow(filter(presence.points.clean.filtered, establishmentMeans == non.native.range.label)),
    Total = nrow(presence.points.clean.filtered),
    Obs = paste(
      if (is.null(glm.fit.model)) "GLM: not fitted; sample-size rule not met;" else "GLM: poly = 2;",
      "Predictors:", length(predictors.names),
      "; Threshold criterion:", models.threshold
    )
  )


# GAM
gam.fit.model <- fit_gam(
  data = data.model,
  response = "pr_ab",
  predictors = predictors.names,
  partition = ".part",
  thr = models.threshold,
  k = 5
)

gam.select.model <- sdm_summarize(list(gam.fit.model))
gam.select.model

# Adiciona ao relatório
report.occ.df <- report.occ.df %>%
  add_row(
    Occurrences = "Dataset for the final model",
    Native = nrow(filter(presence.points.clean.filtered, establishmentMeans == native.range.label)),
    Invasive = nrow(filter(presence.points.clean.filtered, establishmentMeans == non.native.range.label)),
    Total = nrow(presence.points.clean.filtered),
    Obs = paste(
      "GAM: k = 5;",
      "Predictors:", length(predictors.names),
      "; Threshold criterion:", models.threshold
    )
  )


# Seleciona os modelos que atendem ao critério
list.models <- mget(grep("\\.fit\\.model$", ls(), value = TRUE))
performance.models <- sdm_summarize(list.models)

best.models <- performance.models %>%
  filter(
    threshold == models.threshold,
    .data[[ensemble.threshold]] >= ensemble.cut,
    # round(.data[[ensemble.threshold]], digits = 1) >= ensemble.cut,
    thr_value > 0,
    thr_value < 1
  )

matching.models <- names(list.models)[unique(best.models$model_ID)]
matching.models

# Interrompe se nenhum modelo atender ao critério
if (length(matching.models) == 0) {
  stop("Nenhum modelo atingiu o critério definido para o ensemble.")
}

ens.models <- list.models[matching.models]

# Combina dois ou mais modelos; um único selecionado permanece modelo individual.
if (length(ens.models) == 1L) {
  ens.fit.model <- ens.models[[1]]
  warning("Apenas um algoritmo selecionado: a saída é um modelo individual, não um ensemble.")
} else {
  ens.fit.model <- fit_ensemble(
    models = ens.models, ens_method = ensemble.method,
    thr = models.threshold, metric = models.metric
  )
}
report.occ.df <- report.occ.df %>%
  add_row(Occurrences = "Final prediction", Total = length(ens.models),
          Obs = paste(if (length(ens.models) == 1L) "Single model:" else "Median ensemble:",
                      paste(names(ens.models), collapse = "; ")))

# Gravação e recuperação dos modelos SDM (opcional) ---------------------------
# Execute apenas se desejar guardar os ajustes ou retomar uma análise.
# Os dados, preditores e demais objetos do fluxo continuam sendo necessários.
# Para ler em outra sessão, informe o caminho do RDS gravado anteriormente.
#
# Grava os modelos individuais, a seleção e o ensemble ajustado
# file.models <- file.path(sdm.folder, "SDM Models.rds")
# saveRDS(
#   list(models = list.models, selected = ens.models,
#        best = best.models, ensemble = ens.fit.model),
#   file = file.models
# )
#
# Recupera os ajustes e a seleção, sem ajustar os modelos novamente
# sdm.models <- readRDS(file.models)
# list.models <- sdm.models$models
# ens.models <- sdm.models$selected
# best.models <- sdm.models$best
# ens.fit.model <- sdm.models$ensemble
# list2env(list.models, envir = .GlobalEnv)

result.select.model <- sdm_summarize(list(ens.fit.model))
result.select.model


# Relatório de Modelagem ----------------------------------------------------------------
# Tabela do Relatório
tmp.table <- 
  report.occ.df |>
  gt() |>
  cols_label(
    Occurrences	= md("**Occurences**"),
    Native      = md("**Native countries (inferred)**"),
    Invasive    = md("**Outside native countries (inferred)**"),
    Total       = md("**Total**"),
    Obs         = md("**Complementar information**")
  ) |>
  fmt_number(
    decimals = 0
  ) |>
  sub_missing(
    missing_text = "-"
  )

file.name <- paste0("Table ", count.table, " - Model REPORT.docx")
file.name <- file.path(sdm.folder, file.name)
count.table = count.table + 1
gtsave(data = tmp.table, filename = file.name)

tmp.table



# Métricas do Modelo -------------------------------------------------------------------------------
# Tabela de Métricas

# Do ajuste do modelo
TPR <- result.select.model$TPR_mean          # sensitivity
TNR <- result.select.model$TNR_mean          # specificity
WTPRTNR <- result.select.model$W_TPR_TNR_mean
TSS <- result.select.model$TSS_mean
Sorensen <- result.select.model$SORENSEN_mean
Jaccard <- result.select.model$JACCARD_mean
FPB <- result.select.model$FPB_mean          # F-measure on presence-background data
OR <- result.select.model$OR_mean
Boyce <- result.select.model$BOYCE_mean
IMAE <- result.select.model$IMAE_mean        # Inverse MAE ( 1−(Mean Absolute Error))
maxSSS <- result.select.model$thr_value
AUCROC <- result.select.model$AUC_mean
CRPS <- result.select.model$CRPS_mean
MCC <- result.select.model$MCC_mean
AUC <- result.select.model$AUC_mean
# Calcula a raiz em cada partição antes de obter a média, como no script ESM.
RMSE <- mean(sqrt(pmax(0, 1 - ens.fit.model$performance_part$CRPS)), na.rm = TRUE)

# Calculadas, usando as anteriores
FPR <- (1 - TNR)
FNR <- (1 - TPR)                              # OR (Omission Rate)
bACC <- (TPR + TNR) / 2                       # Balanced Accuracy

# Ajusta métricas e nomes
metric.names <- c("True Positive Rate, Sensitivity or Recall (TPR)", 
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

metric.values <- c(TPR, TNR, WTPRTNR, TSS, Sorensen, Jaccard, FPB, OR, 
                   Boyce, AUCROC, IMAE, FPR, bACC, CRPS, MCC, RMSE)

metric.df <- data.frame(metric.names, metric.values)

# Tabela de métricas
tmp.table <- 
  metric.df %>%
  gt() %>%
  cols_label(
    metric.names	= md("**Metric Names**"),
    metric.values = md("**Values**")
  ) %>%
  fmt_number(
    columns = c("metric.values"),
    decimals = 5
  ) %>%
  gt::tab_source_note(source_note = paste(
    "Spatial cross-validation after variable/model selection using the available data;",
    "not external or nested validation. Class 0 consists of pseudoabsences.",
    "RMSE: arithmetic mean of partition-level RMSE; not sqrt(1 - mean CRPS)."
  ))

file.name <- paste0("Table ", count.table, " - Model METRICS.docx")
file.name <- file.path(sdm.folder, file.name)
count.table = count.table + 1
gtsave(data = tmp.table, filename = file.name)

tmp.table


# PDPs e Importância das Variáveis--------------------------------------------------------------------------
# Partial dependence plots (flexsdm)
message("Calculando curvas de resposta. Importância por permutação: bloco opcional comentado.")
model.varimp <- list()

# Seleciona apenas os melhores para PDPs
model.prefix <- sub("\\..*$", "", names(list.models))
selected <- model.prefix %in% best.models$model
selected.models <- list.models[selected]
names(selected.models) <- model.prefix[selected]

gc()

for (i in 1:length(selected.models)) {
  model.name <- names(selected.models)[i]
  model.obj  <- selected.models[[i]]
  message(paste("Modelo:", model.name))
  
  model.pdp.plot <- p_pdp(
    model = model.obj$model,
    resolution = 100,
    training_data = data.model.background,
    clamping = FALSE,
  )
  
  if (!is.null(model.pdp.plot)) {
    file.name <- paste0("Plot ", count.plot, " - PDP All - ", model.name, ".png")
    file.name <- file.path(sdm.folder, file.name)
    count.plot = count.plot + 1
    
    png(filename = file.name, units = "cm", width = 20, height = 20, res = 400)
    
    plot(model.pdp.plot)
    
    dev.off()
  }
  
  # # Importância das variáveis
  # model.varimp[[i]] <- flexsdm::sdm_varimp(
  #   models = model.obj,
  #   data = data.model.background,
  #   response = "pr_ab",
  #   predictors = names(predictors.model),
  #   n_sim = 30,
  #   n_cores = max(1L, parallel::detectCores() - 2L, na.rm = TRUE),
  #   thr = model.obj$performance$threshold, 
  #   clamp = TRUE,
  #   pred_type = "cloglog"
  # )
}

# # Dataframe de importância das variáveis
# model.varimp.df <- dplyr::bind_rows(model.varimp)
# 
# # Plot the variable importance 
# file.name <- paste0("Plot ", count.plot, " - Variable Permutation Importance.png")
# file.name <- file.path(sdm.folder, file.name)
# count.plot = count.plot + 1
# 
# png(filename = file.name, units = "cm", width = 35, height = 35, res = 400)
# 
# model.varimp.df %>%
#   tidyr::pivot_longer(
#     cols = TPR:IMAE,
#     names_to = "metric",
#     values_to = "value"
#   ) %>%
#   dplyr::filter(metric %in% c("SORENSEN", "TSS", "AUC")) %>%
#   ggplot(aes(x = predictors, y = value, fill = model)) +
#   scale_fill_brewer(palette = "Dark2") +
#   geom_bar(stat = "identity", position = position_dodge()) +
#   facet_grid(model ~ metric, scales = "free") +
#   coord_flip() + 
#   labs(x = "Predictors", y = "Variable Importance") +
#   theme_bw() +
#   theme(
#     text = element_text(size = 16),
#     axis.title.x = element_text(size = 14),   # título do eixo X
#     axis.title.y = element_text(size = 14),   # título do eixo Y
#     axis.text.x  = element_text(size = 14),   # rótulos do eixo X
#     axis.text.y  = element_text(size = 14)    # rótulos do eixo Y
#   )
# 
# dev.off()

# Histogramas das variáveis ambientais nas ocorrências
# Valores das variáveis nos pontos de ocorrência limpos
presence.points.hist <- presence.points.model[,5:ncol(presence.points.model)]
  
loop.vector <- 1:length(names(predictors.model))

# Histogramas
for (i in loop.vector) {
  
  var.bio <- presence.points.hist[[i]]
  xlabel <- predictors.labels[i]
  breaks <- round(min(var.bio) - 1, digits = 0):round(max(var.bio) + 1, digits = 0)
  min <- min(breaks)
  max <- max(breaks)
  
  hist <- ggplot(presence.points.hist, aes(var.bio)) +
    geom_histogram(aes(y = after_stat(density)),
                   color = "blue",
                   # fill = "#56B4E9",
                   fill = "lightblue",
                   alpha = 0.8,
                   bins = 40) +
    scale_x_continuous(labels = function(x) round(as.numeric(x))) +
    geom_density(color = "#FC4E07", 
                 fill = "#FC4E07",
                 alpha = 0.5) +
    geom_vline(aes(xintercept = mean(var.bio)), 
               linetype = "dashed", 
               linewidth = 0.6) +
    labs(x = xlabel,
         y = "Density")
  
  file.name <- paste0("Plot ", count.plot, " - Histograma and Density - ", predictors.names[i], ".png")
  file.name <- file.path(sdm.folder, file.name)
  count.plot = count.plot + 1
  
  png(filename = file.name, units = "cm", width = 20, height = 20, res = 400)
  plot(hist)
  message(predictors.names[i])
  dev.off()
  
}

# Projeção do Modelo -------------------------------------------------------------
# Projeção global do modelo
# Predict do modelo usando 2,5 minutos
ens.predict.W <- sdm_predict(
  models = ens.fit.model,
  pred = predictors.model,
  con_thr = FALSE, clamp = TRUE, pred_type = "cloglog"
)

# Adequabilidade nas mesmas presenças utilizadas no ajuste final.
# presence.points.model já contém as ocorrências limpas, filtradas e com preditores completos.
# Background e pseudoausências não entram no cálculo de MTP, MTP10 ou desta mediana.
stopifnot(nrow(presence.points.model) > 0L, all(presence.points.model$pr_ab == 1))
presence.points.prob <-
  sdm_extract(
    data = presence.points.model,
    x = "x",
    y = "y",
    env_layer = ens.predict.W[[1]][[1]],
    filter_na = FALSE
  )

# Mantém um data.frame/tibble e nome estável, inclusive com apenas um algoritmo.
presence.points.prob <- presence.points.prob[, ncol(presence.points.prob), drop = FALSE]
names(presence.points.prob) <- "Suitability"
# Não reduzir silenciosamente a amostra de calibração ao definir os limiares.
if (nrow(presence.points.prob) != nrow(presence.points.model) ||
    any(!is.finite(presence.points.prob$Suitability))) {
  stop("Predições ausentes ou não finitas nas presenças de calibração. Conferir o mapa e os dados antes de calcular os limiares.")
}

# Limiares do mapa final, derivados das presenças de calibração.
# Não são validação independente. Para avaliar MTP/MTP10 por partição, calcular
# cada limiar nas presenças de treinamento do fold e aplicá-lo aos dados de teste.
MTP        <- min(presence.points.prob$Suitability)
MTP10      <- as.numeric(quantile(presence.points.prob$Suitability, 0.10, type = 1))
ens.median <- median(presence.points.prob$Suitability)

# Registra a base e os valores usados nos mapas e nas classes.
calibration.thresholds <- data.frame(
  Metric = c("MTP", "MTP10", "Median"), Value = c(MTP, MTP10, ens.median),
  Presences = nrow(presence.points.model), Dataset = "presence.points.model",
  Prediction = "Final fitted model or ensemble; continuous suitability",
  Method = c("minimum", "quantile 0.10; type=1", "median")
)
write.csv(calibration.thresholds, file.path(sdm.folder, "Thresholds - Calibration presences.csv"), row.names = FALSE)

# Grava TIFF do Modelo
raster.name <- paste0(sdm.folder, "/ensemble.predict.W.BASE.tiff")
terra::writeRaster(ens.predict.W[[1]][[1]], 
                   raster.name, 
                   overwrite = TRUE, 
                   gdal = c("COMPRESS=NONE"))

# Histograma de ocorrências pelo modelo
n.breaks <- 10

file.name <- paste0("Plot ", count.plot, " - GLOBAL Histogram of Suitability.png")
file.name <- file.path(sdm.folder, file.name)
count.plot = count.plot + 1

png(filename = file.name, units = "cm", width = 30, height = 20, res = 400)

ggplot(
  data = presence.points.prob,
  aes(x = .data[[ names(presence.points.prob)[ncol(presence.points.prob)] ]])
) + 
  geom_histogram(
    aes(
      y = after_stat(count),
      fill = after_stat(x)
    ),
    breaks = seq(0, 1, length.out = n.breaks + 1),
    color = "black"
  ) +
  # linha de threshold
  geom_vline(
    xintercept = result.select.model$thr_value,
    linetype   = "dashed",
    linewidth  = 1
  ) +
  scale_fill_gradientn(
    colours = color.model(100),
    limits = c(0, 1),
    name = "Suitability"
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(
    x = "Model Suitability",
    y = "Frequency",
    title = NULL
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.title.x = element_text(size = 14),   # título do eixo X
    axis.title.y = element_text(size = 14),   # título do eixo Y
    axis.text.x  = element_text(size = 14),   # rótulos do eixo X
    axis.text.y  = element_text(size = 14)    # rótulos do eixo Y
  )

dev.off()

# ROC do ensemble --------------------------------------------------------------

roc.parts <- lapply(seq_along(ens.models), function(i) {
  d <- ens.models[[i]]$data_ens[, c("rnames", "replicates", "part", "pr_ab", "pred")]
  # Algoritmos podem guardar a resposta como fator ou número.
  d$pr_ab <- as.double(as.character(d$pr_ab))
  names(d)[5] <- paste0("model_", i)
  d
})

roc.data <- Reduce(function(a, b) dplyr::full_join(
  a, b, by = c("rnames", "replicates", "part", "pr_ab")
), roc.parts)

roc.columns <- paste0("model_", seq_along(ens.models))
if (!all(is.finite(as.matrix(roc.data[, roc.columns, drop = FALSE])))) {
  stop("Predições OOF ausentes ou não finitas: conferir modelos e partições.")
}

roc.data$Suitability <- apply(roc.data[, roc.columns, drop = FALSE], 1, median)

# Uma predição por registro, após combinar réplicas disponíveis.
roc.data <- roc.data %>%
  dplyr::group_by(rnames, pr_ab) %>%
  dplyr::summarise(Suitability = mean(Suitability), .groups = "drop")
write.csv(roc.data, file.path(sdm.folder, "ROC - Cross-validation predictions.csv"), row.names = FALSE)

# Calcula a ROC
# 0 = pseudoausência; 1 = presença
# Valores maiores de suitability devem indicar presença
ens.ROC <- pROC::roc(
  response = roc.data$pr_ab,
  predictor = roc.data$Suitability,
  levels = c(0, 1),
  direction = "<",
  percent = FALSE,
  ci = FALSE,
  quiet = TRUE
)

# Sem IC por bootstrap de registros: blocos espaciais não são observações independentes.

# Grava a figura
file.name <- paste0("Plot ", count.plot, " - Ensemble ROC.png")
file.name <- file.path(sdm.folder, file.name)
count.plot <- count.plot + 1

png(
  filename = file.name,
  units = "cm",
  width = 20,
  height = 20,
  res = 400
)

pROC::plot.roc(
  ens.ROC,
  main = "Cross-validation (selection not nested)",
  legacy.axes = TRUE,
  xlab = "1 - Specificity (pseudoabsence)",
  ylab = "Sensitivity (presence)",
  col = "#1c61b6",
  lwd = 2,
  auc.polygon = TRUE,
  auc.polygon.col = "lightblue",
  print.auc = TRUE,
  print.thres = "best",
  print.thres.best.method = "youden",
  print.thres.col = "black",
  print.thres.pch = 20,
  grid = FALSE,
  cex.axis = 1,
  cex.lab = 1
)


dev.off()

# Valores para consulta
ens.ROC$auc



# pAUC corrigida de McClish (pROC); não é teste de razão de ROC parcial ecológica.
# ROC e AUC parciais -----------------------------------------------------------

file.name <- paste0("Plot ", count.plot, " - Ensemble Partial ROC.png")
file.name <- file.path(sdm.folder, file.name)
count.plot <- count.plot + 1

png(
  filename = file.name,
  units = "cm",
  width = 20,
  height = 20,
  res = 400
)

# AUC parcial na faixa de especificidade de 90 a 100%
ens.pROC.sp <- pROC::plot.roc(
  x = roc.data$pr_ab,
  predictor = roc.data$Suitability,
  levels = c(0, 1),
  direction = "<",
  quiet = TRUE,
  legacy.axes = TRUE,
  xlab = "1 - Specificity (pseudoabsence, %)",
  ylab = "Sensitivity (presence, %)",
  percent = TRUE,
  partial.auc = c(100, 90),
  partial.auc.focus = "specificity",
  partial.auc.correct = TRUE,
  print.auc = TRUE,
  print.auc.pattern = "Corrected pAUC (90-100%% spec.):\n%.1f%%",
  print.auc.col = "#1c61b6",
  print.auc.y = 25,
  auc.polygon = TRUE,
  auc.polygon.col = "#1c61b666",
  max.auc.polygon = TRUE,
  max.auc.polygon.col = "#1c61b622",
  main = "Cross-validation (selection not nested)",
  cex.axis = 1,
  cex.lab = 1
)

# AUC parcial na faixa de sensibilidade de 90 a 100%
ens.pROC.se <- pROC::plot.roc(
  x = roc.data$pr_ab,
  predictor = roc.data$Suitability,
  levels = c(0, 1),
  direction = "<",
  quiet = TRUE,
  percent = TRUE,
  add = TRUE,
  type = "n",
  partial.auc = c(100, 90),
  partial.auc.focus = "sensitivity",
  partial.auc.correct = TRUE,
  print.auc = TRUE,
  print.auc.pattern = "Corrected pAUC (90-100%% sens.):\n%.1f%%",
  print.auc.col = "#008600",
  print.auc.y = 40,
  auc.polygon = TRUE,
  auc.polygon.col = "#00860066",
  max.auc.polygon = TRUE,
  max.auc.polygon.col = "#00860022"
)

dev.off()

# threshold que será usado
th <- maxSSS
# th <- 0.2

# Classes descritivas de adequabilidade, sem probabilidades calibradas.
ens.predict.W.values <- terra::values(
  ens.predict.W[[1]][[1]],
  na.rm     = TRUE,
  dataframe = FALSE
)

# Intervalos fixos baseados nos limiares derivados das ocorrências.
intermediate <- MTP + (MTP10 - MTP) / 2
class.breaks <- c(0, MTP, intermediate, MTP10, ens.median, 1)
if (any(!is.finite(class.breaks)) || any(diff(class.breaks) <= 0)) {
  stop("Limites das classes não são estritamente crescentes. Rever MTP, MTP10/2 e mediana; não ordenar automaticamente, pois isso altera o significado das classes.")
}

ens.predict.W.classes <- classInt::classIntervals(
  ens.predict.W.values,
  n = 5,
  style = "fixed",
  fixedBreaks = class.breaks,
  intervalClosure = "right"
)

# Adiciona limites das classes ao relatório
report.occ.df <- report.occ.df %>% 
  dplyr::add_row(
    Occurrences = "Class limits:",
    Native      = NA,
    Invasive    = NA,
    Total       = NA,
    Obs         = paste0(
      "0; MTP = ", round(MTP, 4),
      "; MTP10/2 = ", round(MTP10 / 2, 4),
      "; MTP10 = ", round(MTP10, 4),
      "; median = ", round(ens.median, 4),
      "; 1"
    )
  )

ens.c0.W <- ens.predict.W[[1]][[1]] <= ens.predict.W.classes$brks[[2]]
ens.c1.W <- ens.predict.W[[1]][[1]] > ens.predict.W.classes$brks[[2]] & ens.predict.W[[1]][[1]] <= ens.predict.W.classes$brks[[3]]
ens.c2.W <- ens.predict.W[[1]][[1]] > ens.predict.W.classes$brks[[3]] & ens.predict.W[[1]][[1]] <= ens.predict.W.classes$brks[[4]]
ens.c3.W <- ens.predict.W[[1]][[1]] > ens.predict.W.classes$brks[[4]] & ens.predict.W[[1]][[1]] <= ens.predict.W.classes$brks[[5]]
ens.c4.W <- ens.predict.W[[1]][[1]] > ens.predict.W.classes$brks[[5]] 

# Labels para as classes do mapa
labels.breaks <- c(paste0("Unsuitable"),
                   paste0("Marginal (", formatC(round(expanse(subst(ens.c1.W, FALSE, NA), "km"), 0)$area, big.mark = ",", format = "f", digits = 0), " km\u00B2)"),
                   paste0("Moderate (", formatC(round(expanse(subst(ens.c2.W, FALSE, NA), "km"), 0)$area, big.mark = ",", format = "f", digits = 0), " km\u00B2)"),
                   paste0("High (", formatC(round(expanse(subst(ens.c3.W, FALSE, NA), "km"), 0)$area, big.mark = ",", format = "f", digits = 0), " km\u00B2)"),
                   paste0("Very High (", formatC(round(expanse(subst(ens.c4.W, FALSE, NA), "km"), 0)$area, big.mark = ",", format = "f", digits = 0), " km\u00B2)")
)


# MAPAS do modelo para o Mundo -----------------------------------------------
# Usa points.map.W, preparado no bloco inicial dos mapas.

# 1) Mapa global do modelo ------------------------------------------------
ens.predict.W.map <-
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
  
  # Raster do modelo
  tm_shape(ens.predict.W[[1]][[1]], is.main = TRUE) +
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
  
  # Contorno dos países
  tm_shape(world) +
  tm_polygons(
    col = "black",
    fill = NULL,
    lwd = 0.6
  ) +
  
  # Complementos
  tm_graticules(alpha = 0.5, labels.size = 1) +
  tm_compass(
    type = "arrow",
    size = 1.5,
    position = tm_pos_in(
      pos.h = "LEFT",
      pos.v = "BOTTOM",
      align.h = "center"
    )
  ) +
  tm_credits(
    "Geographic coordinates\nDatum: WGS84",
    position = tm_pos_in(
      pos.h = "LEFT", 
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
      pos.h = "LEFT",
      pos.v = "BOTTOM",
      align.h = "center"
    )
  ) +
  tm_layout(scale = 1) +
  tm_crs(4326)

# Adiciona ocorrências, se existirem
if (nrow(points.map.W) > 0 && plot.occurrences) {
  ens.predict.W.map <- ens.predict.W.map +
    tm_shape(points.map.W) +
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

file.name <- paste0("Map ", count.map, " - Ensemble World.png")
file.name <- file.path(sdm.folder, file.name)
count.map <- count.map + 1

suppressWarnings(
  tmap_save(
    tm = ens.predict.W.map, filename = file.name,
    units = "cm", dpi = 400,
    scale = 1, 
    device = ragg::agg_png
  )
)

# 2) Mapa global de classes ------------------------------------------------
ens.classes.W.map <-
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
  
  # Raster do modelo
  tm_shape(ens.predict.W[[1]][[1]], is.main = TRUE) +
  tm_raster(
    col_alpha = 1,
    col.scale = tm_scale_intervals(
      style = "fixed",
      values = c(
        "#FFFFFF", "#8DD2E8", "#C6E82D",
        "#FFB200", "#FF0000"
      ),
      breaks = c(0, ens.predict.W.classes$brks[2:6]),
      labels = labels.breaks
    ),
    col.legend = tm_legend(
      group_id = "LEG",
      title = "Suitability (Classes)",
      reverse = TRUE,
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
  
  # Contorno dos países
  tm_shape(world) +
  tm_polygons(
    col = "black",
    fill = NULL,
    lwd = 0.6
  ) +
  
  # Complementos
  tm_graticules(alpha = 0.5, labels.size = 1) +
  tm_compass(
    type = "arrow",
    size = 1.5,
    position = tm_pos_in(
      pos.h = "LEFT",
      pos.v = "BOTTOM",
      align.h = "center"
    )
  ) +
  tm_credits(
    "Geographic coordinates\nDatum: WGS84",
    position = tm_pos_in(
      pos.h = "LEFT", 
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
      pos.h = "LEFT",
      pos.v = "BOTTOM",
      align.h = "center"
    )
  ) +
  tm_layout(scale = 1) +
  tm_crs(4326)

# Adiciona ocorrências, se existirem
if (nrow(points.map.W) > 0 && plot.occurrences) {
  ens.classes.W.map <- ens.classes.W.map +
    tm_shape(points.map.W) +
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

# Salva o mapa
file.name <- paste0("Map ", count.map, " - Ensemble World Classes.png")
file.name <- file.path(sdm.folder, file.name)
count.map <- count.map + 1

suppressWarnings(
  tmap_save(
    tm = ens.classes.W.map,
    filename = file.name,
    units = "cm",
    dpi = 400,
    scale = 1,
    device = ragg::agg_png
  )
)


# 3) Mapa global binário ------------------------------------------------
ens.binary.W.map <-
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
  
  # Raster binário do ensemble
  tm_shape(ens.predict.W[[1]][[1]] >= th, is.main = TRUE) +
  tm_raster(
    col_alpha = 1,
    col.scale = tm_scale_categorical(
      levels = c(FALSE, TRUE),
      levels.drop = FALSE,
      values = color.bin,
      labels = c("Unsuitable", "Suitable")
    ),
    col.legend = tm_legend(
      group_id = "LEG",
      title = paste0(
        "Suitability > ",
        formatC(th, digits = 4, format = "f")
      ),
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
  
  # Contorno dos países
  tm_shape(world) +
  tm_polygons(
    col = "black",
    fill = NULL,
    lwd = 0.6
  ) +
  
  # Complementos
  tm_graticules(alpha = 0.5, labels.size = 1) +
  tm_compass(
    type = "arrow",
    size = 1.5,
    position = tm_pos_in(
      pos.h = "LEFT",
      pos.v = "BOTTOM",
      align.h = "center"
    )
  ) +
  tm_credits(
    "Geographic coordinates\nDatum: WGS84",
    position = tm_pos_in(
      pos.h = "LEFT", 
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
      pos.h = "LEFT",
      pos.v = "BOTTOM",
      align.h = "center"
    )
  ) +
  tm_layout(scale = 1) +
  tm_crs(4326)

# Adiciona ocorrências, se existirem
if (nrow(points.map.W) > 0 && plot.occurrences) {
  ens.binary.W.map <- ens.binary.W.map +
    tm_shape(points.map.W) +
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

# Salva o mapa
file.name <- paste0("Map ", count.map, " - Ensemble World Threshold Binary.png")
file.name <- file.path(sdm.folder, file.name)
count.map <- count.map + 1

suppressWarnings(
  tmap_save(
    tm = ens.binary.W.map,
    filename = file.name,
    units = "cm",
    dpi = 400,
    scale = 1,
    device = ragg::agg_png
  )
)


# 4) Mapa global MTP e MTP10 ------------------------------------------------
area.mtp   <- ens.predict.W[[1]][[1]] >= MTP
area.mtp   <- terra::ifel(area.mtp, 1, NA)

area.mtp10 <- ens.predict.W[[1]][[1]] >= MTP10
area.mtp10 <- terra::ifel(area.mtp10, 1, NA)

ens.binary.W.map <-
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
  
  # Área adequada pelo MTP
  tm_shape(area.mtp, is.main = TRUE) +
  tm_raster(
    col_alpha = 1,
    col.scale = tm_scale_categorical(
      values = "grey",
      labels = "Suitable",
      value.na = NA
    ),
    col.legend = tm_legend(
      group_id = "LEG",
      title = paste0("MTP = ", formatC(MTP, digits = 4, format = "f")),
      title.size = 0.8,
      title.align = "left",
      title.fontface = "bold",
      title.padding = c(0.5, 0, 0, 0),
      text.size = 0.8,
      show = TRUE,
      na.show = FALSE,
      orientation = "portrait",
      item.height = 1,
      item.width = 1,
      item.space = 0.3,
      margins = c(0.3, 0.5, 0.5, 0.5)
    )
  ) +
  
  # Área adequada pelo MTP10
  tm_shape(area.mtp10) +
  tm_raster(
    col_alpha = 1,
    col.scale = tm_scale_categorical(
      values = "#D37295",
      labels = "Suitable",
      value.na = NA
    ),
    col.legend = tm_legend(
      group_id = "LEG",
      title = paste0("MTP10 = ", formatC(MTP10, digits = 4, format = "f")),
      title.size = 0.8,
      title.align = "left",
      title.fontface = "bold",
      title.padding = c(0.5, 0, 0, 0),
      text.size = 0.8,
      show = TRUE,
      na.show = FALSE,
      orientation = "portrait",
      item.height = 1,
      item.width = 1,
      item.space = 0.3,
      margins = c(0.3, 0.5, 0.5, 0.5)
    )
  ) +
  
  # Contorno dos países
  tm_shape(world) +
  tm_polygons(
    col = "black",
    fill = NULL,
    lwd = 0.6
  ) +
  
  # Complementos
  tm_graticules(alpha = 0.5, labels.size = 1) +
  tm_compass(
    type = "arrow",
    size = 1.5,
    position = tm_pos_in(
      pos.h = "LEFT",
      pos.v = "BOTTOM",
      align.h = "center"
    )
  ) +
  tm_credits(
    "Geographic coordinates\nDatum: WGS84",
    position = tm_pos_in(
      pos.h = "LEFT", 
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
      pos.h = "LEFT",
      pos.v = "BOTTOM",
      align.h = "center"
    )
  ) +
  tm_layout(scale = 1) +
  tm_crs(4326)

# Adiciona ocorrências, se existirem
if (nrow(points.map.W) > 0 && plot.occurrences) {
  ens.binary.W.map <- ens.binary.W.map +
    tm_shape(points.map.W) +
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

# Salva o mapa
file.name <- paste0(
  "Map ", count.map, " - Ensemble World Threshold MTP x MTP10.png"
)
file.name <- file.path(sdm.folder, file.name)
count.map <- count.map + 1

suppressWarnings(
  tmap_save(
    tm = ens.binary.W.map,
    filename = file.name,
    units = "cm",
    dpi = 400,
    scale = 1,
    device = ragg::agg_png
  )
)


# MAPAS do modelo para o Brasil -----------------------------------------------
# Usa points.map.BR, preparado no bloco inicial dos mapas.

# 1) Mapa do modelo para o Brasil ------------------------------------------------
ens.predict.BR.map <-
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
  tm_shape(crop(ens.predict.W[[1]][[1]], mask.BR, mask = TRUE), is.main = TRUE) +
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
  ens.predict.BR.map <- ens.predict.BR.map +
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

file.name <- paste0("Map ", count.map, " - Ensemble Brazil.png")
file.name <- file.path(sdm.folder, file.name)
count.map <- count.map + 1

suppressWarnings(
  tmap_save(
    tm = ens.predict.BR.map, filename = file.name,
    units = "cm", dpi = 400,
    scale = 1, 
    device = ragg::agg_png
  )
)


# 2) Mapa de classes para o Brasil ------------------------------------------------
ens.classes.BR.map <-
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
  tm_shape(crop(ens.predict.W[[1]][[1]], mask.BR, mask = TRUE), is.main = TRUE) +
  tm_raster(
    col_alpha = 1,
    col.scale = tm_scale_intervals(
      style = "fixed",
      values = c(
        "#FFFFFF", "#8DD2E8", "#C6E82D",
        "#FFB200", "#FF0000"
      ),
      breaks = c(0, ens.predict.W.classes$brks[2:6]),
      labels = labels.breaks
    ),
    col.legend = tm_legend(
      group_id = "LEG",
      title = "Suitability (Classes)",
      reverse = TRUE,
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
  ens.classes.BR.map <- ens.classes.BR.map +
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

file.name <- paste0("Map ", count.map, " - Ensemble Brazil Classes.png")
file.name <- file.path(sdm.folder, file.name)
count.map <- count.map + 1

suppressWarnings(
  tmap_save(
    tm = ens.classes.BR.map, filename = file.name,
    units = "cm", dpi = 400,
    scale = 1, 
    device = ragg::agg_png
  )
)

# 3) Mapa binário para o Brasil ------------------------------------------------
ens.binary.BR.map <-
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
  tm_shape(crop(ens.predict.W[[1]][[1]] >= th, mask.BR, mask = TRUE), is.main = TRUE) +
  tm_raster(
    col_alpha = 1,
    col.scale = tm_scale_categorical(
      levels = c(FALSE, TRUE),
      levels.drop = FALSE,
      values = color.bin,
      labels = c("Unsuitable", "Suitable")
    ),
    col.legend = tm_legend(
      group_id = "LEG",
      title = paste0(
        "Suitability > ",
        formatC(th, digits = 4, format = "f")
      ),
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
  ens.binary.BR.map <- ens.binary.BR.map +
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

file.name <- paste0("Map ", count.map, " - Ensemble Brazil Threshold Binary.png")
file.name <- file.path(sdm.folder, file.name)
count.map <- count.map + 1

suppressWarnings(
  tmap_save(
    tm = ens.binary.BR.map, filename = file.name,
    units = "cm", dpi = 400,
    scale = 1, 
    device = ragg::agg_png
  )
)


# 4) Mapa de extrapolação (Shape) para o Brasil ------------------------------------------------
model.predict.extrapolation.BR <-
  extra_eval(
    training_data = data.model,
    pr_ab = "pr_ab",
    projection_data = crop(predictors.model, mask.BR, mask = TRUE),
    metric = "mahalanobis",
  )

ens.extrapolation.BR.map <-
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
  tm_shape(model.predict.extrapolation.BR, is.main = TRUE) +
  tm_raster(
    col_alpha = 1,
    col.scale = tm_scale_continuous(
      n = 5,
      values = color.bin
    ),
    col.legend = tm_legend(
      group_id = "LEG",
      title = "Extrapolation Pattern",
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
  ens.extrapolation.BR.map <- ens.extrapolation.BR.map +
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

file.name <- paste0("Map ", count.map, " - Model Extrapolation Brazil.png")
file.name <- file.path(sdm.folder, file.name)
count.map <- count.map + 1

suppressWarnings(
  tmap_save(
    tm = ens.extrapolation.BR.map, filename = file.name,
    units = "cm", dpi = 400,
    scale = 1, 
    device = ragg::agg_png
  )
)


# MAPAS do modelo para um estado do Brasil -----------------------------------------------
# Usa points.map.BR, preparado no bloco inicial dos mapas.
state.abbrev     <- "MG"
state.br         <- BR1[BR1$abbrev_state == state.abbrev, ]
mask.state       <- terra::vect(state.br$geom)
points.map.state <- terra::mask(terra::vect(points.map.W), mask.state)

# 1) Mapa do modelo para o estado ------------------------------------------------
ens.predict.state.map <-
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
  tm_shape(crop(ens.predict.W[[1]][[1]], mask.state, mask = TRUE), is.main = TRUE) +
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
  tm_shape(BR1) +
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
if (nrow(points.map.state) > 0 && plot.occurrences) {
  ens.predict.state.map <- ens.predict.state.map +
    tm_shape(points.map.BR) +
    tm_symbols(
      size = 0.3,
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

file.name <- paste0("Map ", count.map, " - Ensemble ", state.abbrev, ".png")
file.name <- file.path(sdm.folder, file.name)
count.map <- count.map + 1

suppressWarnings(
  tmap_save(
    tm = ens.predict.state.map, filename = file.name,
    units = "cm", dpi = 400,
    scale = 1, 
    device = ragg::agg_png
  )
)


# 2) Mapa de classes para o estado ------------------------------------------------
ens.classes.state.map <-
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
  tm_shape(crop(ens.predict.W[[1]][[1]], mask.state, mask = TRUE), is.main = TRUE) +
  tm_raster(
    col_alpha = 1,
    col.scale = tm_scale_intervals(
      style = "fixed",
      values = c(
        "#FFFFFF", "#8DD2E8", "#C6E82D",
        "#FFB200", "#FF0000"
      ),
      breaks = c(0, ens.predict.W.classes$brks[2:6]),
      labels = labels.breaks
    ),
    col.legend = tm_legend(
      group_id = "LEG",
      title = "Suitability (Classes)",
      reverse = TRUE,
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
  tm_shape(BR1) +
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
if (nrow(points.map.state) > 0 && plot.occurrences) {
  ens.classes.state.map <- ens.classes.state.map +
    tm_shape(points.map.BR) +
    tm_symbols(
      size = 0.3,
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

file.name <- paste0("Map ", count.map, " - Ensemble ", state.abbrev ," Classes.png")
file.name <- file.path(sdm.folder, file.name)
count.map <- count.map + 1

suppressWarnings(
  tmap_save(
    tm = ens.classes.state.map, filename = file.name,
    units = "cm", dpi = 400,
    scale = 1, 
    device = ragg::agg_png
  )
)

# 3) Mapa binário para o estado ------------------------------------------------
ens.binary.state.map <-
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
  tm_shape(crop(ens.predict.W[[1]][[1]] >= th, mask.state, mask = TRUE), is.main = TRUE) +
  tm_raster(
    col_alpha = 1,
    col.scale = tm_scale_categorical(
      levels = c(FALSE, TRUE),
      levels.drop = FALSE,
      values = color.bin,
      labels = c("Unsuitable", "Suitable")
    ),
    col.legend = tm_legend(
      group_id = "LEG",
      title = paste0(
        "Suitability > ",
        formatC(th, digits = 4, format = "f")
      ),
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
  tm_shape(BR1) +
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
if (nrow(points.map.state) > 0 && plot.occurrences) {
  ens.binary.state.map <- ens.binary.state.map +
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

file.name <- paste0("Map ", count.map, " - Ensemble ", state.abbrev ," Threshold Binary.png")
file.name <- file.path(sdm.folder, file.name)
count.map <- count.map + 1

suppressWarnings(
  tmap_save(
    tm = ens.binary.state.map, filename = file.name,
    units = "cm", dpi = 400,
    scale = 1, 
    device = ragg::agg_png
  )
)


# Projeção para climas futuros - CMIP6 -----------------------------------------

# # Configurações
# CMIP6.models <- c("ACCESS-CM2", "CMCC-ESM2", "EC-Earth3-Veg","IPSL-CM6A-LR", "MIROC6", "MRI-ESM2-0")
# CMIP6.ssps   <- c("245", "370", "585")
# CMIP6.times  <- c("2041-2060", "2061-2080")

CMIP6.models <- c("MIROC6", "MRI-ESM2-0")
CMIP6.ssps   <- c("245", "370")
CMIP6.times  <- c("2061-2080")

# Opcional: diagnosticar novidade ambiental futura no Brasil por GCM.
# Aumenta o tempo de execução; clamping não substitui esse diagnóstico.
evaluate.future.extrapolation <- FALSE

# Mantém o limiar e as classes definidos no cenário base.
future.threshold     <- th
future.breaks        <- ens.predict.W.classes$brks
future.class.labels  <- c("Unsuitable", "Marginal", "Moderate", "High", "Very high")
future.class.colors  <- c("#FFFFFF", "#8DD2E8", "#C6E82D", "#FFB200", "#FF0000")
future.change.labels <- c("Stable unsuitable", "Loss", "Gain", "Stable suitable")
future.change.colors <- c("grey85", "#FF0000", "#00B0F0", "#00B050")

future.gcm.folder    <- file.path(future.folder, "GCM")
future.tiff.folder   <- file.path(future.folder, "GeoTIFF")
dir.create(future.gcm.folder, recursive = TRUE, showWarnings = FALSE)
dir.create(future.tiff.folder, recursive = TRUE, showWarnings = FALSE)

# Base e preditores
future.base.W        <- ens.predict.W[[1]][[1]]
future.template      <- predictors.model[[1]]
stopifnot(terra::compareGeom(future.base.W, future.template))
future.bio.names     <- paste0("Bio", formatC(1:19, width = 2, flag = "0"))
future.climate.vars  <- intersect(names(predictors.model), future.bio.names)
future.static.vars   <- setdiff(names(predictors.model), future.bio.names)
stopifnot(length(future.climate.vars) > 0)

# Preditores sem projeção CMIP6, como solo e elevação, permanecem constantes.
# Essa hipótese deve ser registrada na interpretação dos cenários.
# Não aplica restrição de dispersão: mudanças representam adequabilidade potencial.

# Reutiliza points.map.W e points.map.BR, preparados nos mapas do presente.

# Registro das decisões da projeção
future.config <- data.frame(
  Item = c("Species", "GCMs", "SSPs", "Periods", "GCM combination",
           "Binary threshold", "Class breaks", "Climate predictors",
           "Static predictors", "Dispersal", "SD between GCMs",
           "R", "flexsdm", "geodata", "terra"),
  Value = c(
    my.species, paste(CMIP6.models, collapse = "; "),
    paste(CMIP6.ssps, collapse = "; "), paste(CMIP6.times, collapse = "; "),
    paste0("Arithmetic mean of ", length(CMIP6.models), " GCM projections; NA preserved"),
    paste0("Suitability >= ", future.threshold), paste(future.breaks, collapse = "; "),
    paste(future.climate.vars, collapse = "; "), paste(future.static.vars, collapse = "; "),
    "No dispersal restriction; potential suitability",
    paste0("Population SD across ", length(CMIP6.models), " GCMs; terra::stdev(pop = TRUE)"),
    as.character(getRversion()), as.character(packageVersion("flexsdm")),
    as.character(packageVersion("geodata")), as.character(packageVersion("terra"))
  )
)
write.csv(future.config, file.path(future.folder, "Future - Settings.csv"), row.names = FALSE)
saveRDS(ens.fit.model, file.path(future.folder, "Future - Fitted ensemble.rds"))

if (!evaluate.future.extrapolation) {
  warning("Novidade ambiental futura não diagnosticada. Clamping ativo não garante transferência; ativar evaluate.future.extrapolation para diagnóstico Shape no Brasil.")
}
writeLines(c(
  paste("Future Shape diagnostic in Brazil:", evaluate.future.extrapolation),
  "Two GCMs are a didactic subset, not a complete uncertainty sample.",
  "SD across GCM projections is not total predictive uncertainty.",
  "Clamping does not rule out novel predictor combinations."
), file.path(future.folder, "Future - Interpretation limits.txt"))

future.areas   <- data.frame()
future.changes <- data.frame()
future.fuzzy   <- data.frame()

# Projeções por SSP, período e GCM ---------------------------------------------
for (CMIP6.ssp in CMIP6.ssps) {
  for (CMIP6.time in CMIP6.times) {
    message("Cenário: SSP", CMIP6.ssp, " - ", CMIP6.time)
    future.scenario <- paste0("SSP", CMIP6.ssp, " - ", CMIP6.time)
    future.gcm.files <- setNames(character(length(CMIP6.models)), CMIP6.models)

    for (CMIP6.model in CMIP6.models) {
      message("Projetando: ", CMIP6.model)

      predictors.CMIP6 <- geodata::cmip6_world(
        model = CMIP6.model,
        ssp = CMIP6.ssp,
        time = CMIP6.time,
        var = "bioc",
        res = 2.5,
        path = CMIP6.folder
      )
      stopifnot(terra::nlyr(predictors.CMIP6) == 19)
      names(predictors.CMIP6) <- future.bio.names
      predictors.CMIP6.model <- predictors.CMIP6[[future.climate.vars]]
      predictors.CMIP6.model <- terra::crop(predictors.CMIP6.model, future.template)

      # Compatibiliza a grade antes de combinar variáveis e comparar cenários.
      if (!terra::compareGeom(predictors.CMIP6.model, future.template, stopOnError = FALSE)) {
        predictors.CMIP6.model <- terra::project(
          predictors.CMIP6.model, future.template, method = "bilinear"
        )
      }
      if (length(future.static.vars) > 0) {
        predictors.CMIP6.model <- c(
          predictors.CMIP6.model, predictors.model[[future.static.vars]]
        )
      }
      predictors.CMIP6.model <- predictors.CMIP6.model[[names(predictors.model)]]
      stopifnot(identical(names(predictors.CMIP6.model), names(predictors.model)))

      ens.predict.CMIP6.W <- flexsdm::sdm_predict(
        models = ens.fit.model,
        pred = predictors.CMIP6.model,
        con_thr = FALSE,
        clamp = TRUE,
        pred_type = "cloglog"
      )
      if (evaluate.future.extrapolation) {
        future.extrapolation.BR <- flexsdm::extra_eval(
          training_data = data.model, pr_ab = "pr_ab",
          projection_data = terra::crop(predictors.CMIP6.model, mask.BR, mask = TRUE),
          metric = "mahalanobis"
        )
        terra::writeRaster(future.extrapolation.BR,
          file.path(future.tiff.folder, paste0("Shape - BR - ", CMIP6.model, " - ", future.scenario, ".tif")),
          datatype = "FLT4S", gdal = "COMPRESS=LZW", overwrite = TRUE)
      }
      future.gcm.files[CMIP6.model] <- file.path(
        future.gcm.folder,
        paste0("Ensemble - ", CMIP6.model, " - ", future.scenario, ".tif")
      )
      terra::writeRaster(
        ens.predict.CMIP6.W[[1]][[1]],
        filename = future.gcm.files[CMIP6.model],
        datatype = "FLT4S", gdal = "COMPRESS=LZW", overwrite = TRUE
      )
    }

    # Usa exclusivamente os arquivos dos GCMs configurados deste SSP e período.
    # Não mistura arquivos de cenários anteriores por busca genérica de nomes.
    future.gcms.W <- terra::rast(future.gcm.files)
    stopifnot(terra::nlyr(future.gcms.W) == length(CMIP6.models))
    future.mean.W <- terra::app(future.gcms.W, fun = mean, na.rm = FALSE)
    future.sd.gcm.W <- terra::stdev(future.gcms.W, pop = TRUE, na.rm = FALSE)

    # Projeções CMIP6 Globais ------------------------------------
    future.region <- "World"
    future.base <- future.base.W
    future.mean <- future.mean.W
    future.sd.gcm <- future.sd.gcm.W
    future.gcms <- future.gcms.W
    future.points <- points.map.W

    stopifnot(terra::compareGeom(future.base, future.mean))

    # Domínio comum: não converte ausência de informação em inadequabilidade.
    future.common <- terra::ifel(is.na(future.base) | is.na(future.mean), NA, 1)
    future.base <- terra::mask(future.base, future.common)
    future.mean <- terra::mask(future.mean, future.common)
    future.sd.gcm <- terra::mask(future.sd.gcm, future.common)
    future.gcms <- terra::mask(future.gcms, future.common)

    # Binários: mantém exatamente o mesmo limiar nos dois cenários.
    future.base.binary <- terra::ifel(future.base >= future.threshold, 1, 0)
    future.binary <- terra::ifel(future.mean >= future.threshold, 1, 0)

    # 1 = 0->0; 2 = 1->0; 3 = 0->1; 4 = 1->1.
    future.change <- 1 + future.base.binary + 2 * future.binary
    future.classes <- terra::classify(
      future.mean, rcl = future.breaks, include.lowest = TRUE, right = TRUE
    ) + 1

    # Áreas em km2: pesos pela área real das células, sem contar pixels como km2.
    future.cell.area <- terra::cellSize(future.base, unit = "km", mask = TRUE)
    future.change.area <- terra::zonal(future.cell.area, future.change, fun = "sum", na.rm = TRUE)
    names(future.change.area) <- c("Code", "Area_km2")
    future.change.area <- merge(
      data.frame(Code = 1:4, Change = future.change.labels),
      future.change.area, by = "Code", all.x = TRUE, sort = TRUE
    )
    future.change.area$Area_km2[is.na(future.change.area$Area_km2)] <- 0
    future.change.area$Region <- future.region
    future.change.area$Scenario <- future.scenario
    future.changes <- rbind(future.changes, future.change.area)

    # Área adequada e áreas por classe: base, cada GCM e média futura.
    future.area.rasters <- c(future.base, future.gcms, future.mean)
    future.area.names <- c("BASE", CMIP6.models, "GCM mean")
    for (j in seq_along(future.area.names)) {
      future.r <- future.area.rasters[[j]]
      future.suitable.area <- terra::global(
        terra::ifel(future.r >= future.threshold, future.cell.area, 0),
        fun = "sum", na.rm = TRUE
      )[1, 1]
      if (j == 1) future.base.area <- future.suitable.area
      future.area.change <- NA_real_
      if (future.base.area > 0) {
        future.area.change <- 100 * (future.suitable.area - future.base.area) / future.base.area
      }
      future.class.r <- terra::classify(future.r, future.breaks, include.lowest = TRUE, right = TRUE) + 1
      future.class.area <- terra::zonal(future.cell.area, future.class.r, fun = "sum", na.rm = TRUE)
      names(future.class.area) <- c("Code", "Area_km2")
      future.class.area <- merge(
        data.frame(Code = 1:5, Class = future.class.labels), future.class.area,
        by = "Code", all.x = TRUE, sort = TRUE
      )
      future.class.area$Area_km2[is.na(future.class.area$Area_km2)] <- 0
      future.areas <- rbind(future.areas, data.frame(
        Region = future.region, Scenario = future.scenario, Model = future.area.names[j],
        Class = future.class.area$Class, Class_area_km2 = future.class.area$Area_km2,
        Suitable_area_km2 = future.suitable.area, Change_percent = future.area.change
      ))
    }

    # Mudanças fuzzy: índices de adequabilidade, não áreas geográficas em km2.
    future.values <- terra::values(c(future.base, future.mean), mat = TRUE)
    future.values <- future.values[complete.cases(future.values), , drop = FALSE]
    future.fuzzy.result <- fuzzySim::fuzzyRangeChange(
      future.values[, 1], future.values[, 2], number = FALSE, plot = FALSE,
      measures = c("Gain", "Loss", "Stable positive", "Stable negative", "Balance")
    )
    future.fuzzy.df <- data.frame(
      Region = future.region, Scenario = future.scenario,
      Measure = rownames(future.fuzzy.result), Proportion = future.fuzzy.result$Proportion
    )
    future.fuzzy <- rbind(future.fuzzy, future.fuzzy.df)
    future.fuzzy.plot <- ggplot(future.fuzzy.df, aes(x = Measure, y = Proportion, fill = Measure)) +
      geom_col() +
      scale_fill_manual(values = color.barplot, guide = "none") +
      labs(x = NULL, y = "Fuzzy suitability change", title = paste(future.region, future.scenario)) +
      theme_bw() + theme(axis.text.x = element_text(angle = 25, hjust = 1))
    file.name <- file.path(future.folder, paste0("Plot ", count.plot, " - Fuzzy change - ", future.region, " - ", future.scenario, ".png"))
    count.plot <- count.plot + 1
    ggsave(file.name, plot = future.fuzzy.plot, width = 25, height = 16,
           units = "cm", dpi = 400, device = ragg::agg_png)

    # Áreas por classe na legenda: calculadas separadamente para cada região.
    future.legend.areas <- terra::zonal(
      future.cell.area, future.classes, fun = "sum", na.rm = TRUE
    )
    names(future.legend.areas) <- c("Code", "Area_km2")
    future.legend.areas <- merge(
      data.frame(Code = 1:5, Class = future.class.labels),
      future.legend.areas, by = "Code", all.x = TRUE, sort = TRUE
    )
    future.legend.areas$Area_km2[is.na(future.legend.areas$Area_km2)] <- 0
    future.labels.breaks <- paste0(
      future.legend.areas$Class, " (",
      formatC(future.legend.areas$Area_km2, format = "f", digits = 0, big.mark = ","),
      " km²)"
    )


    # 1) Adequabilidade média - World ----------------------------------------
    raster.name <- file.path(future.tiff.folder, paste0(
      "Ensemble - World - ", future.scenario, " - Mean.tif"
    ))
    terra::writeRaster(future.mean, raster.name, datatype = "FLT4S",
                       gdal = "COMPRESS=LZW", overwrite = TRUE)

    future.map.raster <- future.mean
    ens.future.mean.W.map <-
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

      # Raster do modelo
      tm_shape(future.map.raster, is.main = TRUE) +
      tm_raster(
        col_alpha = 1,
        col.scale = tm_scale_continuous(
        n = 10, values = color.model(10), midpoint = 0.5, limits = c(0, 1)
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
          item.space = 0,
          margins = c(0.3, 0.5, 0.5, 0.5)
        )
      ) +

      # Contorno dos países
      tm_shape(world) +
      tm_polygons(
        col = "black",
        fill = NULL,
        lwd = 0.6
      ) +

      # Complementos
      tm_graticules(alpha = 0.5, labels.size = 1) +
      tm_compass(
        type = "arrow",
        size = 1.5,
        position = tm_pos_in(
          pos.h = "LEFT",
          pos.v = "BOTTOM",
          align.h = "center"
        )
      ) +
      tm_credits(
        "Geographic coordinates\nDatum: WGS84",
        position = tm_pos_in(
          pos.h = "LEFT", 
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
          pos.h = "LEFT",
          pos.v = "BOTTOM",
          align.h = "center"
        )
      ) +
      tm_title(paste("Mean suitability -", future.scenario),
               position = tm_pos_out("center", "top"), size = 0.9) +
      tm_layout(scale = 1) +
      tm_crs(4326)

    # Adiciona ocorrências, se existirem
    if (nrow(future.points) > 0 && plot.occurrences) {
      ens.future.mean.W.map <- ens.future.mean.W.map +
        tm_shape(future.points) +
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
            title = "Current occurrences",
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

    file.name <- file.path(future.folder, paste0(
      "Map ", count.map, " - World - ", future.scenario, " - Mean.png"
    ))
    count.map <- count.map + 1
    suppressWarnings(
      tmap_save(
        tm = ens.future.mean.W.map, filename = file.name,
        units = "cm", dpi = 400, scale = 1, device = ragg::agg_png
      )
    )

    # 2) Classes de adequabilidade - World ----------------------------------------
    raster.name <- file.path(future.tiff.folder, paste0(
      "Ensemble - World - ", future.scenario, " - Classes.tif"
    ))
    terra::writeRaster(future.classes, raster.name, datatype = "FLT4S",
                       gdal = "COMPRESS=LZW", overwrite = TRUE)

    future.map.raster <- future.classes
    levels(future.map.raster) <- data.frame(ID = 1:5, Class = future.class.labels)
    ens.future.classes.W.map <-
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

      # Raster do modelo
      tm_shape(future.map.raster, is.main = TRUE) +
      tm_raster(
        col_alpha = 1,
        col.scale = tm_scale_categorical(
        values = future.class.colors, labels = future.labels.breaks,
        levels.drop = FALSE
      ),
        col.legend = tm_legend(
          group_id = "LEG",
          title = "Suitability (Classes)",
          reverse = TRUE,
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

      # Contorno dos países
      tm_shape(world) +
      tm_polygons(
        col = "black",
        fill = NULL,
        lwd = 0.6
      ) +

      # Complementos
      tm_graticules(alpha = 0.5, labels.size = 1) +
      tm_compass(
        type = "arrow",
        size = 1.5,
        position = tm_pos_in(
          pos.h = "LEFT",
          pos.v = "BOTTOM",
          align.h = "center"
        )
      ) +
      tm_credits(
        "Geographic coordinates\nDatum: WGS84",
        position = tm_pos_in(
          pos.h = "LEFT", 
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
          pos.h = "LEFT",
          pos.v = "BOTTOM",
          align.h = "center"
        )
      ) +
      tm_title(paste("Suitability classes -", future.scenario),
               position = tm_pos_out("center", "top"), size = 0.9) +
      tm_layout(scale = 1) +
      tm_crs(4326)

    # Adiciona ocorrências, se existirem
    if (nrow(future.points) > 0 && plot.occurrences) {
      ens.future.classes.W.map <- ens.future.classes.W.map +
        tm_shape(future.points) +
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
            title = "Current occurrences",
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

    file.name <- file.path(future.folder, paste0(
      "Map ", count.map, " - World - ", future.scenario, " - Classes.png"
    ))
    count.map <- count.map + 1
    suppressWarnings(
      tmap_save(
        tm = ens.future.classes.W.map, filename = file.name,
        units = "cm", dpi = 400, scale = 1, device = ragg::agg_png
      )
    )

    # 3) Adequabilidade binária - World ----------------------------------------
    raster.name <- file.path(future.tiff.folder, paste0(
      "Ensemble - World - ", future.scenario, " - Binary.tif"
    ))
    terra::writeRaster(future.binary, raster.name, datatype = "FLT4S",
                       gdal = "COMPRESS=LZW", overwrite = TRUE)

    future.map.raster <- future.binary
    levels(future.map.raster) <- data.frame(ID = 0:1, Class = c("Unsuitable", "Suitable"))
    ens.future.binary.W.map <-
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

      # Raster do modelo
      tm_shape(future.map.raster, is.main = TRUE) +
      tm_raster(
        col_alpha = 1,
        col.scale = tm_scale_categorical(
        values = color.bin, levels.drop = FALSE
      ),
        col.legend = tm_legend(
          group_id = "LEG",
          title = paste0("Suitability >= ", formatC(future.threshold, digits = 4, format = "f")),
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

      # Contorno dos países
      tm_shape(world) +
      tm_polygons(
        col = "black",
        fill = NULL,
        lwd = 0.6
      ) +

      # Complementos
      tm_graticules(alpha = 0.5, labels.size = 1) +
      tm_compass(
        type = "arrow",
        size = 1.5,
        position = tm_pos_in(
          pos.h = "LEFT",
          pos.v = "BOTTOM",
          align.h = "center"
        )
      ) +
      tm_credits(
        "Geographic coordinates\nDatum: WGS84",
        position = tm_pos_in(
          pos.h = "LEFT", 
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
          pos.h = "LEFT",
          pos.v = "BOTTOM",
          align.h = "center"
        )
      ) +
      tm_title(paste("Binary suitability -", future.scenario),
               position = tm_pos_out("center", "top"), size = 0.9) +
      tm_layout(scale = 1) +
      tm_crs(4326)

    # Adiciona ocorrências, se existirem
    if (nrow(future.points) > 0 && plot.occurrences) {
      ens.future.binary.W.map <- ens.future.binary.W.map +
        tm_shape(future.points) +
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
            title = "Current occurrences",
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

    file.name <- file.path(future.folder, paste0(
      "Map ", count.map, " - World - ", future.scenario, " - Binary.png"
    ))
    count.map <- count.map + 1
    suppressWarnings(
      tmap_save(
        tm = ens.future.binary.W.map, filename = file.name,
        units = "cm", dpi = 400, scale = 1, device = ragg::agg_png
      )
    )

    # 4) Desvio-padrão entre GCMs - World ----------------------------------------
    raster.name <- file.path(future.tiff.folder, paste0(
      "Ensemble - World - ", future.scenario, " - SD_GCM.tif"
    ))
    terra::writeRaster(future.sd.gcm, raster.name, datatype = "FLT4S",
                       gdal = "COMPRESS=LZW", overwrite = TRUE)

    future.map.raster <- future.sd.gcm
    ens.future.sd.gcm.W.map <-
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

      # Raster do modelo
      tm_shape(future.map.raster, is.main = TRUE) +
      tm_raster(
        col_alpha = 1,
        col.scale = tm_scale_continuous(
        values = c("white", "#6BAED6", "#08306B"),
        limits = c(0, 0.5), ticks = seq(0, 0.5, 0.1)
      ),
        col.legend = tm_legend(
          group_id = "LEG",
          title = "SD among GCMs",
          reverse = TRUE,
          title.size = 0.8,
          title.align = "left",
          title.fontface = "bold",
          title.padding = c(0.5, 0, 0, 0),
          text.size = 0.8,
          show = TRUE,
          orientation = "portrait",
          height = 13,
          item.space = 0,
          margins = c(0.3, 0.5, 0.5, 0.5)
        )
      ) +

      # Contorno dos países
      tm_shape(world) +
      tm_polygons(
        col = "black",
        fill = NULL,
        lwd = 0.6
      ) +

      # Complementos
      tm_graticules(alpha = 0.5, labels.size = 1) +
      tm_compass(
        type = "arrow",
        size = 1.5,
        position = tm_pos_in(
          pos.h = "LEFT",
          pos.v = "BOTTOM",
          align.h = "center"
        )
      ) +
      tm_credits(
        "Geographic coordinates\nDatum: WGS84",
        position = tm_pos_in(
          pos.h = "LEFT", 
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
          pos.h = "LEFT",
          pos.v = "BOTTOM",
          align.h = "center"
        )
      ) +
      tm_title(paste("SD among GCMs -", future.scenario),
               position = tm_pos_out("center", "top"), size = 0.9) +
      tm_layout(scale = 1) +
      tm_crs(4326)

    # Adiciona ocorrências, se existirem
    if (nrow(future.points) > 0 && plot.occurrences) {
      ens.future.sd.gcm.W.map <- ens.future.sd.gcm.W.map +
        tm_shape(future.points) +
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
            title = "Current occurrences",
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

    file.name <- file.path(future.folder, paste0(
      "Map ", count.map, " - World - ", future.scenario, " - SD_GCM.png"
    ))
    count.map <- count.map + 1
    suppressWarnings(
      tmap_save(
        tm = ens.future.sd.gcm.W.map, filename = file.name,
        units = "cm", dpi = 400, scale = 1, device = ragg::agg_png
      )
    )

    # 5) Mudança binária - World ----------------------------------------
    raster.name <- file.path(future.tiff.folder, paste0(
      "Ensemble - World - ", future.scenario, " - Binary_change.tif"
    ))
    terra::writeRaster(future.change, raster.name, datatype = "FLT4S",
                       gdal = "COMPRESS=LZW", overwrite = TRUE)

    future.map.raster <- future.change
    levels(future.map.raster) <- data.frame(ID = 1:4, Change = future.change.labels)
    ens.future.change.W.map <-
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

      # Raster do modelo
      tm_shape(future.map.raster, is.main = TRUE) +
      tm_raster(
        col_alpha = 1,
        col.scale = tm_scale_categorical(
        values = future.change.colors, levels.drop = FALSE
      ),
        col.legend = tm_legend(
          group_id = "LEG",
          title = "Binary change",
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

      # Contorno dos países
      tm_shape(world) +
      tm_polygons(
        col = "black",
        fill = NULL,
        lwd = 0.6
      ) +

      # Complementos
      tm_graticules(alpha = 0.5, labels.size = 1) +
      tm_compass(
        type = "arrow",
        size = 1.5,
        position = tm_pos_in(
          pos.h = "LEFT",
          pos.v = "BOTTOM",
          align.h = "center"
        )
      ) +
      tm_credits(
        "Geographic coordinates\nDatum: WGS84",
        position = tm_pos_in(
          pos.h = "LEFT", 
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
          pos.h = "LEFT",
          pos.v = "BOTTOM",
          align.h = "center"
        )
      ) +
      tm_title(paste("Binary change -", future.scenario),
               position = tm_pos_out("center", "top"), size = 0.9) +
      tm_layout(scale = 1) +
      tm_crs(4326)

    # Adiciona ocorrências, se existirem
    if (nrow(future.points) > 0 && plot.occurrences) {
      ens.future.change.W.map <- ens.future.change.W.map +
        tm_shape(future.points) +
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
            title = "Current occurrences",
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

    file.name <- file.path(future.folder, paste0(
      "Map ", count.map, " - World - ", future.scenario, " - Binary_change.png"
    ))
    count.map <- count.map + 1
    suppressWarnings(
      tmap_save(
        tm = ens.future.change.W.map, filename = file.name,
        units = "cm", dpi = 400, scale = 1, device = ragg::agg_png
      )
    )

    # Projeções CMIP6 para o Brasil  ------------------------------------
    future.region <- "Brazil"
    future.base <- terra::crop(future.base.W, mask.BR, mask = TRUE)
    future.mean <- terra::crop(future.mean.W, mask.BR, mask = TRUE)
    future.sd.gcm <- terra::crop(future.sd.gcm.W, mask.BR, mask = TRUE)
    future.gcms <- terra::crop(future.gcms.W, mask.BR, mask = TRUE)
    future.points <- points.map.BR

    stopifnot(terra::compareGeom(future.base, future.mean))

    # Domínio comum: não converte ausência de informação em inadequabilidade.
    future.common <- terra::ifel(is.na(future.base) | is.na(future.mean), NA, 1)
    future.base <- terra::mask(future.base, future.common)
    future.mean <- terra::mask(future.mean, future.common)
    future.sd.gcm <- terra::mask(future.sd.gcm, future.common)
    future.gcms <- terra::mask(future.gcms, future.common)

    # Binários: mantém exatamente o mesmo limiar nos dois cenários.
    future.base.binary <- terra::ifel(future.base >= future.threshold, 1, 0)
    future.binary <- terra::ifel(future.mean >= future.threshold, 1, 0)

    # 1 = 0->0; 2 = 1->0; 3 = 0->1; 4 = 1->1.
    future.change <- 1 + future.base.binary + 2 * future.binary
    future.classes <- terra::classify(
      future.mean, rcl = future.breaks, include.lowest = TRUE, right = TRUE
    ) + 1

    # Áreas em km2: pesos pela área real das células, sem contar pixels como km2.
    future.cell.area <- terra::cellSize(future.base, unit = "km", mask = TRUE)
    future.change.area <- terra::zonal(future.cell.area, future.change, fun = "sum", na.rm = TRUE)
    names(future.change.area) <- c("Code", "Area_km2")
    future.change.area <- merge(
      data.frame(Code = 1:4, Change = future.change.labels),
      future.change.area, by = "Code", all.x = TRUE, sort = TRUE
    )
    future.change.area$Area_km2[is.na(future.change.area$Area_km2)] <- 0
    future.change.area$Region <- future.region
    future.change.area$Scenario <- future.scenario
    future.changes <- rbind(future.changes, future.change.area)

    # Área adequada e áreas por classe: base, cada GCM e média futura.
    future.area.rasters <- c(future.base, future.gcms, future.mean)
    future.area.names <- c("BASE", CMIP6.models, "GCM mean")
    for (j in seq_along(future.area.names)) {
      future.r <- future.area.rasters[[j]]
      future.suitable.area <- terra::global(
        terra::ifel(future.r >= future.threshold, future.cell.area, 0),
        fun = "sum", na.rm = TRUE
      )[1, 1]
      if (j == 1) future.base.area <- future.suitable.area
      future.area.change <- NA_real_
      if (future.base.area > 0) {
        future.area.change <- 100 * (future.suitable.area - future.base.area) / future.base.area
      }
      future.class.r <- terra::classify(future.r, future.breaks, include.lowest = TRUE, right = TRUE) + 1
      future.class.area <- terra::zonal(future.cell.area, future.class.r, fun = "sum", na.rm = TRUE)
      names(future.class.area) <- c("Code", "Area_km2")
      future.class.area <- merge(
        data.frame(Code = 1:5, Class = future.class.labels), future.class.area,
        by = "Code", all.x = TRUE, sort = TRUE
      )
      future.class.area$Area_km2[is.na(future.class.area$Area_km2)] <- 0
      future.areas <- rbind(future.areas, data.frame(
        Region = future.region, Scenario = future.scenario, Model = future.area.names[j],
        Class = future.class.area$Class, Class_area_km2 = future.class.area$Area_km2,
        Suitable_area_km2 = future.suitable.area, Change_percent = future.area.change
      ))
    }

    # Mudanças fuzzy: índices de adequabilidade, não áreas geográficas em km2.
    future.values <- terra::values(c(future.base, future.mean), mat = TRUE)
    future.values <- future.values[complete.cases(future.values), , drop = FALSE]
    future.fuzzy.result <- fuzzySim::fuzzyRangeChange(
      future.values[, 1], future.values[, 2], number = FALSE, plot = FALSE,
      measures = c("Gain", "Loss", "Stable positive", "Stable negative", "Balance")
    )
    future.fuzzy.df <- data.frame(
      Region = future.region, Scenario = future.scenario,
      Measure = rownames(future.fuzzy.result), Proportion = future.fuzzy.result$Proportion
    )
    future.fuzzy <- rbind(future.fuzzy, future.fuzzy.df)
    future.fuzzy.plot <- ggplot(future.fuzzy.df, aes(x = Measure, y = Proportion, fill = Measure)) +
      geom_col() +
      scale_fill_manual(values = color.barplot, guide = "none") +
      labs(x = NULL, y = "Fuzzy suitability change", title = paste(future.region, future.scenario)) +
      theme_bw() + theme(axis.text.x = element_text(angle = 25, hjust = 1))
    file.name <- file.path(future.folder, paste0("Plot ", count.plot, " - Fuzzy change - ", future.region, " - ", future.scenario, ".png"))
    count.plot <- count.plot + 1
    ggsave(file.name, plot = future.fuzzy.plot, width = 25, height = 16,
           units = "cm", dpi = 400, device = ragg::agg_png)

    # Áreas por classe na legenda: calculadas separadamente para cada região.
    future.legend.areas <- terra::zonal(
      future.cell.area, future.classes, fun = "sum", na.rm = TRUE
    )
    names(future.legend.areas) <- c("Code", "Area_km2")
    future.legend.areas <- merge(
      data.frame(Code = 1:5, Class = future.class.labels),
      future.legend.areas, by = "Code", all.x = TRUE, sort = TRUE
    )
    future.legend.areas$Area_km2[is.na(future.legend.areas$Area_km2)] <- 0
    future.labels.breaks <- paste0(
      future.legend.areas$Class, " (",
      formatC(future.legend.areas$Area_km2, format = "f", digits = 0, big.mark = ","),
      " km²)"
    )


    # 1) Adequabilidade média - Brazil ----------------------------------------
    raster.name <- file.path(future.tiff.folder, paste0(
      "Ensemble - Brazil - ", future.scenario, " - Mean.tif"
    ))
    terra::writeRaster(future.mean, raster.name, datatype = "FLT4S",
                       gdal = "COMPRESS=LZW", overwrite = TRUE)

    future.map.raster <- future.mean
    ens.future.mean.BR.map <-
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
      tm_shape(future.map.raster, is.main = TRUE) +
      tm_raster(
        col_alpha = 1,
        col.scale = tm_scale_continuous(
        n = 10, values = color.model(10), midpoint = 0.5, limits = c(0, 1)
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
          item.space = 0,
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
      tm_title(paste("Mean suitability -", future.scenario),
               position = tm_pos_out("center", "top"), size = 0.9) +
      tm_layout(scale = 1) +
      tm_crs(4326)

    # Adiciona ocorrências, se existirem
    if (nrow(future.points) > 0 && plot.occurrences) {
      ens.future.mean.BR.map <- ens.future.mean.BR.map +
        tm_shape(future.points) +
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
            title = "Current occurrences",
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

    file.name <- file.path(future.folder, paste0(
      "Map ", count.map, " - Brazil - ", future.scenario, " - Mean.png"
    ))
    count.map <- count.map + 1
    suppressWarnings(
      tmap_save(
        tm = ens.future.mean.BR.map, filename = file.name,
        units = "cm", dpi = 400, scale = 1, device = ragg::agg_png
      )
    )

    # 2) Classes de adequabilidade - Brazil ----------------------------------------
    raster.name <- file.path(future.tiff.folder, paste0(
      "Ensemble - Brazil - ", future.scenario, " - Classes.tif"
    ))
    terra::writeRaster(future.classes, raster.name, datatype = "FLT4S",
                       gdal = "COMPRESS=LZW", overwrite = TRUE)

    future.map.raster <- future.classes
    levels(future.map.raster) <- data.frame(ID = 1:5, Class = future.class.labels)
    ens.future.classes.BR.map <-
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
      tm_shape(future.map.raster, is.main = TRUE) +
      tm_raster(
        col_alpha = 1,
        col.scale = tm_scale_categorical(
        values = future.class.colors, labels = future.labels.breaks,
        levels.drop = FALSE
      ),
        col.legend = tm_legend(
          group_id = "LEG",
          title = "Suitability (Classes)",
          reverse = TRUE,
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
      tm_title(paste("Suitability classes -", future.scenario),
               position = tm_pos_out("center", "top"), size = 0.9) +
      tm_layout(scale = 1) +
      tm_crs(4326)

    # Adiciona ocorrências, se existirem
    if (nrow(future.points) > 0 && plot.occurrences) {
      ens.future.classes.BR.map <- ens.future.classes.BR.map +
        tm_shape(future.points) +
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
            title = "Current occurrences",
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

    file.name <- file.path(future.folder, paste0(
      "Map ", count.map, " - Brazil - ", future.scenario, " - Classes.png"
    ))
    count.map <- count.map + 1
    suppressWarnings(
      tmap_save(
        tm = ens.future.classes.BR.map, filename = file.name,
        units = "cm", dpi = 400, scale = 1, device = ragg::agg_png
      )
    )

    # 3) Adequabilidade binária - Brazil ----------------------------------------
    raster.name <- file.path(future.tiff.folder, paste0(
      "Ensemble - Brazil - ", future.scenario, " - Binary.tif"
    ))
    terra::writeRaster(future.binary, raster.name, datatype = "FLT4S",
                       gdal = "COMPRESS=LZW", overwrite = TRUE)

    future.map.raster <- future.binary
    levels(future.map.raster) <- data.frame(ID = 0:1, Class = c("Unsuitable", "Suitable"))
    ens.future.binary.BR.map <-
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
      tm_shape(future.map.raster, is.main = TRUE) +
      tm_raster(
        col_alpha = 1,
        col.scale = tm_scale_categorical(
        values = color.bin, levels.drop = FALSE
      ),
        col.legend = tm_legend(
          group_id = "LEG",
          title = paste0("Suitability >= ", formatC(future.threshold, digits = 4, format = "f")),
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
      tm_title(paste("Binary suitability -", future.scenario),
               position = tm_pos_out("center", "top"), size = 0.9) +
      tm_layout(scale = 1) +
      tm_crs(4326)

    # Adiciona ocorrências, se existirem
    if (nrow(future.points) > 0 && plot.occurrences) {
      ens.future.binary.BR.map <- ens.future.binary.BR.map +
        tm_shape(future.points) +
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
            title = "Current occurrences",
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

    file.name <- file.path(future.folder, paste0(
      "Map ", count.map, " - Brazil - ", future.scenario, " - Binary.png"
    ))
    count.map <- count.map + 1
    suppressWarnings(
      tmap_save(
        tm = ens.future.binary.BR.map, filename = file.name,
        units = "cm", dpi = 400, scale = 1, device = ragg::agg_png
      )
    )

    # 4) Desvio-padrão entre GCMs - Brazil ----------------------------------------
    raster.name <- file.path(future.tiff.folder, paste0(
      "Ensemble - Brazil - ", future.scenario, " - SD_GCM.tif"
    ))
    terra::writeRaster(future.sd.gcm, raster.name, datatype = "FLT4S",
                       gdal = "COMPRESS=LZW", overwrite = TRUE)

    future.map.raster <- future.sd.gcm
    ens.future.sd.gcm.BR.map <-
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
      tm_shape(future.map.raster, is.main = TRUE) +
      tm_raster(
        col_alpha = 1,
        col.scale = tm_scale_continuous(
        values = c("white", "#6BAED6", "#08306B"),
        limits = c(0, 0.5), ticks = seq(0, 0.5, 0.1)
      ),
        col.legend = tm_legend(
          group_id = "LEG",
          title = "SD among GCMs",
          reverse = TRUE,
          title.size = 0.8,
          title.align = "left",
          title.fontface = "bold",
          title.padding = c(0.5, 0, 0, 0),
          text.size = 0.8,
          show = TRUE,
          orientation = "portrait",
          height = 13,
          item.space = 0,
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
      tm_title(paste("SD among GCMs -", future.scenario),
               position = tm_pos_out("center", "top"), size = 0.9) +
      tm_layout(scale = 1) +
      tm_crs(4326)

    # Adiciona ocorrências, se existirem
    if (nrow(future.points) > 0 && plot.occurrences) {
      ens.future.sd.gcm.BR.map <- ens.future.sd.gcm.BR.map +
        tm_shape(future.points) +
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
            title = "Current occurrences",
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

    file.name <- file.path(future.folder, paste0(
      "Map ", count.map, " - Brazil - ", future.scenario, " - SD_GCM.png"
    ))
    count.map <- count.map + 1
    suppressWarnings(
      tmap_save(
        tm = ens.future.sd.gcm.BR.map, filename = file.name,
        units = "cm", dpi = 400, scale = 1, device = ragg::agg_png
      )
    )

    # 5) Mudança binária - Brazil ----------------------------------------
    raster.name <- file.path(future.tiff.folder, paste0(
      "Ensemble - Brazil - ", future.scenario, " - Binary_change.tif"
    ))
    terra::writeRaster(future.change, raster.name, datatype = "FLT4S",
                       gdal = "COMPRESS=LZW", overwrite = TRUE)

    future.map.raster <- future.change
    levels(future.map.raster) <- data.frame(ID = 1:4, Change = future.change.labels)
    ens.future.change.BR.map <-
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
      tm_shape(future.map.raster, is.main = TRUE) +
      tm_raster(
        col_alpha = 1,
        col.scale = tm_scale_categorical(
        values = future.change.colors, levels.drop = FALSE
      ),
        col.legend = tm_legend(
          group_id = "LEG",
          title = "Binary change",
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
      tm_title(paste("Binary change -", future.scenario),
               position = tm_pos_out("center", "top"), size = 0.9) +
      tm_layout(scale = 1) +
      tm_crs(4326)

    # Adiciona ocorrências, se existirem
    if (nrow(future.points) > 0 && plot.occurrences) {
      ens.future.change.BR.map <- ens.future.change.BR.map +
        tm_shape(future.points) +
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
            title = "Current occurrences",
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

    file.name <- file.path(future.folder, paste0(
      "Map ", count.map, " - Brazil - ", future.scenario, " - Binary_change.png"
    ))
    count.map <- count.map + 1
    suppressWarnings(
      tmap_save(
        tm = ens.future.change.BR.map, filename = file.name,
        units = "cm", dpi = 400, scale = 1, device = ragg::agg_png
      )
    )

    # Salva o progresso das tabelas ao concluir cada cenário.
    write.csv(future.areas, file.path(future.folder, "Future - Areas.csv"), row.names = FALSE)
    write.csv(future.changes, file.path(future.folder, "Future - Binary changes.csv"), row.names = FALSE)
    write.csv(future.fuzzy, file.path(future.folder, "Future - Fuzzy changes.csv"), row.names = FALSE)
  }
}

# Tabelas CMIP6 finais -----------------------------
openxlsx2::write_xlsx(
  list(Settings = future.config, Areas = future.areas,
       Binary_changes = future.changes, Fuzzy_changes = future.fuzzy),
  file = file.path(future.folder, "Future - Summary.xlsx"), overwrite = TRUE
)

# Tabela final - World
future.region <- "World"
tmp.table <- future.areas %>%
  dplyr::filter(Region == future.region) %>%
  dplyr::select(-Region) %>%
  gt::gt() %>%
  gt::tab_header(title = paste("Future climate areas -", future.region)) %>%
  gt::fmt_number(columns = c(Class_area_km2, Suitable_area_km2), decimals = 0) %>%
  gt::fmt_number(columns = Change_percent, decimals = 2) %>%
  gt::sub_missing(missing_text = "NA") %>%
  gt::tab_source_note(source_note = "Same base threshold and class breaks in all scenarios. Suitable area is repeated across class rows; do not sum it across classes.")
file.name <- file.path(future.folder, paste0("Table ", count.table, " - ", future.region, " - Future Climate Areas.docx"))
count.table <- count.table + 1
gt::gtsave(tmp.table, filename = file.name)

# Tabela final - Brazil
future.region <- "Brazil"
tmp.table <- future.areas %>%
  dplyr::filter(Region == future.region) %>%
  dplyr::select(-Region) %>%
  gt::gt() %>%
  gt::tab_header(title = paste("Future climate areas -", future.region)) %>%
  gt::fmt_number(columns = c(Class_area_km2, Suitable_area_km2), decimals = 0) %>%
  gt::fmt_number(columns = Change_percent, decimals = 2) %>%
  gt::sub_missing(missing_text = "NA") %>%
  gt::tab_source_note(source_note = "Same base threshold and class breaks in all scenarios. Suitable area is repeated across class rows; do not sum it across classes.")
file.name <- file.path(future.folder, paste0("Table ", count.table, " - ", future.region, " - Future Climate Areas.docx"))
count.table <- count.table + 1
gt::gtsave(tmp.table, filename = file.name)


# Limpeza opcional dos rasters intermediários ----------------------------------
# Os arquivos são preservados por padrão. A exclusão exige confirmação explícita.
if (interactive() && isTRUE(utils::askYesNo("Excluir a pasta GCM com os rasters intermediários?", default = FALSE))) {
  future.gcm.expected <- file.path(future.folder, "GCM")
  if (dir.exists(future.gcm.folder)) {
    if (normalizePath(future.gcm.folder) != normalizePath(future.gcm.expected)) {
      stop("Pasta GCM diferente do caminho esperado; limpeza interrompida.")
    }
    unlink(future.gcm.folder, recursive = TRUE)
    if (dir.exists(future.gcm.folder)) {
      warning("Não foi possível remover completamente a pasta GCM.")
    }
  }
}

message("Projeções futuras concluídas. Resultados em: ", future.folder)

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
