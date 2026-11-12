# =============================================================================
# 01 — Téléchargement des données
# Matrice de risque MVB — appui à la surveillance ORACLE (Bunia, Butembo)
# Dr Gaylord Amani Ngaboyeka (ERSP/UCB ; projet ORACLE BDBV)
#
# 1. Données publiques INSP/INRB-UMIE (cas, décès, limites des zones de santé,
#    population, formations sanitaires, privation, temps de route).
# 2. ACLED (incidents de sécurité) si les identifiants myACLED sont fournis dans
#    les variables d'environnement ACLED_EMAIL et ACLED_PASSWORD (secrets GitHub).
# =============================================================================
suppressPackageStartupMessages({ library(jsonlite) })
dir.create("donnees", showWarnings = FALSE)
options(timeout = 600)

BASE <- "https://raw.githubusercontent.com/INRB-UMIE/Ebola_DRC_2026/main"
fichiers <- c(
  "data/insp_sitrep/processed/insp_sitrep__cumulative_confirmed_cases__daily.csv" = "cumulative_confirmed_cases.csv",
  "data/insp_sitrep/processed/insp_sitrep__cumulative_confirmed_deaths__daily.csv" = "cumulative_confirmed_deaths.csv",
  "data/aliases.csv" = "aliases.csv",
  "build/drc_health_zones.geojson" = "hz.geojson",
  "data/worldpop/processed/worldpop__pop_count__static.csv" = "worldpop__pop_count__static.csv",
  "data/grid3_healthsites/processed/grid3_healthsites__healthsite_count__static.csv" = "grid3_healthsites__healthsite_count__static.csv",
  "data/ccvi/processed/ccvi__socioeconomic_deprivation__static.csv" = "ccvi__socioeconomic_deprivation__static.csv",
  "data/osrm/processed/osrm__travel_time__static.matrix.csv" = "osrm__travel_time__static.matrix.csv"
)
for (src in names(fichiers)) {
  dest <- file.path("donnees", fichiers[[src]])
  statiques <- grepl("hz.geojson|worldpop|grid3|ccvi|osrm|aliases", dest)
  if (statiques && file.exists(dest) && file.size(dest) > 0) next   # couches statiques : une seule fois
  ok <- tryCatch({ download.file(file.path(BASE, src), dest, quiet = TRUE, mode = "wb"); TRUE },
                 error = function(e) { message("Échec : ", src, " — ", conditionMessage(e)); FALSE })
  if (!ok && !file.exists(dest)) stop("Donnée indispensable manquante : ", src)
}
message("Données INSP/INRB-UMIE à jour.")

# ------------------------------------------------------------------ ACLED
statut_acled <- list(utilise = FALSE, message = "Identifiants ACLED non fournis : indicateur ACLED non calculé.")
email <- Sys.getenv("ACLED_EMAIL"); mdp <- Sys.getenv("ACLED_PASSWORD")
if (nzchar(email) && nzchar(mdp)) {
  statut_acled <- tryCatch({
    h <- curl::new_handle()
    curl::handle_setform(h, username = email, password = mdp, grant_type = "password", client_id = "acled")
    tok <- fromJSON(rawToChar(curl::curl_fetch_memory("https://acleddata.com/oauth/token", handle = h)$content))
    if (is.null(tok$access_token)) stop("jeton refusé")
    debut <- format(Sys.Date() - 180); fin <- format(Sys.Date())
    evts <- list(); page <- 1
    repeat {
      url <- paste0("https://acleddata.com/api/acled/read?_format=json&country=Democratic%20Republic%20of%20Congo",
                    "&admin1=Ituri|North%20Kivu&event_date=", debut, "|", fin, "&event_date_where=BETWEEN",
                    "&fields=event_id_cnty|event_date|event_type|sub_event_type|admin1|admin2|location|latitude|longitude|fatalities",
                    "&limit=5000&page=", page)
      h2 <- curl::new_handle(); curl::handle_setheaders(h2, Authorization = paste("Bearer", tok$access_token))
      rep <- fromJSON(rawToChar(curl::curl_fetch_memory(url, handle = h2)$content))
      d <- rep$data
      if (is.null(d) || length(d) == 0 || NROW(d) == 0) break
      evts[[page]] <- as.data.frame(d); if (NROW(d) < 5000) break; page <- page + 1
    }
    ev <- do.call(rbind, evts)
    if (is.null(ev) || !nrow(ev)) stop("aucun événement reçu (niveau d'accès du compte ?)")
    write.csv(ev, "donnees/acled_evenements.csv", row.names = FALSE)
    list(utilise = TRUE, message = sprintf("ACLED : %d événements du %s au %s (date du dernier événement : %s).",
                                           nrow(ev), debut, fin, max(ev$event_date)))
  }, error = function(e) list(utilise = FALSE, message = paste("ACLED indisponible :", conditionMessage(e))))
}
write_json(statut_acled, "donnees/statut_acled.json", auto_unbox = TRUE)
message(statut_acled$message)
