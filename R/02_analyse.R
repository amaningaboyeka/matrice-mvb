# =============================================================================
# 02 — Analyse : matrice de risque, classements, dynamique, projections
# Matrice de risque MVB — appui à la surveillance ORACLE (Bunia, Butembo)
# Dr Gaylord Amani Ngaboyeka, co-investigateur et biostatisticien de l'essai clinique
# ORACLE BDBV ; ERSP/UCB
#
# Version 2 (après validation rétrospective, juin–octobre 2026) :
#  - probabilité et impact restent deux dimensions séparées (le score mélangé
#    prédisait mal l'extension) ;
#  - deux listes opérationnelles : zones à surveiller (indemnes, alerte si la
#    pression d'importation est dans le quintile supérieur) et zones actives à
#    renforcer (classées par cas récents) ;
#  - l'impact intègre la sécurité (ACLED + avis des équipes de terrain) ;
#  - projections à 4 semaines calibrées (marche aléatoire sur le taux de
#    croissance ; couverture rétrospective IP80 = 80 %, IP95 = 93 %).
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(jsonlite)
  library(sf); library(MASS); library(EpiEstim)
})
select <- dplyr::select
dir.create("sortie", showWarnings = FALSE)
f <- function(x) file.path("donnees", x)
sf_use_s2(FALSE)
set.seed(2026)

PROVINCES <- c("Ituri", "Nord-Kivu")
CATCH_MIN <- 180      # zone d'attraction d'un site : <= 3 h de route
TAU_MIN   <- 120      # décroissance de la pression d'importation
K_RW      <- 0.25     # calibration des projections (validation rétrospective)
SEUIL_CAS_ELEVE   <- 20   # probabilité élevée : >= 20 cas confirmés en 21 jours...
SEUIL_INCID_ELEVE <- 10   # ... ou >= 10 cas pour 100 000 habitants en 21 jours
VERSION   <- "2.1"

# ------------------------------------------------------------------ Données
alias <- read_csv(f("aliases.csv"), show_col_types = FALSE)
harm <- function(x) { i <- match(x, alias$observed_name); ifelse(is.na(i), x, alias$canonical_nom[i]) }
hz_all <- st_read(f("hz.geojson"), quiet = TRUE) |> select(nom, province)
hz <- hz_all |> filter(province %in% PROVINCES)

lire <- function(file, col) read_csv(f(file), show_col_types = FALSE, col_types = cols(.default = "c")) |>
  filter(!is.na(nom)) |> mutate(nom = harm(nom), date = as.Date(date), v = suppressWarnings(as.numeric(.data[[col]]))) |>
  filter(!is.na(v)) |> group_by(nom, date) |> summarise(v = max(v), .groups = "drop")
cas <- lire("cumulative_confirmed_cases.csv", "cumulative_confirmed_cases")
dec <- lire("cumulative_confirmed_deaths.csv", "cumulative_confirmed_deaths")
FIN <- max(cas$date)
grille <- seq(as.Date("2026-05-14"), FIN, by = "day")
quot <- function(d) d |> group_by(nom) |> arrange(date, .by_group = TRUE) |>
  mutate(v = rev(cummin(rev(v)))) |>
  group_modify(~ { x <- if (nrow(.x) >= 2) approx(as.numeric(.x$date), .x$v, xout = as.numeric(grille), rule = 2)$y else rep(.x$v[1], length(grille))
                   x[grille < min(.x$date)] <- 0; tibble(date = grille, cum = x) }) |> ungroup()
cas_j <- quot(cas); dec_j <- quot(dec)
val_a <- function(dj, d) dj |> filter(date == d) |> select(nom, cum)
fen <- function(dj, a, b) val_a(dj, b) |> rename(y = cum) |> left_join(val_a(dj, a) |> rename(x = cum), by = "nom") |>
  transmute(nom, n = pmax(round(y - coalesce(x, 0)), 0))

num <- function(file, col) read_csv(f(file), show_col_types = FALSE) |> mutate(nom = harm(nom)) |> select(nom, !!col := 2)
tt <- read_csv(f("osrm__travel_time__static.matrix.csv"), show_col_types = FALSE)
TT <- as.matrix(tt[, -1]); rownames(TT) <- harm(tt$nom); colnames(TT) <- harm(colnames(TT))

# --------------------------------------------------- Données de terrain
terrain <- read_csv("donnees_terrain/securite_terrain.csv", show_col_types = FALSE, col_types = cols(.default = "c")) |>
  transmute(nom = harm(zone_de_sante), acces_terrain = coalesce(acces, "Non renseigné"),
            acces_date = date_mise_a_jour, acces_commentaire = commentaire)
oracle <- read_csv("donnees_terrain/zones_oracle.csv", show_col_types = FALSE, col_types = cols(.default = "c")) |>
  transmute(nom = harm(zone_de_sante), oracle_site = site, oracle_statut = statut, oracle_commentaire = commentaire)
SITES <- sort(unique(oracle$oracle_site)); names(SITES) <- SITES
if (!length(SITES)) SITES <- c(Bunia = "Bunia", Butembo = "Butembo")

# ------------------------------------------------------------------ ACLED
statut_acled <- if (file.exists(f("statut_acled.json"))) fromJSON(f("statut_acled.json")) else list(utilise = FALSE, message = "ACLED non configuré.")
acled <- tibble(nom = hz$nom, acled_evt_90j = NA_real_, acled_morts_90j = NA_real_)
if (isTRUE(statut_acled$utilise) && file.exists(f("acled_evenements.csv"))) {
  ev <- read_csv(f("acled_evenements.csv"), show_col_types = FALSE) |>
    mutate(event_date = as.Date(event_date), latitude = as.numeric(latitude), longitude = as.numeric(longitude),
           fatalities = as.numeric(fatalities)) |>
    filter(event_date > FIN - 90, event_type %in% c("Battles", "Violence against civilians", "Explosions/Remote violence"))
  pts <- st_as_sf(ev, coords = c("longitude", "latitude"), crs = 4326) |> st_transform(st_crs(hz))
  j <- st_join(pts, hz["nom"], join = st_within) |> st_drop_geometry() |> filter(!is.na(nom)) |>
    group_by(nom) |> summarise(acled_evt_90j = n(), acled_morts_90j = sum(fatalities, na.rm = TRUE))
  acled <- tibble(nom = hz$nom) |> left_join(j, by = "nom") |>
    mutate(across(c(acled_evt_90j, acled_morts_90j), ~ coalesce(.x, 0)))
}

# ------------------------------------------------------------ Indicateurs
calculer <- function(D) {
  ind <- hz |> st_drop_geometry() |>
    left_join(val_a(cas_j, D) |> transmute(nom, cas_cumules = round(cum)), by = "nom") |>
    left_join(val_a(dec_j, D) |> transmute(nom, deces_cumules = round(cum)), by = "nom") |>
    left_join(fen(cas_j, D - 21, D) |> rename(cas_21j = n), by = "nom") |>
    left_join(fen(cas_j, D - 42, D - 21) |> rename(cas_21j_prec = n), by = "nom") |>
    left_join(fen(cas_j, D - 42, D) |> rename(cas_42j = n), by = "nom") |>
    left_join(fen(dec_j, D - 42, D) |> rename(deces_42j = n), by = "nom") |>
    left_join(cas |> filter(date <= D) |> group_by(nom) |> arrange(date) |> filter(v > lag(v, default = 0)) |> summarise(dernier_cas = max(date)), by = "nom") |>
    left_join(num("worldpop__pop_count__static.csv", "population"), by = "nom") |>
    left_join(num("grid3_healthsites__healthsite_count__static.csv", "formations_sanitaires"), by = "nom") |>
    left_join(num("ccvi__socioeconomic_deprivation__static.csv", "privation"), by = "nom") |>
    left_join(terrain, by = "nom") |> left_join(oracle, by = "nom") |> left_join(acled, by = "nom") |>
    mutate(across(c(cas_cumules, deces_cumules, cas_21j, cas_21j_prec, cas_42j, deces_42j), ~ coalesce(.x, 0)),
           acces_terrain = coalesce(acces_terrain, "Non renseigné"),
           incidence_21j_100k = 1e5 * cas_21j / population,
           tendance = ifelse(cas_21j + cas_21j_prec > 0, (cas_21j + 1) / (cas_21j_prec + 1), NA),
           letalite_42j = ifelse(cas_42j >= 10, deces_42j / cas_42j, NA),
           jours_depuis_dernier_cas = as.numeric(D - dernier_cas),
           fs_10k = 1e4 * formations_sanitaires / population,
           densite = population / (as.numeric(st_area(st_transform(hz, 32735))[match(nom, hz$nom)]) / 1e6))
  
  a21 <- fen(cas_j, D - 21, D)
  ind$pression_importation <- sapply(ind$nom, function(z) {
    if (!z %in% rownames(TT)) return(NA)
    s <- a21 |> filter(nom != z, nom %in% colnames(TT), n > 0)
    sum(s$n * exp(-TT[z, s$nom] / TAU_MIN), na.rm = TRUE) })
  for (s in names(SITES)) ind[[paste0("temps_", s, "_min")]] <- as.numeric(TT[ind$nom, SITES[[s]]])
  
  # ------------------------------------------------ Scores et matrice (v2)
  pct <- function(x) { r <- percent_rank(x); r[is.na(r)] <- median(r, na.rm = TRUE); r }
  note_terrain <- c("Accessible" = 0, "Partiellement accessible" = 0.5, "Inaccessible" = 1)
  ind <- ind |> mutate(
    s_cas = pct(log1p(cas_21j)), s_incid = pct(incidence_21j_100k),
    s_tendance = ifelse(cas_21j == 0, 0, pct(tendance)), s_import = pct(log1p(pression_importation)),
    s_densite = pct(log(densite)), s_fs = pct(-fs_10k), s_priv = pct(privation), s_letal = pct(letalite_42j),
    s_acled = if (all(is.na(acled_evt_90j))) NA_real_ else pct(log1p(acled_evt_90j)),
    s_terrain = unname(note_terrain[acces_terrain]),
    s_secu = case_when(!is.na(s_terrain) & !is.na(s_acled) ~ (s_terrain + s_acled) / 2,
                       !is.na(s_terrain) ~ s_terrain, !is.na(s_acled) ~ s_acled, TRUE ~ NA_real_),
    niveau_securite = case_when(is.na(s_secu) ~ "Non renseigné", s_secu >= 2/3 ~ "Élevé",
                                s_secu >= 1/3 ~ "Moyen", TRUE ~ "Faible"),
    probabilite = 0.40 * s_cas + 0.25 * s_incid + 0.10 * s_tendance + 0.25 * s_import,
    impact = {
      w <- cbind(s_densite = .25, s_fs = .20, s_priv = .15, s_letal = .20, s_secu = .20)
      m <- cbind(s_densite, s_fs, s_priv, s_letal, s_secu)
      ww <- matrix(w, nrow(m), 5, byrow = TRUE); ww[is.na(m)] <- 0; m[is.na(m)] <- 0
      rowSums(m * ww) / rowSums(ww) },
    survivants_estimes = pmax(cas_cumules - deces_cumules, 0),
    statut = case_when(cas_21j > 0 ~ "Active", cas_cumules > 0 ~ "Touchée, sans cas récent", TRUE ~ "Jamais touchée"))
  
  # Liste 1 : zones à surveiller (sans cas récent) — alerte = quintile supérieur de pression d'importation
  ind <- ind |> group_by(actif = cas_21j > 0) |>
    mutate(rang_pression = ifelse(!actif, rank(-pression_importation, ties.method = "first"), NA),
           alerte_precoce = !actif & percent_rank(pression_importation) >= 0.8) |> ungroup() |> select(-actif)
  # Liste 2 : zones actives à renforcer — classées par cas récents puis incidence
  ind <- ind |> mutate(rang_actif = ifelse(cas_21j > 0, rank(-(cas_21j + incidence_21j_100k / 1000), ties.method = "first"), NA))
  # Classes : seuils ABSOLUS pour la probabilité (stables dans le temps), relatifs pour l'impact
  ind <- ind |> mutate(
    classe_prob = factor(case_when(
      cas_21j >= SEUIL_CAS_ELEVE | incidence_21j_100k >= SEUIL_INCID_ELEVE ~ "Élevée",
      cas_21j >= 1 | alerte_precoce ~ "Moyenne",
      TRUE ~ "Faible"), levels = c("Faible", "Moyenne", "Élevée")),
    classe_impact = cut(impact, c(-Inf, 1/3, 2/3, Inf), labels = c("Faible", "Moyen", "Élevé")),
    produit = as.integer(classe_prob) * as.integer(classe_impact),
    niveau_risque = case_when(produit == 9 ~ "Très élevé", produit == 6 ~ "Élevé", produit >= 3 ~ "Modéré", TRUE ~ "Faible"))
  ind <- ind |> arrange(desc(produit), desc(probabilite)) |> mutate(rang = row_number())
  for (s in names(SITES)) ind[[paste0("attraction_", s)]] <- !is.na(ind[[paste0("temps_", s, "_min")]]) &
    ind[[paste0("temps_", s, "_min")]] <= CATCH_MIN
  # une zone à moins de 3 h de deux sites est rattachée au site le plus proche (pas de double compte)
  tm <- do.call(cbind, lapply(names(SITES), function(s) ind[[paste0("temps_", s, "_min")]]))
  proche <- names(SITES)[apply(tm, 1, function(x) if (all(is.na(x))) NA_integer_ else which.min(x))]
  for (s in names(SITES)) ind[[paste0("attraction_", s)]] <- ind[[paste0("attraction_", s)]] & !is.na(proche) & proche == s
  ind
}
ind <- calculer(FIN)

# ------------------------------------------ Évolution sur 7 jours
ordre <- c("Faible" = 1, "Modéré" = 2, "Élevé" = 3, "Très élevé" = 4)
prec <- calculer(FIN - 7) |> transmute(nom, niveau_j7 = niveau_risque, rang_j7 = rang, alerte_j7 = alerte_precoce, cas_21j_j7 = cas_21j)
ind <- ind |> left_join(prec, by = "nom") |>
  mutate(evolution_7j = case_when(ordre[niveau_risque] > ordre[niveau_j7] ~ "En hausse",
                                  ordre[niveau_risque] < ordre[niveau_j7] ~ "En baisse", TRUE ~ "Stable"),
         alerte_7j = case_when(alerte_precoce & !alerte_j7 ~ "Nouvelle alerte", !alerte_precoce & alerte_j7 & cas_21j > 0 ~ "Alerte confirmée (cas notifiés)",
                               !alerte_precoce & alerte_j7 ~ "Alerte levée", TRUE ~ ""))

# ---------------------------------------- Dynamique et projections calibrées
serie_site <- function(zones) cas_j |> filter(nom %in% zones) |> group_by(date) |> summarise(cum = sum(cum))
hebdo <- function(zones, n_sem = 16) { d <- serie_site(zones); fins <- rev(seq(FIN, by = "-7 days", length.out = n_sem))
  tibble(fin = fins, cas = sapply(fins, function(x) max(0, d$cum[d$date == x] - d$cum[d$date == x - 7]))) }
projeter <- function(zones, h = 4, k = 6) {
  w <- hebdo(zones, 18); ajust <- tail(w, k) |> mutate(t = seq_len(k))
  m <- glm(cas ~ t, family = quasipoisson, data = ajust); phi <- max(summary(m)$dispersion, 1.0001)
  B <- mvrnorm(4000, coef(m), vcov(m))
  g <- diff(log(tail(w$cas, 13) + 5)); sig <- K_RW * sd(g)
  lv <- B[, 1] + B[, 2] * k; gr <- B[, 2]; sims <- matrix(0, 4000, h)
  for (i in seq_len(h)) { gr <- gr + rnorm(4000, 0, sig); lv <- lv + gr; mu <- exp(lv)
    sims[, i] <- rnbinom(4000, mu = mu, size = mu / (phi - 1)) }
  q <- apply(sims, 2, quantile, c(.025, .1, .5, .9, .975)); tot <- rowSums(sims)
  list(observe = tail(w, 16),
       projection = tibble(fin = FIN + 7 * seq_len(h), mediane = round(q[3, ]), pi80_bas = round(q[2, ]), pi80_haut = round(q[4, ]),
                           pi95_bas = round(q[1, ]), pi95_haut = round(q[5, ])),
       total = c(mediane = round(median(tot)), bas80 = unname(round(quantile(tot, .1))), haut80 = unname(round(quantile(tot, .9)))),
       croissance = exp(coef(m)[2]) - 1, ic = exp(confint.default(m)[2, ]) - 1)
}
rt <- function(zones) { d <- serie_site(zones) |> mutate(n = pmax(cum - lag(cum, default = 0), 0)) |> filter(date >= as.Date("2026-06-01"))
  r <- estimate_R(round(d$n), method = "parametric_si", config = make_config(list(mean_si = 15.3, std_si = 9.3,
       t_start = seq(2, nrow(d) - 13), t_end = seq(15, nrow(d)))))
  tibble(date = d$date[r$R$t_end], rt = r$R$`Median(R)`, bas = r$R$`Quantile.0.025(R)`, haut = r$R$`Quantile.0.975(R)`) }

res_sites <- lapply(names(SITES), function(s) { zz <- ind$nom[ind[[paste0("attraction_", s)]]]
  p <- projeter(zz); r <- rt(zz); list(zones = zz, p = p, r = r) }); names(res_sites) <- names(SITES)

# ------------------------------------------------------------------ Sorties
serie <- bind_rows(lapply(names(res_sites), function(s) { p <- res_sites[[s]]$p; bind_rows(
  p$observe |> transmute(site = s, semaine_fin = fin, type = "Observé", cas),
  p$projection |> transmute(site = s, semaine_fin = fin, type = "Projection", cas = mediane, pi80_bas, pi80_haut, pi95_bas, pi95_haut)) }))
write_csv(serie, "sortie/series_hebdo_projections.csv", na = "")
write_csv(bind_rows(lapply(names(res_sites), function(s) res_sites[[s]]$r |> mutate(site = s, .before = 1))) |>
            mutate(across(c(rt, bas, haut), ~ round(.x, 3))), "sortie/rt_par_site.csv")
resume <- bind_rows(lapply(names(res_sites), function(s) { x <- res_sites[[s]]; z <- ind |> filter(nom %in% x$zones); rr <- tail(x$r, 1)
  tibble(site = s, date_donnees = format(FIN), zones_attraction = length(x$zones), zones_actives = sum(z$cas_21j > 0),
         cas_21j = sum(z$cas_21j), cas_21j_prec = sum(z$cas_21j_prec), survivants_estimes = sum(z$survivants_estimes),
         croissance_hebdo_pct = round(100 * x$p$croissance, 1), croissance_ic95_bas = round(100 * x$p$ic[1], 1),
         croissance_ic95_haut = round(100 * x$p$ic[2], 1), rt = round(rr$rt, 2), rt_bas = round(rr$bas, 2), rt_haut = round(rr$haut, 2),
         proj_4sem_mediane = x$p$total[["mediane"]], proj_4sem_bas80 = x$p$total[["bas80"]], proj_4sem_haut80 = x$p$total[["haut80"]],
         alertes_precoces = sum(z$alerte_precoce, na.rm = TRUE)) }))
write_csv(resume, "sortie/resume_sites.csv")

zones_out <- ind |> transmute(rang, nom, province, niveau_risque, probabilite = round(probabilite, 3), impact = round(impact, 3),
  classe_prob, classe_impact, statut, evolution_7j, niveau_j7, rang_j7, alerte_7j, cas_cumules, deces_cumules, survivants_estimes, cas_21j, cas_21j_prec,
  incidence_21j_100k = round(incidence_21j_100k, 1), tendance = round(tendance, 2), letalite_42j = round(letalite_42j, 2),
  jours_depuis_dernier_cas, pression_importation = round(pression_importation, 1), rang_pression, alerte_precoce = ifelse(alerte_precoce, "Oui", "Non"),
  rang_actif, niveau_securite, acces_terrain, acled_evt_90j, acled_morts_90j, population = round(population),
  fs_10k = round(fs_10k, 2), privation = round(privation, 2),
  oracle_site = coalesce(oracle_site, ""), oracle_statut = coalesce(oracle_statut, ""), oracle_commentaire = coalesce(oracle_commentaire, ""),
  across(starts_with("temps_"), ~ round(.x)), across(starts_with("attraction_"), ~ ifelse(.x, "Oui", "Non")))
write_csv(zones_out, "sortie/zones_classement.csv", na = "")

geo <- hz |> st_transform(32735) |> st_simplify(dTolerance = 800, preserveTopology = TRUE)
pts <- st_coordinates(st_transform(st_point_on_surface(st_geometry(geo)), 4326))
geo <- geo |> st_transform(4326) |> mutate(cx = round(pts[, 1], 4), cy = round(pts[, 2], 4)) |> select(nom, cx, cy)
st_write(geo, "sortie/zones_geo_brut.geojson", delete_dsn = TRUE, quiet = TRUE, layer_options = "COORDINATE_PRECISION=4")

# ---- Projections à 4 semaines pour chaque zone d'intervention ORACLE (même modèle que pour les sites ;
# effectifs faibles par zone : intervalles larges, à lire comme un ordre de grandeur)
zo <- intersect(oracle[[1]], unique(cas_j$nom))
proj_oracle <- bind_rows(lapply(c(if (length(zo) > 1) list(Ensemble = zo), setNames(as.list(zo), zo)), function(zz) { z <- if (length(zz) > 1) "Ensemble" else zz; p <- tryCatch(projeter(zz), error = function(e) NULL); if (is.null(p)) return(NULL)
  bind_rows(p$observe |> transmute(zone = z, semaine_fin = fin, type = "Observé", cas = round(cas)),
            p$projection |> transmute(zone = z, semaine_fin = fin, type = "Projection", cas = mediane, pi80_bas, pi80_haut, pi95_bas, pi95_haut)) |>
    mutate(total_4sem = p$total[["mediane"]], total_bas80 = p$total[["bas80"]], total_haut80 = p$total[["haut80"]]) }))
if (!nrow(proj_oracle)) proj_oracle <- tibble(zone = character(), semaine_fin = as.Date(character()), type = character(), cas = numeric())
write_csv(proj_oracle, "sortie/projections_oracle.csv", na = "")

# ---- Notifications quotidiennes (cas et décès confirmés), telles que rapportées : aucune interpolation,
# dernier cumul rapporté reporté d'un jour à l'autre ; une révision à la baisse donne 0 ce jour-là.
jours <- seq(FIN - 66, FIN, by = "day")
cumul_j <- function(d, zz) { d <- d[d$nom %in% zz, ]
  sapply(jours, function(t) { x <- d[d$date <= t, ] |> group_by(nom) |> slice_max(date, n = 1, with_ties = FALSE); sum(x$v) }) }
mm7 <- function(x) as.numeric(stats::filter(x, rep(1 / 7, 7), sides = 1))
dates_rapport <- unique(cas$date)
perim <- c(list(Ensemble = unique(cas$nom)), lapply(res_sites, function(x) x$zones), if (length(zo)) list(ORACLE = zo))
quotidien <- bind_rows(lapply(names(perim), function(p) {
  n_c <- pmax(diff(cumul_j(cas, perim[[p]])), 0); n_d <- pmax(diff(cumul_j(dec, perim[[p]])), 0)
  tibble(perimetre = p, date = jours[-1], cas = n_c, deces = n_d, cas_mm7 = round(mm7(n_c), 2), deces_mm7 = round(mm7(n_d), 2),
         rapport = ifelse(jours[-1] %in% dates_rapport, "Oui", "Non")) |> filter(date > FIN - 60) }))
write_csv(quotidien, "sortie/quotidien.csv", na = "")

meta <- list(version = VERSION, date_donnees = format(FIN), genere_le = format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"),
             acled = statut_acled$message, acled_utilise = isTRUE(statut_acled$utilise),
             zones_terrain_renseignees = sum(ind$acces_terrain != "Non renseigné"),
             n_zones = nrow(ind), n_alertes = sum(ind$alerte_precoce, na.rm = TRUE),
             n_tres_eleve_eleve = sum(ind$niveau_risque %in% c("Très élevé", "Élevé")))
write_json(meta, "sortie/meta.json", auto_unbox = TRUE, pretty = TRUE)
cat("Analyse terminée — données au", format(FIN), "—", nrow(ind), "zones ;", meta$n_alertes, "alertes précoces\n")
