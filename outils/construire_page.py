"""Construit la page publique (docs/index.html), les CSV téléchargeables et le PDF.
Matrice de risque MVB — appui à la surveillance ORACLE (Bunia, Butembo)
Dr Gaylord Amani Ngaboyeka (ERSP/UCB ; projet ORACLE BDBV)
"""
import json, os, shutil, sys, pathlib

RACINE = pathlib.Path(__file__).resolve().parent.parent
SORTIE, DOCS = RACINE / "sortie", RACINE / "docs"
(DOCS / "donnees").mkdir(parents=True, exist_ok=True)

def lire(p): return (SORTIE / p).read_text(encoding="utf-8")

def aire(anneau):  # aire signée (degrés), > 0 = sens anti-horaire
    return sum(x1 * y2 - x2 * y1 for (x1, y1), (x2, y2) in zip(anneau, anneau[1:] + anneau[:1])) / 2

def orienter(geom):  # d3 attend l'anneau extérieur dans le sens horaire
    def poly(p):
        out = []
        for i, a in enumerate(p):
            horaire = aire(a) < 0
            out.append(a if (horaire if i == 0 else not horaire) else a[::-1])
        return out
    if geom["type"] == "Polygon": geom["coordinates"] = poly(geom["coordinates"])
    elif geom["type"] == "MultiPolygon": geom["coordinates"] = [poly(p) for p in geom["coordinates"]]
    return geom

geo = json.loads(lire("zones_geo_brut.geojson"))
for f in geo["features"]:
    f["geometry"] = orienter(f["geometry"])
    f["properties"] = {k: f["properties"][k] for k in ("nom", "cx", "cy")}

donnees = {k: lire(v) for k, v in {"zones": "zones_classement.csv", "series": "series_hebdo_projections.csv",
                                    "rt": "rt_par_site.csv", "sites": "resume_sites.csv"}.items()}
meta = json.loads(lire("meta.json"))
d3 = (RACINE / "outils" / "d3.min.js").read_text(encoding="utf-8")
corps = (RACINE / "modele" / "page.html").read_text(encoding="utf-8")

THEME = """:root{--font-anthropic-sans:"Segoe UI",system-ui,-apple-system,Roboto,"Helvetica Neue",Arial,sans-serif;--font-anthropic-serif:Georgia,"Times New Roman",serif;
--color-fg:#1f2328;--color-fg-muted:#5c6470;--color-bg:#fff;--color-panel:#f7f7f5;--color-border-line:#e1e3e6;--color-ok:#2e7d32;--color-warn:#e8a33d;--color-bad:#c0392b;
--cds-font-size-caption:12.5px;--cds-font-size-body:15px;--cds-font-size-heading:17px;--cds-font-size-title:26px;--cds-font-weight-medium:600;
--cds-gap-xs:4px;--cds-gap-sm:8px;--cds-gap-md:14px;--cds-gap-lg:22px;--cds-pad-sm:8px;--cds-pad-md:12px;--cds-pad-lg:18px;--cds-radius:8px;
--cds-chart-muted:#d5d8dc;--cds-chart-grid:#eceef0;--cds-chart-axis:#c3c7cc;--cds-chart-reference:#8a9099;--cds-surface-popover:#fff;--cds-border:#d0d4d9;--cds-shadow-popover:0 4px 14px rgba(0,0,0,.12);--cds-text-secondary:#5c6470;}
body{margin:0;background:var(--color-bg)}.page{max-width:1180px;margin:0 auto;padding:28px 20px 40px}
.foot{font-family:var(--font-anthropic-sans);font-size:12px;color:var(--color-fg-muted);margin-top:26px;border-top:1px solid var(--color-border-line);padding-top:10px}
.foot a{color:inherit}
@media print{#filtre-site,.mr .seg{display:none!important}.page{padding:0}.mr .card{break-inside:avoid}.mr .tw{overflow:visible!important}.mr table{font-size:9.5px!important}.mr td,.mr th{padding:3px 5px!important}.mr th{white-space:normal!important}}"""

SHIM = """<script>(function(){const RAW=%s,GEO=%s;window.META=%s;const store={};
for(const k in RAW){const rows=d3.csvParse(RAW[k]);store[k]={status:'ok',data:rows,columns:rows.columns};}store.geo={status:'ok',data:GEO};
const calcs={},draws={};window.dash={colors:['#2563a8','#c2410c','#6b7280','#0f766e'],
data(id){if(calcs[id]){const c=calcs[id];return{status:'ok',data:c.fn(...c.inputs.map(i=>store[i].data))};}return store[id]||{status:'missing',data:[]};},
calc(id,o){calcs[id]=o;},loader(){},onData(fn){fn();},draw(name,fn){if(fn)draws[name]=fn;const f=draws[name];if(!f)return;document.querySelectorAll('[data-draw="'+name+'"]').forEach(el=>f(el));},
params(){return{};},setParams(){},resetParams(){},setLink(){},refresh(){}};
window.__drawAll=function(){Object.keys(draws).forEach(n=>dash.draw(n));};let t;addEventListener('resize',()=>{clearTimeout(t);t=setTimeout(window.__drawAll,150);});})();</script>""" % (
    json.dumps(donnees, ensure_ascii=False), json.dumps(geo, separators=(",", ":")), json.dumps(meta, ensure_ascii=False))

titre = f"Matrice de risque MVB – appui à la surveillance ORACLE, Bunia & Butembo (données au {meta['date_donnees']})"
html = f"""<!DOCTYPE html><html lang="fr"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>{titre}</title><meta name="author" content="Dr Gaylord Amani Ngaboyeka"><meta name="description" content="Matrice de risque et classement des zones de santé, maladie à virus Bundibugyo, Ituri et Nord-Kivu (RDC). Mise à jour quotidienne.">
<style>{THEME}</style><script>{d3}</script>{SHIM}</head><body><div class="page">
{corps}
<div class="foot">© 2026 Dr Gaylord Amani Ngaboyeka (ERSP/UCB ; projet ORACLE BDBV) — amani.ngaboyeka@ucbukavu.ac.cd · Page générée automatiquement le {meta['genere_le']} à partir des rapports de situation de l’INSP (transcription INRB-UMIE), WorldPop, GRID3, OSRM{', ACLED' if meta.get('acled_utilise') else ''} et des données de terrain ORACLE.</div>
</div><script>window.__drawAll();</script></body></html>"""
(DOCS / "index.html").write_text(html, encoding="utf-8")
for p in ["zones_classement.csv", "series_hebdo_projections.csv", "rt_par_site.csv", "resume_sites.csv", "meta.json"]:
    shutil.copy(SORTIE / p, DOCS / "donnees" / p)
(DOCS / ".nojekyll").write_text("")
print("Page écrite :", DOCS / "index.html")

# ---------------------------------------------------------------- PDF
chrome = next((c for c in ["/usr/bin/google-chrome", "/usr/bin/google-chrome-stable", "/usr/bin/chromium", "/usr/bin/chromium-browser"] if os.path.exists(c)), None)
try:
    from playwright.sync_api import sync_playwright
    with sync_playwright() as pw:
        b = pw.chromium.launch(executable_path=chrome) if chrome else pw.chromium.launch()
        pg = b.new_page(viewport={"width": 1100, "height": 800})
        erreurs = []; pg.on("pageerror", lambda e: erreurs.append(str(e)))
        pg.goto((DOCS / "index.html").as_uri()); pg.wait_for_timeout(1500)
        if erreurs: print("ERREURS JS :", erreurs); sys.exit(1)
        pg.emulate_media(media="print"); pg.evaluate("document.querySelectorAll('details').forEach(d=>d.open=true)")
        pg.pdf(path=str(DOCS / "matrice_risque_MVB.pdf"), format="A4", landscape=True, print_background=True,
               margin={"top": "10mm", "bottom": "10mm", "left": "10mm", "right": "10mm"})
        b.close()
    print("PDF écrit :", DOCS / "matrice_risque_MVB.pdf")
except ImportError:
    print("playwright absent : PDF non généré")
