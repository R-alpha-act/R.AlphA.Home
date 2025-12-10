---
date: 2025-12-10
type: Refactor
subject: Session 4 - Remplacer stringi/stringr par base R
fix: Éliminer dépendances problématiques stringi (14.79 MB) et stringr
short_story: Préparation Session 4 - Audit complet des usages stringi/stringr. 32 occurrences dans 6 fichiers. Équivalents base R identifiés.
status: 📋 Planifié
category: Architecture
archive_after: 6m
keywords: [stringi, stringr, base R, dependencies, regex, gsub, grepl, regmatches]
---

# Session 4 : Remplacer stringi/stringr par base R

## Contexte

**Problème :** stringi/stringr posent des problèmes récurrents :

1. **Taille** : stringi = 14.79 MB (plus gros package du projet)
2. **CI/CD** : Échec GitHub Actions - dépendance système ICU library non disponible
3. **Installation** : Corruption gzip récurrente sur macOS
4. **Double dépendance** : openxlsx tire aussi stringi (donc resterait présent)

**Solution :** Remplacer par base R (comme fait dans R.AlphA.AI Session 3)

---

## Audit des usages

### stringi (4 occurrences - 1 fichier)

**R/ralpha_fold.R :**
- `stri_extract(lineStart, regex = escapePatt)` (lignes 100, 152)
- `stri_count(escaping, regex = "\\\\")` (ligne 153)
- `stri_replace(regex = "\\.[0-9]+$", "")` (ligne 211)

### stringr (28 occurrences - 6 fichiers)

| Fichier | Fonctions | Occurrences |
|---------|-----------|-------------|
| `foldAllBr.R` | str_remove, str_detect, str_remove_all, str_extract, str_count | 15 |
| `ralpha_unfold.R` | str_detect, str_locate, str_locate_all | 10 |
| `left_join_checks.R` | str_remove | 2 |
| `cols_pad.R` | str_replace | 1 |
| `importAll.R` | str_detect | 1 (déjà traité en Session 3) |

**Total : 32 usages dans 6 fichiers**

---

## Équivalents base R

### Table de conversion stringr → base R

| stringr | base R | Exemple |
|---------|--------|---------|
| `str_detect(x, pattern)` | `grepl(pattern, x)` | `grepl("foo", x)` |
| `str_remove(x, pattern)` | `sub(pattern, "", x)` | `sub("foo", "", x)` |
| `str_remove_all(x, pattern)` | `gsub(pattern, "", x)` | `gsub("foo", "", x)` |
| `str_extract(x, pattern)` | `regmatches(x, regexpr(pattern, x))` | Voir note 1 |
| `str_count(x, pattern)` | `lengths(gregexpr(pattern, x))` | Voir note 2 |
| `str_locate(x, pattern)` | `regexpr(pattern, x)` + attr | Voir note 3 |
| `str_locate_all(x, pattern)` | `gregexpr(pattern, x)` + attr | Voir note 4 |
| `str_replace(x, pattern, repl)` | `sub(pattern, repl, x)` | `sub("foo", "bar", x)` |

### Table de conversion stringi → base R

| stringi | base R | Exemple |
|---------|---------|---------|
| `stri_extract(x, regex = pattern)` | `regmatches(x, regexpr(pattern, x, perl=TRUE))` | Voir note 1 |
| `stri_count(x, regex = pattern)` | `lengths(gregexpr(pattern, x, perl=TRUE))` | Voir note 2 |
| `stri_replace(x, regex = pattern, repl)` | `sub(pattern, repl, x, perl=TRUE)` | `sub("\\.[0-9]+$", "", x, perl=TRUE)` |

### Notes techniques

**Note 1 - str_extract / stri_extract :**
```r
# stringr/stringi
result <- str_extract(x, "pattern")

# base R
m <- regexpr("pattern", x, perl = TRUE)
result <- ifelse(m == -1, NA_character_, regmatches(x, m))
```

**Note 2 - str_count / stri_count :**
```r
# stringr/stringi
count <- str_count(x, "pattern")

# base R
m <- gregexpr("pattern", x, perl = TRUE)
count <- ifelse(m[[1]][1] == -1, 0, lengths(m))
```

**Note 3 - str_locate :**
```r
# stringr
loc <- str_locate(x, "pattern")  # matrix [start, end]

# base R
m <- regexpr("pattern", x, perl = TRUE)
start <- as.vector(m)
end <- start + attr(m, "match.length") - 1
loc <- cbind(start = start, end = end)
```

**Note 4 - str_locate_all :**
```r
# stringr
locs <- str_locate_all(x, "pattern")  # list of matrices

# base R
m <- gregexpr("pattern", x, perl = TRUE)
locs <- lapply(m, function(match) {
    if (match[1] == -1) return(matrix(ncol = 2, nrow = 0))
    start <- as.vector(match)
    end <- start + attr(match, "match.length") - 1
    cbind(start = start, end = end)
})
```

---

## Plan de travail

### Ordre recommandé (du plus simple au plus complexe)

1. ✅ **cols_pad.R** (1 usage - str_replace)
2. ✅ **left_join_checks.R** (2 usages - str_remove)
3. ✅ **importAll.R** (1 usage - str_detect) - déjà fait Session 3
4. **ralpha_fold.R** (4 usages stringi + quelques stringr)
5. **foldAllBr.R** (15 usages stringr)
6. **ralpha_unfold.R** (10 usages stringr)

### Fichiers modifiés

Pour chaque fichier :
- Remplacer les fonctions stringr/stringi par équivalents base R
- Supprimer `@importFrom stringr ...` et `@importFrom stringi ...`
- Tester le comportement (surtout regex patterns)

### DESCRIPTION

Après toutes les modifications :
```
Imports actuels : 14
- stringi
- stringr
= 12 imports (-2)
```

⚠️ **Important :** openxlsx tire toujours stringi, donc stringi restera installé mais ne sera plus un import direct.

---

## TODO Session 4

- [ ] Modifier cols_pad.R (str_replace → sub)
- [ ] Modifier left_join_checks.R (str_remove × 2 → sub)
- [ ] Modifier ralpha_fold.R (stri_extract, stri_count, stri_replace)
- [ ] Modifier foldAllBr.R (15 usages stringr)
- [ ] Modifier ralpha_unfold.R (10 usages stringr)
- [ ] Supprimer stringi de DESCRIPTION Imports
- [ ] Supprimer stringr de DESCRIPTION Imports
- [ ] Régénérer NAMESPACE (devtools::document())
- [ ] Tester les fonctions modifiées
- [ ] Documenter résultat final

---

## Références

**Docs connexes :**
- Log principal : `./2025-12-10_reduire-imports-description.md`
- Arbre dépendances : `./2025-12-10_arbre-dependances-pak.md`
- Point d'entrée : `../../__START_HERE.md`

**Même travail ailleurs :**
- **R.AlphA.AI Session 3** : `../../../R.AlphA.AI/doc/log/2025-12-09_reduire-imports-description.md`
  - Exemples de remplacement stringr → base R
  - Patterns utilisés : `gsub("(?s)...", perl=TRUE)`, `regmatches()`, `substr()`

**Ressources :**
- Base R regex : `?regex`, `?grep`, `?sub`, `?regmatches`
- stringr vignette : pour comprendre comportement attendu

---

## Impact estimé

**Avant Session 4 :**
```
Imports : 14
Packages installés : ? (à mesurer)
stringi : 14.79 MB (tiré par openxlsx + stringr)
```

**Après Session 4 :**
```
Imports : 12 (-2)
Packages installés : ? (stringi reste via openxlsx)
stringi : 14.79 MB (reste, mais plus un import direct)
```

**Bénéfices :**
- ✅ Suppression de 2 imports directs problématiques
- ✅ Résolution échec CI/CD GitHub Actions
- ✅ Moins de vulnérabilité aux packages tiers
- ⚠️ stringi reste installé via openxlsx (évaluer remplacement openxlsx → writexl en Session 5 ?)

---

## Transition vers Session 5

**Analyse préparatoire :**
- openxlsx : 2 fonctions utilisées (read.xlsx, write.xlsx) → remplaçables par readxl + writexl
- tidyr : 1 fonction utilisée (replace_na, 5 occurrences) → remplaçable par dplyr::coalesce

**Décision :** Session 5 planifiée pour éliminer complètement stringi de l'arbre de dépendances.

→ Voir `doc/log/2025-12-10_session5-eliminer-stringi-openxlsx-tidyr.md` pour l'implémentation complète.

---

## Notes pour la prochaine session

**Stratégie de test :**
1. Créer des tests unitaires pour chaque fonction modifiée AVANT les changements
2. Modifier le code
3. Vérifier que les tests passent toujours
4. Tester manuellement les cas edge (NA, vecteurs vides, patterns complexes)

**Regex patterns à surveiller :**
- `perl = TRUE` nécessaire pour certains patterns (lookahead, etc.)
- Gestion des NA dans regmatches (retourne character(0) au lieu de NA)
- gregexpr retourne -1 pour no match (pas pareil que stringr)

**Prochaine session potentielle (Session 5) :**
- Remplacer openxlsx par writexl (élimine stringi complètement)
- writexl : 0 dépendances, plus léger, plus rapide
