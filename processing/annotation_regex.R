## Annotate the data using regex
## This script is called (sourced) from "coding_manual-automatic.R"

library(stringr)

seematch <- function(entry = NULL, regex = NULL, df = tr) {
  match_rows <- str_detect(df$Target, regex)
  out <- df[match_rows,] %>% select(-Matches)
  out$VocabEntry <- entry
  out$MyMatch <- str_extract(out$Target, regex)
  out
}

annotate <- function(entry = NULL, regex = NULL) {
  match_rows <- str_detect(tr$Target, regex)
#   # for print only (does the same as seematch())
#   df4print <- tr[match_rows,] %>% select(-Matches)
#   df4print$VocabEntry <- entry
#   df4print$MyMatch <- str_extract(df4print$Target, regex)
#   print(df4print)
  # do the annotation
  current_match <- str_extract(tr$Target[match_rows], regex)
  old_match <- tr$Matches[match_rows]
  new_match <- paste(old_match, paste(current_match, entry, sep = ":"), sep = "/")
  tr[match_rows, "Matches"] <<- new_match
}


# Add empty column for matches
tr$Matches <- ""


# Main verbs --------------------------------------------------------------

seematch("arrastrar_V", "(ha arrastrado|est(á|aba) arrastrando|acaba de arrastrar|(?<!mientras )arrastra(\\b|ba)|arrastró)")
annotate("arrastrar_V", "(ha arrastrado|est(á|aba) arrastrando|acaba de arrastrar|(?<!mientras )arrastra(\\b|ba)|arrastró)")

seematch("ascender_V", "asciende")
annotate("ascender_V", "asciende")

seematch("atravesar_V", "(está atravesando|ha atravesado|atraviesa)")
annotate("atravesar_V", "(está atravesando|ha atravesado|atraviesa)")

seematch("bajar_V", "(ha bajado|est(á|aba) bajando|acaba de bajar|(?<!mientras )baja(\\b|ba)|bajó)")
annotate("bajar_V", "(ha bajado|est(á|aba) bajando|acaba de bajar|(?<!mientras )baja(\\b|ba)|bajó)")

seematch("caminar_V", "(camin(a|ó)|vuelve a caminar)")
annotate("caminar_V", "camin(a|ó)")

seematch("cargar_V", "(está cargando|\\bcarga\\b)")
annotate("cargar_V", "(está cargando|\\bcarga\\b)")

seematch("coger_V", "\\bcog")
annotate("coger_V", "\\bcog")

seematch("cruzar_V", "((ha vuelto|vuelve) a cruzar|ha cruzado|está cruzando|cruza(\\b|ba)|cruzó)")
annotate("cruzar_V", "((ha vuelto|vuelve) a cruzar|ha cruzado|está cruzando|cruza(\\b|ba)|cruzó)")

seematch("dar-vuelta_V", "está dando vuelta")
annotate("dar-vuelta_V", "está dando vuelta")

seematch("descender_V", "desciende")
annotate("descender_V", "desciende")

seematch("deslizar_V", "(está deslizando|desliza\\b)")
annotate("deslizar_V", "(está deslizando|desliza\\b)")

seematch("empujar_V", "(ha empujado|(ha )?est(á|aba|ado) empujando|(?<!mientras )empuja(\\b|ba)|empujó)")
annotate("empujar_V", "(ha empujado|est(á|aba) empujando|(?<!mientras )empuja(\\b|ba)|empujó)")

seematch("entrar_V", "(vuelve a entrar|ha entrado( (a|en))?|está entrando( (a|en))?|entra(\\b|ba)( (a|en))?|entró( (a|en))?)")
annotate("entrar_V", "(vuelve a entrar|ha entrado( (a|en))?|está entrando( (a|en))?|entra(\\b|ba)( (a|en))?|entró( (a|en))?)")

seematch("estirar_V", "ha estirado")
annotate("estirar_V", "ha estirado")

seematch("girar_V", "gira\\b")
annotate("girar_V", "gira\\b")

seematch("guardar_V", "(?<!donde se )guarda\\b")
annotate("guardar_V", "(?<!donde se )guarda\\b")

seematch("hacer-descender_V", "hace descender")
annotate("hacer-descender_V", "hace descender")

seematch("hacer-deslizar_V", "hace deslizar")
annotate("hacer-deslizar_V", "hace deslizar")

seematch("hacer-girar_V", "(?<!mientras )hace girar")
annotate("hacer-girar_V", "(?<!mientras )hace girar")

seematch("hacer-rodar_V", "((está haciendo|hace|ha hecho) rodar)")
annotate("hacer-rodar_V", "((está haciendo|hace|ha hecho) rodar)")

seematch("hacer-subir_V", "hace subir")
annotate("hacer-subir_V", "hace subir")

seematch("introducir_V", "(está introduciendo|introduce)")
annotate("introducir_V", "(está introduciendo|introduce)")

seematch("ir_V", "(ha venido|(?<!que (lo )?)\\bva\\b)")
annotate("ir_V", "(ha venido|(?<!que (lo )?)\\bva\\b)")

seematch("jugar_V", "(está jugando|juega)")
annotate("jugar_V", "(está jugando|juega)")

seematch("llevar_V", "(ha llevado|est(á|aba) llev[aá]ndo(se)?|(?<!mientras (la )?)lleva(\\b|ba)|llevó)")
annotate("llevar_V", "(ha llevado|est(á|aba) llev[aá]ndo(se)?|(?<!mientras (la )?)lleva(\\b|ba)|llevó)")

seematch("meter_V", "(ha metido|está metiendo|mete\\b|metió)")
annotate("meter_V", "(ha metido|está metiendo|mete\\b|metió)")

seematch("mover_V", "(ha movido|está moviendo|mueve)")
annotate("mover_V", "(ha movido|está moviendo|mueve)")

seematch("pasar_V", "(ha pasado|está pasando|\\bpasa\\b)")
annotate("pasar_V", "(ha pasado|está pasando|\\bpasa\\b)")

seematch("pasear_V", "pasea")
annotate("pasear_V", "pasea")

seematch("rodar_V", "(ha rodado|está rodando|(?<!(una?|la|mientras|su|gran|con|de) )rueda\\b)")
annotate("rodar_V", "(ha rodado|está rodando|(?<!(una?|la|mientras|su|gran|con|de) )rueda\\b)")

seematch("subir_V", "((vuelve|ha vuelto) a subir|ha subido|(ha )?est(á( ahora)?|aba|ado) subiendo|sube\\b|subió)")
annotate("subir_V", "((vuelve|ha vuelto) a subir|ha subido|(ha )?est(á( ahora)?|aba|ado) subiendo|sube\\b|subió)")

seematch("tirar-de_V", "(ha tirado|est(á|aba) tirando( de)?|tira(\\b|ba)( de)?)")
annotate("tirar-de_V", "(ha tirado|est(á|aba) tirando( de)?|tira(\\b|ba)( de)?)")

seematch("traer_V", "(trae\\b|traído)")
annotate("traer_V", "(trae\\b|traído)")

seematch("transportar_V", "transporta")
annotate("transportar_V", "transporta")

seematch("trasladar_V", "traslad")
annotate("trasladar_V", "traslad")

seematch("venir_V", "(ha venido|viene)")
annotate("venir_V", "(ha venido|viene)")

seematch("volver_v", "vuelve (?!a (caminar|cruzar|subir))")
annotate("volver_v", "vuelve (?!a (caminar|cruzar|subir))")





## targets with no matches
sum(tr$Matches == "")  # how many?
tr[tr$Matches == "", ]  # show


# seematch("", "")
# annotate("", "")
# 
# seematch("", "")
# annotate("", "")




