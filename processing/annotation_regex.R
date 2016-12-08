## Annotate the data using regex
## This script is called (sourced) from "coding_manual-automatic.R"

library(stringr)



# Functions ---------------------------------------------------------------

seematch <- function(entry = NULL, regex = NULL, df = tr) {
  match_rows <- str_detect(df$Target, regex)
  sum(match_rows)
  if (sum(match_rows) == 0) stop("No matches for this regex!")
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
  new_match <- paste(old_match, paste(current_match, entry, sep = ":"), sep = "%")
  tr[match_rows, "Matches"] <<- new_match
}



# Prepare and clear annotation column -------------------------------------

# Add empty column for matches
tr$Matches <- ""


# Main verbs --------------------------------------------------------------

seematch("andar_V", "anda\\b")
annotate("andar_V", "anda\\b")

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

seematch("conducir_V", "conduce")
annotate("conducir_V", "conduce")

seematch("cruzar_V", "((ha vuelto|vuelve) a cruzar|ha cruzado|está cruzando|cruza(\\b|ba)|cruzó)")
annotate("cruzar_V", "((ha vuelto|vuelve) a cruzar|ha cruzado|está cruzando|cruza(\\b|ba)|cruzó)")

seematch("dar-vuelta_VPhrase", "está dando vuelta")
annotate("dar-vuelta_VPhrase", "está dando vuelta")

seematch("dejar_V", "(ha dejado|\\bdeja\\b)")
annotate("dejar_V", "(ha dejado|\\bdeja\\b)")

seematch("desaparecer_V", "(fue desaparecido|desaparec)")
annotate("desaparecer_V", "(fue desaparecido|desaparec)")

seematch("descender_V", "desciende")
annotate("descender_V", "desciende")

seematch("deslizar_V", "(está deslizando|desliza\\b)")
annotate("deslizar_V", "(está deslizando|desliza\\b)")

seematch("empujar_V", "(ha empujado|(ha )?est(á|aba|ado) empujando|(?<!mientras )empuja(\\b|ba)|empujó)")
annotate("empujar_V", "(ha empujado|(ha )?est(á|aba|ado) empujando|(?<!mientras )empuja(\\b|ba)|empujó)")

seematch("entrar_V", "(vuelve a entrar|ha entrado( (a|en))?|est(á|aba) entrando( (a|en))?|entra(\\b|ba)( (a|en))?|entró( (a|en))?)")
annotate("entrar_V", "(vuelve a entrar|ha entrado( (a|en))?|est(á|aba) entrando( (a|en))?|entra(\\b|ba)( (a|en))?|entró( (a|en))?)")

seematch("estirar_V", "ha estirado")
annotate("estirar_V", "ha estirado")

seematch("girar_V", "gira\\b")
annotate("girar_V", "gira\\b")

seematch("guardar_V", "(?<!donde se )guarda\\b")
annotate("guardar_V", "(?<!donde se )guarda\\b")

seematch("hacer-descender_Vphrase", "hace descender")
annotate("hacer-descender_Vphrase", "hace descender")

seematch("hacer-deslizar_Vphrase", "hace deslizar")
annotate("hacer-deslizar_Vphrase", "hace deslizar")

seematch("hacer-girar_Vphrase", "(?<!mientras )hace girar")
annotate("hacer-girar_Vphrase", "(?<!mientras )hace girar")

seematch("hacer-rodar_Vphrase", "((está haciendo|hace|ha hecho) rodar)")
annotate("hacer-rodar_Vphrase", "((está haciendo|hace|ha hecho) rodar)")

seematch("hacer-subir_Vphrase", "hace subir")
annotate("hacer-subir_Vphrase", "hace subir")

seematch("introducir_V", "(está introduciendo|introduce)")
annotate("introducir_V", "(está introduciendo|introduce)")

seematch("ir_V", "(ha ido|(?<!que (lo )?)\\bva\\b)")
annotate("ir_V", "(ha ido|(?<!que (lo )?)\\bva\\b)")

seematch("jugar_V", "(está jugando|juega)")
annotate("jugar_V", "(está jugando|juega)")

seematch("llevar_V", "(ha llevado|est(á|aba) llev[aá]ndo(se)?|(?<!mientras (la )?)lleva(\\b|ba)|llevó)")
annotate("llevar_V", "(ha llevado|est(á|aba) llev[aá]ndo(se)?|(?<!mientras (la )?)lleva(\\b|ba)|llevó)")

seematch("meter_V", "(ha metido|está metiendo|mete\\b|metió)")
annotate("meter_V", "(ha metido|está metiendo|mete\\b|metió)")

seematch("montar_V", "ha montado")
annotate("montar_V", "ha montado")

seematch("mover_V", "(ha movido|está moviendo|mueve|movió)")
annotate("mover_V", "(ha movido|está moviendo|mueve|movió)")

seematch("mudar_V", "\\bmud")
annotate("mudar_V", "\\bmud")

seematch("pasar_V", "(ha pasado|está pasando|\\bpasa\\b)")
annotate("pasar_V", "(ha pasado|está pasando|\\bpasa\\b)")

seematch("pasear_V", "pasea")
annotate("pasear_V", "pasea")

seematch("poner_V", "(está poniendo|\\bpone\\b|puso)")
annotate("poner_V", "(está poniendo|\\bpone\\b|puso)")

seematch("rodar_V", "(ha rodado|está rodando|rodaba|rodó|(?<!(una?|la|mientras|su|gran|con|de) )rueda\\b)")
annotate("rodar_V", "(ha rodado|está rodando|rodaba|rodó|(?<!(una?|la|mientras|su|gran|con|de) )rueda\\b)")

seematch("salir-de-paseo_VPhrase", "sale de paseo")
annotate("salir-de-paseo_VPhrase", "sale de paseo")

seematch("salir-de-viaje_VPhrase", "sale de viaje")
annotate("salir-de-viaje_VPhrase", "sale de viaje")

seematch("subir_V", "((vuelve|ha vuelto) a subir|ha subido|(ha )?est(á( ahora)?|aba|ado) subi(e|é)ndo|sube\\b|subió)")
annotate("subir_V", "((vuelve|ha vuelto) a subir|ha subido|(ha )?est(á( ahora)?|aba|ado) subi(e|é)ndo|sube\\b|subió)")

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



# Gerunds (adjuncts) ------------------------------------------------------

seematch("andando_Ger", "andando")
annotate("andando_Ger", "andando")

seematch("arrastrando_Ger", "(?<!est(á|aba) )arrastrando")
annotate("arrastrando_Ger", "(?<!est(á|aba) )arrastrando")

seematch("atravesando_Ger", "(?<!est(á|aba) )atravesando")
annotate("atravesando_Ger", "(?<!est(á|aba) )atravesando")

seematch("bajando_Ger", "(?<!est(á|aba) )bajando")
annotate("bajando_Ger", "(?<!est(á|aba) )bajando")

# seematch("caminar_V", "(camin(a|ó)|vuelve a caminar)")
# annotate("caminar_V", "camin(a|ó)")
# 
# seematch("cargar_V", "(está cargando|\\bcarga\\b)")
# annotate("cargar_V", "(está cargando|\\bcarga\\b)")
# 
# seematch("coger_V", "\\bcog")
# annotate("coger_V", "\\bcog")
# 
# seematch("conducir_V", "conduce")
# annotate("conducir_V", "conduce")
# 
# seematch("cruzar_V", "((ha vuelto|vuelve) a cruzar|ha cruzado|está cruzando|cruza(\\b|ba)|cruzó)")
# annotate("cruzar_V", "((ha vuelto|vuelve) a cruzar|ha cruzado|está cruzando|cruza(\\b|ba)|cruzó)")
# 
# seematch("dar-vuelta_VPhrase", "está dando vuelta")
# annotate("dar-vuelta_VPhrase", "está dando vuelta")
# 
# seematch("dejar_V", "(ha dejado|\\bdeja\\b)")
# annotate("dejar_V", "(ha dejado|\\bdeja\\b)")
# 
# seematch("desaparecer_V", "(fue desaparecido|desaparec)")
# annotate("desaparecer_V", "(fue desaparecido|desaparec)")
# 
# seematch("descender_V", "desciende")
# annotate("descender_V", "desciende")
# 
# seematch("deslizar_V", "(está deslizando|desliza\\b)")
# annotate("deslizar_V", "(está deslizando|desliza\\b)")
# 
# seematch("empujar_V", "(ha empujado|(ha )?est(á|aba|ado) empujando|(?<!mientras )empuja(\\b|ba)|empujó)")
# annotate("empujar_V", "(ha empujado|(ha )?est(á|aba|ado) empujando|(?<!mientras )empuja(\\b|ba)|empujó)")
# 
# seematch("entrar_V", "(vuelve a entrar|ha entrado( (a|en))?|est(á|aba) entrando( (a|en))?|entra(\\b|ba)( (a|en))?|entró( (a|en))?)")
# annotate("entrar_V", "(vuelve a entrar|ha entrado( (a|en))?|est(á|aba) entrando( (a|en))?|entra(\\b|ba)( (a|en))?|entró( (a|en))?)")
# 
# seematch("estirar_V", "ha estirado")
# annotate("estirar_V", "ha estirado")
# 
# seematch("girar_V", "gira\\b")
# annotate("girar_V", "gira\\b")
# 
# seematch("guardar_V", "(?<!donde se )guarda\\b")
# annotate("guardar_V", "(?<!donde se )guarda\\b")
# 
# seematch("hacer-descender_Vphrase", "hace descender")
# annotate("hacer-descender_Vphrase", "hace descender")
# 
# seematch("hacer-deslizar_Vphrase", "hace deslizar")
# annotate("hacer-deslizar_Vphrase", "hace deslizar")
# 
# seematch("hacer-girar_Vphrase", "(?<!mientras )hace girar")
# annotate("hacer-girar_Vphrase", "(?<!mientras )hace girar")
# 
# seematch("hacer-rodar_Vphrase", "((está haciendo|hace|ha hecho) rodar)")
# annotate("hacer-rodar_Vphrase", "((está haciendo|hace|ha hecho) rodar)")
# 
# seematch("hacer-subir_Vphrase", "hace subir")
# annotate("hacer-subir_Vphrase", "hace subir")
# 
# seematch("introducir_V", "(está introduciendo|introduce)")
# annotate("introducir_V", "(está introduciendo|introduce)")
# 
# seematch("ir_V", "(ha ido|(?<!que (lo )?)\\bva\\b)")
# annotate("ir_V", "(ha ido|(?<!que (lo )?)\\bva\\b)")
# 
# seematch("jugar_V", "(está jugando|juega)")
# annotate("jugar_V", "(está jugando|juega)")
# 
# seematch("llevar_V", "(ha llevado|est(á|aba) llev[aá]ndo(se)?|(?<!mientras (la )?)lleva(\\b|ba)|llevó)")
# annotate("llevar_V", "(ha llevado|est(á|aba) llev[aá]ndo(se)?|(?<!mientras (la )?)lleva(\\b|ba)|llevó)")
# 
# seematch("meter_V", "(ha metido|está metiendo|mete\\b|metió)")
# annotate("meter_V", "(ha metido|está metiendo|mete\\b|metió)")
# 
# seematch("montar_V", "ha montado")
# annotate("montar_V", "ha montado")
# 
# seematch("mover_V", "(ha movido|está moviendo|mueve|movió)")
# annotate("mover_V", "(ha movido|está moviendo|mueve|movió)")
# 
# seematch("mudar_V", "\\bmud")
# annotate("mudar_V", "\\bmud")
# 
# seematch("pasar_V", "(ha pasado|está pasando|\\bpasa\\b)")
# annotate("pasar_V", "(ha pasado|está pasando|\\bpasa\\b)")
# 
# seematch("pasear_V", "pasea")
# annotate("pasear_V", "pasea")
# 
# seematch("poner_V", "(está poniendo|\\bpone\\b|puso)")
# annotate("poner_V", "(está poniendo|\\bpone\\b|puso)")
# 
# seematch("rodar_V", "(ha rodado|está rodando|rodaba|rodó|(?<!(una?|la|mientras|su|gran|con|de) )rueda\\b)")
# annotate("rodar_V", "(ha rodado|está rodando|rodaba|rodó|(?<!(una?|la|mientras|su|gran|con|de) )rueda\\b)")
# 
# seematch("salir-de-paseo_VPhrase", "sale de paseo")
# annotate("salir-de-paseo_VPhrase", "sale de paseo")
# 
# seematch("salir-de-viaje_VPhrase", "sale de viaje")
# annotate("salir-de-viaje_VPhrase", "sale de viaje")
# 
# seematch("subir_V", "((vuelve|ha vuelto) a subir|ha subido|(ha )?est(á( ahora)?|aba|ado) subi(e|é)ndo|sube\\b|subió)")
# annotate("subir_V", "((vuelve|ha vuelto) a subir|ha subido|(ha )?est(á( ahora)?|aba|ado) subi(e|é)ndo|sube\\b|subió)")
# 
# seematch("tirar-de_V", "(ha tirado|est(á|aba) tirando( de)?|tira(\\b|ba)( de)?)")
# annotate("tirar-de_V", "(ha tirado|est(á|aba) tirando( de)?|tira(\\b|ba)( de)?)")
# 
# seematch("traer_V", "(trae\\b|traído)")
# annotate("traer_V", "(trae\\b|traído)")
# 
# seematch("transportar_V", "transporta")
# annotate("transportar_V", "transporta")
# 
# seematch("trasladar_V", "traslad")
# annotate("trasladar_V", "traslad")
# 
# seematch("venir_V", "(ha venido|viene)")
# annotate("venir_V", "(ha venido|viene)")
# 
# seematch("volver_v", "vuelve (?!a (caminar|cruzar|subir))")
# annotate("volver_v", "vuelve (?!a (caminar|cruzar|subir))")





