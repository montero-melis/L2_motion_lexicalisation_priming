## Annotate the data using regex
## This script is called (sourced) from "coding_manual-automatic.R"

library(stringr)


# Functions ---------------------------------------------------------------

seematch <- function(entry = NULL, regex = NULL, df = tr) {
  match_rows <- str_detect(df$Target, regex)
  sum(match_rows)
  if (sum(match_rows) == 0) stop("No matches for this regex!")
  out <- df[match_rows,] %>% select(-Match)
  out$VocabEntry <- entry
  out$MyMatch <- str_extract(out$Target, regex)
  out
}

annotate <- function(entry = NULL, regex = NULL) {
  match_rows <- str_detect(tr$Target, regex)
#   # for print only (does the same as seematch())
#   df4print <- tr[match_rows,] %>% select(-Match)
#   df4print$VocabEntry <- entry
#   df4print$MyMatch <- str_extract(df4print$Target, regex)
#   print(df4print)
  # do the annotation
  current_match <- str_extract(tr$Target[match_rows], regex)
  old_match <- tr$Match[match_rows]
  new_match <- paste(old_match, paste(current_match, entry, sep = ":"), sep = "%")
  tr[match_rows, "Match"] <<- new_match
}


# Prepare and clear annotation column -------------------------------------

# Add empty column for matches
tr$Match <- ""

# Some of the regex idioms used below:
# (?<!pattern)  # negative look-behind of "pattern"
# (?!pattern)  # negative look-ahead of "pattern"



#  ------------------------------------------------------------------------
#  Data annotation
#  ------------------------------------------------------------------------


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

seematch("dejar_V", "(ha dejado|\\bdeja\\b)")
annotate("dejar_V", "(ha dejado|\\bdeja\\b)")

seematch("desaparecer_V", "(fue desaparecido|desaparec)")
annotate("desaparecer_V", "(fue desaparecido|desaparec)")

seematch("descender_V", "desciende")
annotate("descender_V", "desciende")

seematch("deslizar_V", "(está deslizando|desliza\\b)")
annotate("deslizar_V", "(está deslizando|desliza\\b)")

seematch("empujar_V", "(ha empujado|(?<!mientras )(ha )?est(á|aba|ado) empujando|(?<!mientras )empuja(\\b|ba)|empujó)")
annotate("empujar_V", "(ha empujado|(?<!mientras )(ha )?est(á|aba|ado) empujando|(?<!mientras )empuja(\\b|ba)|empujó)")

seematch("entrar_V", "(vuelve a entrar|ha entrado( (a|en))?|est(á|aba) entrando( (a|en))?|entra(\\b|ba)( (a|en|dentro))?|entró( (a|en))?)")
annotate("entrar_V", "(vuelve a entrar|ha entrado( (a|en))?|est(á|aba) entrando( (a|en))?|entra(\\b|ba)( (a|en|dentro))?|entró( (a|en))?)")

seematch("estirar_V", "ha estirado")
annotate("estirar_V", "ha estirado")

seematch("girar_V", "gira\\b")
annotate("girar_V", "gira\\b")

seematch("guardar_V", "(?<!donde se )guarda\\b")
annotate("guardar_V", "(?<!donde se )guarda\\b")

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

seematch("mudar_V", "(está mudando|mudó)")
annotate("mudar_V", "(está mudando|mudó)")

seematch("pasar_V", "(ha pasado|está pasando|\\bpasa\\b)")
annotate("pasar_V", "(ha pasado|está pasando|\\bpasa\\b)")

seematch("pasear_V", "pasea")
annotate("pasear_V", "pasea")

seematch("poner_V", "(está poniendo|\\bpone\\b|puso)")
annotate("poner_V", "(está poniendo|\\bpone\\b|puso)")

seematch("rodar_V", "(ha rodado|está rodando|rodaba|rodó|(?<!(una?|la|mientras|su|gran|con|de) )rueda\\b)")
annotate("rodar_V", "(ha rodado|está rodando|rodaba|rodó|(?<!(una?|la|mientras|su|gran|con|de) )rueda\\b)")

seematch("subir_V", "((vuelve|ha vuelto) a subir|ha subido|(?<!mientras )(ha )?est(á( ahora)?|aba|ado) subi(e|é)ndo|sube\\b|subió)")
annotate("subir_V", "((vuelve|ha vuelto) a subir|ha subido|(?<!mientras )(ha )?est(á( ahora)?|aba|ado) subi(e|é)ndo|sube\\b|subió)")

seematch("tirar-de_V", "(ha tirado( de)?|est(á|aba) tirando( de)?|tira(\\b|ba)( de)?)")
annotate("tirar-de_V", "(ha tirado( de)?|est(á|aba) tirando( de)?|tira(\\b|ba)( de)?)")

seematch("traer_V", "(trae\\b|traído)")
annotate("traer_V", "(trae\\b|traído)")

seematch("transportar_V", "transporta")
annotate("transportar_V", "transporta")

seematch("trasladar_V", "traslad")
annotate("trasladar_V", "traslad")

seematch("venir_V", "(ha venido|viene)")
annotate("venir_V", "(ha venido|viene)")

seematch("volver_V", "vuelve (?!a (caminar|cruzar|entrar|subir))")
annotate("volver_V", "vuelve (?!a (caminar|cruzar|entrar|subir))")



# Verb phrases as main verbs ----------------------------------------------

seematch("dar-vuelta_VPhrase", "está dando vuelta")
annotate("dar-vuelta_VPhrase", "está dando vuelta")

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

seematch("salir-de-paseo_VPhrase", "sale de paseo")
annotate("salir-de-paseo_VPhrase", "sale de paseo")

seematch("salir-de-viaje_VPhrase", "sale de viaje")
annotate("salir-de-viaje_VPhrase", "sale de viaje")



# Adverbial phrases (adjuncts) --------------------------------------------

seematch("(a)dentro_AdvP", "(?<!(cosas|desde|hacia|hasta|para|por) )\\ba?dentro(?! el pueblo)")
annotate("(a)dentro_AdvP", "(?<!(cosas|desde|hacia|hasta|para|por) )\\ba?dentro(?! el pueblo)")

seematch("a-cuestas_AdvP", "a cuestas")
annotate("a-cuestas_AdvP", "a cuestas")

seematch("a-rastras_AdvP", "a rastras")
annotate("a-rastras_AdvP", "a rastras")

seematch("abajo_AdvP", "(?<!(desde|hacia|hasta|para) )\\babajo")
annotate("abajo_AdvP", "(?<!(desde|hacia|hasta|para) )\\babajo")

seematch("arriba_AdvP", "(?<!(de|desde|hacia|hasta|para( el parte más)?|por) )arriba")
annotate("arriba_AdvP", "(?<!(de|desde|hacia|hasta|para( el parte más)?|por) )arriba")

seematch("delante_AdvP", "delante")
annotate("delante_AdvP", "delante")

seematch("detrás/atrás_AdvP", "(\\bcon\\b.*)?(a|de)trás( de sí)?")
annotate("detrás/atrás_AdvP", "(\\bcon\\b.*)?(a|de)trás( de sí)?")

seematch("en-frente_AdvP", "en frente( de (sí|él))?")
annotate("en-frente_AdvP", "en frente( de (sí|él))?")

seematch("verticalmente_AdvP", "verticalmente")
annotate("verticalmente_AdvP", "verticalmente")



# Prepositional phrases (adjuncts) ----------------------------------------

seematch("a-través-de_PP", "a través(?! de una cuerda)")
annotate("a-través-de_PP", "a través(?! de una cuerda)")

seematch("de-una-acera-a-otra_PP", "(a la otra acera|de una acera a otra|de un lado de la acera a otro)")
annotate("de-una-acera-a-otra_PP", "(a la otra acera|de una acera a otra|de un lado de la acera a otro)")

seematch("al-topp@sw_PP", "al topp@sw")
annotate("al-topp@sw_PP", "al topp@sw")

seematch("con-una-cuerda_PP", "((atado|sujeto) )?((en|a|con|por|de) )?una cuerda")
annotate("con-una-cuerda_PP", "((atado|sujeto) )?((en|a|con|por|de) )?una cuerda")

seematch("de-un-lado-al-otro_PP", "\\bde un lado\\b.*otro(?! lado)")
annotate("de-un-lado-al-otro_PP", "\\bde un lado\\b.*otro(?! lado)")

seematch("encima_PP", "((hacia|por) )?encima(?!.*(agua|caballo|camino|río))")
annotate("encima_PP", "((hacia|por) )?encima(?!.*(agua|caballo|camino|río))")

seematch("hacia/hasta-BOUNDARY_PP", "(\\a|hacia|hasta) (un (almacén|garaje)|(las?|una) ((puertas de una )?casa|(oscura )?cueva))")
annotate("hacia/hasta-BOUNDARY_PP", "(\\a|hacia|hasta) (un (almacén|garaje)|(las?|una) ((puertas de una )?casa|(oscura )?cueva))")

seematch("hacia-la-parte-superior_PP", "hacia la parte superior de una ladera")
annotate("hacia-la-parte-superior_PP", "hacia la parte superior de una ladera")

seematch("PREP-(a)dentro_PP", "(hacia|hasta|para|por) a?dentro")
annotate("PREP-(a)dentro_PP", "(hacia|hasta|para|por) a?dentro")

seematch("PREP-abajo_PP", "(hacia|hasta|para|por) abajo")
annotate("PREP-abajo_PP", "(hacia|hasta|para|por) abajo")

seematch("PREP-arriba_PP", "(hacia|hasta|para|por) arriba")
annotate("PREP-arriba_PP", "(hacia|hasta|para|por) arriba")

seematch("PREP-el-interior_PP", "((al|hacia (el|un)|hasta el) )interior( de.*)?")
annotate("PREP-el-interior_PP", "((al|hacia (el|un)|hasta el) )interior( de.*)?")

seematch("PREP-la-cima_PP", "(\\ba|hacia|hasta|para|por) (la|el) cima")
annotate("PREP-la-cima_PP", "(\\ba|hacia|hasta|para|por) (la|el) cima")

seematch("PREP-la-cumbre_PP", "(a|hacia) la cumbre")
annotate("PREP-la-cumbre_PP", "(a|hacia) la cumbre")

seematch("PREP-la-punta_PP", "(hacia|hasta) la punta")
annotate("PREP-la-punta_PP", "(hacia|hasta) la punta")

seematch("PREP-lo-alto_PP", "(a|hacia|hasta) lo alto")
annotate("PREP-lo-alto_PP", "(a|hacia|hasta) lo alto")

seematch("PREP-otro-lado_PP", "((al|hacia el|hasta el|para el) )?otro lado(?! (de una iglesia|de la paja|del agua|del mar))( de.*)?")
annotate("PREP-otro-lado_PP", "((al|hacia el|hasta el|para el) )?otro lado(?! (de una iglesia|de la paja|del agua|del mar))( de.*)?")



# Gerunds (adjuncts) ------------------------------------------------------

seematch("andando_Ger", "andando")
annotate("andando_Ger", "andando")

seematch("arrastrando_Ger", "(?<!est(á|aba) )arrastr[áa]ndo(lo|la)?")
annotate("arrastrando_Ger", "(?<!est(á|aba) )arrastr[áa]ndo(lo|la)?")

seematch("atravesando_Ger", "(?<!est(á|aba) )atravesando")
annotate("atravesando_Ger", "(?<!est(á|aba) )atravesando")

seematch("bajando_Ger", "(?<!est(á|aba) )bajando")
annotate("bajando_Ger", "(?<!est(á|aba) )bajando")

seematch("cargando_Ger", "(?<!est(á|aba) )cargando")
annotate("cargando_Ger", "(?<!est(á|aba) )cargando")

seematch("cruzando_Ger", "(?<!est(á|aba) )cruzando")
annotate("cruzando_Ger", "(?<!est(á|aba) )cruzando")

seematch("dejando_Ger", "dejándola")
annotate("dejando_Ger", "dejándola")

seematch("empujando_Ger", "(?<!est(á|aba) )empuj[aá]ndo(lo|la|se)?")
annotate("empujando_Ger", "(?<!est(á|aba) )empuj[aá]ndo(lo|la|se)?")

seematch("entrando_Ger", "(?<!est(á|aba) )entr[áa]ndo(lo)?( (en|a))?")
annotate("entrando_Ger", "(?<!est(á|aba) )entr[áa]ndo(lo)?( (en|a))?")

seematch("estirando_Ger", "estirando")
annotate("estirando_Ger", "estirando")

seematch("introduciendo_Ger", "(?<!est(á|aba) )introduciendo")
annotate("introduciendo_Ger", "(?<!est(á|aba) )introduciendo")

seematch("llevando_Ger", "(?<!est(á|aba) )llev[áa]ndo(lo|la|se)?")
annotate("llevando_Ger", "(?<!est(á|aba) )llev[áa]ndo(lo|la|se)?")

seematch("metiendo_Ger", "(?<!est(á|aba) )meti[ée]ndo(lo|la|se)?")
annotate("metiendo_Ger", "(?<!est(á|aba) )meti[ée]ndo(lo|la|se)?")

seematch("pasando_Ger", "(?<!(está|y) )pasando")
annotate("pasando_Ger", "(?<!(está|y) )pasando")

seematch("portando_Ger", "portando")
annotate("portando_Ger", "portando")

seematch("rodando_Ger", "(?<!est(á|aba) )rodándo(lo|la|se)?")
annotate("rodando_Ger", "(?<!est(á|aba) )rodándo(lo|la|se)?")

seematch("subiendo_Ger", "(?<!est(á|aba|amos) )subi[ée]ndo(lo|la|se)?")
annotate("subiendo_Ger", "(?<!est(á|aba|amos) )subi[ée]ndo(lo|la|se)?")

seematch("tirando-de_Ger", "(?<!est(á|aba) )tirando( de)?")
annotate("tirando-de_Ger", "(?<!est(á|aba) )tirando( de)?")

seematch("trayendo_Ger", "trayendo")
annotate("trayendo_Ger", "trayendo")

seematch("yendo_Ger", "\\byendo")
annotate("yendo_Ger", "\\byendo")



# Verbs in infinitive (part of adjuncts) ----------------------------------

seematch("bajar_Inf", "(?<!acaba de )(al |para )?bajar")
annotate("bajar_Inf", "(?<!acaba de )(al |para )?bajar")

seematch("cruzar_Inf", "para cruzar")
annotate("cruzar_Inf", "para cruzar")

seematch("guardar_Inf", "para guardar(lo|la)?")
annotate("guardar_Inf", "para guardar(lo|la)?")

seematch("introducir_Inf", "para introducirlo")
annotate("introducir_Inf", "para introducirlo")

seematch("meter_Inf", "((hasta|para) )?meter(la)?")
annotate("meter_Inf", "((hasta|para) )?meter(la)?")



# Subordinate finite verbs (adjuncts) -------------------------------------

seematch("arrastrar_SubordV", "mientras arrastra(ba)?")
annotate("arrastrar_SubordV", "mientras arrastra(ba)?")

seematch("bajar_SubordV", "mientras baja")
annotate("bajar_SubordV", "mientras baja")

seematch("empujar_SubordV", "mientras (está empujando|empuja)")
annotate("empujar_SubordV", "mientras está empujando")

seematch("llevar_SubordV", "mientras (la)? lleva")
annotate("llevar_SubordV", "mientras lleva")

seematch("rodar_SubordV", "mientras rueda")
annotate("rodar_SubordV", "mientras rueda")

seematch("subir_SubordV", "mientras está subiendo")
annotate("subir_SubordV", "mientras está subiendo")



# Verb phrases in gerundive form (adjuncts) -------------------------------

seematch("haciendo-girar_Vphrase-Ger", "haciendo girar")
annotate("haciendo-girar_Vphrase-Ger", "haciendo girar")

seematch("haciendo-rodar_Vphrase-Ger", "(?<!est(á|aba) )haci[eé]ndo(la)? rodar")
annotate("haciendo-rodar_Vphrase-Ger", "(?<!est(á|aba) )haci[eé]ndo(la)? rodar")



## targets with no matches
sum(tr$Match == "")  # how many?
tr[tr$Match == "", ]  # show



