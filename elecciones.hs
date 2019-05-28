data Dirigente = Dirigente{nombre::String,imagen::Int,habilidades::[String],cargo::String,partido::String}deriving(Eq,Show)
varios = ["pro","ucr","cc","fpv"]
frenteDeIzquierda = ["po","pcr"]
macri = Dirigente{nombre="macri",imagen=40,habilidades=["bailar","bailar mas"],cargo="concejal",partido="pro"}
cfk = Dirigente{nombre="cfk",imagen=50,habilidades=["nose"],cargo="gobernador",partido="fpv"}
massa = Dirigente{nombre="massa",imagen=30,habilidades=["honestidad"],cargo="gobernador",partido="ucr"}

candidatosValidos alianza candidatos = filter (pertenece alianza) candidatos
pertenece alianza candidato = elem (partido candidato) alianza
----------------------------------------------------------------------------------------------------------
mayorCargo candidatos = [elMejor candidatos]++[masPuntos (sacarAlMejor candidatos)]
sacarAlMejor candidatos = filter (noEs (elMejor candidatos)) candidatos
noEs mejor candidato = (nombre mejor)/=(nombre candidato)

masPuntos [x] = x
masPuntos (cand1:resto) | ((imagen cand1)>20) = cand1
 | otherwise = masPuntos resto

elMejor [x] = x
elMejor (candidato1:candidato2:resto) = elMejor((mejorPuesto candidato1 candidato2):resto)

mejorPuesto cand1 cand2 | (cargo cand1)=="presidente" && (cargo cand2)/="presidente" = cand1
 | (cargo cand1)=="gobernador" && (((cargo cand2)=="gobernador") || (cargo cand2)=="concejal") = cand1
 | (cargo cand1)=="concejal" && (cargo cand2)=="concejal" = cand1
 | otherwise = cand2
----------------------------------------------------------------------------------------------------------
mayorImagenPresidente candidatos = [mejorImagen candidatos]++[gobernadorHonesto (sacarMejorImagen candidatos)]
sacarMejorImagen candidatos = filter (noEs (mejorImagen candidatos)) candidatos

mejorImagen [x] = x
mejorImagen (cand1:cand2:resto) = mejorImagen((masImagen cand1 cand2):resto)
masImagen cand1 cand2 | (imagen cand1)>=(imagen cand2) = cand1
 | otherwise = cand2
 
gobernadorHonesto [x] = x
gobernadorHonesto (cand1:resto) | (((cargo cand1)=="gobernador")&&(elem "honestidad" (habilidades cand1))) = cand1
 | otherwise = gobernadorHonesto resto
----------------------------------------------------------------------------------------------------------
mayorImagenVicepresidente candidatos = [masHabilidades candidatos]++[mejorImagen (sacarMasHabil candidatos)]
sacarMasHabil candidatos = filter (noEs (masHabilidades candidatos)) candidatos

masHabilidades [x] = x
masHabilidades (cand1:cand2:resto) = masHabilidades((masCantHabilidades cand1 cand2):resto)
masCantHabilidades cand1 cand2 | length(habilidades cand1)>length(habilidades cand2) = cand1
 | otherwise = cand2
----------------------------------------------------------------------------------------------------------
obtenerFormula alianza candidatos modoConstruccion = modoConstruccion (candidatosValidos alianza candidatos)