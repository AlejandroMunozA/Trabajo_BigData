###########################Instrucciones Trabajo Final###########################

#Antes de comenzar crearán un repositorio en Git para la entrega del trabajo final, el cual será llamado “Trabajo_BigData” .
#Los pasos a seguir, serán clonar el repositorio previamente creado en su equipo local.
#Configure el repositorio descargado, como su espacio de trabajo en el RStudio. 


#El plazo de entrega será hasta el 4 de Enero a las 23:59
#enviar el link del git a los correos diana.lopez@utem.cl y amaru.fernandezd@utem.cl .

########################### Se les pide lo siguiente: ###########################

########################### A) Cree una presentación (PPT) que tenga como estructura ( 10 ptos ): ###########################

#-Portada: Nombre de los autores, título del trabajo, nombre profesores de cátedra.
#-Introducción: Antecedentes y motivación del estudio
#-Objetivos que se plantearon, los cuales se establecieron en la primera tarea
#-Metodología: Donde expliquen y muestren donde encontraron la clase, tag, id etc de sus datos. 
#Esto a grandes rasgos, podrían agregar otra cosa que les parece #relevante para complementar
#-Resultados: Que exponga estadística descriptiva de los datos y algunos gráficos
#que permitan un análisis más profundo.
#-Conclusiones y comentarios finales.
#No deben ser más de 10 slides, donde lo señalado anteriormente es lo mínimo a contener.

########################### B) Script que contenga (20 ptos): ###########################

#-Desarrollo de la extracción de datos
#-Desarrollo de los gráficos
#-Comentarios de los procesos
#-Pequeños análisis de los datos
#Recordar ser ordenado, cuidar redacción y ortografía , ya que será penalizado en el caso de tener faltas.

########################### C) Un video de 15 min máximo (10 ptos). ###########################

#donde muestre y explique sus resultados. En definitiva, presente la ppt que hizo en la letra A. 

############################ D) Las bases de datos que recopiló (5 ptos)###########################

#las cuales deben contener una pequeña descripción de sus columnas.


######################################################     DESARROLLO SCRIPT     ######################################################



#==================== usando twitchtracker ====================

# FALTA: Abrir y transformar a data.frame el archivo CVS
# FALTA: FOR-LOOP para recorrer de página en página

# Inicializando la var de archivo con el nombre de la página a utilizar
paginatwitchtracker <- 'https://twitchtracker.com/'

# Leyendo el html del archivo
webpagetwitchtracker <- read_html(paginatwitchtracker)

# Extraccion del texto contenido en la clase thumb-under
contenidoWebtwitchtracker <- html_nodes(webpagetwitchtracker,'.thumb-under > p > a')
print (contenidoWebtwitchtracker)

# Viendo el contenido de la posición 1 de la variable contenidoWebtwitchtracker
print(contenidoWebtwitchtracker[1])

# Extrayendo los links de los videos
linksVIDEOS <- html_attr(contenidoWebtwitchtracker,"href")

# Arreglando los links de todos los videos
todosLosLinkstwitchtracker <- ""
for(i in 1:27){
  todosLosLinkstwitchtracker <- print(paste("twitchtracker",linksVIDEOS,sep = ""))
}

# ###########                                                  Viendo que tiene la posicion 1 de la variable todosLosLinksXvideo
print(todosLosLinksXvideo[1])

# Extrayendo el texto de contenidoWebXVideos
textoXVideos <- html_text(contenidoWebXVideos)

# Viendo que tiene la posicion 1 la variable textoXVideos
print(textoXVideos[1])

# Extraccion de duracion de cada video
DurationXVideos <- html_nodes(webpageXVideos,'.duration')

#Limpieza de los datos de duracion
DuracionXVideos <- html_text(DurationXVideos)

# Viendo que tiene la posición 1 de la variable DuracionXVideos
print(DuracionXVideos[1])

# Primer paso para extraer el numero de visitas de cada video
VistasXVideos <- html_nodes(webpageXVideos,'.thumb-under > p > span')

# Limpiando los datos para tener solo el texto 
texto_VistasXVideos <- html_text(VistasXVideos)

# Separando el texto obtenido con un guion para despues eliminar la duracion
split_VistasXVideos <- strsplit(texto_VistasXVideos,"-")

# Obteniendo el primer dato de views 
viewsXVideos <- list()
for(i in 1:length(split_VistasXVideos)){
  print(split_VistasXVideos[[i]][[2]])
  viewsXVideos[i] <- split_VistasXVideos[[i]][[2]]
}

# Limpiando los datos obtenidos de views
viewsXVideos <-  gsub("Views","",viewsXVideos)
viewsXVideos <- gsub(" ","",viewsXVideos)
viewsXVideos <- gsub("k","-k",viewsXVideos)  
viewsXVideos <- gsub("M","-M",viewsXVideos)  

# Separando los datos para luego reemplazar k y M numericamente
Visitas <- strsplit(viewsXVideos,"-")

# Crear funcion para reemplazar k y M numericamente #

# VisitasXVideo: string -> double
# VisitasXVideo: entrega la cantidad de visitas de cada video
# si aparece una k se multiplica el numero por mil 
# si aparece una M se multimplica por un millon
# Ejemplo: VisitasXVideo(4k)-> 4000

VisitasXVideo <- function (entrada){
  # para los elementos que no tienen ni k, ni M, se usa is.na
  if(is.na(entrada[2])){
    entrada[1] <- as.numeric(entrada[1])
  }else if(entrada[2]=="k"){
    entrada[1] <- as.numeric(entrada[1])*1000
  }else if(entrada[2]=="M"){
    entrada[1] <- as.numeric(entrada[1])*1000000
  }
  return(entrada[1])
}

# Recorriendo cada elemento aplicando la funcion VisitasXVideo 
for(i in 1:length(Visitas)){
  Visitas[i] <- VisitasXVideo(Visitas[[i]])
}

# Extrae los elementos de la lista y los pasa a una lista
unlistVisitas <- unlist(Visitas)


#==================== UNA GRAN TABLA ====================#

# Creando una tabla con mas de una columna
dfvideos <- data.frame(LINKS = todosLosLinksXvideo, TITULO = textoXVideos, DURACION = DuracionXVideos, VIEWS = unlistVisitas)

# FALTA: unir los data.frames

##### Se guardan los datos porque hay que empezar a tener algo ya po :c
#alamacenando la informacion en CSV
write.csv(dfvideos, file="01TablaXVideos.csv")

#rbin recordar# 

# Tablas datos por separado

# Tabla de los titulos de la pág 1 de new 
tabla_titulos <- table(textoXVideos)

# Transformando a data framtabla
tituloXVideos <- as.data.frame(tabla_titulos)

# Unificando los títulos
todosLosTitulosXVideo <- ""
for(i in 1 : length(textoXVideos)){
  todosLosTitulosXVideo <- paste(todosLosTitulosXVideo," ",textoXVideos[[i]])
}

# Separando las palabras por espacio
todosLosTitulosXVideo <- strsplit(todosLosTitulosXVideo," ")[[1]]

# Pasando todas las palabras a minúsculas
todosLosTitulosXVideo <- tolower(todosLosTitulosXVideo)

# Contando palabras
unlistTitulosXVideos <- unlist(todosLosTitulosXVideo)
tablaXVideos <- table(unlistTitulosXVideos)

# Transformando a data framtabla
tituloXVideos <- as.data.frame(tablaXVideos)

#Duracion de videos en tabla
tabla_duracion <- table(DuracionXVideos)
Duracion_tabla <- as.data.frame(tabla_duracion)

# Tabla de numero de visitas
# El "transpose" es para que quede en vertical
df <- data.frame("vistas" = transpose(a))
