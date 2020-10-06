# Se consideran los siguiente paises: Spain, England, Germany, Italy
# Se dispone de información para los años en el intervalo [2010,2015]

ligas<-function(pais,temporada){
  # Se dispone de información de 4 países: 
  leag<-c("Spain","England","Germany","Italy")
  
  # filtros de función
  if (! pais %in% leag) {
    return("No se dispone de información para este país.")
  }
  if (temporada < 2010 | temporada > 2015) {
    return("No se dispone de información para esta temporada.")
  }
      
  cabeza<-"http://www.livefutbol.com/calendario/"
  paises=list("Spain"="esp-primera-division-","England"="eng-premier-league-",
              "Germany"="bundesliga-","Italy"="ita-serie-a-")
  # por defecto busca la jornada más grande, por lo que nos da el resultado final de liga.
  url<-paste0(cabeza,paises[which(names(paises)==pais)],temporada,"-",temporada+1,"/")
  
  liga <- readHTMLTable(url, header = TRUE)[[4]] 
  colnames(liga)<-c("Posic","V2","Equipo","P","V","E","D","Goles","DG","Puntos")
  cat("El equipo ganador de la liga",temporada,"-",temporada+1,"para",pais,"es:\n")
  return(liga[1,c("Equipo","Puntos")])
}

# Ejemplos

ligas("Spain",2013)
ligas("Spain",2016)

ligas("England",2012)

