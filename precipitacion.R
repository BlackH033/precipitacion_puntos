

# info --------------------------------------------------------------------

#          _,.-------.,_
#      ,;~'             '~;,
#    ,;                     ;,
#   ;                         ;
#  ,'                          ',
# ,;                           ;,
# ; ;      .           .      ; ;
# | ;   ______       ______   ; |   ~ Victor Rios~ ~ Black H 
# |  `/~"     ~" . "~     "~\'  |   ~ Estudiante Ing. Forestal UNALMED
# |  ~  ,-~~~^~, | ,~^~~~-,  ~  |   ~ Estudiante Desarrollo de software
#  |   |        }:{        |   |     
#  |   l       / | \       !   |    GitHub: https://github.com/BlackH033
#  .~  (__,.--" .^. "--.,__)  ~.    
#  |     ---;' / | \ `;---     |    
#   \__.       \/^\/       .__/           Contacto
#    V| \                 / |V      - vdriosf@unal.edu.co
#     | |T~\___!___!___/~T| |       - 3225920497
#     | |`IIII_I_I_I_IIII'| |
#     |  \,III I I I III,/  |
#      \   `~~~~~~~~~~'    /
#        \   .       .   /     
#          \.    ^    ./
#            ^~~~^~~~^


# descargas --------------------------------------------------------

#actualizar R ultima versión 4.1.3

#http://mirror.fcaglp.unlp.edu.ar/CRAN/ 
#descargar el  base


#descargar r tools

#https://cran.r-project.org/bin/windows/Rtools/rtools40.html
#descargar instalador (.exe) 32bits o 64bits según su pc


# configurar Rtools -------------------------------------------------------

#ejecutar siguiente linea:
#write('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', file = "~/.Renviron", append = TRUE)


# paquetes ----------------------------------------------------------------

#instalar si no ha instalado
install.packages('raster')
install.packages('rgdal')
install.packages('sp')
install.packages('xlsx')
install.packages("svDialogs") 
           


library(sp)
library(raster)

library(xlsx)
library(svDialogs)


# carpeta con imagenes ----------------------------------------------------
setwd("D:/UN/Hidrología/precipitación") #ruta de la carpeta que contiene solo SOLO las imagenes .tiff
(imagenes<-stack(dir()))


# base de datos con los puntos --------------------------------------------
setwd("D:/UN/Hidrología") #carpeta donde está guardado este archivo (.R)
(puntos<-read.csv2("puntos.csv")) #tabla en csv con 3 campos (altura,coordenada X, coordenada Y) con los 4 datos


# funcion para generar tabla --------------------------

#función recibe la base de datos por punto y la organiza por año

valores<-function(x){
  preci<-array(dim = 12)
  inicio<-1
  fin<-12
  datos<-data.frame(mes=c("enero", "febrero", "marzo", "abril", "mayo", "junio",
                          "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"))
  z<-0
  for (i in seq(1,4)){
    for (i in seq(inicio,fin)){
      preci[[i-(12*z)]]<-x[[i]]
    }
    datos<-cbind(datos,año=preci)
    preci<-array(dim = 12)
    inicio<-inicio+12
    fin<-fin+12
    z<-z+1
  }
  colnames(datos)<-c("mes","2015","2016","2017","2018")
  
  #promedio por mes
  prom<-array(dim = 12)
  for (i in 1:12){
    prom[[i]]<-rowMeans(datos[i,2:5])
  }
  datos$promedio<-prom
  datos<-rbind(datos,c(0,apply(datos[,2:6], 2,sum)))
  datos[13,1]<-"TOTAL"
  return(datos)
}


# extraccción de los 4 puntos ---------------------------------------------

for (i in 1:4){
  print("-------------------------------------------")
  print(paste("generando tabla para ",as.character(puntos$punto[i]),"msnm",sep = ""))
  base<-extract(imagenes,data.frame(x=puntos$x[i],y=puntos$y[i]))
  nombre<-paste("precipitacion_",as.character(puntos$punto[i]),".xlsx",sep = "")
  write.xlsx(valores(base),file = nombre)
  print(valores(base))
  print("se ha generado con exito la tabla de precipitación :D")
  if (i==4){
    dlg_message(c("Proceso finalizado con exito. \n",paste("\n las tablas se han guardado en la carpeta: ",getwd())),type = 'ok')$res
    
  }
}



# -------------------------------------------------------------------------

#~ Victor Rios~ ~ Black H ~ 
