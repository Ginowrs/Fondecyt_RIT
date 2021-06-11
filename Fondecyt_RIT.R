#FONDECYT RIT

# Cargar paquetes --------------------------------------------------------------
pacman::p_load(readxl,tidyverse,survey, srvyr,writexl,sjPlot)

# Options ----------------------------------------------------------------------
library(survey);  options(survey.lonely.psu = "certainty")

#Cargar Bases
sindicatos<-read_excel("bbdd-sindicatos-bp.xlsx")
trabajadores<-read_excel("bbdd-trabajadores-bp.xlsx")
names(sindicatos)
names(trabajadores)


#Selecci?n de variables
##Agregar g11 para BBDD_Sindicato
BBDD_Sindicatos<-dplyr::select(sindicatos,b4,d7_1,d7_2,d7_4,d7_6,d7_7,d7_8,d7_10,d7_11,d7_12,d7_13,d7_14,d7_15,e4_1,e4_2,e4_3,e5_1,e5_2,e5_3,e5_4,e5_5,e5_6,f1,f2,f3_1,f3_2,f3_3,f3_4,f3_5,f3_77,f3_77_otro_c,f4_1,f4_2,f4_3,f4_4,f4_5,f4_6,f5_1,f5_2,f5_3,f5_4,f5_5,f5_6,f5_7,f5_8,f8_1,f8_2,f8_3,f8_5,f8_6,g1,g2_1,g2_2,g2_3,g7,g9_1,g9_2,g9_3,g9_4,g9_5,g9_77,g10_1,g10_2,g10_3,g10_4,g10_5,g10_77,g11,g14_1,g14_2,g14_3,g14_4,g14_5,g14_77,g16,g17_1,g17_2,g17_3,g17_4,g17_5,g17_6,g17_7,g17_8,g17_9,g17_10,g17_77,g24,g25,h1_1,h1_2,h1_4,h1_5,h1_6,h1_7,h1_77,h2_1,h2_2,h2_3,h2_4,h2_5,h2_6,h2_7,h2_8,h2_9,h2_77,h3_1,h3_2,h3_3,h3_4,h3_5,h3_6,h3_7,h3_8,h3_9,h3_10,h3_77,h4,i1,i2,i3,i4,i6,i10,i11,i17,i21,nt,densidad_sindical,agrupacion_actividad,tamano,agrupacion_regional,estratos,fe_empresa,empresa_feminizada)
BBDD_Trabajadores<-dplyr::select(trabajadores,b2,d7_1,d7_2,d7_4,d7_6,d7_7,d7_8,d7_10,d7_11,d7_12,d7_13,d7_14,d7_15,e4_1,e4_2,e4_3,e5_1,e5_2,e5_3,e5_4,e5_5,e5_6,f1,f2,f3_1,f3_2,f3_3,f3_4,f3_5,f3_77,f3_77_otro_c,f4_1,f4_2,f4_3,f4_4,f4_5,f4_6,f5_1,f5_2,f5_3,f5_4,f5_5,f5_6,f5_7,f5_8,f7_1,f7_2,f7_3,f7_4,f7_5,g1,h1_1,h1_2,h1_3,h1_4,h1_5,h1_6,h1_77,h2_1,h2_2,h2_3,h2_4,h2_5,h2_6,h2_7,h2_77,h3_1,h3_2,h3_3,h3_4,h3_5,h3_6,h3_7,h3_8,h3_9,h3_10,h3_77,h4,i1,i2,i3,i4,i6,nt,sindicatos_presencia,agrupacion_actividad,tamano,agrupacion_regional,estratos,fe_empresa,empresa_feminizada)

1172+2498

##no se pueden homologar i3 (diferencia entre sindicato y grupo negociador), se deber?a crear una variable de grupo negociador para la BBDD_Trabajadores 
BBDD_Sindicatos<- BBDD_Sindicatos %>% rename("i3_sindicato"=i3)
BBDD_Trabajadores<- BBDD_Trabajadores %>% rename("i3_gn"=i3)

#No se puede homologar g1 (en BBDD_Sindicato se pregunta por e tipo de sindicato y en BBDD_Trabjadores se pregunta si es que hubo un sindicato anteriormente)
BBDD_Sindicatos<- BBDD_Sindicatos %>% rename("tipo_sindicato"=g1)
BBDD_Trabajadores<- BBDD_Trabajadores %>% rename("Sin_Ant"=g1)

#Homologar nombres de variables de trabajadores con sindicatos
BBDD_Trabajadores<- BBDD_Trabajadores %>% rename("b4"=b2,
                                                 "f8_1"=f7_1, 
                                                 "f8_2"=f7_2 ,
                                                 "f8_3"=f7_3,
                                                 "f8_5"=f7_4,
                                                 "f8_6"=f7_5,
                                                 "i11"=i6) 

#Crear la variable "sindicatos_presencia" en la BBDD_Sindicatos 
BBDD_Sindicatos$sindicatos_presencia<-1
table(BBDD_Trabajadores$sindicatos_presencia)

#Crear variables dependientes
#Incidencia de conflicto en la BBDD_Sindicatos
BBDD_Sindicatos$h1_legal<-ifelse(BBDD_Sindicatos$h1_1=="1"|BBDD_Sindicatos$h1_2=="1","1",
                                 ifelse(BBDD_Sindicatos$h1_1=="2"&BBDD_Sindicatos$h1_2=="2","2",
                                        NA))
BBDD_Sindicatos$h1_defacto<-ifelse(BBDD_Sindicatos$h1_4=="1"|BBDD_Sindicatos$h1_5=="1"|BBDD_Sindicatos$h1_5=="1"|BBDD_Sindicatos$h1_6=="1"|BBDD_Sindicatos$h1_7=="1","1",
                                   ifelse(BBDD_Sindicatos$h1_4=="2"&BBDD_Sindicatos$h1_5=="2"&BBDD_Sindicatos$h1_5=="2"&BBDD_Sindicatos$h1_6=="2"&BBDD_Sindicatos$h1_7=="2","2",
                                          NA))

prop.table(table(BBDD_Sindicatos$h1_legal))     
479+686 #1165
prop.table(table(BBDD_Sindicatos$h1_defacto)) 
146+1023 # 1169

#Incidencia de conflicto en la BBDD_Trabajadores
BBDD_Trabajadores$h1_legal<-ifelse(BBDD_Trabajadores$h1_1=="1"|BBDD_Trabajadores$h1_2=="1","1",
                                   ifelse(BBDD_Trabajadores$h1_1=="2"&BBDD_Trabajadores$h1_2=="2","2",
                                          NA))

BBDD_Trabajadores$h1_defacto<-ifelse(BBDD_Trabajadores$h1_3=="1"|BBDD_Trabajadores$h1_4=="1"|BBDD_Trabajadores$h1_5=="1"|BBDD_Trabajadores$h1_6=="1","1",
                                     ifelse(BBDD_Trabajadores$h1_4=="2"&BBDD_Trabajadores$h1_5=="2"&BBDD_Trabajadores$h1_5=="2"&BBDD_Trabajadores$h1_6=="2","2",
                                            NA))

prop.table(table(BBDD_Trabajadores$h1_legal)) 
204+2233 #2437
prop.table(table(BBDD_Trabajadores$h1_defacto))
60+2432  #2492
names(BBDD_Sindicatos)
names(BBDD_Trabajadores)
#Descritptivos variables de inter?s

#N?mero de Sindicato
table(sindicatos$g11)
prop.table(table(BBDD_Sindicatos$h1_legal,BBDD_Sindicatos$g11),1)
prop.table(table(BBDD_Sindicatos$h1_defacto,BBDD_Sindicatos$g11),1)

#Grupos negociadores
table(BBDD_Trabajadores$i3_gn)
prop.table(table(BBDD_Trabajadores$h1_legal,BBDD_Trabajadores$i3_gn),2)
prop.table(table(BBDD_Trabajadores$h1_defacto,BBDD_Trabajadores$i3_gn),2)

#Fusionar Bases
BBDD_Fusion<-full_join(BBDD_Sindicatos,BBDD_Trabajadores)
names(BBDD_Fusion)


#Variables numericas a factor---------------------------------------------------
attach(BBDD_Fusion)
table(BBDD_Sindicatos$empresa_feminizada)
#Conflicto legal
BBDD_Fusion$h1_legal<-recode_factor(BBDD_Fusion$h1_legal,"1"="1","2"="0")
BBDD_Fusion$h1_legal<-factor(BBDD_Fusion$h1_legal,levels = c(0,1),labels = c("No existe","Existe"))
#Conflicto extralegal
BBDD_Fusion$h1_defacto<-recode_factor(h1_defacto,"1"="1","2"="0")
BBDD_Fusion$h1_defacto<-factor(BBDD_Fusion$h1_defacto,levels = c(0,1),labels = c("No existe","Existe"))
#tama?o
BBDD_Fusion$tamano<-factor(BBDD_Fusion$tamano,levels = c(1,2,3,4),labels = c("Grande","Mediana","Pequeña","Micro"))
#Sindicato
BBDD_Fusion$sindicatos_presencia<-recode_factor(BBDD_Fusion$sindicatos_presencia,"1"="1","2"="0")
BBDD_Fusion$sindicatos_presencia<-factor(BBDD_Fusion$sindicatos_presencia,levels = c(0,1),labels = c("Sin sindicato","Con sindicato"))  
#Grupo negociador
BBDD_Fusion$i3_gn<-recode_factor(BBDD_Fusion$i3_gn,"1"="1","2"="0","88"="88")
BBDD_Fusion$i3_gn[BBDD_Fusion$i3_gn=="88"]<-NA
BBDD_Fusion$i3_gn<-factor(BBDD_Fusion$i3_gn,levels = c(0,1),labels = c("No existe","Existe")) 
#M?s de un sindicato

BBDD_Fusion$g11<-factor(BBDD_Fusion$g11,levels = c(1,2),labels = c("1 Sindicato","M?s de 1 Sindicato"))

#agrupaci?n regional
BBDD_Fusion$agrupacion_regional<-factor(BBDD_Fusion$agrupacion_regional,levels = c(1,2,3,4,5,6),labels = c("Norte Grande","Norte Chico","Zona Centro","Centro Sur","Sur","Zona Austral"))

#agrupaci?n actividad econ?mica
BBDD_Fusion$agrupacion_actividad<-factor(BBDD_Fusion$agrupacion_actividad,levels = c(1:13),labels = c("Agricultura, ganader?a y pesca",
                                                                              "Miner?a",
                                                                              "Industria Manufacturera",
                                                                              "Suministro,electricidad,agua,etc...",
                                                                              "Construcci?n",
                                                                              "Comercio",
                                                                              "Transporte y comunicaciones",
                                                                              "Actividades de alojamiento y de servicio de comidas",
                                                                              "Actividades financieras e inmobiliarias",
                                                                              "Actividades profesionales y de servicios administrativos",
                                                                              "Ense?anza",
                                                                              "Salud",
                                                                              "otras actividades art?sticas, entretenimiento"))
#Empresa feminizada
BBDD_Fusion$empresa_feminizada<-recode_factor(BBDD_Fusion$empresa_feminizada,"1"="1","2"="0")
BBDD_Fusion$empresa_feminizada<-factor(BBDD_Fusion$empresa_feminizada,levels = c(0,1),labels = c("No feminizada","Feminizada"))
#Condiciones ambientales y de trabajo

BBDD_Fusion$d7_1<-recode_factor(BBDD_Fusion$d7_1,"1"="1","2"="2","3"="3","85"="9","88"="9","96"="9")
BBDD_Fusion$d7_1<-factor(BBDD_Fusion$d7_1,levels = c(1,2,3),labels = c("Buena","Regular","Mala"))
BBDD_Fusion$d7_1[BBDD_Fusion$d7_1=="9"]<-NA

BBDD_Fusion$d7_2<-recode_factor(BBDD_Fusion$d7_2,"1"="1","2"="2","3"="3","85"="9","88"="9","96"="9")
BBDD_Fusion$d7_2<-factor(BBDD_Fusion$d7_2,levels = c(1,2,3),labels = c("Buena","Regular","Mala"))
BBDD_Fusion$d7_2[BBDD_Fusion$d7_2=="9"]<-NA

BBDD_Fusion$d7_4<-recode_factor(BBDD_Fusion$d7_4,"1"="1","2"="2","3"="3","85"="9","88"="9","96"="9")
BBDD_Fusion$d7_4<-factor(BBDD_Fusion$d7_4,levels = c(1,2,3),labels = c("Buena","Regular","Mala"))
BBDD_Fusion$d7_4[BBDD_Fusion$d7_4=="9"]<-NA

BBDD_Fusion$d7_6<-recode_factor(BBDD_Fusion$d7_6,"1"="1","2"="2","3"="3","85"="9","88"="9","96"="9")
BBDD_Fusion$d7_6<-factor(BBDD_Fusion$d7_6,levels = c(1,2,3),labels = c("Buena","Regular","Mala"))
BBDD_Fusion$d7_6[BBDD_Fusion$d7_6=="9"]<-NA

BBDD_Fusion$d7_7<-recode_factor(BBDD_Fusion$d7_7,"1"="1","2"="2","3"="3","85"="9","88"="9","96"="9")
BBDD_Fusion$d7_7<-factor(BBDD_Fusion$d7_7,levels = c(1,2,3),labels = c("Buena","Regular","Mala"))
BBDD_Fusion$d7_7[BBDD_Fusion$d7_7=="9"]<-NA

BBDD_Fusion$d7_8<-recode_factor(BBDD_Fusion$d7_8,"1"="1","2"="2","3"="3","85"="9","88"="9","96"="9")
BBDD_Fusion$d7_8<-factor(BBDD_Fusion$d7_8,levels = c(1,2,3),labels = c("Buena","Regular","Mala"))
BBDD_Fusion$d7_8[BBDD_Fusion$d7_8=="9"]<-NA

BBDD_Fusion$d7_10<-recode_factor(BBDD_Fusion$d7_10,"1"="1","2"="2","3"="3","85"="9","88"="9","96"="9")
BBDD_Fusion$d7_10<-factor(BBDD_Fusion$d7_10,levels = c(1,2,3),labels = c("Buena","Regular","Mala"))
BBDD_Fusion$d7_10[BBDD_Fusion$d7_10=="9"]<-NA

BBDD_Fusion$d7_11<-recode_factor(BBDD_Fusion$d7_11,"1"="1","2"="2","3"="3","85"="9","88"="9","96"="9")
BBDD_Fusion$d7_11<-factor(BBDD_Fusion$d7_11,levels = c(1,2,3),labels = c("Buena","Regular","Mala"))
BBDD_Fusion$d7_11[BBDD_Fusion$d7_11=="9"]<-NA

BBDD_Fusion$d7_12<-recode_factor(BBDD_Fusion$d7_12,"1"="1","2"="2","3"="3","85"="9","88"="9","96"="9")
BBDD_Fusion$d7_12<-factor(BBDD_Fusion$d7_12,levels = c(1,2,3),labels = c("Buena","Regular","Mala"))
BBDD_Fusion$d7_12[BBDD_Fusion$d7_12=="9"]<-NA

BBDD_Fusion$d7_13<-recode_factor(BBDD_Fusion$d7_13,"1"="1","2"="2","3"="3","85"="9","88"="9","96"="9")
BBDD_Fusion$d7_13<-factor(BBDD_Fusion$d7_13,levels = c(1,2,3),labels = c("Buena","Regular","Mala"))
BBDD_Fusion$d7_13[BBDD_Fusion$d7_13=="9"]<-NA

BBDD_Fusion$d7_14<-recode_factor(BBDD_Fusion$d7_14,"1"="1","2"="2","3"="3","85"="9","88"="9","96"="9")
BBDD_Fusion$d7_14<-factor(BBDD_Fusion$d7_14,levels = c(1,2,3),labels = c("Buena","Regular","Mala"))
BBDD_Fusion$d7_14[BBDD_Fusion$d7_14=="9"]<-NA

BBDD_Fusion$d7_15<-recode_factor(BBDD_Fusion$d7_15,"1"="1","2"="2","3"="3","85"="9","88"="9","96"="9")
BBDD_Fusion$d7_15<-factor(BBDD_Fusion$d7_15,levels = c(1,2,3),labels = c("Buena","Regular","Mala"))
BBDD_Fusion$d7_15[BBDD_Fusion$d7_15=="9"]<-NA

#Disposici?n de la empresa para informar a trabajadores
BBDD_Fusion$f1<-recode_factor(BBDD_Fusion$f1,"1"="1","2"="2","3"="3","88"="9","96"="9")
BBDD_Fusion$f1<-factor(BBDD_Fusion$f1,levels = c(1,2,3),labels = c("Buena","Regular","Mala"))
BBDD_Fusion$f1[BBDD_Fusion$f1=="9"]<-NA

#Actitud de la empresa para recibir sugerencias y opiniones de les trabajadores
BBDD_Fusion$f2<-recode_factor(BBDD_Fusion$f2,"1"="1","2"="2","3"="3","88"="9")
BBDD_Fusion$f2<-factor(BBDD_Fusion$f2,levels = c(1,2,3),labels = c("Buena","Regular","Mala"))
BBDD_Fusion$f2[BBDD_Fusion$f2=="9"]<-NA

#En esta empresa, ?existe alguno de los siguientes mecanismos de participaci?n para el personal?:Delegados o delegadas
BBDD_Fusion$f3_1<-recode_factor(BBDD_Fusion$f3_1,"1"="1","2"="2","88"="9","96"="9")
BBDD_Fusion$f3_1<-factor(BBDD_Fusion$f3_1, levels = c(1,2),labels = c("S?","No"))
BBDD_Fusion$f3_1[BBDD_Fusion$f3_1=="9"]<-NA
#En esta empresa, ?existe alguno de los siguientes mecanismos de participaci?n para el personal?:Comit?s de cultura o recreaci?n
BBDD_Fusion$f3_2<-recode_factor(BBDD_Fusion$f3_2,"1"="1","2"="2","88"="9","96"="9","99"="9")
BBDD_Fusion$f3_2<-factor(BBDD_Fusion$f3_2, levels = c(1,2),labels = c("S?","No"))
BBDD_Fusion$f3_2[BBDD_Fusion$f3_2=="9"]<-NA
#En esta empresa, ?existe alguno de los siguientes mecanismos de participaci?n para el personal?:Comit?s de bienestar
BBDD_Fusion$f3_3<-recode_factor(BBDD_Fusion$f3_3,"1"="1","2"="2","88"="9","96"="9","99"="9")
BBDD_Fusion$f3_3<-factor(BBDD_Fusion$f3_3, levels = c(1,2),labels = c("S?","No"))
BBDD_Fusion$f3_3[BBDD_Fusion$f3_3=="9"]<-NA
#En esta empresa, ?existe alguno de los siguientes mecanismos de participaci?n para el personal?:Comit? Bipartito de capacitaci?n
BBDD_Fusion$f3_4<-recode_factor(BBDD_Fusion$f3_4,"1"="1","2"="2","88"="9","96"="9")
BBDD_Fusion$f3_4<-factor(BBDD_Fusion$f3_4, levels = c(1,2),labels = c("S?","No"))
BBDD_Fusion$f3_4[BBDD_Fusion$f3_4=="9"]<-NA
#En esta empresa, ?existe alguno de los siguientes mecanismos de participaci?n para el personal?:Grupos paritarios diferentes de los de higiene y seguridad
BBDD_Fusion$f3_5<-recode_factor(BBDD_Fusion$f3_5,"1"="1","2"="2","88"="9","96"="9")
BBDD_Fusion$f3_5<-factor(BBDD_Fusion$f3_5, levels = c(1,2),labels = c("S?","No"))
BBDD_Fusion$f3_5[BBDD_Fusion$f3_5=="9"]<-NA
#En esta empresa, ?existe alguno de los siguientes mecanismos de participaci?n para el personal?: Otro
BBDD_Fusion$f3_77<-recode_factor(BBDD_Fusion$f3_77,"1"="1","2"="2","88"="9","96"="9")
BBDD_Fusion$f3_77<-factor(BBDD_Fusion$f3_77, levels = c(1,2),labels = c("S?","No"))
BBDD_Fusion$f3_77[BBDD_Fusion$f3_77=="9"]<-NA

#Clima Laboral:los sueldos tienen coherencia con las responsabilidades de cada cargo
BBDD_Fusion$f8_1<-recode_factor(BBDD_Fusion$f8_1,"1"="1","2"="2","3"="3","4"="4","5"="5","88"="9","96"="9","99"="9")
BBDD_Fusion$f8_1<-factor(BBDD_Fusion$f8_1,levels =c(1,2,3,4,5) ,labels = c("Muy de acuerdo","De acuerdo", "Ni acuerdo ni desacuerdo","En desacuerdo","Muy en desacuerdo"))
BBDD_Fusion$f8_1[BBDD_Fusion$f8_1=="9"]<-NA
#Clima Laboral:Si se realiza bien el trabajo, se tiene la seguridad de mantener el empleo en la empresa
BBDD_Fusion$f8_2<-recode_factor(BBDD_Fusion$f8_2,"1"="1","2"="2","3"="3","4"="4","5"="5","88"="9","96"="9","99"="9")
BBDD_Fusion$f8_2<-factor(BBDD_Fusion$f8_2,levels =c(1,2,3,4,5) ,labels = c("Muy de acuerdo","De acuerdo", "Ni acuerdo ni desacuerdo","En desacuerdo","Muy en desacuerdo"))
BBDD_Fusion$f8_2[BBDD_Fusion$f8_2=="9"]<-NA

#Clima Laboral:la relaci?n entre las y los trabajadores y superiores es buena
BBDD_Fusion$f8_3<-recode_factor(BBDD_Fusion$f8_3,"1"="1","2"="2","3"="3","4"="4","5"="5","88"="9","96"="9","99"="9")
BBDD_Fusion$f8_3<-factor(BBDD_Fusion$f8_3,levels =c(1,2,3,4,5) ,labels = c("Muy de acuerdo","De acuerdo", "Ni acuerdo ni desacuerdo","En desacuerdo","Muy en desacuerdo"))
BBDD_Fusion$f8_3[BBDD_Fusion$f8_3=="9"]<-NA

#Clima Laboral:Frecuentemente las jefaturas tratan con malas palabras a las y los trabajadores
BBDD_Fusion$f8_5<-recode_factor(BBDD_Fusion$f8_5,"1"="1","2"="2","3"="3","4"="4","5"="5","88"="9","96"="9","99"="9")
BBDD_Fusion$f8_5<-factor(BBDD_Fusion$f8_5,levels =c(1,2,3,4,5) ,labels = c("Muy de acuerdo","De acuerdo", "Ni acuerdo ni desacuerdo","En desacuerdo","Muy en desacuerdo"))
BBDD_Fusion$f8_5[BBDD_Fusion$f8_5=="9"]<-NA

#Clima Laboral:En esta empresa es com?n que se despidan trabajadores en forma arbitraria
BBDD_Fusion$f8_6<-recode_factor(BBDD_Fusion$f8_6,"1"="1","2"="2","3"="3","4"="4","5"="5","88"="9","96"="9","99"="9")
BBDD_Fusion$f8_6<-factor(BBDD_Fusion$f8_6,levels =c(1,2,3,4,5) ,labels = c("Muy de acuerdo","De acuerdo", "Ni acuerdo ni desacuerdo","En desacuerdo","Muy en desacuerdo"))
BBDD_Fusion$f8_6[BBDD_Fusion$f8_6=="9"]<-NA

#El reajuste  obtenido por los trabajadores  durante 2018, fue logrado:
BBDD_Fusion$b4<-recode_factor(BBDD_Fusion$b4,"1"="1","2"="2","88"="9","96"="9","99"="9")
BBDD_Fusion$b4<-factor(BBDD_Fusion$b4,levels =c(1,2) ,labels = c("Mediante negociacion colectiva","Sin negociacion colectiva"))
BBDD_Fusion$b4[BBDD_Fusion$b4=="9"]<-NA

#Tipo de sindicato
BBDD_Fusion$tipo_sindicato<-factor(BBDD_Fusion$tipo_sindicato, levels = c(1,2), labels = c("Sindicato de empresa", "Sindicato de establecimiento"))

#¿Hubo sindicato anteriormente en esta empresa?
BBDD_Fusion$Sin_Ant<-recode_factor(BBDD_Fusion$Sin_Ant,"1"="1","2"="2","85"="9","88"="9")
BBDD_Fusion$Sin_Ant<-factor(BBDD_Fusion$Sin_Ant, levels = c(1,2), labels = c("Sí", "No"))
BBDD_Fusion$Sin_Ant[BBDD_Fusion$Sin_Ant=="9"]<-NA

#¿su sindicato contó con el apoyo de esta empresa para alguna de sus actividades?
BBDD_Fusion$g7<-recode_factor(BBDD_Fusion$g7,"1"="1","2"="2","85"="9","96"="9")
BBDD_Fusion$g7<-factor(BBDD_Fusion$g7, levels = c(1,2), labels = c("Sí", "No"))
BBDD_Fusion$g7[BBDD_Fusion$g7=="9"]<-NA


#Defensa de los trabajadores ¿qué acciones realizó su sindicato durante el año 2018?:Interceder ante el empleador por un trabajador o trabajadora en particular
BBDD_Fusion$g9_1<-recode_factor(BBDD_Fusion$g9_1,"1"="1","2"="2","88"="9","96"="9","99"="9")
BBDD_Fusion$g9_1<-factor(BBDD_Fusion$g9_1, levels = c(1,2), labels = c("Sí", "No"))
BBDD_Fusion$g9_1[BBDD_Fusion$g9_1=="9"]<-NA

#Defensa de los trabajadores ¿qué acciones realizó su sindicato durante el año 2018?:Exigir que la empresa cumpla alguna cláusula del instrumento colectivo
BBDD_Fusion$g9_2<-recode_factor(BBDD_Fusion$g9_2,"1"="1","2"="2","88"="9","96"="9","99"="9")
BBDD_Fusion$g9_2<-factor(BBDD_Fusion$g9_2, levels = c(1,2), labels = c("Sí", "No"))
BBDD_Fusion$g9_2[BBDD_Fusion$g9_2=="9"]<-NA  

#Defensa de los trabajadores ¿qué acciones realizó su sindicato durante el año 2018?:Denunciar una vulneración ante la Inspección del Trabajo
BBDD_Fusion$g9_3<-recode_factor(BBDD_Fusion$g9_3,"1"="1","2"="2","88"="9","96"="9","99"="9")
BBDD_Fusion$g9_3<-factor(BBDD_Fusion$g9_3, levels = c(1,2), labels = c("Sí", "No"))
BBDD_Fusion$g9_3[BBDD_Fusion$g9_3=="9"]<-NA  

#Defensa de los trabajadores ¿qué acciones realizó su sindicato durante el año 2018?:Denunciar una vulneración en Tribunales
BBDD_Fusion$g9_4<-recode_factor(BBDD_Fusion$g9_4,"1"="1","2"="2","88"="9","96"="9","99"="9")
BBDD_Fusion$g9_4<-factor(BBDD_Fusion$g9_4, levels = c(1,2), labels = c("Sí", "No"))
BBDD_Fusion$g9_4[BBDD_Fusion$g9_4=="9"]<-NA 

#Defensa de los trabajadores ¿qué acciones realizó su sindicato durante el año 2018?:Denunciar una vulneración ante la opinión pública
BBDD_Fusion$g9_5<-recode_factor(BBDD_Fusion$g9_5,"1"="1","2"="2","88"="9","96"="9","99"="9")
BBDD_Fusion$g9_5<-factor(BBDD_Fusion$g9_5, levels = c(1,2), labels = c("Sí", "No"))
BBDD_Fusion$g9_5[BBDD_Fusion$g9_5=="9"]<-NA

#Participación política y social¿qué acciones realizó su sindicato durante el año 2018?:Participación en actividades orgánicas de movimientos sociales
BBDD_Fusion$g10_1<-recode_factor(BBDD_Fusion$g10_1,"1"="1","2"="2","88"="9","96"="9","99"="9")
BBDD_Fusion$g10_1<-factor(BBDD_Fusion$g10_1, levels = c(1,2), labels = c("Sí", "No"))
BBDD_Fusion$g10_1[BBDD_Fusion$g10_1=="9"]<-NA

#Participación política y social¿qué acciones realizó su sindicato durante el año 2018?:Participación en protestas, concentraciones o marchas de movimientos sociales
BBDD_Fusion$g10_2<-recode_factor(BBDD_Fusion$g10_2,"1"="1","2"="2","88"="9","96"="9","99"="9")
BBDD_Fusion$g10_2<-factor(BBDD_Fusion$g10_2, levels = c(1,2), labels = c("Sí", "No"))
BBDD_Fusion$g10_2[BBDD_Fusion$g10_2=="9"]<-NA

#Participación política y social¿qué acciones realizó su sindicato durante el año 2018?:Participación en mesas de trabajo o reuniones con autoridades del nivel central (diputados, senadores,...
BBDD_Fusion$g10_3<-recode_factor(BBDD_Fusion$g10_3,"1"="1","2"="2","88"="9","96"="9","99"="9")
BBDD_Fusion$g10_3<-factor(BBDD_Fusion$g10_3, levels = c(1,2), labels = c("Sí", "No"))
BBDD_Fusion$g10_3[BBDD_Fusion$g10_3=="9"]<-NA

#Participación política y social¿qué acciones realizó su sindicato durante el año 2018?:Participación en mesas de trabajo o reuniones con autoridades del nivel local (alcaldes, concejales,...
BBDD_Fusion$g10_4<-recode_factor(BBDD_Fusion$g10_4,"1"="1","2"="2","88"="9","96"="9","99"="9")
BBDD_Fusion$g10_4<-factor(BBDD_Fusion$g10_4, levels = c(1,2), labels = c("Sí", "No"))
BBDD_Fusion$g10_4[BBDD_Fusion$g10_4=="9"]<-NA

#Participación política y social¿qué acciones realizó su sindicato durante el año 2018?:Presionar a las autoridades del nivel central y/o local del Estado a través de la movilización de los socios del sindicato
BBDD_Fusion$g10_5<-recode_factor(BBDD_Fusion$g10_5,"1"="1","2"="2","88"="9","96"="9","99"="9")
BBDD_Fusion$g10_5<-factor(BBDD_Fusion$g10_5, levels = c(1,2), labels = c("Sí", "No"))
BBDD_Fusion$g10_5[BBDD_Fusion$g10_5=="9"]<-NA

#Actualmente, ¿su sindicato está afiliado a una central sindical?
BBDD_Fusion$g24<-factor(BBDD_Fusion$g24, levels = c(1,2), labels = c("Sí", "No"))

#¿A qué central sindical está afiliado su sindicato?
BBDD_Fusion$g25<-recode_factor(BBDD_Fusion$g25,"1"="1","2"="2","85"="3","88"="9","96"="9")
BBDD_Fusion$g25<-factor(BBDD_Fusion$g25, levels = c(1,2,3), labels = c("CUT", "CAT", "UNT"))
BBDD_Fusion$g25[BBDD_Fusion$g25=="9"]<-NA

#Durante el año 2018, ¿qué motivó el(los) conflicto(s) en esta empresa?:Problemas de trato por parte de los superiores




#¿qué medidas adoptó la empresa para enfrentar la(s) expresión(es) de conflicto





#BASE PONDERADA-----------------------------------------------------------------
Fusion_pond<-svydesign(data =BBDD_Fusion ,id=~1, weights= ~fe_empresa, strata = ~estratos)
summary(Fusion_pond)

#Cruces
#conflicto legal según presencia de sindicato
CRUCE_1<-svytable(~h1_legal+sindicatos_presencia,design=Fusion_pond)
summary(CRUCE_1,statistic="Chisq")
CRUCE_1<-addmargins(prop.table(CRUCE_1,2))
CRUCE_1<-as.data.frame(CRUCE_1,optional=FALSE,col.names = names(sindicatos_presencia),stringsAsFactors = default.stringsAsFactors(),make.names())
rm(CRUCE_1)
?as.data.frame
?addmargins
#Conflicto legal según presencia de grupo negociador
CRUCE_2<-svytable(~h1_legal+i3_gn,design=Fusion_pond)
summary(CRUCE_2,statistic="Chisq")
CRUCE_2<-addmargins(prop.table(CRUCE_2,2))
CRUCE_2<-as.data.frame(CRUCE_2)
#Conflicto legal según N° de sindicato
CRUCE_3<-svytable(~h1_legal+g11,design=Fusion_pond)
summary(CRUCE_3,statistic="Chisq")
CRUCE_3<-addmargins(prop.table(CRUCE_3,2))
CRUCE_3<-as.data.frame(CRUCE_3)
#Conflicto legal según tamaño
CRUCE_4<-svytable(~h1_legal+tamano,design=Fusion_pond)
summary(CRUCE_4,statistic="Chisq")
CRUCE_4<-addmargins(prop.table(CRUCE_4,2))
CRUCE_4<-as.data.frame(CRUCE_4)
#Conflicto legal según agrupación regional
CRUCE_5<-svytable(~h1_legal+agrupacion_regional,design=Fusion_pond)
summary(CRUCE_5,statistic="Chisq")
CRUCE_5<-addmargins(prop.table(CRUCE_5,2))
CRUCE_5<-as.data.frame(CRUCE_5)
#Conflicto legal según agrupación actividad
CRUCE_6<-svytable(~h1_legal+agrupacion_actividad,design=Fusion_pond)
summary(CRUCE_6,statistic="Chisq")
CRUCE_6<-addmargins(prop.table(CRUCE_6,2))
CRUCE_6<-as.data.frame(CRUCE_6)
#Conflicto legal según empresa feminizada
CRUCE_7<-svytable(~h1_legal+empresa_feminizada,design=Fusion_pond)
summary(CRUCE_7,statistic="Chisq")
CRUCE_7<-addmargins(prop.table(CRUCE_7,2))
CRUCE_7<-as.data.frame(CRUCE_7)


#conflicto extralegal según presencia de sindicato
CRUCE_8<-svytable(~h1_defacto+sindicatos_presencia,design=Fusion_pond)
summary(CRUCE_8,statistic="Chisq")
CRUCE_8<-addmargins(prop.table(CRUCE_8,2))
CRUCE_8<-as.data.frame(CRUCE_8)

#Conflicto extralegal según presencia de grupo negociador
CRUCE_9<-svytable(~h1_defacto+i3_gn,design=Fusion_pond)
summary(CRUCE_9,statistic="Chisq")
CRUCE_9<-addmargins(prop.table(CRUCE_9,2))
CRUCE_9<-as.data.frame(CRUCE_9)
#Conflicto extralegal según N° de sindicato
CRUCE_10<-svytable(~h1_defacto+g11,design=Fusion_pond)
summary(CRUCE_10,statistic="Chisq")
CRUCE_10<-addmargins(prop.table(CRUCE_10,2))
CRUCE_10<-as.data.frame(CRUCE_10)
#Conflicto extralegal según tamaño
CRUCE_11<-svytable(~h1_defacto+tamano,design=Fusion_pond)
summary(CRUCE_11,statistic="Chisq")
CRUCE_11<-addmargins(prop.table(CRUCE_11,2))
CRUCE_11<-as.data.frame(CRUCE_11)
#Conflicto extralegal según agrupación regional
CRUCE_12<-svytable(~h1_defacto+agrupacion_regional,design=Fusion_pond)
summary(CRUCE_12,statistic="Chisq")
CRUCE_12<-addmargins(prop.table(CRUCE_12,2))
CRUCE_12<-as.data.frame(CRUCE_12)
#Conflicto extralegal según agrupación actividad
CRUCE_13<-svytable(~h1_defacto+agrupacion_actividad,design=Fusion_pond)
summary(CRUCE_13,statistic="Chisq")
CRUCE_13<-addmargins(prop.table(CRUCE_13,2))
CRUCE_13<-as.data.frame(CRUCE_13)
#Conflicto extralegal según empresa feminizada
CRUCE_14<-svytable(~h1_defacto+empresa_feminizada,design=Fusion_pond)
summary(CRUCE_14,statistic="Chisq")
CRUCE_14<-addmargins(prop.table(CRUCE_14,2))
CRUCE_14<-as.data.frame(CRUCE_14)

rm(CRUCE_1,CRUCE_2,CRUCE_3,CRUCE_4,CRUCE_5,CRUCE_6,CRUCE_7,CRUCE_8,CRUCE_9,CRUCE_10,CRUCE_11,CRUCE_12,CRUCE_13,CRUCE_14)


writexl::write_xlsx(list(CRUCE_1,CRUCE_2,CRUCE_3,CRUCE_4,CRUCE_5,CRUCE_6,CRUCE_7,CRUCE_8,CRUCE_9,CRUCE_10,CRUCE_11,CRUCE_12,CRUCE_13,CRUCE_14), "cruces/crucesRL.xlsx", col_names = TRUE,format_headers = TRUE)

writexl::write_xlsx(list(CRUCE_1),"cruces/crucesRL.xlsx", col_names = TRUE,format_headers = TRUE)
str(CRUCE_1)
