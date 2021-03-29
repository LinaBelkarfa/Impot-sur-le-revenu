#10% moins de 1200*12 = 14 400
#20% moins de 1350*12 = 16 200
#30% moins de 1500*12 = 18 000
#40% moins de 1620*12 = 19 440
#50% moins de 1800*12 = 21 600
#60% moins de 2000*12 = 24 000
#70% moins de 2280*12 = 27 360
#80% moins de 2700*12 = 32 400
#90% moins de 3600*12 = 43 200
#95% moins de 4700*12 = 56 400
#99% moins de 8600*12 = 103200


#seq(from, to, by= )  
a<-seq(1440,14400,1440)  #On fait en sorte d'avoir 10 valeurs qui s'ajoute à chaque fois sur les 100 qu'on mettra au total 
b<-c(a,seq(14580,16200,180)) #ces nouvelles valeurs sont toujours supérieure aux anciennes et inférieures aux suivantes
c<-c(b,seq(16380,18000,180))  # car il y la la mention "moins de"
d<-c(c,seq(18001,19440,150))  # on adapte les pas en conséquence afin d'avoir toujours 10 valeurs
e<-c(d,seq(19591,21600,201))
f<-c(e,seq(21800,24000,244))
g<-c(f,seq(24240,27360,346))
h<-c(g,seq(27700,32400,522))
i<-c(h,seq(32900,43200,1144))  
j<-c(i,seq(44000,56400,3100))  # ici on ne prend que 5 valeurs sur 100 car 95% de personnes gagnent moins de 56400 euros par an
k<-c(j,seq(60000, 103200,14400)) # pour la fin on a 99% qui gagnent moins de 103200, on ne rajoute donc que 4 valeur sur 100


VecteurDesSalaires<-c(k,200000)

length(VecteurDesSalaires) #le vecteur est de taille 100 et représente de façon assez proportionel la population
