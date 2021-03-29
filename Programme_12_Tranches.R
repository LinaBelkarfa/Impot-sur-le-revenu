#Nouvelle modélisation avec 12 tranches d'imposition

#1)jusqu'à 9 000 ---> 	0 %
#2)de 9 000 à 10 000 --->	5 %
#3)de 10 000 à 20 000 --->	10 %
#4)de 20 000 à 30 000 --->	15 %
#5)de 30 000 à 40 000 --->	20 %
#6)de 40 000 à 50 000 --->	25 %
#7)de 50 000 à 60 000 --->	30 %
#8)de 60 000 à 70 000 --->	35 %
#9)de 70 000 à 80 000 --->	40 %
#10)de 80 000 à 100 000 --->	42 %
#11)de 100 000 à 150 000 --->	45 %
#12)Au-delà 150 000 --->	47 %

#x montant du salaire annuel en euros, 
#y montant total du prélèvement de l'impot en euros, 
#z taux total d'imposition (y/x)

x<- seq(1,200000,100) #x vecteur test contenant des salaires allant de 0 à 200 000 euros

Impot12 <-function(x){
  y=c()
  z=c()
  T=0
  for (i in x) {
    if (i<=9000 ){ #première tranche
      T= i*0
      y=append(y,T)
      z=append(z,T/i)
    }
    else if (i>9000 && i<=10000 ){  # deuxième tranche
      T= 9000*0+(i-9000)*0.05
      y=append(y,T)
      z=append(z,T/i)
    }
    else if (i>10000 && i<=20000 ){ #3ème tranche
      T= 9000*0+(10000-9000)*0.05+(i-10000)*0.1
      y=append(y,T)
      z=append(z,T/i)
    }
    else if (i>20000 && i<=30000 ){ #4ème tranche
      T= 9000*0+(10000-9000)*0.05+(20000-10000)*0.1+(i-20000)*0.15
      y=append(y,T)
      z=append(z,T/i)
    }
    else if (i>30000 && i<=40000 ){ #5ème tranche
      T= 9000*0+(10000-9000)*0.05+(20000-10000)*0.1+(30000-20000)*0.15+(i-30000)*0.2
      y=append(y,T)
      z=append(z,T/i)
    }
    else if (i>40000 && i<=50000 ){ #6ème tranche
      T= 9000*0+(10000-9000)*0.05+(20000-10000)*0.1+(30000-20000)*0.15+(40000-30000)*0.2+(i-40000)*0.25
      y=append(y,T)
      z=append(z,T/i)
    }
    else if (i>50000 && i<=60000 ){ #7ème tranche
      T= 9000*0+(10000-9000)*0.05+(20000-10000)*0.1+(30000-20000)*0.15+(40000-30000)*0.2+(50000-40000)*0.25+(i-50000)*0.3
      y=append(y,T)
      z=append(z,T/i)
    }
    else if (i>60000 && i<=70000 ){ #8ème tranche
      T= 9000*0+(10000-9000)*0.05+(20000-10000)*0.1+(30000-20000)*0.15+(40000-30000)*0.2+(50000-40000)*0.25+(60000-50000)*0.3+(i-60000)*0.35
      y=append(y,T)
      z=append(z,T/i)
    }
    else if (i>70000 && i<=80000 ){ #9ème tranche
      T= 9000*0+(10000-9000)*0.05+(20000-10000)*0.1+(30000-20000)*0.15+(40000-30000)*0.2+(50000-40000)*0.25+(60000-50000)*0.3+(70000-60000)*0.35+(i-70000)*0.40
      y=append(y,T)
      z=append(z,T/i)
    }
    else if (i>80000 && i<=100000 ){ #10ème tranche
      T= 9000*0+(10000-9000)*0.05+(20000-10000)*0.1+(30000-20000)*0.15+(40000-30000)*0.2+(50000-40000)*0.25+(60000-50000)*0.3+(70000-60000)*0.35+(80000-70000)*0.40+(i-80000)*0.42
      y=append(y,T)
      z=append(z,T/i)
    }
    else if (i>100000 && i<=150000 ){ #11ème tranche
      T= 9000*0+(10000-9000)*0.05+(20000-10000)*0.1+(30000-20000)*0.15+(40000-30000)*0.2+(50000-40000)*0.25+(60000-50000)*0.3+(70000-60000)*0.35+(80000-70000)*0.40+(100000-80000)*0.42+(i-100000)*0.45
      y=append(y,T)
      z=append(z,T/i)
    }
    else if (i>150000){ #12ème tranche
      T= 9000*0+(10000-9000)*0.05+(20000-10000)*0.1+(30000-20000)*0.15+(40000-30000)*0.2+(50000-40000)*0.25+(60000-50000)*0.3+(70000-60000)*0.35+(80000-70000)*0.40+(100000-80000)*0.42+(150000-100000)*0.45+(i-150000)*0.47
      y=append(y,T)
      z=append(z,T/i)
    }
  }
  # représentation graphique des vecteur y en fonction de x et z en fonction de x : 
  par(mar=c(4,4,3,5))
  plot(x=x, y=y, pch=16, axes=F, ylim=c(0,120000),xlim=c(0,200000), xlab="", ylab="", type="l",col="blue", main="L'Impot sur le revenu en France en 2019")
  axis(2, ylim=c(0,120000), col="red",col.axis="black")
  mtext("Montant de l'impôt sur le revenu",side=2,line=2.5)
  axis(1,pretty(range(x),10))
  box() 
  
  par(new=TRUE) 
  
  plot(x=x, y=z, pch=15,  xlab="", ylab="", ylim=c(0,0.5), axes=F, type="l", col="green")
  mtext("Taux d'imposition total (Impot/Revenu)",side=4,col="red",line=2.5)
  axis(4, ylim=c(0,0.5), col="red",col.axis="red")
  mtext("Montant du salaire annuel",side=1,col="black",line=2.5)
  
  legend(x="topleft",legend=c("Montant en euro de l'IRPP","Taux d'imposition (en pourcentage du revenu)", "Montant en euro de l'IRPP avec les 12 tranches","Taux d'imposition avec les 12 tranches (en pourcentage du revenu)" ),text.col=c("black","red","blue","green"),pch=c(16,15,1,3),col=c("black","red","blue","green"))
}  

#On lance la fonction dans la console avec le vecteur x que l'on a créé : 
Impot12(x) #représente les deux courbes
par(new=TRUE)
Impot(x)
