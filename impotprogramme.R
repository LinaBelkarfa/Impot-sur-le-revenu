#jusqu'à 9 964 ??? 	0 %
#de 9 964 à 27 519 ??? 	14 %
#de 27 519 à 73 779 ??? 	30 %
#de 73 779 ??? à 156 244 ??? 	41 %
#Au-delà de 156 244 ??? 	45 %

#x montant du salaire annuel en euros, 
#y montant total du prélèvement de l'impot en euros, 
#z taux total d'imposition (y/x)

x<- seq(1,200000,100) #x vecteur test contenant des salaires allant de 0 à 200 000 euros

Impot <-function(x){
  y=c()
  z=c()
  T=0
  for (i in x) {
    if (i<=9964 ){ #première tranche
      T= i*0
      y=append(y,T)
      z=append(z,T/i)
      }
    else if (i>9964 && i<=27519 ){  # deuxième tranche
      T= 9964*0+(i-9964)*0.14
      y=append(y,T)
      z=append(z,T/i)
    }
    else if (i>27519 && i<=73779 ){ #3ème tranche
      T= 9964*0+(27519-9964)*0.14+(i-27519)*0.3
      y=append(y,T)
      z=append(z,T/i)
    }
    else if (i>73779 && i<=156244 ){ #4ème tranche
      T= 9964*0+(27519-9964)*0.14+(73779-27519)*0.3+(i-73779)*0.41
      y=append(y,T)
      z=append(z,T/i)
    }
    else if (i>156244){ #5ème tranche
      T= 9964*0+(27519-9964)*0.14+(73779-27519)*0.3+(156244-73779)*0.41+(i-156244)*0.45
      y=append(y,T)
      z=append(z,T/i)
    }
  }
  # représentation graphique des vecteur y en fonction de x et z en fonction de x : 
  par(mar=c(4,4,3,5))
  plot(x=x, y=y, pch=16, axes=F, ylim=c(0,120000),xlim=c(0,200000), xlab="", ylab="", type="l",col="black", main="L'Impot sur le revenu en France en 2019")
  axis(2, ylim=c(0,120000), col="red",col.axis="black")
  mtext("Montant de l'impôt sur le revenu",side=2,line=2.5)
  axis(1,pretty(range(x),10))
  box() 
  
  par(new=TRUE) 
  
  plot(x=x, y=z, pch=15,  xlab="", ylab="", ylim=c(0,0.5), axes=F, type="l", col="red")
  mtext("Taux d'imposition total (Impot/Revenu)",side=4,col="red",line=2.5)
  axis(4, ylim=c(0,0.5), col="red",col.axis="red")
  mtext("Montant du salaire annuel",side=1,col="black",line=2.5)
  
legend(x="topleft",legend=c("Montant en euro de l'IRPP","Taux d'imposition (en pourcentage du revenu)"),text.col=c("black","red"),pch=c(16,15),col=c("black","red"))
}  

#On lance la fonction dans la console avec le vecteur x que l'on a créé : 
Impot(x) #représente les deux courbes
