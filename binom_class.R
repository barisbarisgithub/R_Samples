#plot class 
Binomplot = setRefClass(
    "Binomplot",
    fields=list(
        plotfilename="character",  #png name
        plotwidth="numeric",  #png width
        plotheight="numeric", #png height
        plotcolor="character", #plot color for 
        plottype="character",  #plot type 
        plotlwd="numeric" #plot bar or line weighth 
    )

)

#binom 
Binomclass=setRefClass(
"Binomclass",
fields=list(
    n="numeric",  #number of total trials
    k="numeric" , #number of success trials
    p="numeric"  #probability of success 
    
 ),

contains="Binomplot",  #linkink binomplot 

methods = list(
    calculate_binom = function(k){
       comb= factorial(n) / (factorial(n-k)*factorial(k))  #combination
       succes=p^k 
       q=1-p 
       qpow=n-k
       failure = q^qpow
        return (comb*succes*failure)  #return binom proob.
        
    },
    mass_func=function(){  #calculate mass funtion from 0 prob of n 
        i=0 
        mass=c()
        while(i<=n){
            mass[i+1]=calculate_binom(i)
           i=i+1
        }
       return (mass)
    },

    plot_binom =function(){  #plot 
        if(plotfilename==""){
            plotfilename<<-"test.png"
        }
        pict=png(file=plotfilename, width=plotwidth, height=plotheight)
        #hist(mass_func(),main="test")
       # seq=seq(1,n, by=1)
        seq=0:n-1
        plot(seq,mass_func(), type=plottype, lwd=plotlwd, col=plotcolor)
        dev.off()
    }

)
)

### an example ###    
test=Binomclass(
    n=20,
     p=0.5,
     k=5,
     plotfilename="binomsinif1.png",
     plotwidth=1024,
     plotheight=768,
     plotcolor="red",
     plottype="h",
     plotlwd=6
          
)


sprintf("%1.f nolu olasılık = %.10f",1,test$calculate_binom(1))
cat("\n")

prn=test$mass_func()
ii=0
while(ii<=length(prn)-1){
   cat(sprintf("%1.f kere başarııl olma  ihtimali = %.10f  \n",ii,prn[ii+1]))
    ii=ii+1
}
#print(prn)
test$plot_binom()

 