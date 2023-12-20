########## Markov & Chebishev Inequalities ###########
# a simple library that you can calculate this equalities               #
############################################

MarkovChebishev=setRefClass(
    "MarkovChebishev",
    fields=list(
        listitems="numeric",   #mean will be calculated from this list
        x="numeric",  # random variable
        k="numeric" #k coefficient
    ),
    methods=list(
        markov=function(){
                avr=mean(listitems)   #average 
                divided=avr/x # E(x)/a
                return (c(avr,divided))  
        },
    
    chebishev=function(){
       avr=mean(listitems)  
        st_dev=sd(listitems)   #standart dev
        result=1/(k^2)  #chebishev result 
        return(c(avr,st_dev,result))
    }
)
)

##### ##### test sample ##############
test=MarkovChebishev(
    listitems=c(20,50,30,40,60,40,80),
    x=30,
    k=2
)
 
print(sprintf("P(%10.0f>=%.2f)<=%.2f",test$x,test$markov()[1],test$markov()[2]))
print(sprintf("P(|%10.0f-%.10.2f|>=%10.0f*%10.0f)<=%.2f",test$x,test$chebishev()[1],test$k,test$chebishev()[2],test$chebishev()[3]))
########## end of the test ##########
