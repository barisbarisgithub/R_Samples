###### uniform distribution class ####3

Uniform = setRefClass("Uniform",
fields = list(
    theta_1 = "numeric",
    theta_2 = "numeric",
    b = "numeric",
    a = "numeric"
),

methods = list(
    dist_func = function(){
        x = b - a
        y = theta_2 - theta_1
        return(x/y)
    },

    dist_exp_value = function(){
        return ((theta_1 + theta_2)/2)
    },
    dist_variance = function (){
        x = theta_2 - theta_1
        return( x/12 )
    },
    dist_std = function(){
        x = sqrt(dist_variance() )
    }
)

)


##### an example #####

u_example = Uniform(
a = 4,
b = 6,
theta_1=2,
theta_2 = 12
)

print(sprintf("Pdf = %.2f",u_example$dist_func()))
print(sprintf("Expected Value = %.2f",u_example$dist_exp_value()))
print(sprintf("Variance = %.6f",u_example$dist_variance()))
print(sprintf("Standart Dev. = %.6f",u_example$dist_std()))