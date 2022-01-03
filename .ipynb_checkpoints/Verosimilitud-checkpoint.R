## Presentar un resumen de la verosimilitud

## Información dada:
# Datos x
x <- c()
# La función de distribución asociada a ese parámetro (univariada)

## Construcción de la Log-Verosimilitud
Log_veros <- function(datos, f, parAmetros = 1,...){
	if(parAmetros == 1){
		L <- function(theta){
		  sum(log(f(datos, theta, ...)))
		}
		L <- Vectorize(L, "theta")
	}else{
		L <- function(theta1, theta2){
			sum(log(f(datos, theta1, theta2, ...)))
		}
		L <- Vectorize(L, c("theta1", "theta2"))
	}
}

## Ejemplos:

y <- rnorm(100, mean = 2.4, sd = 2)
l_norm <- Log_veros(y, dnorm, parAmetros=2)
media <- seq(0, 6, by = 0.1)
varianza <- seq(0.5, 9, by = 0.1)
Z <- outer(media, varianza, l_norm)
persp(media, varianza, Z, theta = 50, phi = 30, col = "blue")

