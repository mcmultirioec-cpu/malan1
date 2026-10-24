# Función para contabilizar y separar los números pares
# e impares de un vector
visualizar_par_impar <- function(A) {
  if (!is.numeric(A)) {
    stop("El argumento debe ser numérico")
  }
  A <- A[!is.na(A)]
  if (length(A) == 0) {
    stop("El vector no contiene datos válidos")
  }

  enteros <- A[A == round(A)]
  n_decimales <- length(A) - length(enteros)
  if (n_decimales > 0) {
    warning(n_decimales, " valor(es) no entero(s) fueron excluidos")
  }

  pares   <- enteros[enteros %% 2 == 0]
  impares <- enteros[enteros %% 2 != 0]

  cat("Cantidad de pares  :", length(pares), "\n")
  cat("Vector de pares    :", pares, "\n\n")
  cat("Cantidad de impares:", length(impares), "\n")
  cat("Vector de impares  :", impares, "\n")

  invisible(list(
    n_pares   = length(pares),
    n_impares = length(impares),
    pares     = pares,
    impares   = impares
  ))
}
