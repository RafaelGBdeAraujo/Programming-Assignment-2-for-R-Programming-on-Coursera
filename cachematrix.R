# Operador "<<-" usado para atribuir um valor a 
# objeto em um ambiente que é diferente do atual

# Armazenando a inversa de uma matriz
# Inversa da matriz custa tempo de computação
# Melhor armazenar para não computar novamente

makeCacheMatrix = function(x = matrix()) {
        inversa = NULL
        Capta = function(){
                x
        }
        Salva = function(y){
                x <<- y
                inversa <<- NULL
        }
        DetInversa = function(){
                inversa
        }
        SolInversa = function(inv){
                inversa <<- inv
        }
        list(Salva = Salva, Capta = Capta, SolInversa = SolInversa, DetInversa = DetInversa)
}

# Função para calcular a inversa da matriz informada
# Matriz informada em makeCacheMatrix
# Calcula novamente apenas se fornecer nova matriz
# Do contrário cacheSolve mostra inversa armazenada
# calculada anteriormente. Salva tempo de computação!

cacheSolve = function(x, ...) {
        inversa = x$DetInversa()
        if(!is.null(inversa)){
                message("Loading...\nUsing cache!")
                return(inversa)
        }
        matriz = x$Capta()
        inversa = solve(matriz, ...)
        x$SolInversa(inversa)
        inversa
}

# Tests performed
# > teste = makeCacheMatrix(matrix(sample(10,9),3,3))
# > teste$Capta()
#      [,1] [,2] [,3]
# [1,]    2    4    3
# [2,]    1    5    8
# [3,]    9    6   10
# > teste$DetInversa()
# NULL
# > cacheSolve(teste)
#             [,1]        [,2]        [,3]
# [1,]  0.01481481 -0.16296296  0.12592593
# [2,]  0.45925926 -0.05185185 -0.09629630
# [3,] -0.28888889  0.17777778  0.04444444
# > cacheSolve(teste)
# Loading...
# Using cache!
#             [,1]        [,2]        [,3]
# [1,]  0.01481481 -0.16296296  0.12592593
# [2,]  0.45925926 -0.05185185 -0.09629630
# [3,] -0.28888889  0.17777778  0.04444444
# > teste = makeCacheMatrix(matrix(sample(10,9),3,3))
# > cacheSolve(teste)
#             [,1]        [,2]        [,3]
# [1,]  0.12601626 -0.07723577  0.07113821
# [2,]  0.04471545  0.10162602 -0.11991870
# [3,] -0.10569106  0.03252033  0.10162602
# > cacheSolve(teste)
# Loading...
# Using cache!
#             [,1]        [,2]        [,3]
# [1,]  0.12601626 -0.07723577  0.07113821
# [2,]  0.04471545  0.10162602 -0.11991870
# [3,] -0.10569106  0.03252033  0.10162602
