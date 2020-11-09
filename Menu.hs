funcionInicio = do
    funcionMenuPrincipal

funcionMenuPrincipal = do
    putStrLn ("******MENU PRINCIPAL******")
    putStrLn("1.- Serie Fibonacci")
    putStrLn("2.- Lista de numeros del 1-10")
    putStrLn("3.- Factorización")
    putStrLn("4.- Desaparecer Numeros")
    putStrLn("5.- Palindromos")
    putStrLn("6.- Calculadora")
    putStrLn("7.- Salir")
    putStrLn("¿Que opcion quieres realizar?")
    opcion <- getLine


    case opcion of
        "1" -> funcionFibonacci
        "2" -> funcionNumero
        "3" -> funcionFactorial
        "4" -> funcionDesaparece 
        "5" -> funcionPalindromo
        "6" -> funcionMenuCalcu
        "7" -> putStrLn("Saliendo - Hecho por: Fabiola Lizeth Naranjo Sierra")
        _   -> putStrLn("ERROR!!!! Opcion Incorrecta!! ")



{-SERIE FIBONACCI-}

funcionFibonacci = do   
        putStrLn("*****SERIE FIBONACCI*******")
        pedirposicion


pedirposicion = do
    putStrLn("¿Que posición quieres?")
    posicion <- getLine
    putStrLn("el resultado de la posicion es:"++show(fibonacci (read posicion)))
    funcionInicio

fibonacci posicion = do
        if posicion < 2
            then do 
                posicion
        else do
            fibonacci (posicion-1)+fibonacci(posicion-2)
        

        

{-FUNCION NUMEROS-}

funcionNumero = do
    putStrLn("****LISTA DE NUMEROS*****")
    putStrLn("Los numeros son: ")
    numeros 1

numeros inicio = do
        if inicio <= 10
        then do
            print inicio
            numeros(inicio+1)
        else do
            putStrLn("")
            funcionInicio



{-FACTORIZACIÓN-}
funcionFactorial = do
    
    putStrLn("******FACTORIZACIÓN******")
    pedirfactorial

pedirfactorial = do
    putStrLn("¿Que numero deseas Factorizar?")
    factorial <- getLine
    putStrLn("El resultado de la Factorización es:"++show(factorizacion (read factorial)))
    putStrLn("")
    funcionInicio

factorizacion factorial = do
        if factorial == 0
            then do 
                1    
            else do
            factorial * factorizacion (factorial-1)
            
            
{-FUNCION DESAPARECE-}
funcionDesaparece = do
        putStrLn("*****DESAPARECEN******")
        desaparece[0,1,2,3,4,5,6,7,8,9,10]
desaparece numeros = do
        if null numeros
                then do
                        putStrLn("")
                        funcionInicio
                
        else do
                print(numeros)
                let final = init numeros
                desaparece(final)



{-FUNCION PALINDROMO-}
funcionPalindromo = do
    putStrLn("")
    putStrLn ("******MENU PALINDROMO******")
    putStrLn("1.- Saber si la palabra es un palindromo")
    putStrLn("2.- Formar un palindromo")
    putStrLn("3.- Ir al Menu Principal")
    putStrLn("¿Que opcion deseas?")
    opcion <- getLine


    case opcion of
        "1" -> funcionsaber
        "2" -> funcionformar
        "3" -> salirPalindromo
        _   -> putStrLn("ERROR!!!! Opcion Incorrecta!! ")

{-funcion saber-}
funcionsaber = do 
    pedirdato

pedirdato = do
    putStrLn("Escribe los numeros o palabra para sabe si es un palindromo")
    posicion <- getLine
    putStrLn("El palindromo es:"++show(saber (posicion)))
    funcionPalindromo

saber posicion = do
    posicion == reverse posicion
    
{-funcion formar-}   
funcionformar = do
    putStrLn("¿Que numeros o palabras quieres convertir en Palindromo?")
    n <- getLine
    putStrLn("El Palindromo creado es: "++ n ++ reverse n)
    funcionPalindromo

salirPalindromo = do
    funcionInicio
    


{-FUNCION CALCULADORA -}
funcionMenuCalcu = do
    putStrLn("")
    putStrLn ("******MENU CALCULADORA******")
    putStrLn("1.- Suma")
    putStrLn("2.- Resta")
    putStrLn("3.- Multiplicación")
    putStrLn("4.- Division")
    putStrLn("5.- Ir al Menu Principal")
    putStrLn("¿Que opcion deseas?")
    opcion <- getLine


    case opcion of
        "1" -> suma
        "2" -> resta
        "3" -> multiplicacion
        "4" -> division
        "5" -> funcionSalirCalcu
        _   -> putStrLn("ERROR!!!! Opcion Incorrecta!! ")


{-funcion suma-}
suma = do
    putStrLn ("Ingresa el numero 1: ")
    a <- getLine
    putStrLn ("Ingresa el numero 2:")
    b <- getLine

    let aInt = (read a)
    let bInt = (read b)
    let resultado = aInt + bInt

    putStrLn("El resultado de la suma es: "++show resultado) 
    funcionMenuCalcu
{-funcion resta-}
resta = do
    putStrLn ("Ingresa el numero 1: ")
    a <- getLine
    putStrLn ("Ingresa el numero 2:")
    b <- getLine

    let aInt = (read a)
    let bInt = (read b)
    let resultado = aInt - bInt

    putStrLn("El resultado de la resta es: "++show resultado)
    funcionMenuCalcu

{-funcion multiplicacion-}
multiplicacion = do
    putStrLn ("Ingresa el numero 1: ")
    a <- getLine
    putStrLn ("Ingresa el numero 2:")
    b <- getLine

    let aInt = (read a)
    let bInt = (read b)
    let resultado = aInt * bInt

    putStrLn("El resultado de la multiplicacion es: "++show resultado)
    funcionMenuCalcu
    
{-funcion division-}
division = do
    putStrLn ("Ingresa el numero 1: ")
    a <- getLine
    putStrLn ("Ingresa el numero 2:")
    b <- getLine

    let aInt = (read a)
    let bInt = (read b)
    let resultado = div aInt  bInt

    putStrLn("El resultado de la division es: "++show resultado)
    funcionMenuCalcu
{-funcion salir calculadora-}
funcionSalirCalcu = do 
    funcionInicio
    
