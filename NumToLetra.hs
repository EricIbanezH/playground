main = do
 putStrLn "Ingresa el numero: "
 entrada <- getLine
 let
  ent = (read entrada)::Int
  lisNum = (descomponer ent)::[Int]
  lisInv = (inversa lisNum)::[Int]
  long = (longitud lisInv)::Int
  num = (norm lisInv long)::[Int]
 numALetra num long

concatena  x y = x ++ y

inversa [] = []
inversa (a : resto) = (concatena (inversa resto) [a])

cabeza (x:y:z:r) = [x]++[y]++[z] 

resto lista = tail (tail (tail lista))

inserta x lista = (x : lista)

longitud [] = 0
longitud (x : resto) = (longitud resto) + 1

descomponer 0 = []
descomponer n =
 inserta (n `mod` 10) (descomponer (n `div` 10))

norm lista l =
 if (l `mod` 3)==0 then 
  lista
 else
  inserta 0 (norm lista (l+1))

compuesto y
 |y == 0 = do
  putStr ""
 |otherwise =do
  putStr "y "  

base [x,y,z]
 | x == 0 = do
  base [y,z]
 | x == 1 && y ==0 && z==0 = do
  putStr "Cien "
 | x ==1 = do
  putStr "Ciento "
  base [y,z]
 | x == 5 = do 
  putStr "Quinientos "
  base [y,z]
 | otherwise =do 
  base [x]
  putStr "Cientos "
  base [y,z]

base [x,y]
 | x ==0 =do
  base [y]
 | x == 1 && y>5 =do
  putStr "Diez " 
  compuesto y
  base [y]
 | x == 1 && y ==0 = do 
  putStr "Diez "
 | x == 1 && y ==1 = do 
  putStr "Once "
 | x == 1 && y ==2 = do 
  putStr "Doce "
 | x == 1 && y ==3 = do 
  putStr "Trece "
 | x == 1 && y ==4 = do 
  putStr "Catorce "
 | x == 1 && y ==5 = do 
  putStr "Quince "
 | x == 2 = do 
  putStr "Veinte "
  compuesto y
  base [y]
 | x == 3 = do 
  putStr "Treinta "
  compuesto y
  base [y]
 | x == 4 = do 
  putStr "Cuarenta "
  compuesto y
  base [y]
 | x == 5 = do 
  putStr "Cincuenta "
  compuesto y
  base [y]
 | x == 6 = do 
  putStr "Sesenta "
  compuesto y
  base [y]
 | x == 7 = do 
  putStr "Setenta "
  compuesto y
  base [y]
 | x == 8 = do 
  putStr "Ochenta "
  compuesto y
  base [y]
 | x == 9 = do 
  putStr "Noventa "
  compuesto y
  base [y]

base [x]
 | x == 0 = do
  putStr ""
 | x == 1 = do
  putStr "Uno "
 | x == 2 = do
  putStr "Dos "
 | x == 3 = do
  putStr "Tres "
 | x == 4 = do
  putStr "Cuantro "
 | x == 5 = do
  putStr "Cinco "
 | x == 6 = do
  putStr "Seis "
 | x == 7 = do
  putStr "Siete "
 | x == 8 = do
  putStr "Ocho "
 | x == 9 = do
  putStr "Nueve "

numALetra lista l
 | l <= 3 = do
  base lista
 | l >=4 && l <= 6 && (lista!!0)==0 && (lista!!1)==0 && (lista!!2)==1 =do
  putStr "Mil "
  numALetra (resto lista) (l-3)
 | l >=4 && l <= 6 && ((lista !! 0)+(lista !! 1)+(lista !! 2)) /=0 =do
  base (cabeza lista)
  putStr "Mil "
  numALetra (resto lista) (l-3)
 | l >=7 && l <= 9 && (lista!!0)==0 && (lista!!1)==0 && (lista!!2)==1 =do
  putStr "Un millon "
  numALetra (resto lista) (l-3)
 | l >=7 && l <= 9 && ((lista !! 0)+(lista !! 1)+(lista !! 2)) /=0 =do
  base (cabeza lista)
  putStr "Millones "
  numALetra (resto lista) (l-3)
 | l >=10 && l <= 12 && (lista!!3)==0 && (lista!!4)==0 && (lista!!5)==0 && (lista!!0)/=0 && (lista!!1)/=0 && (lista!!2)/=0=do
  base (cabeza lista)
  putStr "Mil Millones "
  numALetra (resto (resto lista)) (l-6)
 | l >=10 && l <= 12 && ((lista !! 3)+(lista !! 4)+(lista !! 5)) /=0 =do
  base (cabeza lista)
  putStr "Mil "
  base (cabeza (resto lista))
  putStr "Millones "
  numALetra (resto (resto lista)) (l-6)
 | l >=13 && l <= 15 && (lista!!0)==0 && (lista!!1)==0 && (lista!!2)==1 =do
  putStr "Un Billon "
  numALetra (resto lista) (l-3)
 | l >=13 && l <= 15 && ((lista !! 0)+(lista !! 1)+(lista !! 2)) /=0 =do
  base (cabeza lista)
  putStr "Billones "
  numALetra (resto lista) (l-3)
 | otherwise = do
  putStr ""
