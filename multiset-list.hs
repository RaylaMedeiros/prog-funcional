module MultisetList (insertElem, removeElem, searchElem)
 where

{- 
 - Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que 
 - permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de 
 - ocorréncias do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia 
 - ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser 
 - implementada de diversas formas. Uma delas é usando a implementacao de Data.List, onde 
 - cada elemento da lista consiste do dado em si e sua quantidade (um par). 
 - Eh recomendavel que voce consulte a documentacao de Data.List
 -}
import Data.List as List

{-
 - Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
 -}
insertElem elem [] = [(elem, 1)]
insertElem elem ((a,b):xs) 
  | elem == a = [(a, b+1)]++xs
  | otherwise = [(a,b)]++(insertElem elem xs)

{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura. 
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}
removeElem elem [] = []
removeElem elem ((a,b):xs)
  | elem == a && b == 1 = xs 
  | elem == a = [(a, b-1)]++xs
  | otherwise = [(a,b)]++(removeElem elem xs)

{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
searchElem elem [] = 0
searchElem elem ((a,b):xs)
  | elem == a = b
  | otherwise = (searchElem elem xs)

{-
 - Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bags com suas maiores quantidades.
 - Por exemplo, A = {(a,1),(c,3)}, B = {(b,2),(c,1)}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
-}
unionElem bag1 bag2 = union' (sortOn fst bag1) (sortOn fst bag2)

union' bag1 [] = bag1
union' [] bag2 = bag2
union' ((a, b):xs) ((c, d):ys)
  | a == c = [(compareGreater a b d)]++(union' xs ys)
  | a < c = [(a,b)]++(union' xs ((c, d):ys))
  | otherwise = [(c,d)]++(union' ((a, b):xs) ys)

compareGreater a b c
  | b > c = (a, b)
  | otherwise = (a, c)

{-
 - Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas 
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso senhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.
-}
intersectionElem bag1 bag2 = intersection' (sortOn fst bag1) (sortOn fst bag2)

intersection' bag1 [] = []
intersection' [] bag2 = []
intersection' ((a, b):xs) ((c, d):ys)
  | a == c = [(compareSmaller a b d)]++(intersection' xs ys)
  | a < c = (intersection' xs ((c, d):ys))
  | otherwise = (intersection' ((a, b):xs) ys)

compareSmaller a b c
  | b < c = (a, b)
  | otherwise = (a, c)

{-
 - Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
   - contem os elementos de A que nao estao em B
   - contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B). 
     Caso essa quantidade seja negativa o elemento deve serremovido do Bag. 
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}
minus bag1 bag2 = undefined

{-
 - Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em otherBag.
-}
inclusion bag1 bag2 = undefined

{-
 - Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas. 
-}
sum bag1 bag2 = undefined

{-
 - Retorna a quantidade total de elementos no Bag
-}
size bag = undefined