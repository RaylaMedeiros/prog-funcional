--Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
--Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- elementos de uma triple.
data Triple a b c = Triple a b c deriving (Eq,Show)

tripleFst (Triple a _ _) = a
tripleSnd (Triple _ b _) = b
tripleThr (Triple _ _ c) = c

--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
data Quadruple a b = Quad a a b b deriving (Eq,Show)

firstTwo (Quad a b _ _) = (a, b)
secondTwo (Quad _ _ c d) = (c, d)

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d deriving (Eq,Show)

tuple1 (Tuple1 a) = Just a
tuple1 (Tuple2 a _) = Just a
tuple1 (Tuple3 a _ _) = Just a
tuple1 (Tuple4 a _ _ _) = Just a

tuple2 (Tuple2 _ b) = Just b
tuple2 (Tuple3 _ b _) = Just b
tuple2 (Tuple4 _ b _ _) = Just b
tuple2 _ = Nothing

tuple3 (Tuple3 _ _ c) = Just c
tuple3 (Tuple4 _ _ c _) = Just c
tuple3 _ = Nothing

tuple4 (Tuple4 _ _ _ d) = Just d
tuple4 _ = Nothing 

data List a = Nil | Cons a (List a) deriving (Eq,Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = error "Empty list"
listHead (Cons x xs) = x

listTail Nil = error "Empty list"
listTail (Cons x xs) = xs

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)


listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f v x) xs 

--Escreva as funcoes sobre a estrutura de dados binary tree
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a)
 deriving (Eq,Show)

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

--verifica se uma BT é uma BST
isBST NIL = True
isBST (Node a left right) = (isGreater a (preOrder(left))) && (isSmaller a (preOrder(right)))

isGreater _ [] = True
isGreater a (x:xs) = (a >= x) && (isGreater a xs)

isSmaller _ [] = True
isSmaller a (x:xs) = (a < x) && (isSmaller a xs)

--insere uma nova chave na BST retornando a BST modificada
insert x NIL = (Node x NIL NIL)
insert x (Node a left right) | a > x  = (Node a (insert x left) right)
                             | otherwise = (Node a left (insert x right))

--retorna o Node da BST contendo o dado procurado ou entao NIL
search x NIL = NIL
search x (Node a left right) | a == x = (Node a left right)
                             | a > x = (search x left)
                             | otherwise = (search x right)

--retorna o elmento maximo da BST
maximumValue NIL = NIL
maximumValue (Node a left NIL) = (Node a left NIL)
maximumValue (Node a left right) = (maximumValue right)

--retorna o elemento minimo da BST
minimumValue NIL = NIL
minimumValue (Node a NIL right) = (Node a NIL right)
minimumValue (Node a left right) = minimumValue left

--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
predecessor x NIL = NIL
predecessor x (Node a left right) | node == NIL = NIL
                                  | maximumChild /= NIL = maximumChild
                                  | otherwise = (predecessorFather x father (Node a left right)) 
    where node = (search x (Node a left right))
          maximumChild = (maximumValue (getLeftTree node))
          father = (searchFather x (Node a left right) NIL)

predecessorFather x NIL _ = NIL
predecessorFather x (Node a left right) tree | x > a = (Node a left right)
                                             | otherwise = predecessorFather x (searchFather a tree NIL) tree

getLeftTree (Node a l r) = l

searchFather x NIL _ = NIL
searchFather x (Node a left right) father | x == a = father
                                          | x < a = (searchFather x left (Node a left right))
                                          | otherwise = (searchFather x right (Node a left right))

--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
sucessor x NIL = NIL
sucessor x (Node a left right) | node == NIL = NIL
                                  | minimumChild /= NIL = minimumChild
                                  | otherwise = (sucessorFather x father (Node a left right))
    where node = (search x (Node a left right))
          minimumChild = (minimumValue (getRightTree node))
          father = (searchFather x (Node a left right) NIL)

sucessorFather x NIL _ = NIL
sucessorFather x (Node a left right) tree | x < a = (Node a left right)
                                          | otherwise = sucessorFather x (searchFather a tree NIL) tree

getRightTree (Node a l r) = r

--remove um elemento da BST
remove x NIL = NIL

--retorna uma lista com os dados da BST nos diversos tipos de caminhamento
preOrder NIL = []
preOrder (Node a left right) = [a]++(preOrder left)++(preOrder right)

order NIL = []
order (Node a left right) = (order left)++[a]++(order right)

postOrder NIL = []
postOrder (Node a left right) = (postOrder left)++(postOrder right)++[a]
