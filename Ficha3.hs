module Ficha3 where
import Ficha1

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

-- 1)

-- a)
etapaConstruida :: Etapa -> Bool
etapaConstruida ((h1,m1),(h2,m2)) 
                                     | h1>23 || m1>59 || h1<0 || m1<0 || h2>23 || m2>59 || h2<0 || m2<0 = False
                                     | h1>h2 = False
                                     | h1 == h2 && m1 > m2 = False
                                     | otherwise = True

-- b)
testaViagem :: Viagem -> Bool
testaViagem [] = True
testaViagem ((a,b):(c,d):t) = if etapaConstruida (a,b) && etapaConstruida (c,d) && etapaConstruida (b,c)
                                     then testaViagem t
                              else False

-- c)
calculaHora :: Viagem -> Etapa 
calculaHora [(a,b)] = (a,b)
calculaHora [(a,b),(c,d)] = (a,d)
calculaHora ((a,b):(c,d):t) = calculaHora ((a,b):t)

-- 2.
type Poligonal = [Ponto]

-- a)
calculaComprimento :: Poligonal -> Double
calculaComprimento [p1] = 0
calculaComprimento (p1:p2:t) = distancia p1 p2 + calculaComprimento (p2:t)

-- b)
linhaFechada :: Poligonal -> Bool
linhaFechada [p1,p2] = False
linhaFechada [p1,p2,p3] = p1 == p3
linhaFechada (p1:p2:p3:t) = linhaFechada (p1:p3:t) 

-- 3)
data Contacto = Casa Integer 
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

-- a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail _ _ [] = []
acrescEmail nome string agenda = agenda ++ [(nome,[Email string])]

-- b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome ((n,c):agenda) = if nome == n 
                                      then Just (aux c)
                               else Nothing
                  where aux (Email a:b) = a:aux b
                        aux [] = []       

-- c)
consTelefs :: [Contacto] -> [Integer]  
consTelefs (a:b) = case a of Casa x -> x:consTelefs b 
                             Trab x -> x:consTelefs b
                             Tlm x -> x:consTelefs b
                             otherwise -> consTelefs b

consTelefs' :: [Contacto] -> [Integer]
consTelefs' ((Casa a):b) = a:consTelefs' b
consTelefs' ((Trab a):b) = a:consTelefs' b
consTelefs' ((Tlm a):b) = a:consTelefs' b
consTelefs' [] = []

-- d)
casa :: Nome -> Agenda -> Maybe Integer
casa nome ((n,c):t) = if nome == n then Just (aux c)
                      else Nothing
              where aux (Casa a:b) = a 
                    aux (_ :t) = aux t

-- 4)
type Dia = Int
type Mes = Int
type Ano = Int

data Data = D Dia Mes Ano
      deriving Show

type TabDN = [(Nome, Data)]

-- a)
procura :: Nome -> TabDN -> Maybe Data
procura nome ((n,d):t) = if nome == n then Just d else procura n t

-- b)
idade :: Data -> Nome -> TabDN -> Maybe Int
idade (D d1 m1 a1) nome ((n,D d2 m2 a2):t) = if nome == n then
                                                              if a1>a2 then Just (a1-a2)
                                                              else if a1==a2 then Just 0 
                                                                   else Nothing
                                             else idade (D d1 m1 a1) nome t

-- c)
anterior :: Data -> Data -> Bool 
anterior (D d1 m1 a1) (D d2 m2 a2) | a1<a2 = True
                                   | a1==a2 && m1<m2 = True
                                   | a1==a2 && m1==m2 && d1<d2 = True
                                   | otherwise = False

-- d)

ordena :: TabDN -> TabDN
ordena [] = []
ordena ((n,d):t) = aux (n,d) (ordena t)
         where 
         	   aux (n,d) [] = [(n,d)] 
               aux (n,d) ((nx,dx):t) = if d < dx then (n,d):(nx,dx):t
                                       else (nx,dx):aux (n,d) t