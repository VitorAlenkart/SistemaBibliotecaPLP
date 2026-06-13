module Tipos where

data Usuario = Usuario
    {
    idUsuario :: Int,
    login :: String, 
    senha :: String, 
    multa :: Double,
    historico :: [String],
    emprestimos :: [Emprestimo]
    } deriving (Eq , Show)


data Livro = Livro
    { codigo :: String
        ,titulo :: String
        ,autor :: String 
        ,disponivel :: Bool
    } deriving (Eq , Show)

data Biblioteca = Biblioteca
    { usuarios :: [Usuario] 
      , livros :: [Livro]
    } deriving (Show)

data Emprestimo = Emprestimo
    { idUser :: Int
        ,codigoLivro :: String
        ,dataEmprestimo :: String
        ,dataDevolucaoPrevista :: String  
        ,qtdRenovacoes         :: Int
    } deriving (Eq, Show)

limiteEmprestimos :: Int
limiteEmprestimos = 3

limiteRenovacoes :: Int   
limiteRenovacoes = 2

valorMultaBasePorDia :: Double
valorMultaBasePorDia = 1.0

valorMultaPorDia :: Double
valorMultaPorDia = 1.5

