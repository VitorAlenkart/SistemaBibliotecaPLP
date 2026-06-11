module Tipos where

data Usuario = Usuario
    {
    idUsuario :: Int,
    login :: String, 
    senha :: String, 
    multa :: Double,
    historico :: [String],
    emprestimos :: [Emprestimo]
    } deriving (Show)

data Livro = Livro
    { codigo :: String, 
    titulo :: String, 
    autor :: String, 
    disponivel :: Bool
    } deriving (Show)

data Biblioteca = Biblioteca
    { usuarios :: [Usuario], 
    livros :: [Livro]
    } deriving (Show)

data Emprestimo = Emprestimo
    {
    idUser :: Int,
    codigoLivro :: String,
    dataEmprestimo :: String
    } deriving (Eq, Show)

limiteEmprestimos :: Int
limiteEmprestimos = 3

valorMultaPorDia :: Double
valorMultaPorDia = 1.5
