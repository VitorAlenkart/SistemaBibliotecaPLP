module Tipos where

data Usuario = Usuario
    {
    idUsuario :: Int,
    login :: String, 
    senha :: String, 
    multa :: Double,
    historico :: [String],
    emprestimos :: [String]
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

limiteEmprestimos :: Int
limiteEmprestimos = 3
