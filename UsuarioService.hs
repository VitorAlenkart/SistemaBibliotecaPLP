module UsuarioService where

import Tipos

cadastrarUsuario :: Usuario -> Biblioteca -> Biblioteca
cadastrarUsuario usuario bib =
    bib { usuarios = usuario : usuarios bib }

pegarUsuario :: Int -> [Usuario] -> Usuario
pegarUsuario id (x:xs)
    | id == (idUsuario x) = x
    | otherwise = pegarUsuario id xs

podeEmprestar :: Int -> Biblioteca -> Bool
podeEmprestar usuario blib =
    length(emprestimos (pegarUsuario usuario (usuarios blib))) <= limiteEmprestimos

usuarioComLivro :: Int -> String -> Biblioteca -> Bool
usuarioComLivro id livro bib =   
    livro `elem` map codigoLivro (emprestimos user)
    where 
        user = pegarUsuario id (usuarios bib)

historicoEmprestimos :: Usuario -> IO()
historicoEmprestimos user = do printarHistorico (historico user)

printarHistorico :: [String] -> IO()
printarHistorico [] = return()
printarHistorico (x:xs) = do
    print ("Nome do Livro: " ++ x ++ "|")
    printarHistorico xs

controleMultas :: Int -> Biblioteca -> Double
controleMultas id bib = multa (pegarUsuario id (usuarios bib))