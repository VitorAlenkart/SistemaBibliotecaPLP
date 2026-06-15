module LivroService where

import Tipos

livroDisponivel :: String -> [Livro] -> Bool
livroDisponivel _ [] = False
livroDisponivel codLivro (x:xs)
    | codigo x == codLivro && disponivel x = True
    | otherwise = livroDisponivel codLivro xs

cadastrarLivro :: Livro -> Biblioteca -> Biblioteca
cadastrarLivro livro bib =
    bib { livros = livro : livros bib }

removerLivro :: String -> Biblioteca -> Biblioteca
removerLivro cod bib =
    bib { livros = filter (\l -> codigo l /= cod) (livros bib) }

pegarLivro :: String -> [Livro] -> String
pegarLivro codLivro (x:xs)
    | codLivro == codigo x = titulo x
    | otherwise = pegarLivro codLivro xs 

consultarLivro :: String -> [Livro] -> Livro
consultarLivro _ [] = error "Livro não encontrado"
consultarLivro termoBusca (x:xs)
    | termoBusca == titulo x || termoBusca == autor x || termoBusca == codigo x = x
    | otherwise = consultarLivro termoBusca xs