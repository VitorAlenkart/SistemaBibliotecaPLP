module LivroService where

import Tipos

livroDisponivel :: String -> [Livro] -> Bool
livroDisponivel _ [] = False
livroDisponivel livro (x:xs)
    | codigo x == livro && disponivel x = True
    | otherwise = livroDisponivel livro xs

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