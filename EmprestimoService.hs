module EmprestimoService where

import Tipos
import LivroService
import UsuarioService

realizarEmprestimo :: String -> Int -> String -> Biblioteca -> Biblioteca
realizarEmprestimo codLivro id date bib 
    | podeEmprestar id bib && 
        livroDisponivel codLivro (livros bib) = 
            bib 
            {
            usuarios = map atualizarUsuario (usuarios bib),
            livros = map atualizarLivro (livros bib)
            }
    | otherwise = bib
            where 
                atualizarUsuario u 
                    | idUsuario u == id = 
                        u {
                            emprestimos = livro: emprestimos u,
                            historico = nomeLivro: historico u
                        }
                    | otherwise = u
                atualizarLivro l
                    | codigo l == codLivro =
                        l {
                            disponivel = False
                        }
                    | otherwise = l
                livro = Emprestimo
                    {
                        idUser = id,
                        codigoLivro = codLivro,
                        dataEmprestimo = date
                    }
                nomeLivro = pegarLivro (codigoLivro livro) (livros bib)

devolverEmprestimo :: Emprestimo -> Biblioteca -> Biblioteca
devolverEmprestimo emprestimo bib =
    bib 
        {
            usuarios = map atualizarUsuario (usuarios bib),
            livros = map atualizarLivro (livros bib)
        }
        where 
            atualizarUsuario u
                | idUsuario u == id =
                    u {
                        emprestimos = filter (/= emprestimo) (emprestimos u)
                    }
                | otherwise = u
            atualizarLivro l
                | codigo l == codLivro =
                    l {
                        disponivel = True
                    }
                | otherwise = l
            id = idUser emprestimo
            codLivro = codigoLivro emprestimo

