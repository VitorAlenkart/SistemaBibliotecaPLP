module EmprestimoService where

import Tipos
import LivroService
import UsuarioService



adicionarEmprestimo :: String -> Int -> Biblioteca -> Biblioteca
adicionarEmprestimo codigoLivro id bib =
    bib {
        usuarios = map atualizarUsuario (usuarios bib)
    }
    where 
        atualizarUsuario u 
            | idUsuario u == id = 
                u {
                    emprestimos = codigoLivro: emprestimos u
                }
            | otherwise = u
        atualizarLivro l
            | codigo l == codigoLivro =
                l {
                    disponivel = False
                }
            | otherwise = l

realizarEmprestimo :: String -> Int -> Biblioteca -> Biblioteca
realizarEmprestimo livro usuario blib
    | podeEmprestar usuario blib && livroDisponivel livro (livros blib) = blib
    | otherwise = blib
