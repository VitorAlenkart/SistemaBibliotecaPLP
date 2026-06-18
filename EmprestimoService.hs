module EmprestimoService where

import Tipos
import LivroService
import UsuarioService
import Data.Time (Day)
import Data.Time
import Data.Time.Format

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
                        dataEmprestimo = date,
                        dataDevolucaoPrevista = extenderPrazo date 30,
                        qtdRenovacoes= 0
                    }
                nomeLivro = pegarLivro (codigoLivro livro) (livros bib)

devolverEmprestimo :: Emprestimo -> String -> Biblioteca -> Biblioteca
devolverEmprestimo emprestimo dataAtual bib =
    bib 
        {
            usuarios = map atualizarUsuario (usuarios bib),
            livros = map atualizarLivro (livros bib)
        }
        where 
            atualizarUsuario u
                | idUsuario u == id =
                    u {
                        emprestimos = filter (/= emprestimo) (emprestimos u),
                        multa = multa u + gerenciarMulta (dataDevolucaoPrevista emprestimo) dataAtual
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

renovarEmprestimo :: Int -> String -> String -> Biblioteca -> (Biblioteca, String)
renovarEmprestimo idUser codLivro dataAtual bib
    | not usuarioExiste = (bib, " Usuario Não Encontrado!")
    | multa usuario > 0 = (bib, "Renovação negada! Usuário possui multas pendentes.")
    | not naoEmprestimo = (bib, " Não possui emprestimo!")
    | qtdRenovacoes empAtual >= limiteRenovacoes = ( bib , " Limites de Renovações!")
    | otherwise =
        let bibAtualizada = bib {usuarios = map  atualizarUsuario (usuarios bib)}
        in (bibAtualizada, "Renovado com sucesso! Novo prazo: " ++ novoPrazo)
    where
        usuariosDaBib = usuarios bib
        usuarioExiste = any (\u -> idUsuario u == idUser) usuariosDaBib
        usuario       = pegarUsuario idUser usuariosDaBib
        
        emprestimosDoUser = emprestimos usuario
        naoEmprestimo = any (\e -> codigoLivro e == codLivro) emprestimosDoUser
        empAtual = head (filter (\e -> codigoLivro e == codLivro) emprestimosDoUser)
        
        novoPrazo = extenderPrazo dataAtual 7

        atualizarUsuario atualizaUser
            | idUsuario atualizaUser == idUser = atualizaUser { emprestimos = map atualizarEmprestimo (emprestimos atualizaUser) }
            | otherwise = atualizaUser

        atualizarEmprestimo emp
            | codigoLivro emp == codLivro = emp
                { dataDevolucaoPrevista = novoPrazo
                , qtdRenovacoes = qtdRenovacoes emp + 1 
                }
            | otherwise = emp

extenderPrazo :: String -> Integer -> String
extenderPrazo dataAtual dias =
    case parseTimeM True defaultTimeLocale "%d/%m/%Y" dataAtual :: Maybe Day of
        Nothing -> error "Data inválida"
        Just dia ->
            formatTime defaultTimeLocale "%d/%m/%Y" (addDays dias dia)