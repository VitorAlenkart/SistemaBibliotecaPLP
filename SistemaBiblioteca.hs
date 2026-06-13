module SistemaBiblioteca where

import Tipos
import EmprestimoService
import UsuarioService

telaLogin :: Biblioteca -> IO()
telaLogin bib = do

    putStrLn "\n====================="
    putStr " Digite seu Login: "
    loginD <- getLine

    putStr " Digite sua Senha: "
    senhaD <- getLine
    putStrLn "\n====================="

    let resultado = autenticarUsuario loginD senhaD ( usuarios bib)

    case resultado of
        Nothing -> do
            putStrLn "\n================="
            putStrLn "\nAcesso Negado! Login ou Senha Inválido(a)!"
            putStrLn "\n================="
            telaLogin bib

        Just usuariologado -> do
            putStrLn "\n================="
            putStrLn (" Acesso Permitido! Bem vindo(a), " ++ (login usuariologado) ++ "!")
            putStrLn "\n================="

            return ()


usuarioTeste :: Usuario
usuarioTeste = Usuario 
    { idUsuario = 145
    , login = "kika"
    , senha = "123"
    , multa = 0.0
    , historico = []
    , emprestimos = []
    }


bibliotecaTeste :: Biblioteca
bibliotecaTeste = Biblioteca 
    { usuarios = [usuarioTeste]
    , livros = []
    }

main :: IO ()
main = do
    putStrLn "Iniciando o Sistema da Biblioteca..."
    telaLogin bibliotecaTeste