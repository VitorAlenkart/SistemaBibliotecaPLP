module UsuarioService where

import Tipos

import Data.Time (Day, parseTimeM, defaultTimeLocale, diffDays)

cadastrarUsuario :: Usuario -> Biblioteca -> Biblioteca
cadastrarUsuario usuario bib =
    bib { usuarios = usuario : usuarios bib }

pegarUsuario :: Int -> [Usuario] -> Usuario
pegarUsuario id [] = error "Usuário não encontrado!"
pegarUsuario id (x:xs)
    | id == (idUsuario x) = x
    | otherwise = pegarUsuario id xs

atualizarUsuario :: Int -> Maybe String -> Maybe String -> Biblioteca -> Biblioteca
atualizarUsuario id novoLogin novaSenha bib =
    bib { usuarios = map atualizar (usuarios bib) }

    where
        atualizar u
            | idUsuario u == id = u
                { login = definirDado novoLogin (login u)
                , senha = definirDado novaSenha (senha u)
                }
            | otherwise = u
        
        definirDado :: Maybe String -> String -> String
        definirDado (Just novoValor) _ = novoValor
        definirDado Nothing valorAntigo = valorAntigo

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

autenticarUsuario :: String -> String -> [Usuario] -> Maybe Usuario
autenticarUsuario loginD senhaD [] = Nothing 
autenticarUsuario loginD senhaD (x:xs)
    | loginD == login x && senhaD == senha x = Just x
    | otherwise = autenticarUsuario loginD senhaD xs


gerenciarMulta :: String -> String -> Double
gerenciarMulta dataRenovacao dataAtual = 
    let prazo = stringParaData dataRenovacao
        hoje =  stringParaData dataAtual
        diasAtraso = diffDays hoje prazo

    in if diasAtraso <= 0
        then 0.0
        else calculaRegra(fromIntegral diasAtraso)
    where
        calculaRegra dias
            | dias <= 7 = dias * valorMultaPorDia
            | otherwise = (7 * valorMultaBasePorDia) + ((dias - 7) * 3.0)

        stringParaData :: String -> Day
        stringParaData str = case parseTimeM True defaultTimeLocale "%d/%m/%Y" str of
            Just dataValida -> dataValida
            Nothing -> error ("Formato de data inválido: " ++ str)

aplicarMulta :: Int -> Double -> Biblioteca -> Biblioteca
aplicarMulta id valorMulta bib =
    bib { usuarios = map atualizar(usuarios bib) }
    where 
        atualizar u
            | idUsuario u == id = u { multa = multa u + valorMulta}
            | otherwise = u
