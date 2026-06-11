module UsuarioService where

import Tipos

cadastrarUsuario :: Usuario -> Biblioteca -> Biblioteca
cadastrarUsuario usuario bib =
    bib { usuarios = usuario : usuarios bib }

podeEmprestar :: Int -> Biblioteca -> Bool
podeEmprestar usuario blib =
    usuario > 1

