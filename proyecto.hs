module Main where

import Data.Time
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Maybe
import Data.List
import System.IO
import System.Directory (doesFileExist)
import Control.Monad (when)

-- Tipo de dato para Estudiante, ahora con nombre
data Estudiante = Estudiante {
    idEstudiante :: String,
    nombre :: String,
    horaEntrada :: Maybe UTCTime,
    horaSalida :: Maybe UTCTime
} deriving (Show, Read)

-- Alias de tipo para la base de datos de estudiantes
type BaseEstudiantes = [Estudiante]

-- Archivo donde se guarda la data
archivoDatos :: FilePath
archivoDatos = "University.txt"

-- Ajustar hora a GMT-5 (restar 5 horas)
ajustarAGMT5 :: UTCTime -> UTCTime
ajustarAGMT5 tiempo = addUTCTime (-18000) tiempo  -- -18000 segundos = -5 horas

-- Función auxiliar para formatear Maybe UTCTime en GMT-5
formatearHora :: Maybe UTCTime -> String
formatearHora Nothing = "No registrada"
formatearHora (Just tiempo) = formatTime defaultTimeLocale "%d/%m/%Y %H:%M" (ajustarAGMT5 tiempo)

-- Funciones Puras --

registrarEntradaPura :: String -> String -> UTCTime -> BaseEstudiantes -> BaseEstudiantes
registrarEntradaPura id nom tiempo base =
    Estudiante id nom (Just tiempo) Nothing : filter (\e -> idEstudiante e /= id) base

registrarSalidaPura :: String -> UTCTime -> BaseEstudiantes -> Maybe BaseEstudiantes
registrarSalidaPura id tiempo base =
    case find (\e -> idEstudiante e == id && isNothing (horaSalida e)) base of
        Nothing -> Nothing
        Just estudiante -> Just $
            estudiante { horaSalida = Just tiempo } : filter (\e -> idEstudiante e /= id) base

buscarEstudiantePura :: String -> BaseEstudiantes -> Maybe Estudiante
buscarEstudiantePura id = find (\e -> idEstudiante e == id)

calcularTiempoPura :: Estudiante -> Maybe NominalDiffTime
calcularTiempoPura estudiante = do
    entrada <- horaEntrada estudiante
    salida <- horaSalida estudiante
    return $ diffUTCTime salida entrada

-- Interfaz IO --

main :: IO ()
main = do
    base <- cargarEstudiantes
    putStrLn "Bienvenido al Sistema de Registro Universitario"
    ejecutarMenu base

ejecutarMenu :: BaseEstudiantes -> IO ()
ejecutarMenu base = do
    putStrLn "\n1. Registrar Entrada\n2. Registrar Salida\n3. Buscar Estudiante\n4. Listar Estudiantes\n5. Calcular Tiempo\n6. Salir"
    putStr "Ingrese opción: "
    opcion <- getLine
    case opcion of
        "1" -> manejarEntrada base >>= ejecutarMenu
        "2" -> manejarSalida base >>= ejecutarMenu
        "3" -> manejarBusqueda base >> ejecutarMenu base
        "4" -> listarEstudiantes base >> ejecutarMenu base
        "5" -> manejarCalculoTiempo base >> ejecutarMenu base
        "6" -> guardarEstudiantes base >> putStrLn "¡Adiós!"
        _   -> putStrLn "Opción inválida" >> ejecutarMenu base

manejarEntrada :: BaseEstudiantes -> IO BaseEstudiantes
manejarEntrada base = do
    putStr "Ingrese ID del estudiante: "
    id <- getLine
    putStr "Ingrese nombre del estudiante: "
    nom <- getLine
    tiempo <- getCurrentTime
    let nuevaBase = registrarEntradaPura id nom tiempo base
    putStrLn $ "Estudiante " ++ nom ++ " (" ++ id ++ ") registrado en entrada"
    guardarEstudiantes nuevaBase
    return nuevaBase

manejarSalida :: BaseEstudiantes -> IO BaseEstudiantes
manejarSalida base = do
    putStr "Ingrese ID del estudiante: "
    id <- getLine
    tiempo <- getCurrentTime
    case registrarSalidaPura id tiempo base of
        Nothing -> putStrLn "Estudiante no encontrado o ya salió" >> return base
        Just nuevaBase -> do
            putStrLn $ "Estudiante con ID " ++ id ++ " registrado en salida"
            guardarEstudiantes nuevaBase
            return nuevaBase

manejarBusqueda :: BaseEstudiantes -> IO ()
manejarBusqueda base = do
    putStr "Ingrese ID del estudiante: "
    id <- getLine
    case buscarEstudiantePura id base of
        Nothing -> putStrLn "Estudiante no encontrado"
        Just est -> putStrLn $ "Estudiante: " ++ nombre est ++ " (ID: " ++ idEstudiante est ++ ")" ++
                               " - Entrada: " ++ formatearHora (horaEntrada est) ++
                               " - Salida: " ++ formatearHora (horaSalida est)

listarEstudiantes :: BaseEstudiantes -> IO ()
listarEstudiantes base = do
    if null base
        then putStrLn "No hay estudiantes registrados"
        else mapM_ (\e -> putStrLn $ "ID: " ++ idEstudiante e ++ " - Nombre: " ++ nombre e ++
                                    " - Entrada: " ++ formatearHora (horaEntrada e) ++
                                    " - Salida: " ++ formatearHora (horaSalida e)) base

manejarCalculoTiempo :: BaseEstudiantes -> IO ()
manejarCalculoTiempo base = do
    putStr "Ingrese ID del estudiante: "
    id <- getLine
    case buscarEstudiantePura id base of
        Nothing -> putStrLn "Estudiante no encontrado"
        Just estudiante -> case calcularTiempoPura estudiante of
            Nothing -> putStrLn "Información de entrada/salida incompleta"
            Just diff -> putStrLn $ "Tiempo transcurrido para " ++ nombre estudiante ++ ": " ++ formatDiffTime diff

-- Función auxiliar para formatear el tiempo transcurrido
formatDiffTime :: NominalDiffTime -> String
formatDiffTime diff =
    let totalSeconds = round diff :: Integer
        hours = totalSeconds `div` 3600
        minutes = (totalSeconds `mod` 3600) `div` 60
        seconds = totalSeconds `mod` 60
    in show hours ++ "h " ++ show minutes ++ "m " ++ show seconds ++ "s"

-- Operaciones con Archivos Mejoradas --

guardarEstudiantes :: BaseEstudiantes -> IO ()
guardarEstudiantes base = do
    withFile archivoDatos WriteMode $ \handle -> do
        hPutStrLn handle (show base)
        hFlush handle

cargarEstudiantes :: IO BaseEstudiantes
cargarEstudiantes = do
    existe <- doesFileExist archivoDatos
    if existe
        then do
            contenido <- withFile archivoDatos ReadMode $ \handle -> do
                texto <- hGetContents handle
                seq (length texto) (return texto)
            return (read contenido :: BaseEstudiantes)
        else return []1
