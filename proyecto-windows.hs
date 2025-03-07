module Main where

import Data.Time
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Maybe
import Data.List
import System.IO
    ( hFlush, hPutStrLn, stdout, withFile, IOMode(WriteMode) )
import System.Directory (doesFileExist)
import Control.Monad (when)

-- Esto es como una objeto o plano de cada estudiante, con su ID, nombre y horas de entrada/salida
data Estudiante = Estudiante {
    idEstudiante :: String,
    nombre :: String,
    horaEntrada :: Maybe UTCTime,
    horaSalida :: Maybe UTCTime
} deriving (Show, Read)

-- Esto es solo un nombre para nuestra lista de estudiantes
type BaseEstudiantes = [Estudiante]

-- El archivo donde guardamos todo, como un libro
archivoDatos :: FilePath
archivoDatos = "University.txt"

-- Ajusta la hora para que sea GMT-5 (hora de colombia), o sea, le resta 5 horas
ajustarAGMT5 :: UTCTime -> UTCTime
ajustarAGMT5 tiempo = addUTCTime (-18000) tiempo  -- -18000 segundos = -5 horas

-- Convierte la hora en algo legible, o dice "No registrada" si no hay nada
formatearHora :: Maybe UTCTime -> String
formatearHora Nothing = "No registrada"
formatearHora (Just tiempo) = formatTime defaultTimeLocale "%d/%m/%Y %H:%M" (ajustarAGMT5 tiempo)

-- Funciones Puras (cosas que hacen calculos sin tocar el mundo real) --
-- siempree nos dan el mismo resultado

-- Añade un estudiante nuevo a la lista con su entrada, y si ya esta lo reemplaza
registrarEntradaPura :: String -> String -> UTCTime -> BaseEstudiantes -> BaseEstudiantes
registrarEntradaPura id nom tiempo base =
    Estudiante id nom (Just tiempo) Nothing : filter (\e -> idEstudiante e /= id) base

-- Marca la salida de un estudiante, pero solo si existe y no ha salido aun
registrarSalidaPura :: String -> UTCTime -> BaseEstudiantes -> Maybe BaseEstudiantes
registrarSalidaPura id tiempo base =
    case find (\e -> idEstudiante e == id && isNothing (horaSalida e)) base of
        Nothing -> Nothing  -- No lo encontro o ya salio, asi que nada
        Just estudiante -> Just $ estudiante { horaSalida = Just tiempo } : filter (\e -> idEstudiante e /= id) base

-- Busca a un estudiante por su ID, te lo da si lo encuentra
buscarEstudiantePura :: String -> BaseEstudiantes -> Maybe Estudiante
buscarEstudiantePura id = find (\e -> idEstudiante e == id)

-- Calcula cuanto tiempo estuvo el estudiante, si tiene entrada y salida
calcularTiempoPura :: Estudiante -> Maybe NominalDiffTime
calcularTiempoPura estudiante = do
    entrada <- horaEntrada estudiante
    salida <- horaSalida estudiante
    return $ diffUTCTime salida entrada

-- Interfaz IO (donde hablamos con el usuario y el programa hace cosas) --

-- Arranca todo: carga la lista de estudiantes y te saluda
main :: IO ()
main = do
    base <- cargarEstudiantes
    putStrLn "Bienvenido al Sistema de Registro Universitario"
    ejecutarMenu base

-- Te muestra el menu y espera a que elijas que hacer
ejecutarMenu :: BaseEstudiantes -> IO ()
ejecutarMenu base = do
    putStrLn "\n1. Registrar Entrada\n2. Registrar Salida\n3. Buscar Estudiante\n4. Listar Estudiantes\n5. Calcular Tiempo\n6. Salir"
    putStr "Ingrese opcion: "
    hFlush stdout  -- Esto hace que el mensaje salga ya, sin esperar
    opcion <- getLine
    case opcion of
        "1" -> manejarEntrada base >>= ejecutarMenu
        "2" -> manejarSalida base >>= ejecutarMenu
        "3" -> manejarBusqueda base >> ejecutarMenu base
        "4" -> listarEstudiantes base >> ejecutarMenu base
        "5" -> manejarCalculoTiempo base >> ejecutarMenu base
        "6" -> guardarEstudiantes base >> putStrLn "¡Adios!"
        _   -> putStrLn "Opcion invalida" >> ejecutarMenu base

-- Pide ID y nombre, registra la entrada y guarda los cambios
manejarEntrada :: BaseEstudiantes -> IO BaseEstudiantes
manejarEntrada base = do
    putStr "Ingrese ID del estudiante: "
    hFlush stdout  -- Para que veas el mensaje sin demora
    id <- getLine
    putStr "Ingrese nombre del estudiante: "
    hFlush stdout
    nom <- getLine
    tiempo <- getCurrentTime
    let nuevaBase = registrarEntradaPura id nom tiempo base
    putStrLn $ "Estudiante " ++ nom ++ " (" ++ id ++ ") registrado en entrada"
    guardarEstudiantes nuevaBase
    return nuevaBase

-- Pide ID y marca la salida si el estudiante esta en la lista
manejarSalida :: BaseEstudiantes -> IO BaseEstudiantes
manejarSalida base = do
    putStr "Ingrese ID del estudiante: "
    hFlush stdout
    id <- getLine
    tiempo <- getCurrentTime
    case registrarSalidaPura id tiempo base of
        Nothing -> putStrLn "Estudiante no encontrado o ya salio" >> return base
        Just nuevaBase -> do
            putStrLn $ "Estudiante con ID " ++ id ++ " registrado en salida"
            guardarEstudiantes nuevaBase
            return nuevaBase

-- Busca un estudiante por ID y te muestra sus datos
manejarBusqueda :: BaseEstudiantes -> IO ()
manejarBusqueda base = do
    putStr "Ingrese ID del estudiante: "
    hFlush stdout
    id <- getLine
    case buscarEstudiantePura id base of
        Nothing -> putStrLn "Estudiante no encontrado"
        Just est -> putStrLn $ "Estudiante: " ++ nombre est ++ " (ID: " ++ idEstudiante est ++ ")" ++
                               " - Entrada: " ++ formatearHora (horaEntrada est) ++
                               " - Salida: " ++ formatearHora (horaSalida est)

-- Te enseña la lista completa de estudiantes, o te dice si no hay nadie
listarEstudiantes :: BaseEstudiantes -> IO ()
listarEstudiantes base = do
    if null base
        then putStrLn "No hay estudiantes registrados"
        else mapM_ (\e -> putStrLn $ "ID: " ++ idEstudiante e ++ " - Nombre: " ++ nombre e ++
                                    " - Entrada: " ++ formatearHora (horaEntrada e) ++
                                    " - Salida: " ++ formatearHora (horaSalida e)) base

-- Calcula cuanto tiempo paso un estudiante adentro, si tiene entrada y salida
manejarCalculoTiempo :: BaseEstudiantes -> IO ()
manejarCalculoTiempo base = do
    putStr "Ingrese ID del estudiante: "
    hFlush stdout
    id <- getLine
    case buscarEstudiantePura id base of
        Nothing -> putStrLn "Estudiante no encontrado"
        Just estudiante -> case calcularTiempoPura estudiante of
            Nothing -> putStrLn "Informacion de entrada/salida incompleta"
            Just diff -> putStrLn $ "Tiempo transcurrido para " ++ nombre estudiante ++ ": " ++ formatDiffTime diff

-- Convierte el tiempo en algo mas facil de leer, como "2h 30m 15s"
formatDiffTime :: NominalDiffTime -> String
formatDiffTime diff =
    let totalSeconds = round diff :: Integer
        hours = totalSeconds `div` 3600
        minutes = (totalSeconds `mod` 3600) `div` 60
        seconds = totalSeconds `mod` 60
    in show hours ++ "h " ++ show minutes ++ "m " ++ show seconds ++ "s"

-- Operaciones con Archivos (guardar y cargar la lista) --

-- Escribe la lista de estudiantes en el archivo para no perderla
guardarEstudiantes :: BaseEstudiantes -> IO () -- indica que realiza una operacion de entrada/salida
guardarEstudiantes base = do
    withFile archivoDatos WriteMode $ \handle -> do
        hPutStrLn handle (show base) -- Escribe la lista en el archivo
        hFlush handle  -- Esto asegura que todo quede guardado ahora

-- Lee la lista del archivo, o empieza con una vacia si no hay nada
cargarEstudiantes :: IO BaseEstudiantes
cargarEstudiantes = do
    existe <- doesFileExist archivoDatos
    if existe
        then do
            contenido <- readFile archivoDatos  -- Carga todo el archivo de golpe
            let base = read contenido :: BaseEstudiantes
            seq (length $ show base) (return base)  -- Esto evita que se trabe por ser "perezoso"
        else return []  -- Si no hay archivo, te da una lista vacia
