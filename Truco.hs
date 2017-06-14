{-
    Truco Argentino.
    Por: 
        *** Matias Tuduri ***
        *** Jose Ibargoyen ***
        *** Leonell Tambasco ***
-}
module Truco where
import Definitions
import Actions
import GameLogic
import Data.List
-------------------------------   UI -------------------------------  

    
mostrarMano :: Jugador -> IO ()
mostrarMano (nombre,puntaje,mano,jugadas,acciones,esMano) = do 
    putStrLn ""
    putStrLn ("******* "++ nombre ++" *******")
    putStrLn ("-> Ptos: " ++ show(puntaje))
    putStrLn ("-> Mano: " ++ show(mano \\ jugadas))
    putStrLn ("-> Es Mano: " ++ show(esMano))
    putStrLn ("-> Jugadas: " ++ show(jugadas))

    putStrLn ("-> Acciones dichas: " ++ show(acciones))
    if(tieneFlor mano ) then
        putStrLn ("-> Flor: " ++ show(calcularFlor mano))
    else
        putStrLn ("-> Envido: " ++ show(calcularEnvido mano))
    putStrLn "**************************"
    putStrLn ""

mostrarOpciones :: Show a => [a] -> IO ()    
mostrarOpciones elementos = mostrarOpcionesAux elementos 0

mostrarOpcionesAux :: (Show t, Show a, Num t) => [a] -> t -> IO ()
mostrarOpcionesAux [] _ = do putStrLn ""
mostrarOpcionesAux (elem:elementos) indice= do 
    putStrLn (show(indice) ++ "- " ++show(elem))
    mostrarOpcionesAux elementos (indice+1)
          

    




jugador1 = ("leo",24,[],[],[Truco],False)
jugador2 = ("mar",22,[],[],[],True)
estadoJuego1 = EstadoJuego Jugador1 (jugador1,jugador2) [] False



----------- Main flow -----------

truco = do
    putStrLn "----------------------------"
    putStrLn "          TRUCO"
    putStrLn "----------------------------"
    putStrLn ""
    putStrLn ""
    putStrLn "Ingrese nombre jugador 1:"
    nombJ1 <- getLine
    putStrLn ""
    putStrLn "Ingrese nombre jugador 2:"
    nombJ2 <- getLine
    jugadores <- repartirMazo mazo nombJ1 nombJ2 0 0 True False
    putStrLn ""
    putStrLn "-------------------------------------------------------------------"
    gameLoop (EstadoJuego (Jugador1) (jugadores) (accionesIni) (False))
    
    
pedirjugada :: Jugador -> IO Jugador
pedirjugada j@(n,p,c,js,a,eM) = do
    mostrarOpciones (c \\ js)
    putStrLn "Seleccione carta a jugar: "
    carta <- readLn
    j1 <- tirarCarta j (carta::Int)
    mostrarMano j1
    return j1

tirarCarta :: Jugador -> Int -> IO Jugador
tirarCarta (_,_,cartas,_,_,_) jugada | jugada >= length cartas = error "Error, carta no se puede seleccionar"
tirarCarta (n,p,cartas, jugadas,acciones,esMano) jugada = do 
    let cartaJugada = ((cartas\\jugadas) !! jugada)
    putStrLn ""
    putStrLn("Jugador tira carta: " ++ show(cartaJugada))
    return (n,p, cartas, jugadas ++ [cartaJugada],acciones,esMano)
puedeDecirFlor::Jugador -> Acciones -> Acciones
puedeDecirFlor 	j1@(n1,p1,m1,t1,a1,e1) acciones 
	|(tieneFlor m1)  && not(dijeAccion a1 Flor) = acciones ++ [Flor]
	| otherwise = acciones

playerAgent :: PlayerAgent
playerAgent estado@(EstadoJuego Jugador1 (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) accionesN True) = return estado
playerAgent estado@(EstadoJuego Jugador1 (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) accionesN termino) | not (termino) = do
    let j = jugadorTurno Jugador1 (j1,j2)
    mostrarMano j
    let acciones= puedeDecirFlor j accionesN
    mostrarOpciones acciones 
    putStrLn "Ingrese nro accion:"
    nro <- readLn
    let accion = (acciones !! (nro::Int))
    putStrLn ("Accion seleccionada: " ++ show (accion))
    let jugadorAct = agregarAccionJugador j accion
    mostrarMano jugadorAct

    -- TOQUES
    if (((accion == NoQuiero) || (accion == Quiero) || (accion == Tiene)) && (accion /= Tirar) && ((elem Envido ultimaAccionOponente) || (elem RealEnvido ultimaAccionOponente) || (elem Flor ultimaAccionOponente) || (elem ContraFlor ultimaAccionOponente))) then do
        devolver <- actualizarPuntos (EstadoJuego Jugador1 (j1,j2) acciones termino) accion
        return devolver
    else do
        if ((accion == Tirar)) then do
            jugador <- pedirjugada jugadorAct
            let jugadorActualizado = actualizarJugador Jugador1 jugador (j1,j2)
            let proxTurnoEstado@(EstadoJuego t j a f) = proximoTurno (EstadoJuego Jugador1 jugadorActualizado acciones (isOver j1 j2))
            return (EstadoJuego Jugador2 j (accionesDisp proxTurnoEstado accion) f)
        else do
            let jugadorActualizado = actualizarJugador Jugador1 jugadorAct (j1,j2)
            let proxTurnoEstado@(EstadoJuego t j a f) = proximoTurno (EstadoJuego Jugador1 jugadorActualizado acciones (isOver j1 j2))
            return (EstadoJuego Jugador2 j (accionesDisp proxTurnoEstado accion) f)
    where {
        jn =jugadorNoTurno Jugador1 (j1,j2);
        ultimaAccionOponente= ultimaAccionDicha jn;
    }

playerAgent estado@(EstadoJuego Jugador2 (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) accionesN termino) | not (termino) = do
    let j = jugadorTurno Jugador2 (j1,j2)
    mostrarMano j
    let acciones= puedeDecirFlor j accionesN
    mostrarOpciones acciones 
    putStrLn "Ingrese nro accion:"
    nro <- readLn
    let accion = (acciones !! (nro::Int))
    putStrLn ("Accion seleccionada: " ++ show (accion))
    let jugadorAct = agregarAccionJugador j accion
    mostrarMano jugadorAct

    -- TOQUES
    if (((accion == NoQuiero) || (accion == Quiero) || (accion == Tiene)) && (accion /= Tirar) && ((elem Envido ultimaAccionOponente) || (elem RealEnvido ultimaAccionOponente) || (elem Flor ultimaAccionOponente) || (elem ContraFlor ultimaAccionOponente))) then do
        devolver <- actualizarPuntos (EstadoJuego Jugador2 (j1,j2) acciones termino) accion
        return devolver
    else do
        if ((accion == Tirar)) then do
            jugador <- pedirjugada jugadorAct
            let jugadorActualizado = actualizarJugador Jugador2 jugador (j1,j2)
            let proxTurnoEstado@(EstadoJuego t j a f) = proximoTurno (EstadoJuego Jugador2 jugadorActualizado acciones (isOver j1 j2))
            return (EstadoJuego Jugador1 j (accionesDisp proxTurnoEstado accion) f)
        else do
            let jugadorActualizado = actualizarJugador Jugador2 jugadorAct (j1,j2)
            let proxTurnoEstado@(EstadoJuego t j a f) = proximoTurno (EstadoJuego Jugador1 jugadorActualizado acciones (isOver j1 j2))
            return (EstadoJuego Jugador1 j (accionesDisp proxTurnoEstado accion) f)
    where {
        jn =jugadorNoTurno Jugador1 (j1,j2);
        ultimaAccionOponente= ultimaAccionDicha jn;
    }



agregarAccionJugador :: Jugador -> Accion -> Jugador
agregarAccionJugador (n1,p1,m1,t1,a1,e1) accion = (n1,p1,m1,t1,a1++[accion],e1)
    

gameLoop ::  EstadoJuego -> IO()
gameLoop (EstadoJuego _ ((n1,p1,_,_,_,_), (n2,p2,_,_,_,_)) _ termino) | termino && p1 > p2 && p1 >= 10 = do putStrLn ("Ganador: "++ n1 ++"!!!!") 
                                                                      | termino && p2 > p1 && p2 >= 10 = do putStrLn ("Ganador: "++ n2 ++"!!!!")
                                                                                                                                                                            
gameLoop estado@(EstadoJuego turno  ((n1,p1,_,_,_,e1), (n2,p2,_,_,_,e2)) _ termino) | termino && (p1 < 10 || p2 <10) = do 
    jugadores <- (repartirMazo mazo n1 n2 p1 p2 e1 e2)
    putStrLn ""
    putStrLn "////////////////// COMIENZA NUEVA MANO //////////////////"
    putStrLn ""
    gameLoop (EstadoJuego turno jugadores accionesIni False)                                                                
gameLoop estado = do
    proxEstado <- playerAgent (estado)
    putStrLn ""
    putStrLn "-------------------------------------------------------------------"
    putStrLn ""
    gameLoop (ganaPartida proxEstado)


