module GameLogic where
import Definitions
import System.Random
import Actions
import Data.List
-------------------------------   GAME LOGIC -------------------------------  
accionesIni = [Truco,Envido,Tirar,MeVoy]
mazo:: IO Mazo
mazo = return [(x,y)| x <- [Cuatro .. Tres], y <- [Copa .. Espada]]

obtenerMano :: Mazo -> IO Mazo 
obtenerMano m = do
    g <- newStdGen
    let c1= (take 1 [m !! i | i <- randomRs (0, (length m) - 1) g])
    let m2=(m \\ c1)
    let c2= (take 1 [m2 !! i | i <- randomRs (0, (length m2) - 1) g])
    let m3=(m2 \\ c2)
    let c3= (take 1 [m3 !! i | i <- randomRs (0, (length m3) - 1) g])
    return (c1++c2++c3)
    
repartirMazo :: IO Mazo -> String -> String -> Int -> Int -> Bool -> Bool -> IO (Jugador,Jugador)
repartirMazo m n1 n2 p1 p2 e1 e2 = do
    m  <- mazo
    j1 <- obtenerMano m 
    j2 <- obtenerMano (m \\ j1) 
    return ((n1,p1,j1,[],[],e1), (n2,p2,j2,[],[],e2))
ganaMano :: Carta -> Carta -> Int
ganaMano (Uno, Espada) _ = 1
ganaMano _ (Uno, Espada)= 2
ganaMano (Uno, Basto) _ = 1
ganaMano _ (Uno, Basto)= 2
ganaMano (Siete, Espada) _ = 1
ganaMano _ (Siete, Espada)= 2
ganaMano (Siete, Oro) _ = 1
ganaMano _ (Siete, Oro) = 2
ganaMano (v1, _ ) (v2, _ ) | v1 == v2 = 0
ganaMano (v1, _ ) (v2, _ ) | v1 > v2 = 1
ganaMano _ _ = 2

puntosCartas :: Carta -> Int
puntosCartas (Diez,x) = 0
puntosCartas (Once,x) = 0
puntosCartas (Doce,x) = 0
puntosCartas (Uno,x) = 1
puntosCartas (Dos,x) = 2
puntosCartas (Tres,x) = 3
puntosCartas (Cuatro,x) = 4
puntosCartas (Cinco,x) = 5
puntosCartas (Seis,x) = 6
puntosCartas (Siete,x) = 7

calcularEnvido :: Mano -> Int
calcularEnvido [(v1,p1), (v2,p2), (v3,p3)] | (p1 == p2) && (p2 == p3) = 20 + puntosCartas (v1,p1) + puntosCartas (v2,p2) + puntosCartas (v3,p3)
{-Dos cartas del mismo palo-}
                                           | (p1 == p2) && (p2 /= p3) = 20 + puntosCartas (v1,p1) + puntosCartas (v2,p2)
                                           | (p1 /= p2) && (p2 == p3) = 20 + puntosCartas (v3,p3) + puntosCartas (v2,p2)
                                           | (p1 /= p2) && (p1 == p3) = 20 + puntosCartas (v1,p1) + puntosCartas (v3,p3)
{-Tres cartas distintas-}
                                           | puntosCartas (v1,p1) >= puntosCartas (v2,p2) && puntosCartas (v1,p1) > puntosCartas (v3,p3) = puntosCartas (v1,p1)
                                           | puntosCartas (v2,p2) >= puntosCartas (v1,p1) && puntosCartas (v2,p2) > puntosCartas (v3,p3) = puntosCartas (v2,p2)
                                           | otherwise = puntosCartas (v3,p3)
calcularEnvido _ = 0

tieneFlor :: Mano -> Bool
tieneFlor [(_,p1), (_,p2) ,(_,p3)] | (p1 == p2) && (p2 == p3) = True
                               | otherwise = False

calcularFlor :: Mano -> Int
calcularFlor [c1, c2, c3] | tieneFlor [c1, c2, c3] == False = 0
                      | otherwise = 20 + puntosCartas c1 + puntosCartas c2 + puntosCartas c3

{- Sumarle puntos al jugador, depende de la accion, se sabe que ganó -}
sumarPuntaje :: Jugador -> Accion -> Jugador
sumarPuntaje (n,puntaje,c,js,acciones,esMano) Flor = (n,puntaje + 3, c,js,acciones,esMano)
sumarPuntaje (n,puntaje,c,js,acciones,esMano) ContraFlor = (n,puntaje + 18, c,js,acciones,esMano)
sumarPuntaje (n,puntaje,c,js,acciones,esMano) Envido = (n,puntaje + 2, c,js,acciones,esMano)
sumarPuntaje (n,puntaje,c,js,acciones,esMano) RealEnvido = (n,puntaje + 3, c,js,acciones,esMano)
sumarPuntaje (n,puntaje,c,js,acciones,esMano) NoQuiero = (n,puntaje + 1, c,js,acciones,esMano)
sumarPuntaje j@(n,puntaje,c,js,acciones,esMano) _ = j

actualizarPuntos (EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) accion | (elem Envido ultimaAccionOponente) || (elem RealEnvido ultimaAccionOponente) = do
    if (accion == NoQuiero) then do
        let jAct = sumarPuntaje jn accion
        devolver <- actualizarPuntosAux (EstadoJuego turno (j1,j2) acciones termino) jAct accion
        return devolver
    else do
        let envidoJugadorTurno= calcularEnvido m1
        let envidoJugadorNoTurno = calcularEnvido m2
        if(envidoJugadorTurno > envidoJugadorNoTurno) then do
            let jAct=sumarPuntaje jn (ultimaAccionOponente !! 0)
            devolver <- actualizarPuntosAux (EstadoJuego turno (j1,j2) acciones termino) jAct accion
            return devolver
        else do
            if(envidoJugadorTurno == envidoJugadorNoTurno) then do
                if (e1) then do
                    let jAct = sumarPuntaje jn (ultimaAccionOponente !! 0)
                    devolver <- actualizarPuntosAux (EstadoJuego turno (j1,j2) acciones termino) jAct accion
                    return devolver
                else do
                    let jAct = sumarPuntaje jugadorAct (ultimaAccionOponente !! 0)
                    devolver <- actualizarPuntosAux (EstadoJuego turno (j1,j2) acciones termino) jAct accion
                    return devolver
            else do
                let jAct=sumarPuntaje jugadorAct (ultimaAccionOponente !! 0)
                devolver <- actualizarPuntosAux (EstadoJuego turno (j1,j2) acciones termino) jAct accion
                return devolver
    where {
        jn =jugadorNoTurno turno (j1,j2);
        jugadorAct = jugadorTurno turno (j1,j2);
        ultimaAccionOponente = ultimaAccionDicha jn;
    }

actualizarPuntos (EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) Flor | (elem Flor ultimaAccionOponente)= do
    let florJugadorTurno= calcularFlor m1
    let florJugadorNoTurno = calcularFlor m2
    
    if(florJugadorTurno > florJugadorNoTurno) then do
        let jAct=sumarPuntaje (sumarPuntaje jugadorAct Flor) Flor
        devolver <- actualizarPuntosAux (EstadoJuego turno (j1,j2) acciones termino) jAct Flor
        return devolver
    else do
        let jAct=sumarPuntaje (sumarPuntaje jn (ultimaAccionOponente !! 0)) (ultimaAccionOponente !! 0) 
        devolver <- actualizarPuntosAux (EstadoJuego turno (j1,j2) acciones termino) jAct Flor
        return devolver
    
    where {
        jn =jugadorNoTurno turno (j1,j2);
        jugadorAct = jugadorTurno turno (j1,j2);
        ultimaAccionOponente= ultimaAccionDicha jn;
    }

actualizarPuntos (EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) Tiene | (elem Flor ultimaAccionOponente) = do
    let jAct=sumarPuntaje jn (ultimaAccionOponente !! 0)
    devolver <- actualizarPuntosAux (EstadoJuego turno (j1,j2) acciones termino) jAct Tiene
    return devolver
    where {
        jn =jugadorNoTurno turno (j1,j2);
        jugadorAct = jugadorTurno turno (j1,j2);
        ultimaAccionOponente= ultimaAccionDicha jn;
    }
    
actualizarPuntos (EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) Quiero | (elem ContraFlor ultimaAccionOponente) = do
    let florJugadorTurno= calcularFlor m1
    let florJugadorNoTurno = calcularFlor m2

    if(florJugadorTurno > florJugadorNoTurno) then do
        let jAct=sumarPuntaje jugadorAct (ultimaAccionOponente !! 0)
        devolver <- actualizarPuntosAux (EstadoJuego turno (j1,j2) acciones termino) jAct Quiero
        return devolver
    else do
        let jAct=sumarPuntaje jn (ultimaAccionOponente !! 0)
        devolver <- actualizarPuntosAux (EstadoJuego turno (j1,j2) acciones termino) jAct Quiero
        return devolver
    where {
        jn =jugadorNoTurno turno (j1,j2);
        jugadorAct = jugadorTurno turno (j1,j2);
        ultimaAccionOponente= ultimaAccionDicha jn;
    }
    
actualizarPuntos (EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) NoQuiero | (elem ContraFlor ultimaAccionOponente) = do
    let jAct=sumarPuntaje (sumarPuntaje jn (ultimaAccionOponente !! 0)) (ultimaAccionOponente !! 0)  
    devolver <- actualizarPuntosAux (EstadoJuego turno (j1,j2) acciones termino) jAct NoQuiero
    return devolver
    where {
        jn =jugadorNoTurno turno (j1,j2);
        jugadorAct = jugadorTurno turno (j1,j2);
        ultimaAccionOponente= ultimaAccionDicha jn;
    }

actualizarPuntosAux :: EstadoJuego -> Jugador -> Accion -> IO EstadoJuego
actualizarPuntosAux (EstadoJuego turno (j1,j2) acciones _) jAct accion = do
    mostrarDesafiosGanados jAct (ultimaAccionOponente !! 0)
    let jugadores = actualizarJugador turno jAct (j1,j2)
    let proxTurnoEstado@(EstadoJuego t j a f) = proximoTurno (EstadoJuego turno jugadores acciones (isOver j1 j2))
    return (EstadoJuego t j (accionesDisp proxTurnoEstado accion) f)
    where {
        jn =jugadorNoTurno turno (j1,j2);
        ultimaAccionOponente= ultimaAccionDicha jn;
    }


actualizarJugador :: Turno -> Jugador -> (Jugador,Jugador) -> (Jugador,Jugador)
actualizarJugador (Jugador1) j (j1,j2) = (j,j2)
actualizarJugador (Jugador2) j (j1,j2) = (j1,j)



jugadorTurno :: Turno -> (Jugador,Jugador) -> Jugador
jugadorTurno (Jugador1) (j1 , j2) = j1
jugadorTurno (Jugador2) (j1 , j2) = j2

jugadorNoTurno :: Turno -> (Jugador,Jugador) -> Jugador
jugadorNoTurno (Jugador1) (j1 , j2) = j2
jugadorNoTurno (Jugador2) (j1 , j2) = j1


mostrarDesafiosGanados :: Jugador -> Accion -> IO ()
mostrarDesafiosGanados (nombre,puntaje,mano,jugadas,acciones,esMano) desafio = do
          putStrLn (nombre ++ " gana "++ show(desafio) ++ " -> su puntaje ahora es: "++(show(puntaje)))

proximoTurno :: EstadoJuego -> EstadoJuego
--J1 Tiradas > J2 tiradas
proximoTurno estado@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | length t1 > length t2 && length a1 >= length a2 = (EstadoJuego Jugador1 (j1, j2) acciones termino)
proximoTurno estado@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | length t1 > length t2 && length a1 < length a2 = (EstadoJuego Jugador2 (j1, j2) acciones termino)
--J2 Tiradas > J1 tiradas
proximoTurno estado@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | length t1 < length t2 && length a1 <= length a2 = (EstadoJuego Jugador1 (j1, j2) acciones termino)
proximoTurno estado@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | length t1 < length t2 && length a1 > length a2 = (EstadoJuego Jugador2 (j1, j2) acciones termino)


--J2 Tiradas == J1 tiradas J1 Acciones > J2 Acciones
proximoTurno estado@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | length t1 == length t2 && length a1 > length a2= (EstadoJuego Jugador2 (j1, j2) acciones termino)
--J2 Tiradas == J1 tiradas J1 Acciones < J2 Acciones
proximoTurno estado@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | length t1 == length t2 && length a1 < length a2= (EstadoJuego Jugador1 (j1, j2) acciones termino)
--J2 Tiradas == J1 tiradas J1 Acciones == J2 Acciones => juega el mano
proximoTurno estado@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | length t1 == length t2 && length a1 == length a2 && e1= (EstadoJuego Jugador1 (j1, j2) acciones termino)
proximoTurno estado@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | length t1 == length t2 && length a1 == length a2 && e2= (EstadoJuego Jugador2 (j1, j2) acciones termino)

isOver :: Jugador -> Jugador -> Bool
isOver (_, p1, _, _, _, _) (_, p2, _, _, _, _) | p1 == 18 = True
isOver (_, p1, _, _, _, _) (_, p2, _, _, _, _) | p2 == 18 = True
                                               | otherwise = False


ganaPartida :: EstadoJuego -> EstadoJuego
--J1 DIJO TRUCO Y J2 NO QUISO J1 VA A SER MANO
ganaPartida ej@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | (dijeAccion a1 Truco) && (dijeAccion a2 NoQuiero) && (not e1) = (EstadoJuego Jugador1 ((n1,p1+(calcularPuntajeTEV ej),m1,[],[],(not e1)), (n2,p2,m2,[],[],(not e2))) accionesIni True)
--J1 DIJO RETRUCO Y J2 NO QUISO J1 VA A SER MANO
ganaPartida ej@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | (dijeAccion a1 ReTruco) && (dijeAccion a2 NoQuiero) && (not e1) = (EstadoJuego Jugador1 ((n1,p1+(calcularPuntajeTEV ej),m1,[],[],(not e1)), (n2,p2,m2,[],[],(not e2))) accionesIni True)
--J1 DIJO Valecuatro Y J2 NO QUISO J1 VA A SER MANO
ganaPartida ej@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | (dijeAccion a1 ValeCuatro) && (dijeAccion a2 NoQuiero) && (not e1) = (EstadoJuego Jugador1 ((n1,p1+(calcularPuntajeTEV ej),m1,[],[],(not e1)), (n2,p2,m2,[],[],(not e2))) accionesIni True)
--J1 DIJO TRUCO Y J2 NO QUISO J2 VA A SER MANO
ganaPartida ej@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | (dijeAccion a1 Truco) && (dijeAccion a2 NoQuiero) && (not e1) = (EstadoJuego Jugador2 ((n1,p1+(calcularPuntajeTEV ej),m1,[],[],(not e1)), (n2,p2,m2,[],[],(not e2))) accionesIni True)
--J1 DIJO RETRUCO Y J2 NO QUISO J2 VA A SER MANO
ganaPartida ej@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | (dijeAccion a1 ReTruco) && (dijeAccion a2 NoQuiero) && (not e1) = (EstadoJuego Jugador2 ((n1,p1+(calcularPuntajeTEV ej),m1,[],[],(not e1)), (n2,p2,m2,[],[],(not e2))) accionesIni True)
--J1 DIJO Valecuatro Y J2 NO QUISO J2 VA A SER MANO
ganaPartida ej@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | (dijeAccion a1 ValeCuatro) && (dijeAccion a2 NoQuiero) && (not e1) = (EstadoJuego Jugador2 ((n1,p1+(calcularPuntajeTEV ej),m1,[],[],(not e1)), (n2,p2,m2,[],[],(not e2))) accionesIni True)

--J2 DIJO TRUCO Y J1 NO QUISO J1 VA A SER MANO
ganaPartida ej@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | (dijeAccion a1 Truco) && (dijeAccion a2 NoQuiero) && (not e1) = (EstadoJuego Jugador1 ((n1,p1,m1,[],[],(not e1)), (n2,p2+(calcularPuntajeTEV ej),m2,[],[],(not e2))) accionesIni True)
--J2 DIJO RETRUCO Y J1 NO QUISO J1 VA A SER MANO
ganaPartida ej@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | (dijeAccion a1 ReTruco) && (dijeAccion a2 NoQuiero) && (not e1) = (EstadoJuego Jugador1 ((n1,p1,m1,[],[],(not e1)), (n2,p2+(calcularPuntajeTEV ej),m2,[],[],(not e2))) accionesIni True)
--J2 DIJO Valecuatro Y J1 NO QUISO J1 VA A SER MANO
ganaPartida ej@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | (dijeAccion a1 ValeCuatro) && (dijeAccion a2 NoQuiero) && (not e1) = (EstadoJuego Jugador1 ((n1,p1,m1,[],[],(not e1)), (n2,p2+(calcularPuntajeTEV ej),m2,[],[],(not e2))) accionesIni True)
--J2 DIJO TRUCO Y J1 NO QUISO J2 VA A SER MANO
ganaPartida ej@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | (dijeAccion a1 Truco) && (dijeAccion a2 NoQuiero) && (not e1) = (EstadoJuego Jugador2 ((n1,p1,m1,[],[],(not e1)), (n2,p2+(calcularPuntajeTEV ej),m2,[],[],(not e2))) accionesIni True)
--J2 DIJO RETRUCO Y J1 NO QUISO J2 VA A SER MANO
ganaPartida ej@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | (dijeAccion a1 ReTruco) && (dijeAccion a2 NoQuiero) && (not e1) = (EstadoJuego Jugador2 ((n1,p1,m1,[],[],(not e1)), (n2,p2+(calcularPuntajeTEV ej),m2,[],[],(not e2))) accionesIni True)
--J2 DIJO Valecuatro Y J1 NO QUISO J2 VA A SER MANO
ganaPartida ej@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | (dijeAccion a1 ValeCuatro) && (dijeAccion a2 NoQuiero) && (not e1) = (EstadoJuego Jugador2 ((n1,p1,m1,[],[],(not e1)), (n2,p2+(calcularPuntajeTEV ej),m2,[],[],(not e2))) accionesIni True)

-- JUGADOR2 SE FUE AL MAZO y le va a tocar a j1 ser mano
ganaPartida ej@(EstadoJuego turno (j1@(n1,p1,m1,t1,a1,e1), j2@(n2,p2,m2,t2,a2,e2)) acciones termino) | (dijeAccion a2 MeVoy) && (not e1) = (EstadoJuego Jugador1 ((n1,p1+(calcularPuntajeTEV ej),m1,[],[],(not e1)), (n2,p2,m2,[],[],(not e2))) accionesIni True)
-- JUGADOR2 SE FUE AL MAZO y le va a tocar a j2 ser mano
                                                                                                     | (dijeAccion a2 MeVoy) && (not e2) = (EstadoJuego Jugador2 ((n1,p1+(calcularPuntajeTEV ej),m1,[],[],(not e1)), (n2,p2,m2,[],[],(not e2))) accionesIni True)

-- JUGADOR1 SE FUE AL MAZO y le va a tocar a j1 ser mano
                                                                                                     | (dijeAccion a1 MeVoy) && (not e1)= (EstadoJuego Jugador1 ((n1,p1,m1,[],[],(not e1)), (n2,p2+(calcularPuntajeTEV ej),m2,[],[],(not e2))) accionesIni True)
-- JUGADOR1 SE FUE AL MAZO y le va a tocar a j2 ser mano
                                                                                                     | (dijeAccion a1 MeVoy) && (not e2)= (EstadoJuego Jugador2 ((n1,p1,m1,[],[],(not e1)), (n2,p2+(calcularPuntajeTEV ej),m2,[],[],(not e2))) accionesIni True)

--SE JUGARON 3 CARTAS EN LA ULTIMA GANO J1 -> SUMO LOS PUNTOS Y LIMPIO TIRADAS Y JUGADOR1 VA A SER MANO
                                                                                                     | largoIgualT1T2 && largoT1 == 3 && ganaManoLast == 1 && (not e1)= (EstadoJuego Jugador1 ((n1,p1+(calcularPuntajeTEV ej),m1,[],[],(not e1)), (n2,p2,m2,[],[],(not e2))) accionesIni True)
--SE JUGARON 3 CARTAS EN LA ULTIMA GANO J1 -> SUMO LOS PUNTOS Y LIMPIO TIRADAS Y JUGADOR2 VA A SER MANO
                                                                                                     | largoIgualT1T2 && largoT1 == 3 && ganaManoLast == 1 && (not e2)= (EstadoJuego Jugador2 ((n1,p1+(calcularPuntajeTEV ej),m1,[],[],(not e1)), (n2,p2,m2,[],[],(not e2))) accionesIni True)


--SE JUGARON 3 CARTAS EN LA ULTIMA GANO J2 -> SUMO LOS PUNTOS Y LIMPIO TIRADAS Y ACCIONES Y JUGADOR1 VA A SER MANO
                                                                                                     | largoIgualT1T2 && largoT1 == 3 && ganaManoLast == 2 && (not e1)= (EstadoJuego Jugador1 ((n1,p1,m1,[],[],(not e1)), (n2,p2+(calcularPuntajeTEV ej),m2,[],[],(not e2))) accionesIni True)
--SE JUGARON 3 CARTAS EN LA ULTIMA GANO J2 -> SUMO LOS PUNTOS Y LIMPIO TIRADAS Y ACCIONES Y JUGADOR2 VA A SER MANO
                                                                                                     | largoIgualT1T2 && largoT1 == 3 && ganaManoLast == 2 && (not e2)= (EstadoJuego Jugador2 ((n1,p1,m1,[],[],(not e1)), (n2,p2+(calcularPuntajeTEV ej),m2,[],[],(not e2))) accionesIni True)



--SE JUGARON 3 CARTAS EN LA ULTIMA FUE EMPARDA -> VEO QUIEN GANO PRIMERA SUMO LOS PUNTOS Y LIMPIO TIRADAS Y ACCIONES J1 tiene q ser mano en la prox
                                                                                                     | largoIgualT1T2 && largoT1 == 3 && ganaManoLast == 0 && (not e1) = if ganaManoHead == 1 then (EstadoJuego Jugador1 ((n1,p1+(calcularPuntajeTEV ej),m1,[],[],(not e1)), (n2,p2,m2,[],[],(not e2))) accionesIni False) else (EstadoJuego Jugador1 ((n1,p1,m1,[],[],(not e1)), (n2,p2+(calcularPuntajeTEV ej),m2,[],[],(not e2))) accionesIni True)
--SE JUGARON 3 CARTAS EN LA ULTIMA FUE EMPARDA -> VEO QUIEN GANO PRIMERA SUMO LOS PUNTOS Y LIMPIO TIRADAS Y ACCIONES J2 tiene q ser mano en la prox
                                                                                                     | largoIgualT1T2 && largoT1 == 3 && ganaManoLast == 0 && (not e2) = if ganaManoHead == 1 then (EstadoJuego Jugador2 ((n1,p1+(calcularPuntajeTEV ej),m1,[],[],(not e1)), (n2,p2,m2,[],[],(not e2))) accionesIni False) else (EstadoJuego Jugador2 ((n1,p1,m1,[],[],(not e1)), (n2,p2+(calcularPuntajeTEV ej),m2,[],[],(not e2))) accionesIni True)
 


--SE JUGARON 2 CARTAS, EN LA PRIMER MANO FUE PARDA Y SEGUNDA GANA J1 Y JUGADOR1 VA A SER MANO
                                                                                                     | largoIgualT1T2 && largoT1 == 2 && ganaManoHead == 0 && ganaManoLast == 1 && (not e1)= (EstadoJuego Jugador1 ((n1,p1+(calcularPuntajeTEV ej),m1,[],[],(not e1)), (n2,p2,m2,[],[],(not e2))) accionesIni True)
--SE JUGARON 2 CARTAS, EN LA PRIMER MANO FUE PARDA Y SEGUNDA GANA J1 Y JUGADOR2 VA A SER MANO
                                                                                                     | largoIgualT1T2 && largoT1 == 2 && ganaManoHead == 0 && ganaManoLast == 1 && (not e2)= (EstadoJuego Jugador2 ((n1,p1+(calcularPuntajeTEV ej),m1,[],[],(not e1)), (n2,p2,m2,[],[],(not e2))) accionesIni True)

--SE JUGARON 2 CARTAS, EN LA PRIMER MANO FUE PARDA Y SEGUNDA GANA J2 y JUGADOR1 VA A SER MANO
                                                                                                     | largoIgualT1T2 && largoT1 == 2 && ganaManoHead == 0 && ganaManoLast == 2 && (not e1)= (EstadoJuego Jugador1 ((n1,p1,m1,[],[],(not e1)), (n2,p2+(calcularPuntajeTEV ej),m2,[],[],(not e2))) accionesIni True)
--SE JUGARON 2 CARTAS, EN LA PRIMER MANO FUE PARDA Y SEGUNDA GANA J2 y JUGADOR2 VA A SER MANO
                                                                                                     | largoIgualT1T2 && largoT1 == 2 && ganaManoHead == 0 && ganaManoLast == 2 && (not e2)= (EstadoJuego Jugador2 ((n1,p1,m1,[],[],(not e1)), (n2,p2+(calcularPuntajeTEV ej),m2,[],[],(not e2))) accionesIni True)



--SE JUGARON 2 CARTAS, EN LA PRIMER MANO GANA J2 Y SEGUNDA GANA J2 y JUGADOR1 VA A SER MANO
                                                                                                     | largoIgualT1T2 && largoT1 == 2 && ganaManoHead == 2 && ganaManoLast == 2 && (not e1)= (EstadoJuego Jugador1 ((n1,p1,m1,[],[],(not e1)), (n2,p2+(calcularPuntajeTEV ej),m2,[],[],(not e2))) accionesIni True)
--SE JUGARON 2 CARTAS, EN LA PRIMER MANO GANA J2 Y SEGUNDA GANA J2 y JUGADOR2 VA A SER MANO
                                                                                                     | largoIgualT1T2 && largoT1 == 2 && ganaManoHead == 2 && ganaManoLast == 2 && (not e2)= (EstadoJuego Jugador2 ((n1,p1,m1,[],[],(not e1)), (n2,p2+(calcularPuntajeTEV ej),m2,[],[],(not e2))) accionesIni True)

--SE JUGARON 2 CARTAS, EN LA PRIMER MANO GANA J1 Y SEGUNDA GANA J1 y JUGADOR1 VA A SER MANO
                                                                                                     | largoIgualT1T2 && largoT1 == 2 && ganaManoHead == 1 && ganaManoLast == 1 && (not e1)= (EstadoJuego Jugador1 ((n1,p1+(calcularPuntajeTEV ej),m1,[],[],(not e1)), (n2,p2,m2,[],[],(not e2))) accionesIni True)
--SE JUGARON 2 CARTAS, EN LA PRIMER MANO GANA J1 Y SEGUNDA GANA J1 y JUGADOR2 VA A SER MANO
                                                                                                     | largoIgualT1T2 && largoT1 == 2 && ganaManoHead == 1 && ganaManoLast == 1 && (not e2)= (EstadoJuego Jugador2 ((n1,p1+(calcularPuntajeTEV ej),m1,[],[],(not e1)), (n2,p2,m2,[],[],(not e2))) accionesIni True)
-- SI NO SE DIO NINGUNO DE LOS CASOS ANTERIORES DEBE SERGUIR JUGANDO PORQUE NO SE TERMINO LA PARTIDA AUN ENTONCES DEVUELVO EL MISMO ESTADO JUEGO
                                                                                                     | otherwise = ej
                                                                                                     where
                                                                                                     {
                                                                                                         largoT1 = length t1;
                                                                                                         largoT2 = length t2;
                                                                                                         largoIgualT1T2 = largoT1 == largoT2;
                                                                                                         ganaManoLast = ganaMano (last t1) (last t2);
                                                                                                         ganaManoHead = ganaMano (head t1) (head t2)                                                                                                         
                                                                                                     }

{-Te devuelve la cantidad de puntos a sumar en base a si se canto ValeCuatro, ReTruco o Truco-}
calcularPuntajeTEV :: EstadoJuego -> Int
calcularPuntajeTEV (EstadoJuego turno ((_,_,_,_,a1,_), (_,_,_,_,a2,_)) _ _) | (dijeAccion (a1++a2) ValeCuatro) = 4
                                                                            | (dijeAccion (a1++a2) ReTruco) = 3
                                                                            | (dijeAccion (a1++a2) Truco) = 2
                                                                            | otherwise = 1