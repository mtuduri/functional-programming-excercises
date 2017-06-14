module Actions where
import Definitions



primeraMano :: Jugador -> Bool
primeraMano (_,_,_,_,[],_) = True
primeraMano (_,_,_,_,_,_) = False

dijeAccion :: Acciones -> Accion -> Bool
dijeAccion acciones accion | elem accion acciones = True
                           | otherwise = False



accionesDisp :: EstadoJuego -> Accion -> Acciones
accionesDisp (EstadoJuego _ _ acciones _) (Truco)      = [Quiero,NoQuiero,ReTruco,MeVoy]
accionesDisp (EstadoJuego _ _ acciones _) (ReTruco)    =  [Quiero,NoQuiero,ValeCuatro,MeVoy]
accionesDisp (EstadoJuego _ _ acciones _) (ValeCuatro) =  [Quiero,NoQuiero,MeVoy]
accionesDisp _ (Envido) = [Quiero,NoQuiero]
accionesDisp (EstadoJuego Jugador1 ((_,_,_,_,a1,_), _) _ _) (Flor)  = [Tiene,MeVoy]
                                                                   
accionesDisp (EstadoJuego Jugador2 (_, (_,_,_,_,a2,_)) _ _) (Flor)  = [Tiene,MeVoy]
                                                                   
accionesDisp (EstadoJuego _ _ _ _) (ContraFlor) = [Quiero,NoQuiero]
accionesDisp (EstadoJuego _ _ _ _) (Tiene)      = [Truco,Tirar]
accionesDisp estado@(EstadoJuego Jugador1 (j, _) _ _) (Quiero)  | hayAccion && (elem (Truco)      ultAccion) = [Tirar,MeVoy]
                                                         | hayAccion && (elem (ReTruco)    ultAccion) = [Tirar,MeVoy] 
                                                         | hayAccion && (elem (ValeCuatro) ultAccion) = [Tirar,MeVoy] 
                                                         | hayAccion && (elem (Envido)     ultAccion) = [Tirar,MeVoy]  ++ (gritoDisponible estado) 
                                                         | hayAccion && (elem (Tiene)      ultAccion) = [Tirar,MeVoy] 
                                                         | otherwise = []
                                                         where { 
                                                            ultAccion = ultimaAccionDicha j;
                                                            hayAccion = length (ultAccion) > 0
                                                         }
accionesDisp estado@(EstadoJuego Jugador2 (_, j) _ _) (Quiero)  | hayAccion && (elem (Truco)      ultAccion) = [Tirar,MeVoy]
                                                         | hayAccion && (elem (ReTruco)    ultAccion) = [Tirar,MeVoy] 
                                                         | hayAccion && (elem (ValeCuatro) ultAccion) = [Tirar,MeVoy] 
                                                         | hayAccion && (elem (Envido)     ultAccion) = [Tirar,MeVoy]  ++ (gritoDisponible estado) 
                                                         | hayAccion && (elem (Tiene)      ultAccion) = [Tirar,MeVoy] 
                                                         | otherwise = []
                                                         where { 
                                                            ultAccion = ultimaAccionDicha j;
                                                            hayAccion = length (ultAccion) > 0
                                                        }

accionesDisp estado (NoQuiero) = [Tirar,MeVoy] ++ (gritoDisponible estado)
accionesDisp estado@(EstadoJuego Jugador2 (j1@(_,_,_,_,a1,_),j2@(_,_,_,_,a2,_)) _ _) (Tirar) | primMano && (dijoEnvido || dijeEnvido) = [Tirar,MeVoy] ++ (gritoDisponible estado)
                                                                                             | primMano && (not (dijoEnvido) && not (dijeEnvido))  = [Tirar,Envido,MeVoy] ++ (gritoDisponible estado)
                                                                                             | not (primMano) = [Tirar,MeVoy] ++ (gritoDisponible estado)
                                                                                             where {
                                                                                                 dijeFlor = dijeAccion a2 (Flor);
                                                                                                 dijeEnvido = dijeAccion a2 (Envido);
                                                                                                 dijoFlor = dijeAccion a1 (Flor);
                                                                                                 dijoEnvido = dijeAccion a1 (Envido);
                                                                                                 primMano = primeraMano j2
                                                                                             }

accionesDisp estado@(EstadoJuego Jugador1 (j1@(_,_,_,_,a1,_),j2@(_,_,_,_,a2,_)) _ _) (Tirar) | primMano && (dijoEnvido || dijeEnvido) = [Tirar,MeVoy] ++ (gritoDisponible estado)
                                                                                             | primMano && (not (dijoEnvido) && not (dijeEnvido))  = [Tirar,Envido,MeVoy] ++ (gritoDisponible estado)
                                                                                             | not (primMano) = [Tirar,MeVoy] ++ (gritoDisponible estado)
                                                                                             where {
                                                                                                 dijeFlor = dijeAccion a1 (Flor);
                                                                                                 dijeEnvido = dijeAccion a1 (Envido);
                                                                                                 dijoFlor = dijeAccion a2 (Flor);
                                                                                                 dijoEnvido = dijeAccion a2 (Envido);
                                                                                                 primMano = primeraMano j1
                                                                                             }
accionesDisp _ _ = []



gritoDisponible :: EstadoJuego -> Acciones
gritoDisponible estado@(EstadoJuego Jugador1 (j1, j2) _ _) | jugUltGrito == j2 && length accUltGrito == 0 = [Truco]
                                                           | jugUltGrito == j1 && length accUltGrito == 0 = [Truco] {-bug-}
                                                           | jugUltGrito == j1 = []
                                                           | jugUltGrito == j2 && elem Truco accUltGrito = [ReTruco]
                                                           | jugUltGrito == j1 = []
                                                           | jugUltGrito == j2 && elem ReTruco accUltGrito = [ValeCuatro]
                                                           | jugUltGrito == j1 = []
                                                           where {
                                                               ultGrito = ultimoGrito estado;
                                                               jugUltGrito = snd(ultGrito);
                                                               accUltGrito = fst(ultGrito);
                                                               }
gritoDisponible estado@(EstadoJuego Jugador2 (j1, j2) _ _) | jugUltGrito == j1 && length (fst(ultimoGrito estado)) == 0 = [Truco]
                                                           | jugUltGrito == j2 = []
                                                           | jugUltGrito == j1 && elem Truco accUltGrito = [ReTruco]
                                                           | jugUltGrito == j2 = []
                                                           | jugUltGrito == j1 && elem ReTruco accUltGrito = [ValeCuatro]
                                                           | jugUltGrito == j2 = []
                                                           where {
                                                               ultGrito = ultimoGrito estado;
                                                               jugUltGrito = snd(ultGrito);
                                                               accUltGrito = fst(ultGrito);
                                                               }
gritoDisponible _ = []





ultimoGrito :: EstadoJuego -> (Acciones,Jugador)
ultimoGrito (EstadoJuego _ (j1@(_,_,_,_,a1,_), j2@(_,_,_,_,a2,_)) _ _) = (compararGritos gritoJ1 gritoJ2,j1)
                        where {
                              gritoJ1 = ultimoGritoAux (reverse a1);
                              gritoJ2 = ultimoGritoAux (reverse a2)
                              }

ultimoGritoAux :: Acciones -> Acciones
ultimoGritoAux (x:xs) | x == ValeCuatro = [ValeCuatro]
ultimoGritoAux (x:xs) | x == ReTruco = [ReTruco]
ultimoGritoAux (x:xs) | x == Truco = [Truco]
ultimoGritoAux (x:xs) = ultimoGritoAux xs
ultimoGritoAux [] = []


compararGritos :: Acciones -> Acciones -> Acciones
compararGritos (a1:as1) (a2:as2) | a1 < a2 = [a2] 
                                 | otherwise = [a1]
compararGritos (a1:as1) _ = [a1]
compararGritos _ (a2:as2) = [a2]
compararGritos _ _ = []


ultimaAccionDicha :: Jugador -> Acciones
ultimaAccionDicha (_,_,_,_,[],_) = []
ultimaAccionDicha (_,_,_,_,acciones,_) = [acciones !! ((length acciones) - 1)]