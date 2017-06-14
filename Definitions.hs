module Definitions where
-------------------------------   DEFINITIONS -------------------------------

data Palo =  Copa | Oro | Basto | Espada deriving (Eq,Enum, Read, Show)
data Valor = Cuatro|Cinco|Seis|Diez|Once|Doce|Siete|Uno|Dos|Tres deriving (Eq,Ord,Enum,Show,Read)
type Carta = (Valor,Palo)
type Mano = [Carta]
type Jugadas = [Carta]
type Mazo = [Carta]

data Accion = Truco | ReTruco | ValeCuatro | Flor | ContraFlor |Tiene| Envido | RealEnvido | Tirar | Quiero | NoQuiero | MeVoy deriving(Eq,Ord,Show)
type Acciones = [Accion]
                    
type Jugador = (String,Int,Mano,Jugadas,Acciones,Bool)
data Turno= Jugador1 | Jugador2 deriving (Eq, Show)
type PlayerAgent = EstadoJuego -> IO EstadoJuego
data EstadoJuego = EstadoJuego Turno (Jugador, Jugador) Acciones Bool deriving (Eq, Show)