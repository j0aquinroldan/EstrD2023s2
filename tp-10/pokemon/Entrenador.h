#include <string>
#include "Pokemon.h"

struct EntrenadorSt
{
    std::string nombre;
    Pokemon* pokemon;
    int cantPokemon;
};
typedef EntrenadorSt* Entrenador;

Entrenador consEntrenador(std::string nombre, int cantidad, Pokemon* pokemon);
std::string nombreDeEntrenador(Entrenador e);
int cantidadDePokemon(Entrenador e);
int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e);
Pokemon pokemonNro(int i, Entrenador e);
bool leGanaATodos(Entrenador e1, Entrenador e2);

