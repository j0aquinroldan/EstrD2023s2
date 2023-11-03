
#include "Entrenador.h"
#include <string>
#include <iostream>
using namespace std;

Pokemon p = consPokemon("Agua");
Pokemon p2 = consPokemon("Fuego");
Pokemon p3 = consPokemon("Planta");
Pokemon p4 = consPokemon("Fuego");
Pokemon ps[4] = {p, p2, p3, p4};

Entrenador e = consEntrenador("pepe", 4, ps);

Pokemon ps2[4] = {p, p, p, p};

Entrenador e2 = consEntrenador("ash", 4, ps2);


void testConsNombreCant()
{

    cout << (nombreDeEntrenador(e) == "pepe") << endl;
    cout << (cantidadDePokemon(e) == 4) << endl;
    cout << (cantidadDePokemonDe("Planta", e) == 1) << endl;
    cout << (cantidadDePokemonDe("Agua", e) == 1) << endl;
    cout << (cantidadDePokemonDe("Fuego", e) == 2) << endl
         << endl;
}

void testPokemonNumero()
{
    cout << (pokemonNro(0, e) == p) << endl;
    cout << (pokemonNro(1, e) == p2) << endl;
    cout << (pokemonNro(2, e) == p3) << endl;
    cout << (pokemonNro(3, e) == p4) << endl
         << endl;

    cout << pokemonNro(4, e) << endl
         << endl;
}

void testLeGanaATodos()
{

    cout << leGanaATodos(e, e2) << endl;
    cout << !leGanaATodos(e2, e) << endl;



}

int main()
{
    cout << boolalpha;
    testConsNombreCant();
    testPokemonNumero();
    testLeGanaATodos();
}
