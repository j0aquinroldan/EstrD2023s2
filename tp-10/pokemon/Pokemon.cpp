#include <iostream>
#include "Pokemon.h"
using namespace std;

Pokemon consPokemon(TipoDePokemon tipo)
// PROP:Dado un tipo devuelve un pokémon con 100 % de energía.
{
    Pokemon p = new PokeSt;
    p->tipo = tipo;
    p->vida = 100;
}

TipoDePokemon tipo(Pokemon p)
// PROP:Devuelve el tipo de un pokémon.
{
    return p->tipo;
}

int energia(Pokemon p)
// PROP:Devuelve el porcentaje de energía.
{
    return p->vida;
}

void perderEnergia(int energia, Pokemon p)
// PROP:Le resta energía al pokémon.
{
    p->vida -= energia;
}

bool superaA(Pokemon p1, Pokemon p2)
// PROP:Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo.
// Agua supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
{

    TipoDePokemon tipoP1 = p1->tipo;
    TipoDePokemon tipoP2 = p2->tipo;

    return (tipoP1 == "Agua" && tipoP2 == "Fuego") ||
           (tipoP1 == "Fuego" && tipoP2 == "Planta") ||
           (tipoP1 == "Planta" && tipoP2 == "Agua");
}