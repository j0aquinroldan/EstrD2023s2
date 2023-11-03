#include "Entrenador.h"
#include <string>




Entrenador consEntrenador(std::string nombre, int cantidad, Pokemon* pokemon){
//PROP: Dado un nombre, una cantidad de pokémon, y un array de pokémon de ese tamaño, 
//devuelve un entrenador.
    Entrenador e = new EntrenadorSt;
    e->nombre=nombre;
    e->cantPokemon = cantidad;
    e->pokemon=pokemon;

    return e;
}

std::string nombreDeEntrenador(Entrenador e){
//PROP:Devuelve el nombre del entrenador.
    return e->nombre;

}

int cantidadDePokemon(Entrenador e){
//PROP:Devuelve la cantidad de pokémon que posee el entrenador.

    return e->cantPokemon;
}



int unoSi(bool b)
{
    if (b)
    {
        return 1;
    }
    return 0;
}

int cantidadDePokemonDe(TipoDePokemon tipo2, Entrenador e){
//PROP:Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.

    int res = 0;

    for (int i = 0; i < e->cantPokemon; i++)
    {
        res += unoSi(tipo(e->pokemon[i]) == tipo2);
    }
    return res;
    
}



Pokemon pokemonNro(int i, Entrenador e){
//PROP:Devuelve el pokémon número i de los pokémon del entrenador.
//PREC: existen al menos i − 1 pokémon.
    if (i >= e->cantPokemon )
    {
        perror("no existe pokemon con ese numero");
    }
    else
    {
        return e->pokemon[i];
    }
    
}

bool leGanaA(Entrenador e, Pokemon p){
//PROP: indica si el entrenador tiene al menos 1 pokemon que le gane al dado por parametro
    bool res = false;
    int i=0;
    while (!res && i < e->cantPokemon)
    {
        res = superaA(e->pokemon[i], p);
        i++;
    }
    
    return res;
    

}

bool leGanaATodos(Entrenador e1, Entrenador e2){
//PROP:Dados dos entrenadores, indica si, para cada pokémon del segundo entrenador, el primero
//posee al menos un pokémon que le gane.
    bool res = true;
    int i=0;
    while (res && i < e1->cantPokemon)
    {
        res = leGanaA(e1, e2->pokemon[i]);
        i++;
    }
    
    return res;
}