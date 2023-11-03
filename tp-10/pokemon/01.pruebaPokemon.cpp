#include "Pokemon.h"
#include <string>
#include <iostream>
using namespace std;

void testConsTipoEnergia()
{

    Pokemon p = consPokemon("Agua");
    cout << (tipo(p) == "Agua") << endl;
    cout << (energia(p) == 100) << endl
         << endl;
}

void testperderEnergia()
{

    Pokemon p = consPokemon("Agua");
    perderEnergia(50, p);
    cout << (energia(p) == 50) << endl
         << endl;
}

void testSuperaA()
{

    Pokemon p1 = consPokemon("Agua");
    Pokemon p2 = consPokemon("Fuego");
    Pokemon p3 = consPokemon("Planta");
    cout << superaA(p1, p2) << endl;
    cout << superaA(p2, p3) << endl;
    cout << superaA(p3, p1) << endl;
    
    cout << superaA(p2, p1) << endl;
    cout << superaA(p3, p2) << endl;
    cout << superaA(p1, p3) << endl;
}

int main()
{
    cout << boolalpha;
    testConsTipoEnergia();
    testperderEnergia();
    testSuperaA();
}