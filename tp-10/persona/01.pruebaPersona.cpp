#include "Persona.h"
#include <string>
#include <iostream>
using namespace std;

void testConsNombreEdad()
{
    Persona p = consPersona("joaq", 22);
    cout << (nombre(p) == "joaq") << endl;
    cout << (edad(p) == 22) << endl;
}

void testCrecer()
{
    Persona p = consPersona("joaq", 22);
    crecer(p);
    cout << (edad(p) == 23) << endl;
}

void testCambioDeNombre()
{
    Persona p = consPersona("joaq", 22);
    cambioDeNombre("pepe",p);
    cout << (nombre(p) == "pepe") << endl;
}



void testMayor()
{
    Persona p1 = consPersona("joaq", 22);
    Persona p2 = consPersona("pepe", 2);
    
    Persona mayor = laQueEsMayor(p2,p1);
    cout << esMayorQueLaOtra(p1,p2) << endl;
    cout << (nombre(mayor) == "joaq") << endl;
}

int main()
{

    cout << boolalpha;
    testConsNombreEdad();
    cout << endl;
    testCrecer();
    cout << endl;
    testCambioDeNombre();
    cout << endl;
    testMayor();
    cout << endl;
}