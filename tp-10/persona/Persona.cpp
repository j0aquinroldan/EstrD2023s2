#include <iostream>
#include <string>
#include "Persona.h"


Persona consPersona(std::string nombre, int edad){
//PROP:Devuelve una persona con el nombre y la edad dados
    Persona p = new PersonaSt;
    p->nombre = nombre;
    p->edad = edad;
    return p;
}

std::string nombre(Persona p){
//PROP:Devuelve el nombre de una persona
    return p->nombre;

}

int edad(Persona p){
//PROP:Devuelve la edad de una persona
    return p->edad;
}

void crecer(Persona p){
//PROP:Aumenta en uno la edad de la persona.
    p->edad++;
}

void cambioDeNombre(std::string nombre, Persona p){
//PROP:Modifica el nombre una persona.
    p->nombre = nombre;
}

bool esMayorQueLaOtra(Persona p1, Persona p2){
//PROP:Dadas dos personas indica si la primera es mayor que la segunda.
    return p1->edad > p2->edad;
}

Persona laQueEsMayor(Persona p1, Persona p2){
//PROP:Dadas dos personas devuelve a la persona que sea mayor.

    if (esMayorQueLaOtra(p1,p2))
    {
        return p1;
    }
    return p2;
}