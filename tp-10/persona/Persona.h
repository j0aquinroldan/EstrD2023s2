#include <string>

struct PersonaSt
{
    std::string nombre;
    int edad;
};

typedef PersonaSt* Persona;

Persona consPersona(std::string nombre, int edad);
std::string nombre(Persona p);
int edad(Persona p);
void crecer(Persona p);
void cambioDeNombre(std::string nombre, Persona p);
bool esMayorQueLaOtra(Persona p1, Persona p2);
Persona laQueEsMayor(Persona p1, Persona p2);
