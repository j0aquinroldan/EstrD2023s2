#include "Set.h"
#include <iostream>

struct NodoS
{
    int elem;         // valor del nodo
    NodoS *siguiente; // puntero al siguiente nodo
};

struct SetSt
{
    int cantidad;   // cantidad de elementos diferentes
    NodoS *primero; // puntero al primer nodo
};

/////////////////////////////////////////////////////////////////////////////////////////

Set emptyS()
{
    // Crea un conjunto vacío.
    Set s = new SetSt;
    s->cantidad = 0;
    s->primero = NULL;
    return s;
}

bool isEmptyS(Set s)
{
    // Indica si el conjunto está vacío.
    return s->cantidad == 0;
}

bool belongsS(int x, Set s)
{
    // Indica si el elemento pertenece al conjunto.
    NodoS *actual = s->primero;
    bool res = false;
    while (!res && actual != NULL)
    {
        res = actual->elem == x;
        actual = actual->siguiente;
    }
    delete actual;
    return res;
}

NodoS* lastNode(int x){
    NodoS* n = new NodoS;
    n->elem = x;
    n->siguiente= NULL;
    return n;
}

void AddS(int x, Set s)
{
    // Agrega un elemento al conjunto.
    NodoS *actual = s->primero;
    while (actual->siguiente != NULL)// mientras no sea el ultimo el elemento
    {
        if (actual->elem == x)      // si el elemento actual coincide sali
        {
            break;
        }
        
        actual = actual->siguiente;
    }

    actual->siguiente = lastNode(x);
}


void AddS2(int x, Set s)
{
    // Agrega un elemento al conjunto.
    NodoS* actual = s->primero;
    while (actual->siguiente != NULL)
    {
        actual = actual->siguiente;
    }

    
}

void RemoveS(int x, Set s)
{
    // Quita un elemento dado.
}

int sizeS(Set s)
{
    // Devuelve la cantidad de elementos.
}

LinkedList setToList(Set s)
{
    // Devuelve una lista con los lementos del conjunto.
}

void DestroyS(Set s)
{
    // Libera la memoria ocupada por el conjunto.
}
