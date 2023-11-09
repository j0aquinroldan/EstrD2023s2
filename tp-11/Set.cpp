#include "Set.h"
#include <iostream>
using namespace std;

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

NodoS *lastNode(int x)
{
    NodoS *n = new NodoS;
    n->elem = x;
    n->siguiente = NULL;
    return n;
}

void AddS(int x, Set s)
{
    // Agrega un elemento al conjunto.

    NodoS *newN = new NodoS; // Crear un nuevo nodo
    newN->elem = x;
    newN->siguiente = NULL;

    // Si el conjunto está vacío, el nuevo nodo va primero y no chequeo si ya esta
    if (s->primero == NULL)
    {
        s->primero = newN;
        s->cantidad++;
        return;
    }

    // si no esta vacio, busco que ningun nodo coincida con el nuevo y busco el último nodo de la lista
    NodoS *temp = s->primero;

    while (temp->siguiente != NULL)
    { // si temp->siguiente ==NULL => es el ultimo nodo
        if (temp->siguiente->elem == x)
        { // si el siguiente es repetido no lo agrega
            delete newN;
            return; // retorna ya que encontro un elemento que coincide con el nuevo => no lo agrega
        }
        temp = temp->siguiente;
    }

    if (temp->elem == x) // si el actual es repetido no lo agrega
    {
        delete newN;
        return; // retorna ya que encontro un elemento que coincide con el nuevo => no lo agrega
    }
    // actualizar el puntero del antiguo último nodo e incrementar cantidad;
    temp->siguiente = newN;
    s->cantidad++;
} // hay que mejorar y hacerlo mas prolijo

void RemoveS(int x, Set s)
{
    // Quita un elemento dado.

    if (s->primero == NULL)
    {
        return;
    }
    NodoS *temp = s->primero;

    while (temp->siguiente != NULL)
    { // si temp->siguiente ==NULL => es el ultimo nodo
        if (temp->siguiente->elem == x)
        {
            NodoS *temp2 = temp->siguiente;
            temp->siguiente = temp->siguiente->siguiente; // hay que liberar el siguiente anterior???
            s->cantidad--;
            delete temp2;
            return;
        }
        temp = temp->siguiente;
    }
}

int sizeS(Set s)
{
    // Devuelve la cantidad de elementos.
    return s->cantidad;
}

LinkedList setToList(Set s)
{
    // Devuelve una lista con los lementos del conjunto.
    LinkedList li = nil();
    NodoS *temp = s->primero;
    while (temp != NULL)
    {
        Snoc(temp->elem, li);
        temp = temp->siguiente;
    }
    return li;
}

void DestroyS(Set s)
{
    // Libera la memoria ocupada por el conjunto.
    NodoS *temp = s->primero;
    while (temp != NULL)
    {
        s->primero = s->primero->siguiente;
        delete temp;
    }
    delete s;
}
