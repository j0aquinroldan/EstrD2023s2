#include "LinkedList.h"
#include <iostream>

struct NodoL
{
    int elem;         // valor del nodo
    NodoL *siguiente; // puntero al siguiente nodo
};
struct LinkedListSt
{
    // INV.REP.: cantidad indica la cantidad de nodos que se pueden recorrer
    // desde primero por siguiente hasta alcanzar a NULL
    int cantidad;   // cantidad de elementos
    NodoL *primero; // puntero al primer nodo
    NodoL *ultimo;  // puntero al ultimo nodo
};
// INV.REP.: el puntero NO es NULL

struct IteratorSt
{
    NodoL *current;
};
// INV.REP.: el puntero NO es NULL

/////////////////////////////////////////////////////////////////////////////////////////////////
// EJ 1

LinkedList nil()
{
    // PROP: Crea una lista vacía.
    // COSTO: O(1)
    LinkedList ls = new LinkedListSt;
    ls->cantidad = 0;
    ls->primero = NULL;
    ls->ultimo = NULL;
}

bool isEmpty(LinkedList xs)
{
    // PROP: Indica si la lista está vacía.
    // COSTO: O(1)
    return xs->cantidad == 0;
}

int head(LinkedList xs)
{
    // PROP: Devuelve el primer elemento.
    // COSTO: O(1)
    return xs->primero->elem;
}

void Cons(int x, LinkedList xs)
{
    // PROP: Agrega un elemento al principio de la lista.
    // COSTO: O(1)
    NodoL *newFirst = new NodoL;
    newFirst->elem = x;
    newFirst->siguiente = xs->primero;

    xs->primero = newFirst;
}

void Tail(LinkedList xs)
{
    // PROP: Quita el primer elemento.
    // COSTO: O(1)
    NodoL *temp = xs->primero; // se guarda el primero en una variable a parte para poder borrarlo una vez eliminado de la lista
                               // y asi se evita un memory leak
    xs->primero = xs->primero->siguiente;

    if (xs->primero == NULL)
    {
        xs->ultimo = NULL;
    }
    xs->cantidad--;
    delete temp;
}

int length(LinkedList xs)
{
    // PROP: Devuelve la cantidad de elementos.
    // COSTO: O(1)
    return xs->cantidad;
}

void Snoc(int x, LinkedList xs)
{
    // PROP: Agrega un elemento al final de la lista.
    // COSTO: O(n), siendo n la cantidad de elementos en la lista
    NodoL *n = new NodoL;
    n->elem = x;
    n->siguiente = NULL;

    if (xs->ultimo != NULL)
    {
        xs->ultimo->siguiente = n;
    }
    else
    {
        xs->primero = n;
    }

    xs->ultimo = n;
    xs->cantidad++;
}

ListIterator getIterator(LinkedList xs)
{
    // PROP: Apunta el recorrido al primer elemento.
    // COSTO: O(1)
    ListIterator li = new IteratorSt;
    li->current = xs->primero;
    return li;
}

int current(ListIterator ixs)
{
    // PROP: Devuelve el elemento actual en el recorrido.
    // COSTO: O(1)
    return ixs->current->elem;
}

void SetCurrent(int x, ListIterator ixs)
{
    // PROP: Reemplaza el elemento actual por otro elemento.
    // COSTO: O(1)
    NodoL *n = new NodoL;
    n->elem = x;
    n->siguiente = ixs->current->siguiente;
    ixs->current = n;
}

void Next(ListIterator ixs)
{
    // PROP: Pasa al siguiente elemento.
    // COSTO: O(1)
    ixs->current = ixs->current->siguiente;
}

bool atEnd(ListIterator ixs)
{
    // PROP: Indica si el recorrido ha terminado.
    // COSTO: O(1)
    return (ixs->current == NULL);
}

void DisposeIterator(ListIterator ixs)
{
    // PROP: Libera la memoria ocupada por el iterador.
    // COSTO: O(1)
    delete ixs;
}

void DestroyL(LinkedList xs)
{
    // PROP: Libera la memoria ocupada por la lista.
    // COSTO: O(n), siendo n la cantidad de elementos en la lista
    NodoL *temp = xs->primero;
    while (xs->primero != NULL)
    {
        xs->primero = xs->primero->siguiente;
        delete temp;
    }
    delete xs;
}

void Append(LinkedList xs, LinkedList ys)
{
    // PROP:Agrega todos los elementos de la segunda lista al final de los de la primera.
    // La segunda lista se destruye.
    // Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría serlo? agregar el ultimo al header
    ListIterator iys = getIterator(ys);
    while (!atEnd(iys))
    {
        Snoc(current(iys), xs);
        Next(iys);
    }
    DisposeIterator(iys);
    DestroyL(ys);
}