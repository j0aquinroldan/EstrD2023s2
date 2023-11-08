#include "LinkedList.h"
#include <iostream>
using namespace std;

// 1.
int sumatoria(LinkedList xs)
{
    // PROP:Devuelve la suma de todos los elementos.
    ListIterator ixs = getIterator(xs);
    int res = 0;
    while (!atEnd(ixs))
    {
        res += current(ixs);
        Next(ixs);
    }
    DisposeIterator(ixs);
    return res;
}

// 2.
void Sucesores(LinkedList xs)
{
    // PROP:Incrementa en uno todos los elementos.
    //????????????????????????
    ListIterator ixs = getIterator(xs);

    while (!atEnd(ixs))
    {
        SetCurrent(current(ixs) + 1, ixs);
        Next(ixs);
    }
    DisposeIterator(ixs);
}

// 3.
bool pertenece(int x, LinkedList xs)
{
    // PROP:Indica si el elemento pertenece a la lista.
    ListIterator ixs = getIterator(xs);
    bool res = false;
    while (!res && !atEnd(ixs))
    {
        if (current(ixs) == x)
        {
            res = true;
        }
        Next(ixs);
    }
    DisposeIterator(ixs);
    return res;
}

// 4.
int apariciones(int x, LinkedList xs)
{
    // PROP:Indica la cantidad de elementos iguales a x.

    ListIterator ixs = getIterator(xs);
    int res = 0;
    while (!atEnd(ixs))
    {
        if (current(ixs) == x)
        {
            res++;
        }
        Next(ixs);
    }
    DisposeIterator(ixs);
    return res;
}

// 5.
int minimo(LinkedList xs)
{
    // PROP:Devuelve el elemento más chico de la lista.
    // COSTO: O(n), siendo n la cantidad de elementos en la lista
    ListIterator ixs = getIterator(xs);
    int min = current(ixs);
    while (!atEnd(ixs))
    {
        min = std::min(min, current(ixs));
        Next(ixs);
    }
    DisposeIterator(ixs);
    return min;
}

// 6.
LinkedList copy(LinkedList xs)
{
    // PROP:Dada una lista genera otra con los mismos elementos, en el mismo orden.
    // COSTO: O(n^2) siendo n la cantidad de elementos de xs
    // Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría serlo?
    // para que mejore el costo de snoc habria que agregar un campo NodoL *ultimo,
    // para asi no tener que recorrer toda la lista buscando donde insertar,
    // sino directamente insertar el nuevo al siguiente del ultimo

    LinkedList xs2 = nil();
    ListIterator ixs = getIterator(xs);
    while (!atEnd(ixs))
    {
        Snoc(current(ixs), xs2);
        next(ixs);
    }
    DisposeIterator(ixs);
    return xs2;
}

// 7.
void Append(LinkedList xs, LinkedList ys)
{
    // PROP:Agrega todos los elementos de la segunda lista al final de los de la primera.
    // La segunda lista se destruye.
    // Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría serlo? agregar el ultimo al header
    ListIterator iys = getIterator(ys);
    while (!atEnd(iys))
    {
        Snoc(current(iys),xs);
        Next(iys);
    }
    DisposeIterator(iys);
    DestroyL(ys);
    
}