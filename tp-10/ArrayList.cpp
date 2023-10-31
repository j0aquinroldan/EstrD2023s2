#include <iostream>
#include "ArrayList.h"
using namespace std;

ArrayList newArrayList()
{

    ArrayList newArray = new ArrayListSt;
    newArray->cantidad = 0;
    newArray->capacidad = 16;
    newArray->elementos = new int[16];
}

ArrayList newArrayListWith(int capacidad)
{

    if (capacidad <= 0)
    {
        perror("incongruencia");
    }

    ArrayList newArray = new ArrayListSt;
    newArray->cantidad = 0;
    newArray->capacidad = capacidad;
    newArray->elementos = new int[capacidad];
}

int lengthAL(ArrayList xs)
{
    return xs->cantidad;
}

int get(int i, ArrayList xs)
{
    if (i >= xs->cantidad)
    {
        perror("no existe ese indice");
    }

    return xs->elementos[i];
}

void set(int i, int x, ArrayList xs)
{

    xs->elementos[i] = x;
}

void resize(int c, ArrayList xs)
{
    int *elemNuevo = new int[c];
    for (int i = 0; i < min(c, xs->cantidad); i++)
    {
        elemNuevo[i] = xs->elementos[i];
    }
    delete (xs->elementos);
    xs->elementos = elemNuevo;
    xs->capacidad = c;
    xs->cantidad = min(c, xs->cantidad);
}

void add(int x, ArrayList xs)
{
    if (xs->cantidad == xs->capacidad)
    {
        duplicarCapacidad(xs);
    }

    xs->elementos[xs->cantidad] = x;
    xs->cantidad++;
}

void duplicarCapacidad(ArrayList xs)
{
    int *elemNuevo = new int[xs->capacidad * 2];
    for (int i = 0; i < xs->cantidad; i++)
    {
        elemNuevo[i] = xs->elementos[i];
    }
    delete (xs->elementos);
    xs->elementos = elemNuevo;
    xs->capacidad *= 2;
}

void remove(ArrayList xs)
{
    if (xs->cantidad == 0)
    {
        perror("array vacio");
    }
    xs->cantidad--;
}