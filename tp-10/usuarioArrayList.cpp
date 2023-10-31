#include <iostream>
#include "ArrayList.cpp"
using namespace std;

int sumatoria(ArrayList xs)
{
    int acum = 0;

    for (int i = 0; i < lengthAL(xs); i++)
    {
        acum += get(i, xs);
    }
    return acum;
}

void sucesores (ArrayList xs){

    for (int i = 0; i < lengthAL(xs); i++)
    {
        set(i, get(i,xs)+1,xs);
    }
    
}