#include "Tree.h"
//#include <algorithm.h>
#include "ArrayList.h"
using namespace std;
#include<algorithm>


ArrayList Append(ArrayList a1, ArrayList a2){

    for (int i = 0; i < a2->cantidad; i++)
    {
        add(get(i,a2), a1);
        
    }
    delete a2;
    return a1;
}

// 1.
int sumarT(Tree t)
{
    // Dado un árbol binario de enteros devuelve la suma entre sus elementos.
    if (isEmptyT(t))
    {
        return 0;
    }
    return rootT(t) + sumarT(left(t)) + sumarT(right(t));
}

// 2.
int sizeT(Tree t)
{
    // Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
    // en inglés).
    if (isEmptyT(t))
    {
        return 0;
    }
    return 1 + sizeT(left(t)) + sizeT(right(t));
}

// 3.
bool perteneceT(int e, Tree t)
{
    // Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
    // árbol.
    if (isEmptyT)
    {
        return false;
    }
    return rootT(t) == e || perteneceT(e, left(t)) || perteneceT(e, right(t));
}

// 4.
int aparicionesT(int e, Tree t)
{
    // Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
    // iguales a e.
    if (isEmptyT(t))
    {
        return 0;
    }
    if (rootT(t) == e)
    {
        return 1 + aparicionesT(e, left(t)) + aparicionesT(e, right(t));
    }
    else
    {
        return aparicionesT(e, left(t)) + aparicionesT(e, right(t));
    }
}

// 5.
int heightT(Tree t)
{
    // Dado un árbol devuelve su altura.
    if (isEmptyT(t))
    {
        return 0;
    }
    else
    {
        return (1 + max(heightT(left(t)), heightT(right(t))));
    }
}


// 6.
ArrayList toList(Tree t){
// Dado un árbol devuelve una lista con todos sus elementos.
    if (isEmptyT(t))
    {
        return newArrayList();
    }
    else
    {
        return  Append(toList(left(t)), toList(right(t)));////falta append
    }
}

// 7.
ArrayList leaves(Tree t){
// Dado un árbol devuelve los elementos que se encuentran en sus hojas.
    ArrayList al = newArrayList();
    if (isEmptyT(t))
    {
        return al;
    }
    if (isEmptyT(left(t)) && isEmptyT(right(t)))
    {
        add(rootT(t), al);
        return al;
    }
    return Append(leaves(left(t)), leaves(right(t))); 
    
}


// 8.
ArrayList levelN(int n, Tree t){
// Dados un número n y un árbol devuelve una lista con los nodos de nivel n.
    ArrayList al = newArrayList();
    if (isEmptyT(t))
    {
        return al;
    }
    if (n==0)
    {
        add(rootT(t),al);
        return al;
    }

    return Append(levelN((n-1), left(t)) , levelN((n-1), right(t)));
}