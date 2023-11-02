#include <iostream>
#include "ArrayList.h"
// #include "usuarioArrayList.cpp"
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

void sucesores(ArrayList xs)
{

    for (int i = 0; i < lengthAL(xs); i++)
    {
        set(i, (get(i, xs) + 1), xs);
    }
}

bool pertenece(int x, ArrayList xs)
{
    // PROP: Indica si el elemento pertenece a la lista.
    for (int i = 0; i < lengthAL(xs); i++)
    {
        if (get(i, xs) == x)
        {
            return true;
        }
    }
    return false;
}

int unoSi(bool b)
{
    if (b)
    {
        return 1;
    }
    return 0;
}

int apariciones(int x, ArrayList xs)
{
    // PROP: Indica la cantidad de elementos iguales a x.
    int cont = 0;
    for (int i = 0; i < lengthAL(xs); i++)
    {
        cont += unoSi(get(i, xs) == x);
    }
    return cont;
}

ArrayList append(ArrayList xs, ArrayList ys)
{
    // PROP: Crea una nueva lista a partir de la primera y la segunda (en ese orden).
    for (int i = 0; i < lengthAL(ys); i++)
    {
        add(get(i, ys), xs);
    }
    return xs;
}

int minimo(ArrayList xs)
{
    // PROP:Devuelve el elemento más chico de la lista.
    if (isEmptyAl(xs))
    {
        perror("el array list no contiene elementos");
    }
    else
    {
        int minTemp = get(0, xs);
        for (int i = 1; i < lengthAL(xs); i++)
        {
            minTemp = min(minTemp, get(i, xs));
        }
        return minTemp;
    }
}

//////////////////////////////////////// TESTS //////////////////////////////////////////////////////
void testSumatoria()
{
    ArrayList al = newArrayList();
    cout << "TEST SUMATORIA" << endl;
    add(1, al);
    add(2, al);
    add(3, al);
    add(4, al);
    add(5, al);

    cout << (sumatoria(al) == 15) << endl;
}

void testSucesores()
{
    ArrayList al = newArrayList();
    cout << "TEST sucesores" << endl;
    add(1, al);
    add(2, al);
    add(3, al);
    add(4, al);
    add(5, al);
    sucesores(al);

    cout << get(0, al) << endl;
    printAL(al);
}

void testPertenece()
{
    ArrayList al = newArrayList();
    cout << "TEST pertenece" << endl;
    add(1, al);
    add(2, al);
    add(3, al);
    add(4, al);
    add(5, al);

    cout << "caso valido: " << pertenece(5, al) << endl;
    cout << "caso invalido: " << pertenece(7, al) << endl;
}

int testApariciones()
{
    ArrayList al = newArrayList();
    cout << "TEST apariciones" << endl;
    add(1, al);
    add(1, al);
    add(3, al);
    add(1, al);
    add(3, al);
    add(4, al);

    cout << "apariciones de 1: " << apariciones(1, al) << endl;
    cout << "apariciones de 3: " << apariciones(3, al) << endl;
    cout << "apariciones de 4: " << apariciones(4, al) << endl;
    cout << "apariciones de 5: " << apariciones(5, al) << endl;
}

void testAppend()
{
    ArrayList al = newArrayList();
    cout << "TEST append" << endl;
    add(1, al);
    add(2, al);
    add(3, al);
    add(4, al);
    add(5, al);

    ArrayList al2 = newArrayList();
    add(6, al);
    add(7, al);
    add(8, al);
    add(9, al);
    add(10, al);

    ArrayList resAL = append(al, al2);

    printAL(resAL);
}

void testMinimo()
{
    ArrayList al = newArrayList();
    cout << "TEST minimo" << endl;
    add(1, al);
    add(2, al);
    add(3, al);
    add(4, al);
    add(5, al);
    cout << (1 == minimo(al) )<< endl;
    ArrayList al2 = newArrayList();
    add(6, al2);
    cout << (1 == minimo(al2) )<< endl;
}

int main()
{
    cout << boolalpha;

    // testSumatoria();
    // cout << endl;
    // testSucesores();
    // cout << endl;
    // testPertenece();
    // cout << endl;
    // testApariciones();
    // cout << endl;
    // testAppend();
    cout << endl;
    testMinimo();
}
