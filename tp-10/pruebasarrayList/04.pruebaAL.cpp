#include <iostream>
#include "ArrayList.h"
using namespace std;

int main()
{
    ArrayList al = newArrayList();

    cout << endl
         << "agregar un numero" << endl;
    add(1, al);
    cout << get(0, al) << endl;
}