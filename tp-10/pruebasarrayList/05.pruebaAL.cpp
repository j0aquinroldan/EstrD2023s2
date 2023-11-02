#include <iostream>
#include "ArrayList.h"
using namespace std;

int main()
{
    ArrayList al = newArrayListWith(2);

    cout << endl
         << "agregar tres numeros en lista de cap 2 (test duplicar)" << endl;
    add(1, al);
    add(2, al);
    add(3, al);
    cout << get(0, al) << endl;
    cout << get(1, al) << endl;
    cout << get(2, al) << endl;
}