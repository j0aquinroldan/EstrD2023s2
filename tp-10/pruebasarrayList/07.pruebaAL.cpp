#include <iostream>
#include "ArrayList.h"
using namespace std;

int main()
{
    ArrayList al = newArrayListWith(2);

    cout << endl
         << "(test printAL) imprime toda la array list" << endl;
    add(1, al);
    add(2, al);
    add(3, al);
    printAL(al);
}