#include <iostream>
#include "ArrayList.h"
using namespace std;

int main()
{
    ArrayList al = newArrayListWith(2);

    cout << endl
         << "resize 2 sobre lista de cap 3 (error get 2)(test resize)" << endl;
    add(1, al);
    add(2, al);
    add(3, al);
    resize(2,al);
    cout << get(0, al) << endl;
    cout << get(1, al) << endl;
    cout << get(2, al) << endl;
}