#include <iostream>
#include "ArrayList.h"
using namespace std;

int main()
{
    ArrayList al = newArrayList();
    cout <<"cantidad: " << lengthAL(al) << endl;
    cout << get(1, al);
}