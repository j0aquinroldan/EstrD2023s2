#include <iostream>
#include "ArrayList.h"
using namespace std;

int main()
{
    ArrayList al = newArrayList();
  
    cout << endl
    << "no se puede remover de array list vacia" << endl;
     remove(al);

}