#include <iostream>
#include "ArrayList.h"
using namespace std;

int main()
{
    ArrayList al = newArrayList();
    
    cout << "no se puede setear un lugar vacio" << endl;
   // set(1, 5, al);

    cout<<"set valido"<<endl;
    add(1,al);
    cout<<get(0,al)<<endl;
    set(0,5,al);
    cout<<get(0,al)<<endl;

}