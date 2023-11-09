#include "Set.h"
#include <iostream>
using namespace std;

int main(){
    Set s = emptyS();
    cout<< sizeS(s)<<endl;

    AddS(1,s);
    cout<< sizeS(s)<<endl;
    
    AddS(1,s);
    cout<< sizeS(s)<<endl;

    // AddS(2,s);
    
    // cout<< sizeS(s)<<endl;
}