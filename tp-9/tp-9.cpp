/*
Ejercicio 2
Indicar el propósito de los siguientes procedimientos o funciones, dando algunos ejemplos de uso
junto con su resultado. Considerar el consumo de memoria de cada programa, y si puede mejorarse
*/

 // 1. 
 // Proposito: imprime los caracteres desde el primero dado por parametro hasta el segundo 
 // Precondición: c1 < c2
void printFromTo(char c1, char c2) {
    for(int i = 0; c1 + i <= c2; i++) {
        cout << c1 + i << ", ";
    }
    cout << endl;
}

//2. 
// Proposito: calcula el factorial de n 
// Precondición: n >= 0
int fc(int n) {
    int x = 1;
    while(n > 0) {
        x = x * n;
        n--;
    }
    return x;
}

//3. 
// Proposito: calcula la sumatoria de todos los enteros entre n y m
// Precondición: n <= m
int ft(int n, int m) {
    if (n == m) {
        return n;
    }
    return n + ft(n+1, m);
}


/*
Ejercicio 3
Dada la estructura de pares representada como struct en C++, definir las siguientes funciones
sobre pares. Recordar probar las implementaciones en un procedimiento main.
;
*/

struct Par {
    int x;
    int y;
}

// Propósito: construye un par
Par consPar(int x, int y){
    Par p = struct Par;
    p.x = x;
    p.y = y;
    return p;
}


// Propósito: devuelve la primera componente
int fst(Par p){
    return p.x;
}



// Propósito: devuelve la segunda componente
int snd(Par p){
    return p.y;
}



// Propósito: devuelve la mayor componente
int maxDelPar(Par p){
    if (p.x > p.y){
        return p.x;
    }
    else{ 
        return p.y;
    }
}


// Propósito: devuelve un par con las componentes intercambiadas
Par swap(Par p){
   int nuevoX = p.y;
   int nuevoY = p.x;
   p.x = nuevoX;
   p.y = nuevoY;
   return p; 

}

// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
Par divisionYResto(int n, int m){
    Par p = struct Par;
    p.x = n DIV m;
    p.y = n MOD m;
    
    return p;
}


/*
Ejercicio 4
Dar dos implementaciones para las siguientes funciones, una iterativa y otra recursiva, y utilizando
la menor cantidad posible de variables. Recordar definir subtareas en caso de que sea estrictamente
necesario.



4. int mult(int n, int m)
Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
5. void primerosN(int n, string s)
Propósito: imprime los primeros n char del string s, separados por un salto de línea.
Precondición: el string tiene al menos n char.
6. bool pertenece(char c, string s)
Propósito: indica si un char c aparece en el string s.
7. int apariciones(char c, string s)
Propósito: devuelve la cantidad de apariciones de un char c en el string s.


*/


//1. 
void printN(int n, string s){
//Propósito: imprime n veces un string s.
    if(n>0){
        cout <<s>>;
        printN ((n-1), s);
    }
}

void iprintN(int n, string s){
//Propósito: imprime n veces un string s.
    for(int i; i<=n; i++){
        cout <<s>>;
    }
}



//2. 
void cuentaRegresiva(int n){
//Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
    
    if(n>=o){
        cout <<n>>;
        cuentaRegresiva (n-1);
    }
}

void icuentaRegresiva(int n){
//Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
    
    while (n>=0){
        cout <<n>>;
        n--;
    }
}


//3. 
void desdeCeroHastaN(int n){
//Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
    desdeMHastaN( 0, n);

}

void desdeMHastaN (int m, int n){
    if(m<=n){
        cout<<m>>
        desdeMHastaN((m+1),n)
    }
}

///////////////////////////////////
void idesdeCeroHastaN(int n){
//Propósito: imprime los números de 0 hasta n, separados por saltos de línea.

    for(int m=0; m<=n; m++){
        cout<<m>>;
    }
}
