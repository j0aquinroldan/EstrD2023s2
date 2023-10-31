#include <iostream>
#include "Par.h"
#include "Fraccion.h"
using namespace std;

/*
Ejercicio 2
Indicar el propósito de los siguientes procedimientos o funciones, dando algunos ejemplos de uso
junto con su resultado. Considerar el consumo de memoria de cada programa, y si puede mejorarse
*/

// 1.
// Proposito: imprime los caracteres desde el primero dado por parametro hasta el segundo
// Precondición: c1 < c2
/*
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

int ft2 (int n, int m ){
   int x = n;
   while(n < m ){
       x += x + 1;
       n++;
   }
   return x;
}
*/

/*
Ejercicio 3
Dada la estructura de pares representada como struct en C++, definir las siguientes funciones
sobre pares. Recordar probar las implementaciones en un procedimiento main.
;
*/

// Propósito: construye un par
Par consPar(int x, int y)
{
    Par p;
    p.x = x;
    p.y = y;
    return p;
}

// Propósito: devuelve la primera componente
int fst(Par p)
{
    return p.x;
}

// Propósito: devuelve la segunda componente
int snd(Par p)
{
    return p.y;
}

// Propósito: devuelve la mayor componente
int maxDelPar(Par p)
{
    if (p.x > p.y)
    {
        return p.x;
    }
    else
    {
        return p.y;
    }
}

// Propósito: devuelve un par con las componentes intercambiadas
Par swap(Par p)
{
    int nuevoY = p.x;
    p.x = p.y;
    p.y = nuevoY;
    return p;
}

// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
Par divisionYResto(int n, int m)
{
    Par p;
    p.x = n / m;
    p.y = n % m;

    return p;
}

/*
Ejercicio 4
Dar dos implementaciones para las siguientes funciones, una iterativa y otra recursiva, y utilizando
la menor cantidad posible de variables. Recordar definir subtareas en caso de que sea estrictamente
necesario.






*/

// 1.
void printN(int n, string s)
{
    // Propósito: imprime n veces un string s.
    if (n > 0)
    {
        std::cout << s;
        printN((n - 1), s);
    }
}

void iprintN(int n, string s)
{
    // Propósito: imprime n veces un string s.
    for (int i; i <= n; i++)
    {
        cout << s;
    }
}

// 2.
void cuentaRegresiva(int n)
{
    // Propósito: imprime los números desde n hasta 0, separados por saltos de línea.

    if (n >= 0)
    {
        cout << n;
        cuentaRegresiva(n - 1);
    }
}

void icuentaRegresiva(int n)
{
    // Propósito: imprime los números desde n hasta 0, separados por saltos de línea.

    while (n >= 0)
    {
        cout << n;
        n--;
    }
}

// 3.

void desdeMHastaN(int m, int n)
{
    if (m <= n)
    {
        cout << m;
        desdeMHastaN((m + 1), n);
    }
}

void desdeCeroHastaN(int n)
{
    // Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
    desdeMHastaN(0, n);
}

///////////////////////////////////
void idesdeCeroHastaN(int n)
{
    // Propósito: imprime los números de 0 hasta n, separados por saltos de línea.

    for (int m = 0; m <= n; m++)
    {
        cout << m;
    }
}

// 4.
int mult(int n, int m)
{
    // Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
    if (n == 0 || m == 0)
    {
        return 0;
    }
    return n + mult(n, (m - 1));
}

int imult(int n, int m)
{
    // Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
    int res = 0;

    while (n != 0 && m > 0)
    {
        res += n;
        m -= 1;
    }
    return res;
}
//////////////////////////////////////////////////////////

// 5.
void primerosN(int n, string s)
{
    // Propósito: imprime los primeros n char del string s, separados por un salto de línea.
    // Precondición: el string tiene al menos n char.
    if (n > s.length())
    {
        n = s.length();
    }

    if (n > 0)
    {
        cout << s[0];
        primerosN((n - 1), s.substr(1, s.length()));
    }
}

void iprimerosN(int n, string s)
{
    // Propósito: imprime los primeros n char del string s, separados por un salto de línea.
    // Precondición: el string tiene al menos n char.

    for (int i = 0; i < n; i++)
    {
        cout << s[i];
    }
}

// 6.
bool pertenece(char c, string s)
{
    // Propósito: indica si un char c aparece en el string s.
    if (s.length() == 0)
    {
        return false;
    }
    else
    {
        if (s[0] == c)
        {
            return true;
        }
        else
        {
            return pertenece(c, s.substr(1, s.length()));
        }
    }
}

bool ipertenece(char c, string s)
{
    // Propósito: indica si un char c aparece en el string s.
    bool b = false;
    int i = 0;
    while (i < s.length() && !b)
    {
        b = s[i] == c;
        i++;
    }
    return b;
}

// 7.

int unoSi(bool b)
{
    if (b)
    {
        return 1;
    }
    return 0;
}

int apariciones(char c, string s)
{
    // Propósito: devuelve la cantidad de apariciones de un char c en el string s.
    if (s == "")
    {
        return 0;
    }

    return unoSi(c == s[0]) + apariciones(c, s.substr(1, s.length()));
}

int iapariciones(char c, string s)
{
    // Propósito: devuelve la cantidad de apariciones de un char c en el string s.
    int res = 0;
    for (int i = 0; i < s.length(); i++)
    {
        res += unoSi(c == s[i]);
    }
    return res;
}

// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador)
{
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return f;
}

// Propósito: devuelve el numerador
int numerador(Fraccion f)
{

    return f.numerador;
}

// Propósito: devuelve el denominador
int denominador(Fraccion f)
{
    return f.denominador;
}

// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f)
{
    return static_cast<float>(f.numerador) / f.denominador;
}

// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2)
{
    return consFraccion(f1.numerador * f2.numerador, f1.denominador * f2.denominador);
}

int mcd(int n, int m)
{
    // PROP: calcular el mcd entre dos numeros usando el algoritmo de euclides

    while (m != 0)
    {
        int temp = m;
        m = n % m;
        n = temp;
    }

    return n;
}

// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro
Fraccion simplificada(Fraccion p)
{
    int num = numerador(p);
    int den = denominador(p);

    int n1 = max(num, den);
    int n2 = min(num, den); 

    int mcd1 = mcd(n1, n2);
    
    return consFraccion(num/mcd1, den/mcd1);
}

// Propósito: devuelve la primera componente
Fraccion sumF(Fraccion f1, Fraccion f2)
{

    int nuevoN = numerador(f1) * denominador(f2) + numerador(f2) * denominador(f1);
    int nuevoD = denominador(f1) * denominador(f2);

    return simplificada(consFraccion(nuevoN, nuevoD));
}

void fraccionTest()
{
    Fraccion f = consFraccion(1, 4);
    Fraccion f2 = consFraccion(2, 3);
    cout << numerador(f) << endl;
    cout << denominador(f) << endl;

    cout << endl<< "division"
         << division(f) << endl;

    Fraccion f3 = multF(f, f2);
    cout <<endl<< numerador(f3) << endl;
    cout << denominador(f3) << endl;

    Fraccion f4 = simplificada(consFraccion(2, 4));
    cout << endl<<numerador(f4) << endl;
    cout << denominador(f4) << endl;


    Fraccion f5= sumF(f,f4);
    cout << endl<<numerador(f5) << endl;
    cout << denominador(f5) << endl;
}

int main()
{

    fraccionTest();
}

void recIndexTest()
{
    cout << "ej1 print" << endl;
    printN(2, "hola");

    iprintN(2, "hola");

    cout << endl
         << "ej2 ceunta" << endl;
    cuentaRegresiva(5);
    cout << endl;
    icuentaRegresiva(5);

    cout << endl
         << "ej3 desde0n" << endl;
    desdeCeroHastaN(3);
    cout << endl;
    idesdeCeroHastaN(3);

    cout << endl
         << "ej4 mult" << endl;
    cout << mult(5, 5) << endl;
    cout << imult(5, 5) << endl;

    cout << endl
         << "ej5 primeros" << endl;
    primerosN(5, "joaquin");
    cout << endl;
    iprimerosN(5, "joaquin");

    cout << endl
         << endl
         << "ej6 pertenece" << endl;
    cout << pertenece('n', "joaquin") << endl;
    cout << ipertenece('j', "joaquin") << endl;

    cout << endl
         << "ej6 pertenece /falso" << endl;
    cout << pertenece('x', "joaquin") << endl;
    cout << ipertenece('x', "joaquin") << endl;

    cout << endl
         << "ej6 pertenece /3" << endl;
    cout << pertenece('j', "") << endl;
    cout << ipertenece('j', "") << endl;

    cout << endl
         << "ej7 aparicciones" << endl;
    cout << apariciones('e', "pepe") << endl;
    cout << iapariciones('e', "pepe") << endl;

    cout << endl
         << "ej7 aparicciones /2" << endl;
    cout << apariciones('x', "pepe") << endl;
    cout << iapariciones('x', "pepe") << endl;
}

void parTest()
{
    cout << "primero" << endl;

    Par p = (consPar(2, 3));
    cout << fst(p) << endl;
    cout << snd(p) << endl;
    cout << maxDelPar(p) << endl;

    cout << "segundo" << endl;
    p = swap(p);
    cout << fst(p) << endl;
    cout << snd(p) << endl;

    cout << "tercero" << endl;
    p = divisionYResto(10, 7);
    cout << fst(p) << endl;
    cout << snd(p) << endl;
}