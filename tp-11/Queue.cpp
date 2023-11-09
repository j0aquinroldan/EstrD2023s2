#include "Queue.h"
#include <iostream>

struct NodoQ {
int elem; // valor del nodo
NodoQ* siguiente; // puntero al siguiente nodo
};
struct QueueSt {
int cantidad; // cantidad de elementos
NodoQ* primero; // puntero al primer nodo
NodoQ* ultimo; // puntero al ultimo nodo
};


Queue emptyQ(){
//PROP: Crea una lista vacía.
//Costo: O(1).
    Queue q = new QueueSt;
    q->cantidad = 0;
    q->primero  = NULL;
    q->ultimo   = NULL;
    return q;
}

bool isEmptyQ(Queue q){
//PROP: Indica si la lista está vacía.
//Costo: O(1).
    return q->cantidad ==0;
}

int firstQ(Queue q){
//PROP: Devuelve el primer elemento.
//Costo: O(1).
    if (q->cantidad ==0)
    {
        perror("la queue esta vacia");
    }
    else{
        return q->primero->elem;
    }
}

void Enqueue(int x, Queue q){
//PROP: Agrega un elemento al final de la cola.
//Costo: O(1).

    NodoQ* n = new NodoQ;
    n->elem = x;
    n->siguiente = NULL;
    q->ultimo->siguiente= n;
}

void Dequeue(Queue q){
//PROP: Quita el primer elemento de la cola.
//Costo: O(1).
    q->primero = q->primero->siguiente;
    delete q->primero;
}

int lengthQ(Queue q){
//PROP: Devuelve la cantidad de elementos de la cola.
//Costo: O(1).
    return q->cantidad;
}

void MergeQ(Queue q1, Queue q2){
//PROP: Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
//Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
//Costo: O(1).
    q1->ultimo->siguiente = q2->primero;
    delete q2;
}

void DestroyQ(Queue q){
//PROP: Libera la memoria ocupada por la lista.
//Costo: O(n).

    NodoQ* temp = q->primero;
    while (q->primero != NULL)
    {
        q->primero = q->primero->siguiente;
        delete temp;
    }
    delete q;
    
}