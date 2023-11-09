struct NodoQ ;
struct QueueSt;
typedef QueueSt* Queue;


Queue emptyQ();
//PROP: Crea una lista vacía.
//Costo: O(1).
bool isEmptyQ(Queue q);
//PROP: Indica si la lista está vacía.
//Costo: O(1).
int firstQ(Queue q);
//PROP: Devuelve el primer elemento.
//Costo: O(1).
void Enqueue(int x, Queue q);
//PROP: Agrega un elemento al final de la cola.
//Costo: O(1).
void Dequeue(Queue q);
//PROP: Quita el primer elemento de la cola.
//Costo: O(1).
int lengthQ(Queue q);
//PROP: Devuelve la cantidad de elementos de la cola.
//Costo: O(1).
void MergeQ(Queue q1, Queue q2);
//PROP: Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
//Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
//Costo: O(1).
void DestroyQ(Queue q);
//PROP: Libera la memoria ocupada por la lista.
//Costo: O(n).
