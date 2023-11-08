struct NodoL;

struct LinkedListSt;

typedef LinkedListSt* LinkedList; // INV.REP.: el puntero NO es NULL

struct IteratorSt;
typedef IteratorSt* ListIterator; // INV.REP.: el puntero NO es NULL



LinkedList nil();
//PROP: Crea una lista vacía.

bool isEmpty(LinkedList xs);
//PROP: Indica si la lista está vacía.

int head(LinkedList xs);
//PROP: Devuelve el primer elemento.

void Cons(int x, LinkedList xs);
//PROP: Agrega un elemento al principio de la lista.

void Tail(LinkedList xs);
//PROP: Quita el primer elemento.

int length(LinkedList xs);
//PROP: Devuelve la cantidad de elementos.

void Snoc(int x, LinkedList xs);
//PROP: Agrega un elemento al final de la lista.

ListIterator getIterator(LinkedList xs);
//PROP: Apunta el recorrido al primer elemento.

int current(ListIterator ixs);
//PROP: Devuelve el elemento actual en el recorrido.

void SetCurrent(int x, ListIterator ixs);
//PROP: Reemplaza el elemento actual por otro elemento.

void Next(ListIterator ixs);
//PROP: Pasa al siguiente elemento.

bool atEnd(ListIterator ixs);
//PROP: Indica si el recorrido ha terminado.

void DisposeIterator(ListIterator ixs);
//PROP: Libera la memoria ocupada por el iterador.

void DestroyL(LinkedList xs);
//PROP: Libera la memoria ocupada por la lista.

void Append(LinkedList xs, LinkedList ys);
    // PROP:Agrega todos los elementos de la segunda lista al final de los de la primera.
    // La segunda lista se destruye.
