struct ArrayListSt
{
    int cantidad; 
    int* elementos;
    int capacidad;
};

typedef ArrayListSt* ArrayList;

ArrayList newArrayList();
ArrayList newArrayListWith(int capacidad);
int lengthAL(ArrayList xs);
int get(int i, ArrayList xs);
void set(int i, int x, ArrayList xs);
void resize(int c, ArrayList xs);
void add(int x, ArrayList xs);
void duplicarCapacidad(ArrayList xs);
void remove(ArrayList xs);

void printAL(ArrayList xs);
bool isEmptyAl(ArrayList xs);
