#include "Tree.h"
#include <iostream>

struct NodeT
{
    int elem;
    NodeT *left;
    NodeT *right;
};

Tree emptyT()
{
    NodeT *n = new NodeT;
    n->elem = NULL;
    n->left = NULL;
    n->right = NULL;
    return n;
}

Tree nodeT(int elem, Tree left, Tree right)
{
    NodeT *n = new NodeT;
    n->elem = elem;
    n->left = left;
    n->right = right;
    return n;
}
bool isEmptyT(Tree t){
    return (t->elem == NULL) &&  (t->left == NULL) && (t->right = NULL);
}

int rootT(Tree t){
    return t->elem;
}

Tree left(Tree t){
    return t->left;
}
Tree right(Tree t){
    return t->right;
}